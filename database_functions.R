trace_dat      <- FALSE
# Notes
# 111: Looks OK
# 112: Internal not found: Data is in database in Internal is present (UF: 339)
# 113: Internal wrong: The score and weight cannot be deduced from the spreadsheet
# 114: Ditto (MTT + Q)
# 115: Seems OK (but double-check against spreadsheet)
mdebug <- function(s,
                   caller    = "No caller supplied",
                   force     = FALSE,
                   debug_log = FALSE) {

  mess <- sprintf("(%25s) [%s] %s", caller, format(Sys.time()), s)
  if ( !length(str_length(mess)) ) {
    message(sprintf("Empty argument supplied when called from %s",
                    deparse(sys.calls()[[sys.nframe() ]])))
  }

  if ( DEBUG | force | trace_dat | debug_log) {
    message(mess)
    if ( debug_log | force  )
      write(sprintf("%s", mess), file = "debug.log", append = TRUE)
  }
}

get_db_server <- function() {
  if_else(on_server(), 'mariadb.marketing-ebreporter', 'localhost')
                       #mariadb.marketing-ebreporter
}

get_con <- function() {
  mdebug(sprintf("Getting connection to database server"), "get_con")
  rv <- try(robust.system("mariadb --help"), silent = TRUE)

  if (rv$exitStatus == 0 ) {
    return(dbConnect(RMariaDB::MariaDB() ,
                     user     = 'marketing',
                     password = 'rhcb2233',
                     dbname   = 'marketing',
                     host     = get_db_server()))
  }  else {
    mdebug(sprintf("No server process detected! Content of stderr: %s", rv$stderr),
           "get_con", debug_db | DEBUG)
    return(NULL)
  }

}

get_qry <- function(qry, db = db) {
  #if (input$is_emergency)
  #  return(NULL)

  mdebug(sprintf("Executing query: %s", qry), "get_qry", debug_db | DEBUG | TRUE)
  db <- get_con()
  if (is.null(db)) {
    return(NULL)
  } else {
    v <- as_tibble(dbGetQuery(db, qry))
    dbDisconnect(db)
    v
  }
}

run_qry <- function(qry, db = db) {
  mdebug(sprintf("Executing query: %s", qry), "get_qry", debug_db)
  db <- get_con()
  if (is.null(db)) {
    return(NULL)
  } else {
    v <- dbGetQuery(db, qry)
    dbDisconnect(db)
    v
  }
}

get_tab <- function(tab_s, db = NULL) {
  if (is.null(db))
    db <- get_con()

  #mdebug(sprintf("Fetching '%s'", tab_s), "get_tab", DEBUG)
  v <- as_tibble(dbReadTable(db, tab_s))
  mdebug(sprintf("Fetched %7s rows from table: '%s'", fmt_n(nrow(v)), tab_s), "get_tab", DEBUG | debug_db)
  dbDisconnect(db)

  v
}

set_tab <- function(tab_name, tab_value, overwrite = TRUE,
		    ids = NULL, db = NULL, print_rows = 0, debug_db = FALSE) {

  if (is.null(db))
    db <- get_con()

  ft <- get_db_fields(tab_name)

  mdebug(sprintf("About to write %s to DB ...", tab_name), "set_tab", force = debug_db)

  if (overwrite) {
    mdebug(sprintf("Over-writing complete table (%s)",
		  tab_name), "set_tab", force = debug_db );
    dbExecute(db, sprintf(" delete from %s", tab_name))
    dbWriteTable(db, tab_name, tab_value, overwrite = FALSE, append = TRUE)
    if (tab_name == 'assessments' & FALSE) {
      dbExecute(db, "CREATE INDEX ass_p ON assessments (paper)")
      dbExecute(db, "CREATE INDEX ass_y ON assessments (year)")
      dbExecute(db, "CREATE INDEX ass_s ON assessments (semester)")
    }
  } else {
    mdebug(sprintf("Appending %s records to %s", fmt_n(nrow(tab_value)), tab_name),
           "set_tab", force = debug_db | TRUE)
    if (print_rows > 0) {
      if (is.null(ids))
        print(head(tab_value, print_rows))
      else
        print(head(tab_value |> dplyr::filter(ID %in% ids), print_rows))
    }

    dbWriteTable(db, tab_name, tab_value,                    append = TRUE)

    }

  mdebug("returning", "set_tab")
  dbDisconnect(db)
}


update_results <- function(dd) {
  mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
         "update_results", force = debug_flicker)

  ####### Add scaled results to results table #####
  # FIXME: add Scaled to database results tab: read all results then mutate ifelse?
  # or SQL update? Can this deal with a list of values?
  if (FALSE) {
    qry <- glue::glue_sql("UPDATE results
                              SET Scaled = ??
                              WHERE Paper = %paper AND Year = %year AND semester = %sem")

    write_csv( dd$results %>% select(ID, Name, Total, Scaled),
               sprintf("Data/Exports/%s_%s_%s.csv", dd$paper, dd$year, dd$semester))
  } else {
    # Add to totals over all papers, instances if they are not already there
    # Writing this table takes several seconds, so avoid it if possible!
    ttab <- get_tab("all_totals")

    ed <- ttab |> #Existing data
      dplyr::filter(   (Paper == dd$paper & Year == dd$year & Semester == dd$semester))

    at <- ttab |> # All data, excluding the current paper's data
      dplyr::filter( !(Paper == dd$paper & Year == dd$year & Semester == dd$semester)) |>
      select(ID, Mark, Year, Semester, Paper, Grade) |>
      mutate(ID   = as.integer(ID),
             Year = as.integer(Year))

    new_rows <- dd$results |>
      select(ID, Name, Mark = Scaled) |>
      mutate(Year     = as.integer(dd$year),
             Semester = dd$semester,
             Paper    = dd$paper,
             Grade    = "",
             ID       = as.integer(ID))

    if (nrow(new_rows))
      at <- bind_rows(at, new_rows)

    if (nrow(new_rows) != nrow(ed) | nrow(ed) == 0 ) {
      new_marks <- new_rows |> arrange(ID) |> pull(Mark)
      old_marks <- ed       |> arrange(ID) |> pull(Mark)
      if (length(new_marks) == length(old_marks))
        are_equal <- all(new_marks == old_marks)
      else
        are_equal <- FALSE

      if (are_equal && nrow(ed) > 1 ) {
        mdebug(sprintf("Not replacing existing data in all_totals (%d existing identical rows)",
                       sum(new_marks == old_marks)),
               "ScaledPlot")
      } else {
        mdebug(sprintf("Starting to update all_totals for %s (%s, %s) to the  database",
                       new_rows$Paper[1], new_rows$Semester[1], new_rows$Year[1]),
               "ScaledPlot", force = (debug_flicker | TRUE ) )
      }
    }
  }
}

update_tab <- function(tab_name, tab_value, paper, year, semester,
		       assessment, ids = NULL, db = NULL) {
  if (is.null(db))
    db <- get_con()

  message(sprintf("About to update records in table: '%s'", tab_name))
  write_log <- function(m, recs) {
    if (tab_name == "results")
      write_lines(sprintf(paste0("%s: ", m, " for %s"),
                          format(Sys.time()),
                          recs,
                          paper),
                  file = "Data/Imports/import_log.txt",
                  append = TRUE)
  }

  if (tab_name == 'results')
    ass_fld <- 'Assessment'
  else
    ass_fld <- 'Title'

  qry <- sprintf("DELETE FROM %s WHERE paper = '%s' AND year = '%s' AND semester = '%s' AND %s = '%s'",
		 tab_name, paper, year, semester, ass_fld, assessment)

  message(sprintf("About to execute %s", qry))
  ed <- dbExecute(db, qry)

  if ( FALSE ) {
    if (tab_name == "results")
      message(sprintf("(update_tab): Not overwriting existing data for '%s' ('%s', '%s')", paper, year, semester))
      write_log("Didn't add records (there are %4d existing records)", nrow(ed))
  } else{
    if (nrow(tab_value) > 0) {
      message("   ==> (update_res) Updating these rows")
      if (is.null(ids))
	  tv <- tab_value
      else
          tv <- tab_value %>% dplyr::filter(ID %in% ids)
      set_tab(tab_name, tv, overwrite = FALSE, ids)
      message("Also success")
    }
  }
  message('(update_res) returning')
  dbDisconnect(db)
}

# Delete all data prior to S2 2020? Or just delete all results and assessments
# tables? Great decision to decouple results, assessments and reports, John!


get_new <- function() {
  # As at 3/12/20, data only contains final result for papers prior to 2014
  read_csv("~/Downloads/all_mart_results.csv")
}

get_name <- function(sur, fir, kno) {
  require(stringr)

  name <- paste0(str_to_title({{sur}}), ", ", str_to_title({{fir}}))

  ifelse(!is.na({{kno}}) & {{kno}} != {{fir}},
         paste0(name, " (", str_to_sentence({{kno}}), ")"),
         name)
}

load_dump <- function(new_stuff, reload = FALSE) {
  if(reload)
    new_stuff <- get_new()

  new_results <- new_stuff %>%
    select(Assessment = assessment_name,
           ID = student_id,
           Mark = assessment_mark,
           Grade = assessment_grade,
           paper = paper_id,
           year = academic_year,
           first_name,
           sur_name,
           known_name,
           semester = teaching_period) %>%
    mutate(Mark = Mark/100,
           Name = get_name(sur_name, first_name, known_name)) %>%
    select(-sur_name, -first_name, -known_name) %>%
    arrange(desc(year), semester, paper)

  new_assessments <- new_stuff %>%
    select(Title = assessment_name,
           Weight = assessment_weight,
           paper = paper_id,
           year = academic_year,
           semester = teaching_period) %>%
    mutate(is_group = 0) %>%
    arrange(desc(year), semester, paper) %>%
    distinct

  new_students <- new_stuff %>%
    select(
      student_id,
      sur_name,
      first_name,
      known_name,
      contact_number,
      university_email
    ) %>%
    distinct %>%
    mutate(name = get_name(sur_name, first_name, known_name)) %>%
    select(-sur_name, -first_name, -known_name)


  new_stuff <- list(new_results     = new_results,
                    new_assessments = new_assessments,
                    new_students    = new_students)

  #set_tab("results",     new_stuff$new_results)
  #set_tab("assessments", new_stuff$new_assessments)
  #set_tab("students",    new_stuff$new_students)

  return(invisible(new_stuff))
}

get_db_fields <- function(tab_name) {
  all_ft <- c( Paper = "Char(7)", Year = "Int", Semester = "Char(12)", ID = "Double", Mark = "Int", Grade = "Char(2)", Name = "Long")
  res_ft <- c( paper = "Char(7)", year = "Int", semester = "Char(12)", ID = "Double", Mark = "Int", Grade = "Char(2)", Name = "Long",
               Assessment = "Long")
  ass_ft <- c( paper = "Char(7)", year = "Int", semester = "Char(12)", Title = "Long",  Weight = "Int", is_group = "Int")
  rep_ft <- c( paper = "Char(7)", year = "Int", semester = "Char(12)", staff = "Long",  group_work = "Long",  exam_format = "Long",
               scaling = "Long",  misconduct = "Long",  feedback_channels = "Long",  feedback_issues = "Long",  feedback_actions = "Long",
               report = "Long",   changes = "Long",  scale_what = "Char(12)",  scale_add = "Int",  scale_mul = "Int",  scale_thr = "Int",
               coordinator = "Long",  title = "Long" )
  ins_ft <- c( Paper = "Char(7)", Year = "Int", Semester = "Char(12)", instance = "Char(255)", Coordinator = "Char(255)", title = "Char(255)")

  stu_ft <- c(ID = "Double", Surname = "Char(255)", Firstname = "Char(255)", Knownname = "Char(255)",
              Gender = "Char(255)", Ethnicity = "Char(255)", IWI = "Char(255)",
              Email = "Char(255)", Mobile = "Char(255)",
              Programme = "Char(255)", Programme2 = "Char(255)")

  if (tab_name == "all_totals")
    return(all_ft)
  if (tab_name == "results")
    return(res_ft)
  if (tab_name == "assessments")
    return(ass_ft)
  if (tab_name == "reports")
    return(rep_ft)
  if (tab_name == "instances")
    return(ins_ft)
  if (tab_name == "students")
    return(stu_ft)

  return(NULL)
}

migrate <- function() {
  require(RMariaDB)

  db_l <- get_con('local')
  db_r <- get_con('remote')

  tabs_source <- dbListTables(db_l, "marketing")

  for (i in 1:length(tabs_source)) {
    tab_name  <- tabs_source[i]
    if (tab_name == "mailchimp")
      next

    message(sprintf("Migrating '%s' to server", tab_name))
    tab_value <- tibble(dbReadTable(db_l, tab_name))

    ft <- get_db_fields(tab_name)
    dbWriteTable(db_r, tab_name, tab_value, overwrite = TRUE, field.types = ft)

  }

  tabs_sink <- dbListTables(db_r, "marketing")
  message("Tables on server:")
  print(tabs_sink)

  dbDisconnect(db_l)
  dbDisconnect(db_r)
}

bud <- function() {
  db_local  <- get_con('local')
  db_server <- get_con('remote')
  tabs <- dbListTables(db_server, "marketing")
  for (tab in tabs) {
	  tab_dat <- tibble(dbReadTable(db_server, tab))
	  dbWriteTable(db_local, tab, tab_dat, overwrite = TRUE)
  }
  dbDisconnect(db_local)
  dbDisconnect(db_server)
}

get_report <- function(t_paper, t_year, t_semester) {
  get_reports() |> filter(paper == t_paper, year == t_year, semester == t_semester)

  #  report <- get_qry(sprintf("SELECT * FROM reports WHERE paper = '%s' AND year = '%s' and semester = '%s'",
  #                          paper, year, semester))
  # return(as_tibble(report))
}

get_year <- function() {
  str_sub(Sys.Date(), 1, 4)
}

fix_dup_ass <- function(bad_paper, bad_year = "2021", bad_semester = "S2") {
  atab <- get_tab("assessments")
  nd <- atab %>%
    dplyr::filter(paper == bad_paper, year == bad_year, semester == bad_semester) %>%
    distinct(paper, year, semester, Title, Weight) %>%
    mutate(is_group = 0)
  ed <- atab %>%
    dplyr::filter(!(paper == bad_paper & year == bad_year & semester == bad_semester))
  set_tab("assessments", ed %>% add_row(nd))


}

restore_report <- function(paper = "MART205", dir = "Data/Reports_Backup/", year = "2021", semester = "S2") {

  files <- list.files(path = dir, pattern = paste0(paper, "_", year, "_", semester))
  reports <- tibble(file = "", len = NA)

  for (fn in files) {
    load(paste0(dir, fn))

    rep_len <- str_length(paste(report, collapse = "; "))

    reports <- reports %>%
      add_row(tibble(file = fn, len = rep_len))
  }

  rv <- reports[-1,]
  rv %>% arrange(desc(len))
}

get_current_pgs <- function() {
  rv <- get_results() |>
    dplyr::filter(grepl("MART4", paper),
                  grepl("Result", Assessment),
                  year %in% get_year(),
                  complete.cases(ID)) |>
    mutate(Mark = round(Mark)) |>
    select(ID, Name, paper, Mark) |>
    distinct(ID) |>
    pull(ID)
}

postgrad_sheet <- function(this_year = get_year()) {
  require(dplyr)
  last_year <- as.numeric(this_year) - 1

  rtab <- get_results()
  pg_tab <- rtab |> dplyr::filter(grepl("MART4", paper),
                  grepl("Result", Assessment),
                  year %in% c(this_year, last_year),
                  complete.cases(ID)) |>
    mutate(Mark = round(Mark)) |>
    select(ID, Name, paper, Mark) |>
    pivot_wider(id_cols     = c(1,2),
                names_from  = paper,
                values_from = Mark,
                values_fn   = {max}) |>
    arrange(Name)

  pg_tab <- pg_tab |>
    mutate(Papers    = apply(pg_tab[, -c(1:2)], 1, function(x) sum(complete.cases(x))),
           Mean      = round(rowMeans(pg_tab[-c(1:2)], na.rm = TRUE)),
           Firstname = get_name_part(Name, "firstname"),
           Surname   = get_name_part(Name)
    ) |>
    select(-Name, everything()) |>
    dplyr::filter(Papers >= 1, ID %in% get_current_pgs())

  pg_papers <- c(sort(grep("MART46", names(pg_tab), value = TRUE)))

  stud_prog <- get_students() |>
    select(ID, Programme, Programme2) |>
    mutate(
      Programme = ifelse(grepl("^Bachelor of C", Programme),
			 Programme2, Programme),
      Programme = case_when(
        grepl("^Master of M",   Programme) ~ "MMart",
        grepl("^Master of B",   Programme) ~ "MBusDSc",
        grepl("^Master of I",   Programme) ~ "MIB",
        grepl("^Master of A",   Programme) ~ "MAppSc",
        grepl("^Cert",          Programme) ~ "CoP",
        grepl("^Postg"   ,      Programme) ~ "PGDip",
        grepl("^Dipl"       ,   Programme) ~ "DipGrad",
        grepl("^Bachelor of A", Programme) ~ "BAppSc",
        TRUE                            ~ "???"
      ),
      ID  = as.numeric(ID))

  rv2 <- pg_tab |>
    left_join(stud_prog, by = "ID") |>
    dplyr::filter(complete.cases(Programme)) |>
    select(-Programme2) |>
    mutate(Award = ifelse(Programme == "PGDip",
			  ifelse(Mean >= 80,
				 "Dist",
				 ifelse(Mean >= 70, "Cred", "")
    ), "")) |>
    select(ID, Surname, Firstname, Programme, Award, Mean,
	   all_of(pg_papers), Papers)

  dupes <- rv2 |>
    count(ID) |>
    dplyr::filter(n > 1) |>
    pull(ID)

  dtab <- rv2 |>
    dplyr::filter(ID %in% dupes)

  urows_n <- dtab |>
    group_by(ID) |>
    summarise(across(starts_with("MART"),~ mean(.x, na.rm = TRUE)))

  urows_s <- dtab |>
    select(ID, !starts_with("MART")) |>
    distinct(Surname, .keep_all = TRUE)

  urows <- urows_s |>
    left_join(urows_n, by = "ID")

  urows <- urows |> relocate(Papers, .after = names(urows)[ncol(urows)])

  urows <- urows |>
	  mutate(Mean = round(rowMeans(urows |>
				       select(starts_with("MART")),
			       na.rm = TRUE)))

  tab_a <- rv2 |> arrange(Surname) |> dplyr::filter(!ID %in% dupes)
  rv <- tab_a |> add_row(urows)
  rv <- rv |>
    mutate(Papers = rv |>
             rowwise() |>
             summarise(across(starts_with("MART"), ~!is.na(.x))) |>
             rowSums(),
           Programme = factor(Programme,
                              levels = c("MMart", "PGDip",
                                         "DipGrad", "CoP",
                                         "MBusDSc",
                                         "MAppSc", "BAppSc",
                                         "MIB"))
    ) |>
    arrange(Programme, Surname)

  return(invisible(rv))
}

make_name <- function(s) {
  Firstname <- str_extract_all(s, "[A-Za-z\\-]+", simplify = TRUE)[,1]
  Surname   <- str_extract_all(s, "[A-Za-z\\-]+", simplify = TRUE)[,2]
  paste(Firstname, Surname)
}

get_name_part <- function (name_list, part = "surname") {
  forename_pat_comma <- ", [A-Za-z\\-]+"
  surname_pat_comma  <-  "([vV]an )*[A-Za-z\\-]+, "

  forename_pat_space <- "[A-Za-z\\-]+ "
  surname_pat_space  <-  " ([vV]an )*[A-Za-z\\-]+$"

  #
  for (i in 1:length(name_list)) {
    f_pat <- ifelse( str_detect(name_list[i], ", "), forename_pat_comma, forename_pat_space)
    s_pat <- ifelse( str_detect(name_list[i], ", "),  surname_pat_comma,  surname_pat_space)
    pat <- ifelse(part == "firstname", f_pat, s_pat)
    #cat(sprintf("Looking for %s in '%s', so using '%s' ... ", part, name_list[i], pat))
    name_list[i] <- str_squish(str_remove(str_extract(name_list[i], pattern = pat), ","))
    #cat(sprintf("Got: '%s'\n",name_list[i]))
  }
  name_list
}

checkit <- function() {
  rv <- get_results() |>
    dplyr::filter(grepl("MART4", paper),
                  grepl("Result", Assessment),
                  year %in% c(2021, 2020),
                  complete.cases(ID)) |>
    mutate(Mark = round(Mark)) |>
    select(ID, Name, paper, Mark)
}

refresh_students <- function(data_path = "~/Documents/Shiny/Data/Imports/",
                             write_tab = TRUE) {
  # SDRPT06 - had contact, programme and also papers! Seems ideal.

  #data_path <- "~/Volumes/NVM/OneDrive/Service/School/TALC/Divisional Dashboard/Data/Students/SDRPT06-StudentCohortReport - 2025.xlsx"

  mdebug("Firing", "refresh_students", TRUE)
  fns <- dir(data_path)
  if (length(dir(data_path)) == 0) {
    is_dir <- FALSE
    # data_path is a file?
    if ( file.exists(data_path) )
      fns <- data_path
  } else {
    is_dir <- TRUE
  }

  all_students <- tribble(~ID, ~Surname, ~Firstname, ~Knownname, ~Gender,
                          ~Ethnicity, ~IWI, ~Email, ~Mobile, ~Programme, ~Programme2,
                          ~Prog1Maj1, ~Prog1Maj2, ~Prog2Maj1, ~Prog2Maj2,
                          ~Residency, ~Citizenship, ~Papers, ~Conditional)

  for (fn in fns) {
    mdebug(sprintf("Processing %s", fn), "refresh_students", TRUE)

    if (is_dir) {
      students <- readxl::read_excel(file.path(data_path, fn))
    } else {
      students <- readxl::read_excel(fn)
    }

    if (grep("SDRPT", students[1,1])) {
      mdebug(sprintf("Processing Cohort data"), "refresh_students", TRUE)
      nskip <- 14
    } else {
      mdebug(sprintf("Processing: WTF!"), "refresh_students", TRUE)
      nskip <- 4
    }

    students        <- students |> slice(-(1:nskip))
    snames          <- students |> slice(1) |> as.character()
    students        <- students |> slice(-1)
    names(students) <- snames
    names(students)[1] <- "ID"

    students <- students |>
      filter(grepl("[0-9]+", ID)) |>
      dplyr::filter(complete.cases(ID)) |>
      mutate(ID = as.integer(ID)) |>
        distinct(ID, .keep_all = TRUE)

    students <- students |>
      select(
        ID, Surname, Firstname = `First Name`, Knownname = `Known Name`,
        Gender, Ethnicity, IWI,
        Email, Mobile = `Cell Phone`,
        Programme = `Programme 1`, Programme2 = `Programme 2`,
        Prog1Maj1 = `Prog1 Major1`, Prog1Maj2 = `Prog1 Major2`,
        Prog2Maj1 = `Prog2 Major1`, Prog2Maj2 = `Prog2 Major2`,
        Residency = `Residency Status`, Citizenship,
        Conditional = `Academic Progress`,
        Papers    = `All Papers for Academic Year`
        ) |>
      mutate(Conditional = if_else(Conditional == "APP_COND", TRUE, FALSE))

    all_students <- all_students |> add_row(students) |> distinct()
  }

  mdebug("About to write to database", "refresh_students", DEBUG|TRUE)
  if (write_tab) {
    #ed <- get_students()codepages <- setNames(iconvlist(), iconvlist())
    ed <- get_tab("students")
    mdebug(sprintf("Existing table has %s rows", fmt_n(nrow(ed))), "refresh_students", DEBUG)
    nd <- ed |> add_row(all_students) |> distinct()
    mdebug(sprintf("New data has %s rows",       fmt_n(nrow(nd))), "refresh_students", DEBUG)
    set_tab("students", nd)
  }

  mdebug(sprintf("Returning %s rows of Cohort data", fmt_n(nrow(all_students))), "refresh_students", DEBUG|TRUE)
  return(invisible(all_students))
}


list_indexes <- function() {
  db <- get_con()
  all_idx <- tibble(Table = '', Column = '')

  for (tab in c("results", "assessments", "reports") ) {
    an_idx  <- as_tibble(dbGetQuery(db, sprintf("show indexes from %s;", tab)))
    all_idx <- all_idx %>%
	    add_row(an_idx %>%
		    select(Table, Column = Column_name))
  }

  all_idx %>% slice(-1)
}

reindex <- function() {
  db <- get_con()

  all_idx <- list_indexes()

  for (tab in c( "results", "assessments", "reports") ) {
    idx_f <- all_idx %>% dplyr::filter(Table == tab) %>% pull(Column)

    flds <- c('paper', 'year', 'semester')

    if (tab == 'results')
      flds <- c("ID", flds)

    for (fld in flds) {
      if ( !(fld %in% idx_f) )
        dbExecute(db, sprintf("CREATE INDEX idx_%s ON %s (%s);",
		        fld, tab, fld))
    }
  }
}

progress <- function() {
  rv_res <- get_results()
  rv_rep <- get_reports()
  tab <- rv_res |>
	   left_join(rv_rep)

  sum_tab <- tab |>
    dplyr::filter(year == 2023, semester == "S1") |>
    group_by(paper) |>
    mutate(Rep = ifelse(str_length(report) > 0, "Y", "N")) |>
    select(Paper = paper, Rep) |>
    distinct()
  print(sum_tab)
  print(sum_tab %>% ungroup %>% count(Rep))
  sum_tab %>% dplyr::filter(Rep == 'N')
}

mk_instance <- function(d = NULL, p = NULL, y = NULL, s = NULL) {
  #if (is.null(d) & !is.null(p))
  #  return(sprintf("%s_%s_%s", p, y, s))
  #p <- d |> slice(1) |> pull(paper)
  #y <- d |> slice(1) |> pull(year)
  #s <- d |> slice(1) |> pull(semester)
  #return(sprintf("%s_%s_%s", p, y, s))
  d |> mutate(instance = sprintf("%s_%s_%s", paper, year, semester))
}


get_instances_tab <- function(year = NULL, semester = NULL) {
  #|> mutate(instance = str_replace(instance, " Semester 1", "S1"))
  itab <- get_instances()
  mdebug(sprintf("Table 'instances' has %d rows now", nrow(itab)), "get_instances", DEBUG|debug_load)

  # Make instance table from results table
  rtab <- get_results() |>
    mk_instance() |>
    select(paper = paper, year = year, semester = semester, instance) |>
    mutate(Coordinator = '', title = '')

  if (nrow(rtab) > nrow(itab) ) {
    itab <- itab |>
      right_join(rtab, by = c("paper", "year", "semester", "instance")) |>
      distinct(instance, .keep_all = TRUE) |>
      mutate(Coordinator = if_else(Coordinator.x != "", Coordinator.x, Coordinator.y),
             title       = if_else(title.x != "",       title.x,       title.y)) |>
      select(names(itab))
  }

  itab
}

get_dept <- function(user) {
  users <- get_users()
  users |> filter(username == user) |> pull(department)
}



get_permissions <- function(uname) {
  #mdebug(sprintf("Getting permissions for user '%s'",     "really?"), "get_permissions", DEBUG|TRUE)
  #mdebug(sprintf("Getting permissions for user '%s'", get_ss(uname)), "get_permissions", DEBUG)
  #mdebug(sprintf("Getting permissions for user '%s'",     "what's wrong?"), "get_permissions", DEBUG|TRUE)

  d <- get_users()

  if (is.null(d))
    return(NULL)

  perms <- d |>
    dplyr::filter(username == uname) |>
    pull(permissions) |>
    str_split_1(",")

  mdebug(sprintf("Getting permissions for user '%s': '%s'",  uname, paste(perms, collapse = "; ")), "get_permissions")

  return(perms)
}

get_user <- function() {
  if (on_server()) {
    user <- get_server_user()
    modalDialog(
      p(sprintf("The detected username is: '%s'", user)),
      title = "Debug user")
  } else  {
    user <- "wiljo00p"
  }

  fusr <- input$fake_user
  if (is_empty(fusr))
    return(user)

  if (fusr == user)
    return(user)

  return(fusr)
}

mk_qry <- function(tab, paper, year, semester) {
  sprintf("SELECT * FROM %s WHERE paper = '%s' AND year = '%s' AND semester = '%s'",
          tab, paper, year, semester)
}


######## Dead code  #################

#get_user_list <- function() {
#  get_tab("users") |> filter(complete.cases(username))
#}


#get_students  <- function(IDs) {
#  get_tab("students") |>
#    dplyr::filter(ID %in% IDs$ID) |>
#    mutate(Name = paste(Firstname, Surname)) |>
#    select(Name, Mobile, Email, ID)
#}

make_user_tab <- function() {
  tribble(~user_f,     ~user_s,     ~username,   ~permissions, ~current,
          "John",      "Williams",  "wiljo00p",  "admin", 1,
          "Cathy",     "Child",     "chica65p",  "all",   1,
          "Kirsten",   "Robertson", "robki359",  "all",   1,
          "Andrea",    "Insch",     "insan69p",  "all",   1,
          "Janette",   "Hart",      "harja17p",  "all",   1,
          "Rob",       "Hamlin",    "hamro00p",  "user",  1,
          "Wiebke",    "Finkler",   "finwi90p",  "user",  1,
          "Troy",      "Mihaka",    "mihtr84p",  "user",  1,
          "Rob",       "Aitken",    "aitro92p",  "user",  1,
          "Lisa",      "McNeill",   "sim3220p",  "user",  1,
          "Mathew",    "Parackal",  "parma63p",  "user",  1,
          "Sergio",    "Biggemann", "bigse79p",  "user",  1,
          "Ismail",    "Shaheer",   "shais61p",  "user",  1,
          "Damien",    "Mather",    "matdo58p",  "user",  1,
          "Masoud",    "Karami",    "karma61p",  "user",  1,
          "Leah",      "Watkins",   "watle82p",  "user",  1,
          "Rob",       "Thomson",   "thoro54p",  "user",  1
  )
}


re_encode_fields <- function(tbls, flds) {
  for (tbl in tbls) {
    for (fld in flds) {
      get_qry(sprintf("ALTER TABLE %s MODIFY COLUMN %s mediumtext COLLATE utf8mb4_general_ci;\n",
              tbl, fld))
    }
    rv <- get_qry(sprintf("select character_set_name collation_name from information_schema.columns where table_schema = 'marketing' AND table_name = '%s';\n", tbl))
    print(rv)
  }
}

create_new_tables <- function() {
  # create table engagement (paper char(7), year int(11), semester char(12), ID int, username char(8), content int, tools int, bb_hours int, echo_hours int, echo_pc int);
  # create table engagement_meta (paper char(7), year int, semester char(2), last_update date, first_date date, last_date date);
  # create table enrolment ( paper char(7), year int, semester char(2), ID int, restriction varchar(255), status varchar(255), approval varchar(255), enrolment varchar(255)   );
  # alter table students add column (Residency varchar(255), Citizenship varchar(255), Papers varchar(255), Conditional int);
  # alter table config add column default_dept varchar(255);
}

