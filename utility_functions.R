result_names     <- c("Overall Result", "Overall paper result", "Overall", "Total",
                     "Total Paper Result", "Total paper result", "Final mark")
exam_names       <- c("Final exam", "Final Exam", "Final Examination", "FE")
internal_names  <- c("Internal", "IA")
BCom_core_papers <- c("ACCT102", "COMP101", "COMP120", "ECON112")
sec_style        <- "color:white;background-color:#107896;padding:5px;text-align:center;heigh:100pc"
h3_style         <- "color:gray"
h4_style         <- "color:teal"

library(tidyverse)
library(RMariaDB)

elapsed <- function(m, s, r = 1) {
  sprintf("%s took %s", m, format(round(Sys.time() - s, r)))
}

robust.system <- function(cmd) {
  stderrFile = tempfile(pattern = "R_robust.system_stderr", fileext = as.character(Sys.getpid()))
  stdoutFile = tempfile(pattern = "R_robust.system_stdout", fileext = as.character(Sys.getpid()))

  retval = list()
  retval$exitStatus = system(paste0(cmd, " 2> ", shQuote(stderrFile), " > ", shQuote(stdoutFile)))
  retval$stdout = readLines(stdoutFile)
  retval$stderr = readLines(stderrFile)

  unlink(c(stdoutFile, stderrFile))
  return(retval)
}

get_emergency_status <- function() {
  load("app_status")

  if (app_status$is_emergency) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


set_emergency_status <- function(is_emergency = FALSE) {
  load("app_status")
  app_status$is_emergency <- is_emergency
  save(app_status, file = "app_status")
}

mdebug <- function(s,
                   caller    = "No caller supplied",
                   force     = FALSE,
                   debug_log = FALSE,
                   DEBUG     = FALSE) {

  mess <- sprintf("(%18s) [%s] %s", caller, format(Sys.time()), s)
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

on_server <- function() {
  hn <- as.character(Sys.info()["nodename"])
  if_else(hn %in% c("hotdog", "lounge"), FALSE, TRUE )
}

fmt_n <- function(x) format(x, big.mark = ",")


source("database_functions.R", local = TRUE)

u2l <- function(s) {
  #https://codepoints.net/U+1ef5?lang=en
  utf82latin1     <- c("ā" = "a", "ō" = "o", "ạ" = "a", "ễ" = "e", "ơ" = "o",
                       "ř" = "r", "Ā" = "A", "Š" = "S", "š" = "S", "ỵ" = "y",
                       "ọ" = "o", "ỳ" = "y", "’" = "'", "ł" = "L")
  st <- str_replace_all(s, utf82latin1)

  stringi::stri_conv(st, "utf8", 'latin1')
}

DEBUG         <<- FALSE
trace_dat     <<- FALSE
debug_db      <<- FALSE
debug_flicker <<- FALSE
debug_load    <<- FALSE
debug_import  <<- FALSE



get_users_name <- function(u, utab) {
  utab |> filter(username == u) |> transmute(name = paste(user_f, user_s) ) |> pull(name)
}

get_n <- function(x, default = 0 ) {
  ifelse(isTruthy(x) & is.numeric(x), x, default)
}

get_s <- function(x, default = "") {
  ifelse(length(x) > 0, x, default)
}

get_ss <- function(s) {
  #mdebug(sprintf("Trying to make sense of something of class(es): '%s'", paste(class(s), collapse = ",")), "get_ss")
  s <- ifelse(is.null(s), "", s)
  rv <- case_when(
    purrr::is_empty(s) ~ "",
    length(s) == 0     ~ "",
    TRUE               ~ as.character(s)
  )
  #mdebug(sprintf("Returning '%s', of class(es) '%s' and length: %d", rv, class(rv), length(rv)), "get_ss")
  rv
}

exam_name <- function(dd = NULL) {
  if (is.null(dd)) {
    return(NULL)
  }

  if ("Assessment" %in% names(dd$results)) {
    ns <- unique(dd$results$Assessment)
  } else {
    ns <- names(dd$results)
  }

  ens <- str_detect(ns, paste(exam_names, collapse = "|") )
  if ( any(ens) ) {
    mdebug(sprintf("Returning '%s'", ns[ens]), "exam_name")
    return(ns[ens])
  } else {
    mdebug(sprintf("Names do not contain any exam names: %s",
                    paste(ns, collapse = ", ")), "exam_name")
    return(NULL)
  }
}

has_exam <- function(x) {
  ifelse(is.null(exam_name(x)), FALSE, TRUE)
}

get_server_user <- function() {
  userName <- Sys.getenv("SHINYPROXY_USERNAME")
  stringr::str_extract(userName, "([a-z0-9]+)@", group = 1)
}

is_valid <- function(x, n) {
  if (length(x)) {
    if (str_length(x) == n)
      TRUE
    else
      FALSE
  } else {
    FALSE
  }
}


get_names <- function(o) paste(names(o), collapse = ", ")

make_results <- function(d, offline = FALSE)  {
  # FIXME: if Internal is present, do not calculate it.
  d$results <- d$results |> distinct(Assessment, ID, .keep_all = TRUE)
  all_names <- d$results |> pull(Assessment) |> unique()
  all_names <- all_names[complete.cases(all_names)]

  has_exam <- any(all_names %in% exam_names)

  mdebug(sprintf("Making results for '%s' '%s' '%s'", d$paper, d$year, d$semester), "make_results", debug_flicker)

  d$results <- d$results |>
    mutate(Mark = case_match(Grade,
                             "FC" ~ -4,
                             "FT" ~ -3,
                             .default = Mark))
  if (debug_load) {
    print(d$results |> count(Grade))
    print(head(d$results |> arrange(desc(Mark))))
  }

  ass_sane <- FALSE
  if (length(all_names) == length(d$assessments$Title))
    if (all(sort(d$assessments$Title[1:length(all_names)]) == sort(all_names)))
      ass_sane <- TRUE

  w_sum    <- d$assessments |> filter(!Title %in% result_names) |> pull(Weight) |> sum()
  ass_sane <- if_else(w_sum == 100, TRUE, FALSE)
  if (w_sum != 100) {
    mdebug(sprintf("Weights do not sum to 100! The sum is %4.0f", w_sum), "make_results", debug_load)
    if (w_sum == 0) {
      rn_n <- sum(unique(d$results$Assessment) %in% result_names)
      mdebug(sprintf("Weights sum to 0, i.e. no internal assessment. Number of columns in result_names is %d",
                     rn_n), "make_results", debug_load)
      if (rn_n > 1) {
        rnames <- all_names[all_names %in% result_names]
        dnames <- all_names[all_names != "Overall Result"]
        mdebug(sprintf("More than one column contains overall results (%s), so dropping %s",
                       plist(rnames), plist(dnames)), "make_results", debug_load)
        d$results <- d$results |> filter(!Assessment %in% dnames)
        all_names <- all_names[!all_names %in% dnames]
        d$assessments <- d$assessments |> filter(!Title %in% dnames)
        ass_sane <- TRUE  # FIXME: ignores possible problem of results$Assessment not matching assessments$Title
      }
    }
  }

  dw <- d$results |>
    pivot_wider(id_cols = c(ID, Name), names_from = Assessment, values_from = Mark) |>
    mutate_at(all_names, coalesce, 0) # Replace NA with 0

  int_names  <- all_names[!( all_names %in% c(result_names, internal_names)) ]
  ass_scores <- dw[, int_names] ; # Remove name and ID columns # dd$assessments$Title

  ass_tab <- d$assessments |>
    mutate(weight = Weight/100)

  calculate_IA <- TRUE

  if (!ass_sane) {
    mdebug("There's something wrong with the assessments data!", "make_results", TRUE)

    if (FALSE) {
      message("Raw data")
      message("Assessment scores")
      print(ass_scores  |>
              mutate(Sum = rowSums(ass_scores)) |>
              arrange(desc(Sum)))

      message("Assessment table")
      print(ass_tab, n = Inf)
    }

    if (sum(ass_tab$weight) != 100) {
      message("Weights do not sum to 1, so not calculating internal assessment")
      calculate_IA <- FALSE

      if (any(ass_tab$Title %in% internal_names)) {
        IN <- ass_tab$Title[ass_tab$Title %in% internal_names]
        message(sprintf("Renaming '%s' as Internal", IN))
        names(dw)[names(dw) == IN] <- "Internal"

        IS <- sum(dw |> pull(Internal), na.rm = TRUE)
        if (IS == 0) {
          message("Internal column exists but has no data, so attempting to calculate")
          nnames <- c(result_names, exam_names, internal_names, "ID", "Name")
          nnames <- names(dw)[names(dw) %in% nnames]

          test_int <- dw |>
            select(-all_of(nnames)) |>
            mutate(Internal = rowSums(across(where(is.numeric))))
          dw <- dw |> mutate(Internal = test_int$Internal)
          print(test_int)
          print(dw)

        }
      } else {
        message(sprintf("And there is no internal assessment column in '%s'",
                        paste(ass_tab$Title, collapse = ", ")))
        message("So Overview results are nonsense")
      }
    }
  }

  if (has_exam) {
    message(sprintf("This paper has an exam: '%s'", exam_name(d)))

    dw <- dw |>
      select(everything(), V = exam_name(d)) |>
      mutate(V = ifelse(V == 0, -3, V))
    names(dw)[names(dw) == "V"] <- exam_name(d)

    exm_wgt <- ass_tab |> filter(Title == exam_name(d)) |> pull(weight)

    int_ass <- ass_scores |>
      select(-exam_name(d),
             -names(ass_scores)[names(ass_scores) %in% internal_names])

    int_wts <- ass_tab |>
      filter(!Title %in% c(result_names, exam_name(d), internal_names)) |>
      pull(weight)

    if (calculate_IA) {
      int_wgt <- 1 - exm_wgt
    } else {
      message("Not calculating Internal score from assessments current data and weights are:")
      int_wgt <- 1
    }
  } else {
    exm_wgt <- 0
    int_ass <- ass_scores
    int_wts <- ass_tab |> filter(!Title %in% c(result_names)) |> pull(weight)
    int_wgt <- 1
  }

  ass_wts    <- ass_tab |> filter(!Title %in% c(result_names)) |> pull(weight)
  final_only <- ifelse(length(ass_wts) == 1 | nrow(ass_tab) == 1 | length(int_names) == 0, TRUE, FALSE)

  if (final_only) {
    mdebug(sprintf("Final exam only paper: %s %s %s", d$paper, d$year, d$semester),                       "make_results", debug_import | TRUE)
    mdebug(sprintf("Final exam only paper (how detected?). Names of results data: %s", plist(names(dw))), "make_results", debug_import | TRUE)
    if (debug_import)
      print(head(dw))

    rn <- names(dw)[names(dw) %in% result_names]
    result <- dw |> pull(rn[1])
    dw <- dw |> mutate(Internal = result, Total = result)
  } else {
    if (debug_load) {
      print(int_wts)
      print(int_ass)
    }

    stopifnot(nrow(int_ass) == nrow(int_wgt))

    if (calculate_IA) {
      message("Calculating Internal score and Total score")

      dw <- dw |>
        mutate(Internal = as.numeric(mround( (as.matrix(int_ass)    %*% as.matrix(int_wts)) * 1/int_wgt)),
               Total    = as.numeric(mround(  as.matrix(ass_scores) %*% as.matrix(ass_wts))))

    } else {
      message("Not calculating Internal score")
      en <- which(names(dw) %in% exam_names)
      EN <- names(dw)[en]
      message(sprintf("Renaming '%s' to Final Exam", EN))
      names(dw)[names(dw) == EN] <- "Final Exam"

      exm_wgt <- ass_tab |> filter(Title == EN) |> pull(Weight)
      if (exm_wgt > 1)
        exm_wgt <- exm_wgt/100
      int_wgt <- 1 - exm_wgt

      #message(sprintf("Calculating Total with weights: int: %3.2f; exam: %3.2f",
      #               int_wgt, exm_wgt))

      dw <- dw |>
        mutate(Total = round(Internal * int_wgt + `Final Exam` * exm_wgt)) |>
        select(ID, everything(), Internal, `Final Exam`, Total)

      rns <- which(names(dw) %in% result_names)
      TN <- names(dw)[rns[1]]
      if (!is.na(TN)) {
        message(sprintf("Using '%s' as Total name", TN))
        if (!TN == "Total" & !"Total" %in% names(dw)) {
          print(dw |> arrange(desc(Total)))
          dw <- dw |> select(everything(), Total = all_of(TN) )
          print(dw |> arrange(desc(Total)))
        }
      }

      if (length(rns) > 1) {
        other_rns <- result_names[result_names != "Total"]
        extra_rns <- names(dw)[names(dw) %in% other_rns]
        message(sprintf("There is more than one result name, so dropping '%s'",
                        paste(other_rns, collapse = ", ")))
        dw <- dw |> select(-all_of(extra_rns))
      }
    }
  }

  if (FALSE) {
    if (any(ww$D != 0 & ww$Result > 0)) {
      mdebug(sprintf("Whoop whoop! Calculated total differs from supplied Final Result"),
             "make_results", TRUE)
      ww |> filter(D != 0, Result > 0) |> print()
    }
  }

  if (any(colnames(dw) %in% result_names) & calculate_IA) {
    #print(dw)
    rn <- which(colnames(dw) %in% result_names[result_names != "Total"])
    if (shiny::isTruthy(rn)) {
      on <- colnames(dw)[rn[1]]
      colnames(dw)[rn[1]] <- "Result"
      dw <- dw |> mutate(Total = if_else(Result < 0, Result, Total))
      colnames(dw)[rn[1]] <- on
    }
  }
  #print(dw |> arrange(desc(ID)))
  list(results = dw, exam_dat = list(has_exam = has_exam, exam_weight = exm_wgt))
}

load_data <- function(t_paper, t_year, t_semester, backup_reports = FALSE,
                      debug_log = FALSE, offline = FALSE) {
  # Loads data from RDBMS
  #t_paper <- "MART112"; t_year <- 2024; t_semester <- "S2"; debug_flicker <- FALSE; backup_reports <- FALSE; debug_log <- TRUE

  mdebug(sprintf("Requested data for '%s' ('%s', '%s')",
                 t_paper, t_year, t_semester), "load_data", debug_load | debug_flicker )

  # Handle possibly missing arguments
  if (get_ss(t_paper) == "") {
    t_paper <- get_config() |> pull(default_paper)
    mdebug(sprintf("No paper supplied, using    '%s'", t_paper), "load_data", force = debug_flicker)
  }

  if (get_ss(t_year) == "") {
    t_year <- get_year()
    mdebug(sprintf("No year supplied, using     '%s'", t_year), "load_data", force = debug_flicker)
  }

  if (get_ss(t_semester) == "") {
    t_semester <- get_config() |> pull(default_semester)
    mdebug(sprintf("No semester supplied, using '%s'", t_semester), "load_data", force = debug_flicker)
  }

  load_start <<- Sys.time()
  mdebug(sprintf("Fetching data for '%s' ('%s', '%s')", t_paper, t_year, t_semester),
         "load_data", force = debug_load)


  s <- Sys.time()
  if (offline) {
    results <- get_tab("results") |> filter(paper == t_paper, year == t_year, semester == t_semester)
  } else {
    results <- get_results() |> filter(paper == t_paper, year == t_year, semester == t_semester)
  }

  e <- Sys.time()
  mdebug(sprintf("Obtained %s rows of data in %s", fmt_n(nrow(results)), format(e - s)), "load_data", force = debug_RAM)

  if (nrow(results) == 0) {
    # Try again
    instances <- get_qry(sprintf("SELECT * FROM results WHERE paper = '%s'",
                                 t_paper))
    last_instance <- instances     |> arrange(desc(year)) |> slice(1)
    year          <- last_instance |> pull(year)
    semester      <- last_instance |> pull(semester)
    mdebug(sprintf("(load_data): No records! Returning most recent instance of %s, which is %s %s",
                   t_paper, t_year, t_semester), force = debug_db)
    results     <- get_qry(mk_qry("results",     t_paper, t_year, t_semester))
  }

  if (offline) {
    instances <- get_tab("instances")
    papers    <- instances |> pull(paper) |> unique()

  } else {
    papers    <- get_paper_list()
    instances <- get_instances()
  }

  if (nrow(instances |> dplyr::filter(paper == t_paper, year == t_year, semester == t_semester)) == 0) {
    nr <-  tibble(Paper       = t_paper,
                  Year        = as.integer(t_year),
                  Semester    = t_semester,
                  title       = "",
                  instance    = sprintf("%s_%s_%s", t_paper, t_year, t_semester),
                  Coordinator = "")
    instances <- instances |>
      add_row(nr)

    set_tab("instances", instances)
  }

  if (offline) {
    assessments <- get_tab("assessments") |> filter(paper == t_paper, year == t_year, semester == t_semester)
    report      <- get_tab("reports")     |> filter(paper == t_paper, year == t_year, semester == t_semester)
  } else {
    assessments <- get_assessments() |> filter(paper == t_paper, year == t_year, semester == t_semester)
    report      <- get_reports()     |> filter(paper == t_paper, year == t_year, semester == t_semester)
  }

  if (backup_reports) {
    mdebug("(load_data): Backing up report", force = debug_load)
    unlink("Data/Reports_Backup/MART112_2021_S1*")
    unlink("Data/Reports_Backup/FOSC112_2020_S2*")
    save(report, file = sprintf("Data/Reports_Backup/%s_%s_%s_report_%s.Rdat",
                                paper, year, semester, str_replace_all(Sys.time(), "[ :\\-]", "_")))
    mdebug(sprintf("(load_data): returning %d records for %s (%s, %s)",
                   nrow(results), paper, year, semester), force = debug_db)
  }

  #if (debug_log)
    #mdebug(report, "load_data", FALSE)

  return(list(results     = results |> select(-year, -semester, -paper),
              assessments = assessments,
              report      = report,
              paper       = t_paper, year = t_year, semester = t_semester))
}

make_dd <- function(paper = "MART112", year = "2024", semester = "S1", offline = FALSE) {
  #paper <- "FOSC112"; year <- "2024"; semester <- "S2"
  #paper <- "ECON112"; year <- "2024"; semester <- "S2"
  d <- load_data(paper, year, semester, offline = offline)
  rv <- make_results(d)
  list(results      = rv$results |> mutate(Scaled = Total),
       results_tidy = d$results,
       assessments  = d$assessments,
       exam_weight  = rv$exam_dat$exam_weight,
       has_exam     = rv$exam_dat$has_exam,
       report       = d$report,
       year         = d$year,
       paper        = d$paper,
       semester     = d$semester)
}


normalise_semesters <- function(sem) {
  if ( stringr::str_detect(sem, "First Semester|Semester 1"))
    sem <- "S1"
  if ( stringr::str_detect(sem, "Second Semester|Semester 2"))
    sem <- "S2"
  if ( stringr::str_detect(sem, "Summer School"))
    sem <- "SS"
  if ( stringr::str_detect(sem, "Full Year"))
    sem <- "FY"
  if ( stringr::str_detect(sem, "1st"))
    sem <- "NS1"
  sem
}


bulk_import <- function(fn = "~/Dropbox (Maestral)/EXRPT11-PaperResultsSummary.XLS") {

  rd <- readxl::read_excel(fn, skip = 2) |> select(1:6)
  names(rd) <- c("ID", "Name", "1", "2", "Mark", "Grade")
  rd <- rd |> select(ID, Name, Mark, Grade)

  if ( nrow(rd) ) {
    starts  <- which(grepl("Student ID",   rd$ID))   + 1
    ends    <- which(grepl("Mark Summary", rd$Name)) - 1
    new_results <- tribble(~Assessment, ~ID, ~Mark, ~Grade, ~paper, ~year, ~semester, ~Name)

    for (i in 1:length(starts)) {
      adat <- rd |> slice(starts[i]:ends[i])

      paper_code        <- str_extract(rd$ID[starts[i] - 5], "[A-Z]{4}[0-9]{3}")
      if(str_detect(paper_code, "T5"))
        break;

      paper_name        <- rd$Name[starts[i] - 5]
      assessment_name   <- rd$Name[starts[i] - 4]
      if(is.na(assessment_name))
        assessment_name <- rd$ID[starts[i] - 4]
      message(sprintf("Processing %s : %s", paper_code, assessment_name))
      semester          <- rd$ID[starts[i]- 6] |> str_extract("S.*|F.*|1s.*")      |> normalise_semesters()
      year              <- rd$ID[starts[i]- 6] |> str_extract("[0-9]{4}") |> as.numeric()
      assessment_weight <- rd$Name[starts[i] - 3]  |> str_remove("%") |> as.numeric()

      this_ass <- tibble(Assessment = assessment_name,
                         ID = as.numeric(adat$ID), Mark = as.integer(adat$Mark), Grade = adat$Grade,
                         paper = paper_code, year = as.integer(year), semester = semester,
                         Name = adat$Name)

      new_results <- new_results |> add_row(this_ass)
    }
    rtab <- get_results()
    rtab <- rtab |> add_row(new_results)
    set_tab("results", rtab)
  }
}

import_report <- function(p = 'MART112', y = 2020, s = 'S1', nd = tibble()) {
  # p <- 'MART112'; y <- 2020; s <- 'S1'
  nd  <- make_dd(p, y, s)
  msg <- sprintf("Import report for %s (%s %s)", p, y, s)

  itab <- get_instances()
  ttab <- get_totals()
  rtab <- get_reports()
  stab <- get_results()

  # check for duplicates
  it_n  <- nrow(itab)
  it_dn <- nrow(itab |> distinct())
  rt_n  <- nrow(rtab)
  rt_dn <- nrow(rtab |> distinct())
  st_n  <- nrow(stab)
  st_dn <- nrow(stab |> distinct())
  tt_n  <- nrow(ttab)
  tt_dn <- nrow(ttab |> distinct())

  it_dr <- ifelse(it_n == it_dn, FALSE, TRUE)
  rt_dr <- ifelse(rt_n == rt_dn, FALSE, TRUE)
  st_dr <- ifelse(st_n == st_dn, FALSE, TRUE)
  tt_dr <- ifelse(tt_n == tt_dn, FALSE, TRUE)

  drs <- list(Instances = it_dr, Reports = rt_dr, Results = st_dr, Totals = tt_dr)
  if (any(unlist(drs) == FALSE) ) {
    dup_list <- names(which(unlist(drs)))

    msg <- c(msg,
             sprintf("There are duplicate records in the %s table(s)",
                     paste(dup_list, collapse = ", ")))
    # For each table with duplicated records
    # FIXME: remove duplicates, with a warning
    for (tab_name in dup_list) {
      if (tab_name == "Results")
        tab <- stab
      if (tab_name == "Totals")
        tab <- ttab
      if (tab_name == "Reports")
        tab <- rtab

      dupes   <- tab |> duplicated()
      tab_dup <- tab |> filter(dupes) |> mk_instance()
      tab_dis <- tab |> distinct()    |> mk_instance()

      print(sprintf("Instances in %s with duplicated data", tab_name))
      print(tab_dup |> count(paper, year, semester) |> arrange(year))

      print("Corresponding instances")
      print(tab_dis |>
              filter(instance %in% tab_dup$instance) |>
              count(paper, year, semester) |>
              arrange(year))
    }
  }

  # Are we replacing existing data?
  ei_n <- itab |> filter(Paper == p, Year == y, Semester == s) |> nrow()
  er_n <- rtab |> filter(paper == p, year == y, semester == s) |> nrow()
  es_n <- stab |> filter(paper == p, year == y, semester == s) |> nrow()
  et_n <- ttab |> filter(Paper == p, Year == y, Semester == s) |> nrow()

  nr_n <- nd$report       |> nrow()
  ns_n <- nd$results_tidy |> nrow()
  nt_n <- nd$results      |> nrow()

  rcs <- c(Reports = er_n == nr_n,
           Totals  = es_n == ns_n,
           Results = et_n == nt_n)

  if (any(rcs == FALSE)) {
    msg <- c(msg,
             sprintf("Existing data for this instance, new data has different number of rows (%s)",
                     paste(names(which(rcs == FALSE)), collapse = ", ")))

    for (i in 1:length(rcs) ) {
      if (rcs[i] == FALSE) {
        tn <- names(rcs)[i]
        if (tn == "Reports")
          msg <- c(msg, sprintf(" - For %s there are %d rows existing and %d rows to be added", tn, er_n, nr_n))
        if (tn == "Results")
          msg <- c(msg, sprintf(" - For %s there are %d rows existing and %d rows to be added", tn, es_n, ns_n))
        if (tn == "Totals")
          msg <- c(msg, sprintf(" - For %s there are %d rows existing and %d rows to be added", tn, et_n, nt_n))
      }
    }
    if ( all(c(er_n, es_n, et_n) == 0) ) {
      msg <- c(msg, "There is no existing data, so adding new rows is safe")
    }
  }

  # Check reports are the same, or old is empty, before removing existing rows
  erep <- rtab |> filter(paper == p, year == y, semester == s) |> pull(report)
  nrep <- nd$report |> pull(report)

  if ( length(erep == nrep) != 0 ) {
    if (!erep == nrep)
      msg <- c(msg, sprintf("Existing report: %s; New report: %s",
                 str_sub(erep, 1, 10), str_sub(nrep, 1, 102)))
  } else {
    msg <- c(msg ,"There is no existing report for this instance")
  }

  msg
}

simulate_UI <- function(paper = "MART112", year = 2024, semester = "S1") {
  #paper <- "MART112"; year <- 2024; semester <- "S1"
  input <<- list()
  rvs   <<- list()

  debug_load <<- FALSE
  debug_RAM  <<- FALSE
  debug_equity <<- FALSE

  input$is_emergency        <- FALSE
  input$filter_levels       <- c("100", "200", "300")
  input$students_grade      <- 60
  input$students_papers     <- 1
  input$students_year       <- 2024
  input$filter_all_students <- 'Yes'
  input$filter_bcom         <- FALSE
  input$filter_mart         <- FALSE
  input$inc_abs             <- FALSE
  input$inc_wdn             <- FALSE
  input$inc_zer             <- FALSE
  input$inc_pg              <- FALSE
  input$inc_ug              <- TRUE
  input$inc_s1              <- TRUE
  input$inc_s2              <- TRUE
  input$dry_run             <- TRUE
  input$debug_import        <- TRUE
  input$paper_to_show       <- paper
  input$year_to_show        <- year
  input$semester_to_show    <- semester
  input$kpis_papers         <- "All"
  input$kpis_years          <- "All"
  input$kpis_semesters      <- "All"
  input$kpis_inc_maj        <- "All"
  input$kpis_inc_UG         <- TRUE
  input$kpis_inc_PG         <- TRUE
  input$kpis_inc_core       <- TRUE
  input$kpis_inc_noncore    <- TRUE
  input$kpis_inc_BEntr      <- TRUE
  input$r1_rgx              <- "MART.*4[0-9]{2}"

  dd <- make_dd(paper, year, semester, offline = TRUE)
  offline <<- TRUE
  return(invisible(list(input = input, dd = dd)))
}

summary_stats <- function(x, digits = 1, iz = TRUE) {
  nzs <- function(x) {
    if (iz) {
      #message("Including zeroes in statistics table")
      x
    } else {
      #message("Excluding zeroes in statistics table")
      x[x != 0]
    }
  }

  m_q25    <- function(x) quantile(nzs(x), 1/4, na.rm = TRUE)
  m_q75    <- function(x) quantile(nzs(x), 3/4, na.rm = TRUE)
  m_IQR    <- function(x) IQR(     nzs(x),      na.rm = TRUE)
  m_mean   <- function(x) mean(    nzs(x),      na.rm = TRUE)
  m_SD     <- function(x) sd(      nzs(x),      na.rm = TRUE)
  m_median <- function(x) median(  nzs(x),      na.rm = TRUE)
  m_min    <- function(x) min(     nzs(x),      na.rm = TRUE)
  m_max    <- function(x) max(     nzs(x),      na.rm = TRUE)
  m_n      <- function(x) length(    nzs(x))

  t <- round(
    data.frame(
      Mean = sapply(x, m_mean),
      #SD   = sapply(x, m_SD),
      Med  = sapply(x, m_median),
      IQR  = sapply(x, m_IQR),
      #Q25  = sapply(x, m_q25),
      #Q75  = sapply(x, m_q75),
      #Min  = sapply(x, m_min),
      Max  = sapply(x, m_max)
      #n    = sapply(x, m_n)
    ),
    digits)
  return(invisible(t))
}

sum_stats <- function(d, pr = FALSE) {
  tab <- d |>
    summarise(Mean   = round(   mean(  Mark, na.rm = TRUE)),
              Median = round(   median(Mark, na.rm =-TRUE), 1),
              SD     = round(   sd(    Mark, na.rm =-TRUE)),
              IQR    = round(   IQR(   Mark, na.rm =-TRUE)),
              Median = round(   median(Mark, na.rm =-TRUE)),
              Pass   = round(100 * sum(Pass, na.rm = TRUE)/n()),
              n      = fmt_n(n()),
              .groups = 'drop')
  if (pr)
    print(tab)

  tab
}

tmod <- function(d, g, pr = FALSE) {
  d |>  sum_stats(pr) |>
    ungroup() |>
    rename(SubGroup = 1, Papers = 2) |>
    add_column(tibble(Group = g), .before = 1)
}

get_gap <- function(tab, g1, g2, v, inst, onam_ty) {
#  tab <- ptab; g1 <- 'Pacific'; g2 <- 'Non-Pacific'
  # FIXME: what if v in a or b is NA?
  a  <- tab |> filter(SubGroup == g1, Papers == inst)
  b  <- tab |> filter(SubGroup == g2, Papers == inst)

  if (nrow(a) > 0 & nrow(b) > 0) {
    ig <- a |> pull({{v}}) - b |> pull({{v}})
  } else {
    ig <- NA
  }

  a  <- tab |> filter(SubGroup == g1, Papers != inst)
  b  <- tab |> filter(SubGroup == g2, Papers != inst)

  if (nrow(a) > 0 & nrow(b) > 0) {
    ag <- a |> pull({{v}}) - b |> pull({{v}})
  } else {
    ag <- NA
  }

  i_n <- tab$n[tab$Papers == inst &    tab$SubGroup == g1]
  if (length(i_n) == 0)
    i_n <- "0"

  a_n <- tab$n[tab$Papers == onam_ty & tab$SubGroup == g1]

  gstr_i <- sprintf("%3.1f (n = %s)", ig, i_n)
  gstr_a <- sprintf("%3.1f (n = %s)", ag, a_n)

  list(instance = ig, all_other = ag, i_n = i_n, a_n = a_n, gstr_i = gstr_i, gstr_a = gstr_a)
}

msum <- function(v) {
  if (is.numeric(v)) {
    median(v, na.rm = TRUE)
  } else {
    100 * sum(v, na.rm = TRUE)/length(v)
  }
}

zz <- function(d) {
  str_remove(d, "M_")
}

vtab <- function(v) {
  v |>
    count(value) |>
    arrange(desc(n)) |>
    filter(complete.cases(value)) |>
    mutate(`%` = 100*n/sum(n)) |>
    filter(`%` > 1)
}

bdt <- function(d, more_opts = NULL, header = NULL) {
  opts <- list(searching   = FALSE,
               length_menu = FALSE,
               paging      = FALSE,
               info        = FALSE,
               autoWidth   = FALSE)

  #more_opts <- NULL; header <- equity_header; d <- etabs$itab

  if ("n" %in% names(d))
    opts$columnDefs  = list(
      list(className = 'dt-right',
           targets   = "n"),
      list(width = 'auto',
           targets = 0))

  if (!is_null(more_opts))
    opts <- c(opts, more_opts)

  if (is.null(header))
    datatable(d,                     rownames = FALSE, options  = opts)
  else
    datatable(d, container = header, rownames = FALSE, options  = opts)
}

dept_stats <- function() {
  rtab <- get_results()

  # FIXME: not credible headcount results. Perhaps different paper coverage over the years?
  rv <- rtab |>
    filter(grepl("MART", paper)) |>
    #filter(grepl("[Oo]verall", Assessment)) |>
    distinct(ID, .keep_all = TRUE) |>
    mutate(Level = str_sub(paper, 5, 5)) |>
    count(year, Level) |>
    pivot_wider(names_from = Level, values_from = n) |>
    rename(`100` = `1`, `200` = `2`, `300` = `3`, `PG` = `4`) |>
    rowwise() |>
    mutate(UG = sum(`100`, `200`, `300`)) |>
    ungroup() |>
    mutate(D_PG = UG - lag(UG))
}

import_Tp <- function(fn) {
  mdebug("Processing Tp", "import_data", debug_import)
  rd <- readxl::read_excel(fn, range = "A2:F99999" ,
                           col_names = c("ID", "Name", "N1", "N2", "Mark", "Grade"))

  year     <- str_match(rd$Name[1], "Academic Year\\(s\\): ([0-9]{4})")[2]
  semester <- str_match(rd$N1[1],   "Teaching Period\\(s\\): (SF[S12Y])")[2]

  Paper    <- str_match(rd$N2[1],   "Paper\\(s\\): ([A-Za-z0-9]+)")[2]
  Title    <- rd$Name[4]
  Ass_1_Name <- rd$Name[5]
  Ass_1_Weight <- as.numeric(str_match(rd$Name[6], "[0-9+]")[1])
  rd <- rd %>% select(-N1, -N2)
  rd <- rd[-c(1:8), ]
  ends <- grep("Mark Summary", rd$Name, )
  class_size <- ends[1] - 1

  assessment_names <- rd$Name[ends + 9]
  assessment_names <- assessment_names[complete.cases(assessment_names)]
  assessment_names <- c(Ass_1_Name, assessment_names)
  assessment_weights <- rd$Name[ends + 10]
  assessment_weights <- assessment_weights[complete.cases(assessment_weights)]
  assessment_weights <- c(Ass_1_Weight, as.numeric(str_match(assessment_weights, "[0-9]+")))
  marks <- rd %>% dplyr::filter(grepl("^[0-9]{2,}$", ID))
  marks <- marks %>%
    mutate(Assessment = unlist(lapply(assessment_names, function(x) {rep(x, class_size)})))

  list(rd = rd, year = year, semester = semester)
}

load_all_data <- function(data_path = NULL) {
  # EXRPT11 - SMR and SAT
  require(dplyr)

  xl_path     <- data_path
  file_list   <- paste0(xl_path, list.files(xl_path, "All_papers_[0-9]{4}.xls"))
  cn          <-  c("ID", "Name", "n1", "n2", "Mark", "Grade")
  rg          <- "A1:F64000"

  all_cooked  <- tibble(year = NA, semester = "", paper = "",
                        ID = NA, Name = "", Mark = NA, Grade = "")
  all_all_raw <- tibble(ID = NA, Name = "", Mark = NA, Grade = "")

  for (fn in file_list) {
    all_raw <- readxl::read_excel(fn, range = rg, col_names = cn) %>%
      select(ID, Name, Mark, Grade) %>%
      dplyr::filter(complete.cases(ID))

    all_all_raw <- rbind(all_all_raw, all_raw)
    fix_nas <- function(df, code_list) {
      for (code in code_list) {
        df$Grade[ df$Mark == code ] <- code
        df$Mark[  df$Mark == code ] <- "0"
      }
      return(df)
    }
    all_raw <- fix_nas(all_raw, c("F", "FT", "W", "WE", "AG", "N"))
    year   <- str_sub(all_raw$ID[3], 1, 4)

    papers <- all_raw %>%
      dplyr::filter(grepl("Paper", ID)) %>%
      mutate(year = year, paper = stringr::str_sub(ID, start = 8)) %>%
      select(year, paper, Name)

    starts <- grep("Paper", all_raw$ID) + 3
    ends   <- starts - 9
    ends <- ends[-1]
    ends <- c(ends, nrow(all_raw) - 3)

    sems_raw <- c("1st Non standard period", "First Semester", "Full Year",
                  "Second Semester", "Short Courses \\(inc\\. Field Trips\\)", "Summer School")
    sems_cooked <- c("NSP", "S1", "FY", "S2", "SC", "SS")
    names(sems_cooked) <- sems_raw

    for (i in 1:nrow(papers)) {
      paper <- papers[i, "Paper"] %>% unlist
      if (stringr::str_sub(paper, 5, 5) > 4)  # Omit MART5* and above
        next
      semester <- str_sub(all_raw[starts[i] - 4, "ID"], 7)
      semester <- stringr::str_replace_all(semester, sems_cooked)
      all_cooked <- rbind(all_cooked,
                          tibble(year = year, paper = paper,
                                 semester = semester,
                                 ID    = all_raw[starts[i]:ends[i], "ID"]    |> unlist(),
                                 Name  = all_raw[starts[i]:ends[i], "Name"]  |> unlist(),
                                 Mark  = all_raw[starts[i]:ends[i], "Mark"]  |> unlist(),
                                 Grade = all_raw[starts[i]:ends[i], "Grade"] |> unlist(),
                          )
      )
    }
  }

  all_cooked <- all_cooked[-1,]
  all_cooked$Mark <- as.numeric(all_cooked$Mark)

  all_cooked <- all_cooked %>%
    dplyr::filter(complete.cases(Mark)) %>%
    mutate(Grade = factor(Grade,
                          levels = c("A+", "A", "A-", "B+", "B", "B-",
                                     "C+", "C", "C-", "F", "FC",
                                     "FD", "FE", "FT", "N", "WE", "W",
                                     "AG") ))

  mdebug("About to write to database", "load_all_data", force = debug_flicker)
  set_tab("all_totals", all_cooked)

  return(invisible(list(cooked = all_cooked[complete.cases(all_cooked$Mark),], raw = all_all_raw[-1, ])))
}


PropCIs <- function(ns, n = NA) {
  # Tab7 <- c(462, 321, 312, 204, 153, 132, 102, 99, 99, 96, 84, 78, 72, 60, 30, 27, 21, 6)
  nss <- sum(ns)

  if (is.na(n))
    n <- 1:length(ns)

  CIs <- round( 100 * DescTools::BinomCI(ns[n], nss) )
  tibble::as_tibble(cbind(ns, CIs))
}

plist <- function(s) {
  paste(s, collapse = ", ")
}

get_paper_codes <- function(dept) {
  itab <- get_instances()
  papers <- unique(itab$paper)
  s <- case_match(dept,
             "Marketing" ~ "FOSC112, MART, ENTR",
             "BCom Core" ~ "ACCT102, BSNS, COMP101, COMP120, ECON112",
             "Economics" ~ "ECON"
             )
  rv <- str_split(s, ", ") |> unlist()

  papers[grep(paste(rv, collapse = "*|"), papers)]

}

mround <- function(x) {
  ifelse(x %% 1 < 0.5, floor(x), floor(x) + 1)
}

grades <- function(x) {
  ordered(
    case_match(x,
               -5:-1 ~ "W/F/A",
                0:49 ~ "F",
               50:54 ~ "C-",
               55:59 ~ "C",
               60:64 ~ "C+",
               65:69 ~ "B-",
               70:74 ~ "B",
               75:79 ~ "B+",
               80:84 ~ "A-",
               85:89 ~ "A",
               00:100 ~ "A+"),
    levels = c("F", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+")
    )
}

m2g <- function(x) {
  case_match(x)
}
mt <- function(v, pct_d = 0) {
  v |>
    fmt_number(columns =  n, sep_mark = ",", decimals = 0) |>
    fmt_number(columns = `%`,                decimals = pct_d)
}

ptab <- function(d, v, thr = 1, rnd = 1) {
  d |>
    distinct(ID, .keep_all = TRUE) |>
    count( {{v}} ) |>
    arrange(desc(n)) |>
    mutate(`%` = round(100 * n / sum(n), rnd)) |>
    filter(`%` >= thr)
}

enlist <- function(cv) {
  sprintf("('%s')", plist(cv) |>
            str_replace_all(c("[, ]([A-Z0-9]+)" = "'\\1", "([A-Z0-9]+)," = "\\1', ")))
}

rebuild_instances <- function() {
  rtab <- get_results()
  itab <- rtab |> distinct(paper, year, semester)
  itab <- itab |> mk_instance() |> mutate(Coordinator = "")
}

medCI <- function(m = 20, n = 15) {
  q <- 1/2
  z <- 1.96

  j <- n * q - z * sqrt(n * q * (1 - q))
  k <- n * q + z * sqrt(n * q * (1 - q))
  sprintf("(%d, %d)", round(j), round(k))
}

gt_f <- function(d, g, v, inst) {
  tab <- d |>
    filter(Mark > 0) |>
    group_by(year, {{g}} ) |>
    summarise(M = msum({{v}}), n = n(), .groups = 'drop') |>
    pivot_wider(names_from = {{g}}, values_from = c(M, n))

  # A table passed to this function should have statistics in columns 2 & 3, so
  # that the gap between them can be calculated and returned.
  lo_cat <- tab |> transmute(lower = if_else(tab[, 2] < tab[, 3], names(tab[2]), names(tab[3]))) |> slice(1) |> pull()
  hi_cat <- tab |> transmute(lower = if_else(tab[, 2] > tab[, 3], names(tab[2]), names(tab[3]))) |> slice(1) |> pull()

  lo_n <- tab |> pull(str_replace(lo_cat, "M_", "n_"))

  tab |>
    mutate(M_gap = tab |> pull(lo_cat) - tab |> pull(hi_cat),
           Gap   = sprintf("%3.1f", M_gap),  n = fmt_n(lo_n)
    ) |>
    select(-M_gap, -starts_with("n_")) |>
    rename_with(.fn = zz)
}

get_grades <- function(d, v) {
  grade_cuts   <- c( -Inf,   -4,    -3,   -2,  -1,   20,  40,   50,  55,   60,   65,  70,   75,   80,  85, 90, Inf)
  grade_labels <- c( "FT", "FC", "ABS", "WE", "W",  "E", "D", "C-", "C", "C+", "B-", "B", "B+", "A-", "A", "A+")

  grade_cuts_coarse   <- c( -Inf,          0,    50,  65,   80, Inf)
  grade_labels_coarse <- c( "ABS/FT/FC", "Fail", "C", "B",  "A")

  d |> mutate(Grade = cut({{v}},
                            grade_cuts,
                            include.lowest = TRUE,
                            labels = grade_labels[1:(length(grade_cuts) - 1)],
                            right  = FALSE),
              G_cat = cut({{v}},
                          grade_cuts_coarse,
                          labels = grade_labels_coarse,
                          right  = FALSE))
}

check_fields <- function(d) {

  fname_OK <- ifelse(max(str_length(d$user_f),      na.rm = TRUE) < 21, TRUE, FALSE)
  sname_OK <- ifelse(max(str_length(d$user_s),      na.rm = TRUE) < 21, TRUE, FALSE)
  uname_OK <- ifelse(max(str_length(d$username),    na.rm = TRUE) <  9, TRUE, FALSE)
  perms_OK <- ifelse(max(str_length(d$permissions), na.rm = TRUE) <  9, TRUE, FALSE)
  crrnt_OK <- ifelse(max(str_length(d$current),     na.rm = TRUE) <  2, TRUE, FALSE)
  dept_OK  <- ifelse(max(str_length(d$department),  na.rm = TRUE) < 99, TRUE, FALSE)

  uname_OK & fname_OK & sname_OK & perms_OK & crrnt_OK & dept_OK
}

get_edato <- function() {
  s <- Sys.time()
  etabs <- get_etab(or_only = FALSE, exam_only = FALSE, offiline = TRUE)
  e <- Sys.time()
  mdebug(sprintf("Got etab in %s", format(round(e - s, 2))), "output$kpis_enrolment", TRUE)

  if (is.null(etabs))
    return(h1("Choices resulted in no data"))
  etabs$edat
}

make_staff_list <- function() {
  dat <- read_table("~/Downloads/commerce.obs.all.staff.20241127.txt")
  dat |>
    select(GivenName, Surname, SamAccountName) |>
    arrange(GivenName) |>
    mutate(Department = '', Current = 1)
}

sniff_contents <- function(fn, filename = NULL, offline = FALSE) {
  is_Echo <- FALSE

  if ( str_detect(fn, "csv") ) {
    rd <- read_csv(fn, n_max = 3, show_col_types = FALSE)
    if ( names(rd)[2] == "Total Engagement" | names(rd)[7] == "Total Engagement") {
      return("Echo")
    }
  }

  fi <- system(sprintf("file %s", path_escape(fn)), intern = TRUE)
  #message("File info")
  #print(fi)
  if ( str_detect(fi, " text,") ) {
    return("GC_csv")
  }

  if (length( grep("\\.xls", fn, ignore.case = TRUE) ))  {
    sheets <- readxl::excel_sheets(fn)
    if (length(sheets) > 1) {
      if (any(str_detect(sheets, "^gc_"))) {
        return("GC_xlsx")
      }
    }

    rd <- readxl::read_excel(fn, range = "A1:B10")

    if (debug_import) {
      mdebug(sprintf("Read %s: %d rows, %d cols", fn, nrow(rd),  ncol(rd)), "import_data", TRUE)
      mdebug(sprintf("rd[1,1] is '%s'", rd[1,1]), "import_data", TRUE)
      mdebug(sprintf("rd[1,2] is '%s'", rd[1,2]), "import_data", TRUE)
      mdebug(sprintf("rd[2,1] is '%s'", rd[2,1]), "import_data", TRUE)
      mdebug(sprintf("rd[2,2] is '%s'", rd[2,2]), "import_data", TRUE)
    }
  }

  if (offline) {
    r1_rgx <- "MART355"
  } else {
    r1_rgx <- paste0(input$r1_rgx, "|MART 355")
  }
  mdebug(sprintf("Using '%s' as the regex to detect R1 content", r1_rgx), "import_data", debug_import)

  case_when(
   str_detect(fn, "gc_[A-Z]{4}[0-9]{3}_[A-Z0-9]{5}_[0-9]{4}") ~ "GC",
   rd[2, 1] == "SDRPT06 - Student Cohort Report"              ~ "SD",
   str_detect(filename, "CARPT004|CARPT002")                  ~ "ENR",
   rd[2, 2] == "Paper Results Summary"                        ~ "BO",
   str_detect(rd[1, 2], r1_rgx)                               ~ "R1",
   names(rd)[1] == "All User Activity inside Content Areas"   ~ "BB_AUA",
   names(rd)[1] == "Course Activity Overview"                 ~ "BB_CAO",
   names(rd)[1] == "Overall Summary of User Activity"         ~ "BB_SUA",
   is_Echo                                                    ~ "Echo",
   .default =                                                  "UK"
  )
}

read_R1 <- function(fn) {
  ##### Import file #####
  readxl::read_excel(fn, range = "A1:Z100")

  names(rd)[1:3] <- c("ID", "Surname", "Firstname")
  instance    <- rd[1, 2] |> str_squish()
  mdebug(sprintf("(import_data): Instance: '%s'", instance), "import_data", debug_import)
  papers       <- instance |> str_sub( 1, 8) |> str_remove(" ")
  semesters    <- instance |> str_extract("S[1-2]|FY")
  years        <- instance |> str_extract("[0-9]{4}") |> as.integer()
  mdebug(sprintf("(import_data): Extracted '%s' ('%s', '%s')",
                 papers, semesters, years), "import_data", debug_import)

  weights     <- t(rd[3,])
  weights     <- as.numeric(weights[complete.cases(weights)])

  assessments <- as.character(t(rd[4,]))
  assessments <- assessments[complete.cases(assessments)]

  fieldnames  <- as.character(t(rd[5,]))
  fieldnames  <- fieldnames[complete.cases(fieldnames)]

  nflds       <- length(fieldnames)
  mdebug(sprintf("There are %d fields, names are: '%s'",
                 nflds, paste(fieldnames, collapse = ", ")),
         "import_data", debug_import)

  rd <- rd |>
    select((1:all_of(nflds + 3))) %>%
    dplyr::filter(complete.cases(ID))

  if (debug_import) {
    mdebug("Head of student data", "import_data", debug_import)
    print(head(rd))
  }

  # remove empty columns
  rd <- rd |>
    select_if(~sum(is.na(.x)) != nrow(rd))

  # remove columns with alpha grades
  rd_data <- rd |>
    select(4:ncol(rd)) |>
    select_if(~!any(grepl("[A-Z]", .x))) |>
    sapply(as.numeric) |> as_tibble()

  rd_ids <- rd |>
    mutate(Name = sprintf("%s %s", Firstname, Surname)) |>
    select(ID, Name)

  rd <- rd_ids |> add_column(rd_data)
  #rd <- rd %>% select(1,2) %>% add_column(rd_data)

  names(rd)[3:ncol(rd)] <- assessments

  if (debug_import) {
    mdebug("Head of student data after munging", "import_data", debug_import)
    print(head(rd))
  }

  results <- rd |>
    pivot_longer(3:ncol(rd),
                 names_to = "Assessment",
                 values_to = "Mark") |>
    mutate(paper = str_squish(papers), year = years, semester = semesters) |>
    select(Assessment, ID, Mark, paper, year, semester, Name)

  assessments <- tibble(Title    = assessments,
                        Weight   = weights * 100,
                        paper    = papers,
                        year     = years,
                        semester = semesters)

  paper_code <- str_remove(papers, " ")
  paper_title <- ""

  papers <- grep("Paper: ", rd |> pull(1), value = TRUE) |>
    unique() |>
    str_remove("Paper: ")

  mdebug(sprintf("File contains data for these papers: %s", plist(papers)), "import_data", debug_import)
  ##### Get assessment info #####
  assessment_rgx            <- paste0("Assessment Number: |", paste0(result_names, collapse = "|"))
  assessment_locations      <- which(grepl(assessment_rgx, rd$ID))
  result_locations          <- which(rd$ID %in% result_names)
  rd$Name[result_locations] <- result_names[1]

  assessment_year_sem  <- rd$ID[assessment_locations - 2]
  assessment_years     <- assessment_year_sem  |> str_extract("[0-9]{4}")
  assessment_sems      <- assessment_year_sem |> stringr::str_remove("[0-9]{4}, ") |>
    lapply(normalise_semesters) |> simplify()
  assessment_papers    <- rd$ID[assessment_locations - 1] |> str_remove("Paper: ")
  assessment_names     <- rd$Name[assessment_locations]
  assessment_weights   <- rd$Name[assessment_locations + 1]

  if (length(assessment_weights) != length(assessment_names))
    if ( assessment_names[nrow(assessment_names)] %in% result_names ) {
      mdebug("Found overall results", "import_data", force = debug_import)
      assessment_names[nrow(assessment_names)] <- "Overall paper result"
      assessment_weights <- assessment_weights |> add_row(tibble(w = "100%"))
    }

  assessments <- tibble(paper    = assessment_papers,
                        year     = assessment_years,
                        semester = assessment_sems,
                        Title    = assessment_names) |>
    mutate(w = assessment_weights)  |>
    mutate(Weight = as.numeric(stringr::str_remove(as.character(w), "%"))) |>
    select(-w) |>
    filter(complete.cases(Title))

  assessments$Weight[assessments$Title == result_names[1]] <- 100

  if (debug_import) {
    mdebug("Found assessment names and weights", "import_data", force = debug_import)
    print(assessments, n = Inf)
  }

  # In at least one file, weights were as floating point, not string!
  bad_asses <- NULL
  if (any(assessments$Weight < 1)) {
    message("Assessment weights less than 1% found!")
    sus_wts <- which(assessments$Weight < 1)
    bad_asses <- assessments[sus_wts, ]
    assessments$Weight[ assessments$Weight < 1] <- assessments$Weight[ assessments$Weight < 1] * 100
    print(bad_asses, n = Inf)
  }

  if (any(assessments$Weight == 0)) {
    message("Assessment weights of 0% found!")
    bad_asses <- assessments |> filter(Weight == 0)
    print(bad_asses, n = Inf)
    mdebug(sprintf("Papers with assessments worth 0%%: %s", plist(unique(bad_asses$paper))), "import_data", TRUE)
  }

  # Parse the data exported from Business Objects
  nskip  <- 13 # number of rows between result sets
  starts <- grep("Student ID", rd$ID) + 1
  ends   <- grep("Student ID", rd$ID) - nskip

  if (length(ends) > 1) {
    class_n <- starts[2] - starts[1] - nskip
  } else {
    ends <- which(is.na(rd$ID[starts[1]:length(rd$ID)])) + starts[1]
    ends <- ends[1]
    #class_n <- ends[1] - starts[1] - 1
  }

  mdebug(sprintf("Starts: '%s'; Ends: '%s'; assessments: %d; class: %d",
                 get_ss(starts[1]), get_ss(ends[1]), nrow(assessments), class_n),
         "import_data", force = debug_import)

  titles  <- rep("", length(papers))

  results <- tibble(Assessment = "", ID = NA, Name = "", Mark = 0, Grade = "", paper = "", year = "", semester = "")
  for (i in seq_len(nrow(assessments) ) ) {
    year_sm <- rd$ID[starts[i] - 6] |> str_split(",")
    year    <- year_sm[[1]][1]
    sem     <- year_sm[[1]][2] |> normalise_semesters()
    paper   <- rd$ID[  starts[i] - 5] |> str_remove("Paper: ")
    titles[i] <- rd$Name[starts[i] - 5] |> str_squish()
    start    <- starts[i]

    if (i == nrow(assessments)) {
      end     <- (start + class_n)
    } else {
      end     <- starts[i + 1] - (nskip + 1)
      class_n <- end - start
    }

    mdebug(sprintf("Processing %s %s %s from rows %6d to %6d for assessment '%s'", paper, year, sem, start, end, assessments$Title[i]),
           "import_data", debug_import)
    mdebug(sprintf("First student: %s; last student: %s", rd$Name[start], rd$Name[end]), "import_data", debug_import)

    ids     <- as.integer(as.character(rd$ID[   start:end]))
    names   <- as.character(           rd$Name[ start:end])
    marks   <- as.numeric(as.character(rd$Mark[ start:end]))
    grades  <- as.character(           rd$Grade[start:end])

    results <- rbind(results, tibble(Assessment = assessments$Title[i],
                                     ID         = ids,
                                     Name       = names,
                                     Mark       = marks,
                                     Grade      = grades,
                                     paper      = paper,
                                     year       = year,
                                     semester   = sem))
  }

  if (!is.null(bad_asses))
    for (Paper in unique(bad_asses$paper) )
      for (Year in unique(bad_asses$year) )
        for (Semester in unique(bad_asses$semester) )
          for (assignment in unique(bad_asses$Title) ) {
            bad_dat <- results |> filter(paper == Paper, year == Year, semester == Semester, Assessment == assignment)
            mdebug(sprintf("There are %4d records for an assignment worth nothing! (%s %s %s %s)",
                           nrow(bad_dat), Paper, Year, Semester, assignment), "import_data", DEBUG)
          }

  results <- results|> filter(complete.cases(results$ID))

  ft <- results |> dplyr::filter(Assessment %in% result_names) |> count(Grade)
  ft_n <- ft |> dplyr::filter(Grade == "FT") |> pull(n)
  ft_n <- ifelse(length(ft_n), ft_n, 0)
  mdebug(sprintf("Failed terms: %d", ft_n), "import_data", force = debug_import)

  fc_n <- ft |> dplyr::filter(Grade == "FC") |> pull(n)
  fc_n <- ifelse(length(fc_n), fc_n, 0)
  mdebug(sprintf("Failed compulsory: %d", fc_n), "import_data", force = debug_import)

  results$Mark[is.na(results$Mark)]   <-  0  # Won't catch cases where student has no record for that assessment (not no value)
  #results$Mark[results$Grade == "W" ] <- -1
  #results$Mark[results$Grade == "WE"] <- -2
  #results$Mark[results$Grade == "FC"] <- -4
  #results$Mark[results$Grade == "FT"] <- -5
  #results$Mark[results$Grade == "AG"] <- -6
  #results$Mark[results$Grade %in% c("FD", "FE")] <- -7

  mdebug(sprintf("Finished import of '%s' ('%s' '%s'), constructing return object",
                 plist(papers), plist(years), plist(unique(semesters))), "import_data", debug_import)

  totals <- results |>
    filter(Assessment %in% result_names) |>
    mutate(Grade = NA) |>
    select(-Assessment)

  titles <- unique(titles)
  papers <- unique(papers)

  if (length(papers) == length(titles)) {
    paper_tab <- tibble(papers, titles)
  } else {
    paper_tab <- NULL
  }

  new_dat <- list(results   = results, assessments = assessments, totals = totals,
                  report    = NULL,
                  papers    = papers,
                  titles    = titles,
                  paper_tab = paper_tab,
                  years     = unique(results$year),
                  semesters = unique(results$semester))
  return(new_dat)
}

read_BO <- function(fn) {
  mdebug("Reading BO format", "import_data", force = debug_import)
  if (is_CSV)
    rd <- read_csv(fn, col_names = c("ID", "Name", "N1", "N2", "Mark", "Grade"))
  else
    rd <- readxl::read_excel(fn, sheet = 1, range = "A1:F99999",
                             col_names = c("ID", "Name", "X1", "X2", "Mark", "Grade"))
  years     <- rd |>
    filter(grepl("Academic Year", Name)) |>
    mutate(Year = str_remove(Name, "Academic Year\\(s\\): ")) |>
    pull(Year)

  semesters <- rd |>
    filter(grepl("Semester", ID)) |>
    mutate(S = str_remove(ID, "[0-9]{4}, ")) |>
    pull(S)
  semesters <- lapply(semesters, normalise_semesters) |> simplify()
  if (is.null(rd$ID)) {
    warning("(myWarning) No data in Data tab, or malformed. Is the data in the Marks tab?")
    message("names of data fields:", paste(names(rd), collapse = ", "))
  }

  mdebug("Importing Business Objects file", "import_data", debug_import)

  papers <- grep("Paper: ", rd |> pull(1), value = TRUE) |>
    unique() |>
    str_remove("Paper: ")

  mdebug(sprintf("File contains data for these papers: %s", plist(papers)), "import_data", debug_import)
  ##### Get assessment info #####
  assessment_rgx            <- paste0("Assessment Number: |", paste0(result_names, collapse = "|"))
  assessment_locations      <- which(grepl(assessment_rgx, rd$ID))
  result_locations          <- which(rd$ID %in% result_names)
  rd$Name[result_locations] <- result_names[1]

  assessment_year_sem  <- rd$ID[assessment_locations - 2]
  assessment_years     <- assessment_year_sem  |> str_extract("[0-9]{4}")
  assessment_sems      <- assessment_year_sem |> stringr::str_remove("[0-9]{4}, ") |> lapply(normalise_semesters) |> simplify()
  assessment_papers    <- rd$ID[assessment_locations - 1] |> str_remove("Paper: ")
  assessment_names     <- rd$Name[assessment_locations]
  assessment_weights   <- rd$Name[assessment_locations + 1]

  if (length(assessment_weights) != length(assessment_names))
    if ( assessment_names[nrow(assessment_names)] %in% result_names ) {
      mdebug("Found overall results", "import_data", force = debug_import)
      assessment_names[nrow(assessment_names)] <- "Overall paper result"
      assessment_weights <- assessment_weights |> add_row(tibble(w = "100%"))
    }

  assessments <- tibble(paper    = assessment_papers,
                        year     = assessment_years,
                        semester = assessment_sems,
                        Title    = assessment_names) |>
    mutate(w = assessment_weights)  |>
    mutate(Weight = as.numeric(stringr::str_remove(as.character(w), "%"))) |>
    select(-w) |>
    filter(complete.cases(Title))

  assessments$Weight[assessments$Title == result_names[1]] <- 100

  if (debug_import) {
    mdebug("Found assessment names and weights", "import_data", force = debug_import)
    print(assessments, n = Inf)
  }

  # In at least one file, weights were as floating point, not string!
  bad_asses <- NULL
  if (any(assessments$Weight < 1)) {
    message("Assessment weights less than 1% found!")
    sus_wts <- which(assessments$Weight < 1)
    bad_asses <- assessments[sus_wts, ]
    assessments$Weight[ assessments$Weight < 1] <- assessments$Weight[ assessments$Weight < 1] * 100
    print(bad_asses, n = Inf)
  }

  if (any(assessments$Weight == 0)) {
    message("Assessment weights of 0% found!")
    bad_asses <- assessments |> filter(Weight == 0)
    print(bad_asses, n = Inf)
    mdebug(sprintf("Papers with assessments worth 0%%: %s", plist(unique(bad_asses$paper))), "import_data", TRUE)
  }

  # Parse the data exported from Business Objects
  nskip  <- 13 # number of rows between result sets
  starts <- grep("Student ID", rd$ID) + 1
  ends   <- grep("Student ID", rd$ID) - nskip

  if (length(ends) > 1) {
    class_n <- starts[2] - starts[1] - nskip
  } else {
    ends <- which(is.na(rd$ID[starts[1]:length(rd$ID)])) + starts[1]
    ends <- ends[1]
    #class_n <- ends[1] - starts[1] - 1
  }

  mdebug(sprintf("Starts: '%s'; Ends: '%s'; assessments: %d; class: %d",
                 get_ss(starts[1]), get_ss(ends[1]), nrow(assessments), class_n),
         "import_data", force = debug_import)

  titles  <- rep("", length(papers))

  results <- tibble(Assessment = "", ID = NA, Name = "", Mark = 0, Grade = "", paper = "", year = "", semester = "")
  for (i in seq_len(nrow(assessments) ) ) {
    year_sm <- rd$ID[starts[i] - 6] |> str_split(",")
    year    <- year_sm[[1]][1]
    sem     <- year_sm[[1]][2] |> normalise_semesters()
    paper   <- rd$ID[  starts[i] - 5] |> str_remove("Paper: ")
    titles[i] <- rd$Name[starts[i] - 5] |> str_squish()
    start    <- starts[i]

    if (i == nrow(assessments)) {
      end     <- (start + class_n)
    } else {
      end     <- starts[i + 1] - (nskip + 1)
      class_n <- end - start
    }

    mdebug(sprintf("Processing %s %s %s from rows %6d to %6d for assessment '%s'", paper, year, sem, start, end, assessments$Title[i]),
           "import_data", debug_import)
    mdebug(sprintf("First student: %s; last student: %s", rd$Name[start], rd$Name[end]), "import_data", debug_import)

    ids     <- as.integer(as.character(rd$ID[   start:end]))
    names   <- as.character(           rd$Name[ start:end])
    marks   <- as.numeric(as.character(rd$Mark[ start:end]))
    grades  <- as.character(           rd$Grade[start:end])

    results <- rbind(results, tibble(Assessment = assessments$Title[i],
                                     ID         = ids,
                                     Name       = names,
                                     Mark       = marks,
                                     Grade      = grades,
                                     paper      = paper,
                                     year       = year,
                                     semester   = sem))
  }

  if (!is.null(bad_asses))
    for (Paper in unique(bad_asses$paper) )
      for (Year in unique(bad_asses$year) )
        for (Semester in unique(bad_asses$semester) )
          for (assignment in unique(bad_asses$Title) ) {
            bad_dat <- results |> filter(paper == Paper, year == Year, semester == Semester, Assessment == assignment)
            mdebug(sprintf("There are %4d records for an assignment worth nothing! (%s %s %s %s)",
                           nrow(bad_dat), Paper, Year, Semester, assignment), "import_data", DEBUG)
          }

  results <- results|> filter(complete.cases(results$ID))

  ft <- results |> dplyr::filter(Assessment %in% result_names) |> count(Grade)
  ft_n <- ft |> dplyr::filter(Grade == "FT") |> pull(n)
  ft_n <- ifelse(length(ft_n), ft_n, 0)
  mdebug(sprintf("Failed terms: %d", ft_n), "import_data", force = debug_import)

  fc_n <- ft |> dplyr::filter(Grade == "FC") |> pull(n)
  fc_n <- ifelse(length(fc_n), fc_n, 0)
  mdebug(sprintf("Failed compulsory: %d", fc_n), "import_data", force = debug_import)

  results$Mark[is.na(results$Mark)]   <-  0  # Won't catch cases where student has no record for that assessment (not no value)
  #results$Mark[results$Grade == "W" ] <- -1
  #results$Mark[results$Grade == "WE"] <- -2
  #results$Mark[results$Grade == "FC"] <- -4
  #results$Mark[results$Grade == "FT"] <- -5
  #results$Mark[results$Grade == "AG"] <- -6
  #results$Mark[results$Grade %in% c("FD", "FE")] <- -7
  totals <- results |>
    filter(Assessment %in% result_names) |>
    mutate(Grade = NA) |>
    select(-Assessment)

  titles <- unique(titles)
  papers <- unique(papers)

  if (length(papers) == length(titles)) {
    paper_tab <- tibble(papers, titles)
  } else {
    paper_tab <- NULL
  }

  new_dat <- list(results   = results, assessments = assessments, totals = totals,
                  report    = NULL,
                  papers    = papers,
                  titles    = titles,
                  paper_tab = paper_tab,
                  years     = unique(results$year),
                  semesters = unique(results$semester))

  if (debug_import) {
    mdebug("Head of Assessments", "import_data", force = debug_import)
    print(head(new_dat$assessments))

    mdebug("Head of Results", "import_data", force = debug_import)
    new_dat$results |>
      dplyr::filter(Assessment %in% result_names) |>
      print()
  }

}


zzz <- function() {
  rd <- readxl::read_excel(fn, range = "A1:Z100")

  names(rd)[1:3] <- c("ID", "Surname", "Firstname")
  instance    <- rd[1, 2] |> str_squish()
  mdebug(sprintf("(import_data): Instance: '%s'", instance), "import_data", debug_import)
  papers       <- instance |> str_sub( 1, 8) |> str_remove(" ")
  semesters    <- instance |> str_extract("S[1-2]|FY")
  years        <- instance |> str_extract("[0-9]{4}") |> as.integer()
  mdebug(sprintf("(import_data): Extracted '%s' ('%s', '%s')",
                 papers, semesters, years), "import_data", debug_import)

  weights     <- t(rd[3,])
  weights     <- as.numeric(weights[complete.cases(weights)])

  assessments <- as.character(t(rd[4,]))
  assessments <- assessments[complete.cases(assessments)]

  fieldnames  <- as.character(t(rd[5,]))
  fieldnames  <- fieldnames[complete.cases(fieldnames)]

  nflds       <- length(fieldnames)
  mdebug(sprintf("There are %d fields, names are: '%s'",
                 nflds, paste(fieldnames, collapse = ", ")),
         "import_data", debug_import)

  rd <- rd |>
    select((1:all_of(nflds + 3))) %>%
    dplyr::filter(complete.cases(ID))

  if (debug_import) {
    mdebug("Head of student data", "import_data", debug_import)
    print(head(rd))
  }

  # remove empty columns
  rd <- rd |>
    select_if(~sum(is.na(.x)) != nrow(rd))

  # remove columns with alpha grades
  rd_data <- rd |>
    select(4:ncol(rd)) |>
    select_if(~!any(grepl("[A-Z]", .x))) |>
    sapply(as.numeric) |> as_tibble()

  rd_ids <- rd |>
    mutate(Name = sprintf("%s %s", Firstname, Surname)) |>
    select(ID, Name)

  rd <- rd_ids |> add_column(rd_data)
  #rd <- rd %>% select(1,2) %>% add_column(rd_data)

  names(rd)[3:ncol(rd)] <- assessments

  if (debug_import) {
    mdebug("Head of student data after munging", "import_data", debug_import)
    print(head(rd))
  }

  results <- rd |>
    pivot_longer(3:ncol(rd),
                 names_to = "Assessment",
                 values_to = "Mark") |>
    mutate(paper = str_squish(papers), year = years, semester = semesters) |>
    select(Assessment, ID, Mark, paper, year, semester, Name)
}

read_BB <- function(fnx, p = "", y = NA, s = "") {
  #fn <- fn308; p <- "";  y <- NA; s <- ""
  fnx <- str_replace_all(fnx, " ", "_")
  fd <- readxl::read_excel(fnx, col_types = "text")

  # try to find dates that this report covers
  has_data <- function(x) sum(as.numeric(x), na.rm = TRUE) > 0
  col1 <- fd |> pull(1)
  dl   <- grep("Access / Date", col1)

  if (length(dl) > 0) {
    ym <- fd |> slice(dl + 2 ) |> pull(2)
    days <- fd |> slice(dl + 2 ) |> select(where(has_data)) |> unlist() |> as.numeric()
    dates <- paste0(ym, "-", days) |> ymd()
  }

  # Detect the report that includes paper information
  if (fd[1,1] == "Course Name" & fd[2,1] == "Course ID") {
    message("Found paper info")
    cn <- fd[1, 6]
    ci <- str_split(fd[2, 6], "_", simplify = TRUE)
    p <- ci[1]; s <- ci[2]; y <- as.numeric(ci[3])
    s <- str_remove(s, "DN.*")
  }

  df <- make_tab(fd)
  rv <- list(df = df, paper = p, semester = s, year = y)

  rv
}

read_Echo <- function(fn, p = "", s = "", y = NA) {
  rd <- read_csv(fn, show_col_types = FALSE) |>
    mutate(paper = p, year = y, semester = s, .before = 1)
  df <- NULL
  df$dat <- rd
  list(df = df, paper = NULL, semester = NULL, year = NA, dates = NULL)
}

read_GC_csv <- function(fn, filename) {

  paper_psy <- str_extract(filename, "[A-Z]{4}[0-9]{3}_S[12]DN._[0-9]{4}") |> str_split("_", simplify = TRUE)

  Paper    <- paper_psy[1]
  occur    <- paper_psy[2]
  Semester <- str_sub(occur, 1, 2)
  Year     <- as.integer(paper_psy[3])

  dat  <- read_tsv(fn, local = locale(encoding = 'utf16'), show_col_types = FALSE)
  read_GC(dat, Paper, Semester, Year, "csv")
}

read_GC_xlsx <- function(fn, filename, debug = FALSE, offline = FALSE) {
  #fn <- "~/Dropbox (Maestral)/Attachments/BSNS113 S2 2024 results 20241115.xlsx"; filename <- fn; offline <- TRUE
  require(tidyverse)
  sheets        <- readxl::excel_sheets(fn)
  result_sheet  <- sheets[grep("All results", sheets)]
  ass_sheet     <- sheets[grep("Assessments", sheets)]
  paper_psy <- str_extract(filename, "[A-Z]{4}[0-9]{3} S[12] [0-9]{4}") |>
    str_split(" ", simplify = TRUE)

  Paper    <- paper_psy[1]
  occur    <- paper_psy[2]
  Semester <- str_sub(occur, 1, 2)
  Year     <- as.integer(paper_psy[3])

  dat  <- readxl::read_excel(fn, sheet = result_sheet)
  ass  <- readxl::read_excel(fn, sheet = ass_sheet) |> filter(complete.cases(Max))
  read_GC(dat, ass, Paper, Semester, Year, "xlsx")
}

read_GC <- function(res_dat, ass_dat, Paper, Semester, Year, ftype, offline = FALSE) {
  if (offline) {
    res_dat <- dat;
    ass_dat <- ass;
    ftype <- "XlsX";
    offline <- TRUE
  }

  plussage_n <- 0; plussage_stem <- NULL

  str_trans  <- c("&#x3a;" =  ":", "&#x25;" = "%", "&#x28;" = "(", "&#x29;" = ")")
  all_names  <- names(res_dat) |> str_replace_all(str_trans)

  names(res_dat) <- all_names

  has_data  <- function(x) any(complete.cases(x))

  data_cols <- res_dat |>
    map(has_data) |>
    simplify() |>
    which() |>
    as.numeric()

  dat       <- res_dat |> select(all_of(data_cols))
  new_names <- names(dat)

  if (ftype == "csv") {
    ass_cols    <- grep("Total Pts:", new_names)
    ass_names   <- str_trim(str_match(names(dat)[ass_cols], "(.*)\\[")[, 2])
    ass_weights <- str_match(names(dat)[ass_cols], "Total Pts:.* ([0-9]+)")[, 2]
  } else {
    ass_cols    <- grep("[A-Za-z0-9]+/[0-9]{1,3}", new_names)
    ass_names   <- str_trim(str_match(names(dat)[ass_cols], "(.*)/")[, 2])
  }

  if (sum(as.numeric(ass_dat$Weight)) == 100) {
    ass_flag <- FALSE
  } else {
    ass_flag <- TRUE
    message("Warning: assessment weights do not sum to 100, guessing plussage")
    possible_plussage_items <- ass_dat |>
      filter(grepl("[A-Za-z]+[0-9]+", Name))
    possible_stem <- str_extract(possible_plussage_items$Name, "([A-Za-z]+)") |>
      unique()
    if (length(possible_stem) == 1) {
      message(sprintf("Found possible plussage column: '%s'", possible_stem))
      plussage_items <- ass_dat |> filter(grepl(paste0(possible_stem, "[0-9]+"), Name))
      plussage_item <-  ass_dat |> filter(grepl(paste0(possible_stem, "$"), Name))
      overage <- (sum(plussage_items$Weight) - sum(plussage_item$Weight)) / plussage_items$Weight[1]
      plussage_stem <- plussage_item$Name
      n <- nrow(plussage_items)
      plussage_n <- n - overage
      if (plussage_n == 0) {
        message("No plussage detected")
      } else {
        message(sprintf("Plussage detected; it looks like best %1.0f of %1.0f", plussage_n, n))
      }
    } else {
      message("Unable to deduce plussage column")
    }
  }

  ass_tab <- ass_dat |>
    rename(Long_name = Title, Title = Name) |>
    mutate(Weight = if_else(Weight <  1, 100 * Weight, Weight),
           #Weight = if_else(Weight == 1, 100,        Weight)
           ) |>
    select(-Long_name)

  results_wide <- dat |>
    select(ID = `Student ID`, `First Name`, `Last Name`, all_of(ass_cols))
  names(results_wide)[-c(1:3)] <- ass_names

  print(results_wide)
  if (!any(names(results_wide) %in% internal_names) ) {
    message("No internal assessment score provided")
    message("Finding assessments that are not final exam, final result or plussage")
    nnames <- c("ID", "First Name", "Last Name",
                plussage_items$Name, exam_names, result_names)
    IA_names <- names(results_wide)[!names(results_wide) %in% nnames]
    IA_dat <- results_wide |>
      select(all_of(IA_names))
    message(sprintf("Calculating Internal score from sum of: '%s'", paste(names(IA_dat), collapse = ", ")))
    IA_dat <- IA_dat|>
       transmute(Internal = rowSums(across(where(is.numeric))))
    results_wide <- results_wide |> add_column(IA_dat)
    FE_w <- ass_tab |> filter(Title %in% exam_names) |> pull(Weight)
    FE_w <- ifelse(is.null(FE_w), 50, FE_w)
    ass_tab <- ass_tab |>
      add_row(Title = "Internal", Weight = 100 - FE_w,
              #paper = Paper, year = Year, semester = Semester
              )
  }

  # Convert non-% data to % data
  ndat <- results_wide |> select(-c(1:3))
  rdat <- ndat
  for (j in 1:length(ass_tab$Max)) {
    cname <- ass_tab$Title[j]
  #  message(sprintf("Converting %s to a percentage out of %3.1f",
   #                 cname, ass_tab$Max[j]))
    if (cname %in% names(ndat))
      rdat[, cname] <- 100 * ndat[, cname] / ass_tab$Max[j]
  }

  # Implement plussage if it's missing
  if (plussage_n > 0 & !is.null(plussage_stem)) {
    pdat <- rdat |> select(all_of(plussage_items$Name))
    pscores <- rep(0, nrow(pdat))
    for (i in 1:nrow(pdat)) {
      scores <- sort(pdat[i,] |> unlist(), decreasing = TRUE)
      pscores[i] <- sum(scores[1:plussage_n])/plussage_n
    }

    print(plussage_stem)
    print(names(rdat))
    if (plussage_stem %in% names(results_wide)) {

      delta_tab <- results_wide |>
        select(provided_score = all_of(plussage_stem)) |>
        mutate(plussage_score = pscores,
               Delta = provided_score - plussage_score)
      delta <- sum(delta_tab$Delta)

      if (delta == 0) {
        message("Plussage was calculated correctly")
        if (!plussage_stem %in% names(results_wide)) {
          rdat <- rdat |> mutate(Pscore = pscores)
          names(rdat)[ncol(rdat)] <- possible_stem
        }
      } else {
        message("Plussage was NOT calculated correctly")
        print(delta_tab)
      }
    }
  }

  if (DEBUG)
    print(rdat)

  results_wide <- results_wide |>
    select(1:3) |>
    add_column(rdat)

  results <- results_wide |>
    select(-`First Name`, -`Last Name`) |>
    map(as.numeric) |>
    as_tibble() |>
    pivot_longer(-1, names_to = "Assessment", values_to = "Mark") |>
    mutate(paper = Paper, semester = Semester, year = as.numeric(Year)) |>
    select(Assessment, ID, Mark, paper, year, semester)

  if (DEBUG | offline) {
    print(dat)
    print(ass_tab)
    print(results_wide)
    print(results)
  }

  list(paper = Paper, semester = Semester, year = Year,
       results = results, results_wide = results_wide,
       assessments = ass_tab, flag = ass_flag)
}

get_examples <- function() {
  fn <- "~/Dropbox (Maestral)/Attachments/BSNS111 S2 2024 results 20241022.xlsx"; filename <- fn; rv1 <- read_GC_xlsx(fn,filename)
  fn <- "~/Dropbox (Maestral)/Attachments/BSNS112 S2 2024 results 20241114.xlsx"; filename <- fn; rv2 <- read_GC_xlsx(fn,filename)
  fn <- "~/Dropbox (Maestral)/Attachments/BSNS113 S2 2024 results 20241115.xlsx"; filename <- fn; rv3 <- read_GC_xlsx(fn,filename)
  fn <- "~/Dropbox (Maestral)/Attachments/BSNS114 S2 2024 results 20241114.xlsx"; filename <- fn; rv4 <- read_GC_xlsx(fn,filename)
  fn <- "~/Dropbox (Maestral)/Attachments/BSNS115 S2 2024 results 20241118.xlsx"; filename <- fn; rv5 <- read_GC_xlsx(fn,filename)

  rv1$assessments; sum(rv1$assessments$Weight)
  rv2$assessments; sum(rv2$assessments$Weight)
  rv3$assessments; sum(rv3$assessments$Weight)
  rv4$assessments; sum(rv4$assessments$Weight)
  rv5$assessments; sum(rv5$assessments$Weight)
}

make_tab <- function(d) {
  #d <- fd
  require(tidyverse)
  # The Excel files are organised for human reading, not data processing.
  # There may be repeated rows for students, with different column headings
  #d <- rv
  name_col <- if_else(names(d)[1] == "Course Activity Overview", 1, 2)

  student_names <- d |>
    select(c1 = all_of(name_col)) |>
    filter(grepl("\\([a-z]{5,5}[0-9]{3,3}\\)", c1)) |>
    pull(c1) |>
    unique()

  all_names <- d |> pull(name_col)
  n_names   <- length(student_names)

  starts <- which(student_names[1]       == all_names)
  ends   <- which(student_names[n_names] == all_names)
  class_size <- ends[1] - starts[1] - 1

  if (length(ends) < length(starts))
    starts <- starts[1:length(ends)]

  b1 <- d |> slice((starts[1] - 1):ends[1])
  b1 <- b1[, colSums(is.na(b1)) < nrow(b1) ]
  cnames <- b1 |> slice(1) |> as.character()
  cnames[1] <- "Student"

  if (any(is.na(cnames))) {
    na_names <- which(is.na(cnames))
    cnames[na_names] <- sprintf("V%d", 1:2)
  }

  names(b1) <- cnames
  b1 <- b1 |> slice(-1)

  b <- b1

  if (length(starts) > 1) {
    for (i in 2:length(starts)) {
      message(sprintf("Processing block %d", i))
      bn <- d |> slice((starts[i] - 1):ends[i])
      bn <- bn[, colSums(is.na(bn)) < nrow(bn) ]
      cnames <- bn |> slice(1) |> as.character()
      cnames[1] <- "Student"

      # Detect calendar data
      if (cnames[2] == "1")  {
        dates      <- bn |> slice(1) |> as.character() # Student 1 2 ... 31 Total
        month_days <- cnames[c(-(1), -length(cnames))] # 1 ... 31
        last_col   <- sprintf("%s_%s", dates[length(dates)], dates[1]) # Rename Total to Total_[Month digit]
        cnames[length(cnames)] <- last_col
        dates <- sprintf("%s-%s", dates[1], month_days)
        cnames[c(-(1), -length(cnames))] <- dates
        #message("Found calendar data")
        #print(cnames)
        #next
      }
      names(bn) <- cnames

      bn <- bn |> slice(-1)
      b <- b |> left_join(bn, by = "Student")
      #print(paste(names(b), collapse = ","))
    }
  }

  d2 <-  b |> select(!matches("Archive"))

  chr_cols <- "^Student|Username|paper|year|semester"
  Student <- d2 |> dplyr::select(matches(chr_cols,  ignore.case = FALSE))
  dat     <- d2 |> dplyr::select(-matches(chr_cols, ignore.case = FALSE)) |> modify(as.numeric)
  dat     <- Student |> add_column(dat)

  calendar_dat <- NULL; calendar_tidy <- NULL;

  col2       <- d |> pull(2)
  cal_starts <- grep("[0-9]{4}-[0-9]{2}", col2)

  ccols <- function(x) sum(any(complete.cases(x))) > 0
  calendar_dat <- dat |> select(Student)
  if (length(cal_starts)) {
    for(cal_start in cal_starts) {
      data_cols <- d |> slice(cal_start) |> unlist() |> complete.cases() |> which()
      month_dat <- d |> slice(cal_start:(cal_start + class_size + 2)) |> select(all_of(data_cols))
      cnames    <- month_dat |> slice(1) |> as.character()
      cnames[-c(1, length(cnames))] <- paste0(cnames[1], "-", cnames[-c(1, length(cnames))])
      names(month_dat) <- cnames
      month_dat    <- month_dat |> slice(-1) |> select(-1, -Total)
      calendar_dat <- calendar_dat |> add_column(month_dat)
    }
  }

  calendar_dat <- calendar_dat |> select(where(ccols))
  dates <- as_date(names(calendar_dat[-1]))

  sumc       <- function(x) sum(as.numeric(x))
  day_counts <- calendar_dat |> select(-c(1, ncol(calendar_dat))) |> map(sumc) |> simplify()
  wdays      <- wday(ymd(names(day_counts)), TRUE)

  calendar_tidy <- tibble(date = ymd(names(day_counts)), day = wdays, hits = day_counts)

  list(dat = dat, calendar_dat = calendar_dat, calendar_tidy = calendar_tidy, dates = dates)
}

sniff_eng <- function(df) {

  if ("bb-achievements" %in% names(df))
    return("SUA")

  if ("Total" %in% names(df))
    return("AUA")

  if ("Course Activity in Hours" %in% names(df))
    return("CAO")

  if ("Section Name" %in% names(df))
    return("Echo")

  return("UK")
}

read_eng_files <- function(d, offline = FALSE) {
  #offline <- TRUE
  if (offline) {
    d1 <- "~/Downloads/MART308_S1_2025/"
    d2 <- paste0(d1, "xlsx/")
    xls_files <- list.files(d1, pattern = "*.xls$")

    for (fn in xls_files) {
      fi <- file.info(paste0(d1, fn))
      file.copy(paste0(d1, fn), "~/Downloads/", overwrite = TRUE)
      system(sprintf("soffice --headless --convert-to xlsx:\"Calc MS Excel 2007 XML\" --outdir %s %s",
                     d2, paste0("~/Downloads/", fn)))

      fn <- paste0(fn, "x")
      rv <- read_BB(paste0(d2, fn))
      message(sniff_eng(rv$df))
    }

    d <- d2
  }

  xlsx_files <- list.files(d, pattern = "*.xlsx")
  for (fn in xlsx_files) {
    fi <- file.info(paste0(d, fn))
    rv <- read_BB(paste0(d, fn))
    if (offline)
      message(sniff_eng(rv$df$dat))
    else
      eng_rvs(list(df = rv$df, fi = fi))
  }

  if (offline)
    d <- d1

  csvfiles <- list.files(d, pattern = "*.csv")
  for (fn in csvfiles) {
    fi <- file.info(paste0(d, fn))
    rv <- read_Echo(paste0(d, fn))
    if (offline)
      message(sniff_eng(rv$df$dat))
    else
      eng_rvs(list(df = rv$df, fi = fi))
  }
}

augment_students <- function(d) {
  d |>
    filter(complete.cases(Papers)) |>
    mutate(
      ACCT     = grepl("ACCT", Papers),
      BSNS     = grepl("BSNS", Papers),
      ECON     = grepl("ECON", Papers),
      COMP     = grepl("COMP", Papers),
      FINC     = grepl("FINC", Papers),
      INFO     = grepl("INFO", Papers),
      MART     = grepl("MART", Papers),
      MANT     = grepl("MANT", Papers),
      TOUR     = grepl("TOUR", Papers),
      Name     = paste(Knownname, Surname),
      username = str_sub(Email, 1, 8),
      Māori    = if_else(str_detect(Ethnicity, mao_rgx), TRUE, FALSE),
      Pacific  = if_else(str_detect(Ethnicity, pac_rgx), TRUE, FALSE),
      MP       = if_else(Māori | Pacific, TRUE, FALSE)
    )
}

# For a student view, three BO reports are needed
# 1. All students at OU enrolled in a calendar year (Eth, Rest?)
# 2. CARPT002 for all students enrolled in Commerce (Eth, Rest?)
# 3. CARPT004 for conditional - needs a list of papers, so 002 is prerred. But where to get conditionals?

read_enr <- function(fn) {
  # FIXME: which report is needed?
  # By paper needs list of papers, but has ethnicity, DOB and conditional;
  # By division has list of papers but no ethnicity, but has all students in all programmes
  # fn  <- "~/Documents/OneDrive/Service/School/TALC/Divisional Dashboard/Data/Students/CARPT001-EnrolmentandCourseApprovalStatusreport(byAcademicYear)-2021onwards.xlsx"
  # fn  <- "~/Documents/OneDrive/Service/School/TALC/Divisional Dashboard/Data/Students/CARPT002-EnrolmentandCourseApprovalStatusreport(byDivision).xlsx";
  # fn  <- "~/Documents/OneDrive/Service/School/TALC/Divisional Dashboard/Data/Students/CARPT004-EnrolmentandCourseApprovalStatusReport(byPaper).XLSx";

  if (str_detect(fn, "CARPT002")) {
    renr_dat <- readxl::read_excel(fn, skip = 8, col_types = "text")
    enr_dat  <- renr_dat |>
      select(ID        = `Student ID`,
             Enrolment = `Programme Enrolment Status`,
             Approval  = `Prog Course Approval Status`,
             Papers    = Module)
  } else {
    enr_dat <- readxl::read_excel(fn, skip = 12, col_types = "text")
    papers  <- grep("[A-Z]{4}[0-9]{3}", enr_dat$`Paper Code`, value = TRUE)
    cnames  <- enr_dat |> slice(4) |> as.character()
    enr_dat <- enr_dat |> slice(-c(1:4))
    names(enr_dat) <- cnames
    enr_dat <- enr_dat |>
      select(Period,
             ID          = `Student ID`,
             Enrolment   = `Paper Enrolment Status`,
             Approval    = `Paper Course Approval Status`,
             Restriction = `Academic Restriction`,
             Status      = `Student Status`
      )

    starts <- grep("Student Details", enr_dat$Period) + 2
    ends   <- grep("Summary per Course Approval Status", enr_dat$Period) - 2
    starts <- c(1, starts)

    b <- enr_dat |>
      slice(starts[1]:ends[1]) |>
      mutate(paper = papers[1], .before = 1)

    for (i in 2:length(papers)) {
      b <- b |>
        add_case(
          enr_dat |>
            slice(starts[i]:ends[i]) |>
            mutate(paper = papers[i], .before = 1)
        )
    }

    enr_dat <- b |>
      mutate(ID = as.numeric(ID), year = 2025) |>
      rename(semester = Period)

    paper_dat <- enr_dat |>
      group_by(ID) |>
      summarise(papers = paste(paper, collapse = ","))

    nenr_dat <- enr_dat |>
      distinct(ID, .keep_all = TRUE) |>
      left_join(paper_dat, by = "ID")
  }

  set_tab("enrolment", nenr_dat |> select(-papers))
  mdebug(sprintf("Imported %s rows of enrolment data",
                  fmt_n(nrow(nenr_dat))), "read_enr", TRUE)
  return(TRUE)
}

dead_code <- function() {
  # New data has just been imported
  df1 <- df1 |> fix_unames()
  df2 <- df2 |> fix_unames()
  df3 <- df3 |> fix_unames()

  psy    <- str_split(rvs$eng_psy, " ", simplify = TRUE)
  eng_tab_e <- get_tab("engagement")

  staff <- get_tab("users") |> pull(username)

  print(names(df1))
  print(names(df2))
  print(names(df3))
  print(names(df4))
  eng_tab_n <- df1 |>       rename(`Total content hits` = Total) |>
    left_join(df2 |> rename(`Total tool hits`    = Total), by = c("Student", "username")) |>
    left_join(df3 |> rename(`Total hours`        = Total), by = c("Student", "username")) |>
    select(Student, starts_with("Total"), everything()) |>
    mutate(Student = str_remove(Student, "\\(.+\\)")) |>
    left_join(df4,  by = "Student") |>
    filter(!username %in% c(staff, "aitro92p")) |>
    left_join(stab, by = "username") |>
    select(ID, username,
           content    = `Total content hits`,
           tools      = `Total tool hits`,
           bb_hours   = `Total hours`,
           echo_hours =  Total,
           echo_pc    = `%`) |>
    mutate(paper = psy[1], semester = psy[2], year = as.integer(psy[3]))

  new_eng_tab <- eng_tab_e |>
    add_case(eng_tab_n) |>
    distinct(paper, year, semester, ID, .keep_all = TRUE)

  set_tab("engagement", new_eng_tab)
  eng_tab <- eng_tab_n |>
    rename(BB_h = bb_hours, Echo_h = echo_hours) |>
    left_join(stab, by = c("ID", "username"))

  rvs$eng_tab <- eng_tab

  eng_meta <- get_tab("engagement_meta") |>
    distinct(paper, year, semester, .keep_all = TRUE)

  print(class(rvs$eng_dates))
  print(rvs$eng_dates)
  eng_meta <- eng_meta |>
    add_case(paper = psy[1], year = as.numeric(psy[3]), semester = psy[2],
             first_date = min(rvs$eng_dates),
             last_date  = max(rvs$eng_dates))

  set_tab("engagement_meta", eng_meta)
}

mm <- function(d, x) {
  d |>
    filter(!is.na({{x}})) |>
    filter({{x}} | 0) |>
    pull({{x}}) |>
    median() |>
    round()
}

mx <- function(d, x) {
  d |>
    filter(!is.na({{x}})) |>
    filter({{x}} | 0) |>
    pull({{x}}) |>
    max()
}

zc <- function(d, x) {
  d |>
    filter(!is.na({{x}})) |>
    filter({{x}} | 0) |>
    nrow()
}


fix_unames <- function(d) {
  rgx1 <- "\\((.*)\\)"
  rgx2 <- "\\s+\\(.*"

  d |> mutate(username = str_extract(Student, rgx1, group = 1),
              Student  = str_remove( Student, rgx2))
}

pc <- function(x, y) round( 100 * (x - y)/y )

smin <- function(x) min(x, na.rm = TRUE)
smax <- function(x) max(x, na.rm = TRUE)

path_escape <- function(p) {
  str_replace_all(p, "([ \\)\\()])", "\\\\\\1")
}

remove_paper <- function(d, p, y, s) {
  d |>
    mutate( instance =  sprintf("%s_%s_%s", paper, semester, year)) |>
    filter(!instance == sprintf("%s_%s_%s", p, s, y)) |>
    select(-instance)
}

process_GC <- function(nd, offline = FALSE) {
  #offline <- TRUE
  mdebug(sprintf("Adding data for %s (%s %s)", nd$paper, nd$year, nd$semester),
         "process_GC")

  if (offline) {
    res_tab <- get_tab("results") |> distinct()
  } else {
    res_tab <- get_results()
  }

  res_tab <- res_tab |> remove_paper(nd$paper, nd$year, nd$semester)
  new_tab <- nd$results
  res_dat <- res_tab |>
    add_case(new_tab)
  set_tab("results", res_dat)

  if (offline) {
    ass_tab <- get_tab("assessments") |>
      distinct(paper, year, semester, Title, .keep_all = TRUE)
  } else {
    ass_tab <- get_assessments()
  }

  ass_tab <- ass_tab |> remove_paper(nd$paper, nd$year, nd$semester)
  new_tab <- nd$assessments |>
    select(Title, Weight) |>
    mutate(paper = nd$paper, year = nd$year, semester = nd$semester)
  message("Imported assessment data")
  print(new_tab)

  ass_dat <- ass_tab |>
    add_case(new_tab)
  set_tab("assessments", ass_dat)
}
