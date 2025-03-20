# Current priorities:
# 1. Make importing data bullet-proof (de-duplicate all tables)
#
# Bugs
#  1. Divisional KPIS load single-paper data first!
#
# Improvements
#  0. Add default Dept/Prog per user; fix default semester
#  1. Find some way to speed up plot generation?
#  2. Divisional KPIs far too slow
#  3. Detect anomalies in BO output, like all internal assessment being worth 0%! (ECON112, 2017)
#  4. make all field names in database tables lower case
#  5. In admin/database maintenance tab, grey out "restore from backup" if no file is chosen

#  6. When importing data, refresh views so it appears in GUI immediately
#  7. Speed-ups, especially generation of plots in Assessment tabs: pre-populate somehow?
#  8. Ensure numeric codes are consistent: make a lookup table and/or function to encode/decode them
#  9. Check that rounding of calculated final result equals that provided in BO
# 10. A new Report record is appended needlessly when report editor tab is loaded
# Features to be added
# 1. Make CI framework to check data integrity after importing new data
# 2. API access to data sources *not download from BO*

# Data Architecture
# 1. Individual Paper results are obtained from EXRPT11 (SAT & SMR),
#    instructions in Help tab
#
# 2. For "All students" tab, need two other data sources, updated annually or
#    per-semester
#    (a) Student contact details, currently coded to be obtained from TTRPT02
#        or SDRPT06, to get programme and major
#    (b) Overall paper results for all MART papers, from EXRPT11
#
# 3. This app calculates Total (Overall paper result) on-the-fly from data held
#    in its own database. This is so users can explore the implications of scaling
#    interactively, and was originally designed to that scaled data could be
#    downloaded from the app and uploaded directly into eVision.

library(tidyverse)
library(ggtext)
library(DT)
library(tidyselect)
library(gt)
library(shiny)

track_load_time   <- TRUE
debug_equity      <- FALSE
DEBUG             <- FALSE
debug_load        <- FALSE
debug_flicker     <- FALSE
debug_dat         <- FALSE
debug_log         <- FALSE
debug_import      <- FALSE
debug_editing     <- FALSE
debug_scaling     <- FALSE
debug_bz          <- FALSE
debug_stats       <- FALSE
debug_permissions <- FALSE
debug_RAM         <- FALSE

load_start      <- Sys.time()
load_end        <- Sys.time()

app_status      <- list(is_emergency = TRUE)

# FIXME: make these configurable via the GUI?
dept            <- "Marketing"
backup_reports  <- FALSE
app_path        <- getwd()
#register_gfont()

shiny::shinyServer(function(input, output, session) {
  #options(shiny.reactlog = TRUE)
  library(tidyverse)
  library(gridExtra)
  library(htmltools)

  source("utility_functions.R",   local = TRUE) #
  source("database_functions.R",  local = TRUE) #

#  DEBUG          <- FALSE

  mdebug("<=========  Start Loading Screen ===========>",
         "debug_load", force = debug_flicker | DEBUG)

  options(shiny.maxRequestSize = 50 * 1024^2)

  #options(shiny.trace = TRUE)
  #options(shiny.fullstacktrace = TRUE)
  #options(shiny.error = browser)
  #options(warn = 2)

#  library(reactlog)
 # reactlog_enable()

  #mariadb -u marketing -prhcb2233 -e'show tables;'
  #mariadb -h mariadb.marketing-ebreporter -u marketing -prhcb2233 < backups/EB_datadump_2024_11_11_15_02.sql

  pac_rgx     <- "Samoan|Tongan|Fijian|Tokelauan|Cook Island M.+ori|Niuean|Other Pacific Peoples"
  mao_rgx     <- "New Zealand Māori.*"

  admin_tabs <- c("Admin", "Log", "Packages", "Database terminal",
                  "Postgraduate sheet", "All students")

  user_tabs  <- c("Overview", "Report", "Internal assessment",
                 "Summary statistics", "Report Editor",
                 "Data", "Notable results")
  kpi_tabs   <- c("Divisional KPIs")
  dt_bare    <- list(length_menu = FALSE,
                     searching = FALSE,
                     ordering = FALSE,
                     paging = FALSE,
                     info = FALSE)

  disad       <- list(Gender = 'Male', Maori = "Māori", Pacific = "Pacific")

  hide_tabs <- function(tl) {
    for (tab in tl) {
      hideTab(inputId = "UI", target = tab)
    }
  }

  show_tabs <- function(tl) {
    for (tab in tl) {
      showTab(inputId = "UI", target = tab)
    }
  }

  emergency_shell <- function() {
    if (input$is_emergency == TRUE) {
      mdebug("Hiding tabs because of emergency", "emergency_shell", TRUE)

      hide_tabs(c(user_tabs, admin_tabs, kpi_tabs, "Kaiawhina", "Engagement"))
      showTab(inputId = "UI", target = "Admin")
      showTab(inputId = "UI", target = "Database terminal")

    } else {
      mdebug("There is no emergency", "emergency_shell", TRUE)
      show_tabs(c(user_tabs, kpi_tabs))

      if ("admin" %in% get_permissions( get_user() )) {
        show_tabs(admin_tabs)
      }
    }
  }

  observeEvent(input$is_emergency, {
    mdebug(sprintf("input$is_emergency is now '%s'", input$is_emergency),
           "observeEvent", DEBUG)
    set_emergency_status(input$is_emergency)
    emergency_shell()
  })

  rvs <- reactiveValues(  #dynamic data object
    staff_list   = NA,
    log          = NULL,
    instance     = NULL,
    instances    = NULL,
    users        = NULL,
    totals       = NULL,
    results      = NULL,
    students     = NULL,
    assessments  = NULL,
    enrolment    = NULL,
    reports      = NULL,
    config       = NULL,
    eng_tab      = NULL,
    bb_AUA       = NULL,
    bb_AUA_fi    = NULL,
    bb_SUA       = NULL,
    bb_SUA_fi    = NULL,
    bb_CAO       = NULL,
    bb_CAO_fi    = NULL,
    Echo360      = NULL,
    Echo360_fi   = NULL,
    eng_psy      = NULL,
    eng_dates    = NULL,
    akoka        = NULL
  )

  get_config <- function(field = NULL) {
    if ( is.null(rvs$config) ) {
      mdebug(sprintf("Fetching config table from database"), "get_config", debug_RAM)
      tab <- get_tab("config")
      isolate(rvs$config <- tab)
    } else {
      mdebug(sprintf("Reading config table from RAM"), "get_config", debug_RAM)
    }

    if (is.null(field)) {
      rvs$config
    } else {
      rvs$config[field]
    }
  }

  get_users <- reactive({
    if ( is.null(rvs$users) ) {
      mdebug(sprintf("Fetching users table from database"), "get_users", debug_RAM)
      tab <- get_tab("users") |> distinct() |> filter(complete.cases(username), username != "")
      isolate(rvs$users <- tab)
      #observe(rvs$users <- tab)
    } else {
      mdebug(sprintf("Reading users table from RAM"), "get_users", debug_RAM)
    }

    rvs$users
  })

  get_instances <- function() {
    if ( is.null(rvs$instances) ) {
      mdebug(sprintf("Fetching instances table from database"), "get_instances", debug_RAM)
      tab <- get_tab("instances") |> distinct()
      isolate(rvs$instances <- tab)
    } else {
      mdebug(sprintf("Reading instances table from RAM"), "get_instances", debug_RAM)
    }

    rvs$instances
  }

  get_assessments <- function() {
    if ( is.null(rvs$assessments) ) {
      mdebug(sprintf("Fetching assessments table from database"), "get_assessments", debug_RAM)
      tab <- get_tab("assessments") |>
        distinct(paper, year, semester, Title, .keep_all = TRUE)
      isolate(rvs$assessments <- tab)
    } else {
      mdebug(sprintf("Reading assessments table from RAM"), "get_assessments", debug_RAM)
    }

    rvs$assessments
  }

  get_enrolment <- function() {
    if ( is.null(rvs$enrolment) ) {
      mdebug(sprintf("Fetching enrolment table from database"), "get_enrolment", debug_RAM)
      tab <- get_tab("enrolment") |> distinct()
      isolate(rvs$enrolment <- tab)
    } else {
      mdebug(sprintf("Reading enrolment table from RAM"), "get_ernolment", debug_RAM)
    }

    rvs$enrolment
  }

  get_reports <- function() {
    if ( is.null(rvs$reports) ) {
      mdebug(sprintf("Fetching reports table from database"), "get_reports", debug_RAM)
      tab <- get_tab("reports") |> distinct()
      isolate(rvs$reports <- tab)
    } else {
      mdebug(sprintf("Reading reports table from RAM"), "get_reports", debug_RAM)
    }

    rvs$reports
  }

  get_totals <- function() {
    if ( is.null(rvs$totals) ) {
      mdebug(sprintf("Fetching totals table from database"), "get_totals", debug_RAM)
      tab <- get_tab("totals") |> distinct()
      isolate(rvs$totals <- tab)
    } else {
      mdebug(sprintf("Reading totals table from RAM"), "get_totals", debug_RAM)
    }

    rvs$totals
  }

  get_students <- function() {
    if (is.null(rvs$students)) {
      mdebug(sprintf("Fetching students table from database"), "get_students", debug_RAM)
      tab <- get_tab("students") |>
        augment_students() |>
        distinct()

      isolate(rvs$students <- tab)
    } else {
      mdebug(sprintf("Reading students table from RAM"), "get_students", debug_RAM)
    }

    rvs$students
  }

  get_engagement <- function() {
    if (is.null(rvs$eng_tab)) {
      utab <- get_tab("users")
      stab <- get_students()

      etab <- get_tab("engagement") |>
        filter(paper    == rvs$instance$paper,
               #year     == rvs$instance$year,
               semester == rvs$instance$semester,
               !username %in% utab$username
        ) |>
        distinct(paper, year, semester, ID, .keep_all = TRUE)

      etab <- etab |>
        left_join(stab |> select(ID, Name, IWI, Māori, Pacific, MP, Conditional,
                                 Mobile, Email, Ethnicity),
                  by = c("ID"))
      rvs$eng_tab <- etab
    } else {
      rvs$eng_tab
    }
  }

  get_results <- function() {
    if (input$is_emergency)
      return(NULL)

    if ( is.null(rvs$results) ) {
      mdebug(sprintf("Fetching results table from database"), "get_results", debug_RAM)
      rtab <- get_tab("results") |>
        distinct(paper, year, semester, ID, Assessment, .keep_all = TRUE) |>
        select(paper, year, semester, ID, Name, Assessment, Mark, Grade)
      isolate(rvs$results <- rtab)
    } else {
      mdebug(sprintf("Reading results table from RAM"), "get_results", debug_RAM)
    }

    rvs$results
  }

  get_dept_list <- function() {
    cf <- get_config()
    list(choices = c("BCom Core", "Economics", "Marketing"),
         selected = cf$default_dept)
  }

  get_paper_list <- function() {
    ptab <- get_results() |>
      distinct(paper, year, semester) |>
      arrange(paper)

    p_list <- ptab |>
      transmute(p = paper) |>
      arrange(p) |>
      unlist() |>
      as.character() |>
      unique()

    i_list <- ptab |>
      transmute(p = sprintf("%s %s %s", paper, year, semester)) |>
      arrange(p) |>
      unlist() |>
      as.character() |>
      unique()

    p_choices        <- p_list
    names(p_choices) <- p_list

    return(list(choices = p_choices, list = p_list, instances = i_list))
  }

  output$choose_kpi_papers <- renderUI({
    if(input$is_emergency)
      return(NULL)
    selectInput(inputId  = "kpis_papers",
                label    = "Include papers",
                choices  = c("All", get_paper_list()$choices),
                selected = "All",
                multiple = TRUE)

  })

  output$coord_years <- renderUI({
    if (input$is_emergency)
      return(NULL)

    yrs <- get_years()

    selectInput(inputId  = "access_year",
                label    = "Year",
                choices  = yrs,
                selected = yrs[1],
                width    = "6em")
  })

  get_years <- function() {
    ytab <- get_results() |> distinct(year)

    if (is.null(ytab))
      return(NULL)
    else
      ytab |>  pull(year) |> sort(decreasing = TRUE)
  }

  update_config <- function(field, data, write = TRUE) {
    if ( input$is_emergency )
      return(NULL)

    if (!is.null(data)) {
      cf <- get_config()

      if (is.null(cf))
        return(NULL)

      cf[, field] <- data


      if (write) {
        mdebug(sprintf("Updating config field '%s' with data: '%s'", field, data), "update_config", DEBUG)
        set_tab("config", cf)
      }

      rvs$config <<- cf
    }
  }

  get_instance_table <- function() {
    if ( input$is_emergency )
      return(NULL)

    rtab <- get_results()
    itab <- get_instances()
    utab <- get_users()

    rvs$instance_tab <<- rtab |>
      group_by(paper, semester, year) |>
      summarise(records = n(), .groups = "keep") |>
      left_join(itab, by = c(paper = "paper", year = "year", semester = "semester")) |>
      left_join(utab, by = c(Coordinator = "username")) |>
      arrange(desc(year), paper)
  }

  output$get_kpis_years <- renderUI({
    bcom_years <- get_totals() |>
      select(paper, year) |>
      filter(paper %in% BCom_core_papers | grepl("BSNS1", paper)) |>
      pull(year) |>
      unique() |>
      sort(decreasing = TRUE)

    selectInput(inputId = "kpis_years",
                label = "Choose years",
                choices = c("All", bcom_years),
                selected = "All",
                multiple = TRUE)
  })

  get_etab <- function(tp = NULL, ty = NULL, ts = NULL,
                       pr = FALSE, exam_only = FALSE,
                       mf = TRUE, offline = FALSE,
                       get_gaps = TRUE, get_grades = TRUE) {

    if (offline) {
      source("utility_functions.R")
      rv        <- simulate_UI()
      input     <- rv$input
      input$is_emergency <- FALSE
      dd        <- rv$dd
      tp          <- dd$paper
      ty           <- dd$year
      ts           <- dd$semester
      mf           <- FALSE
      or_only      <- FALSE
      exam_only    <- TRUE
      mf           <- TRUE
      debug_equity <- TRUE
      tp <- NULL; ty <- NULL; ts <- NULL; pr <- FALSE
    }

    if (input$is_emergency)
      return(NULL)

    ge_s <- Sys.time()

    if (offline) {
      rtab <- get_tab("results")
    } else {
      rtab <- get_results()
    }

    if (exam_only) {
      rtab <- rtab |> filter(grepl("Final [Ee]xam|^[Ee]xam$", Assessment))
    } else {
      rtab <- rtab |> filter(Assessment %in% result_names)
    }

    #### Respond to users choices ####
    if (is_empty(input$kpis_papers)) {
      mdebug("Generating per-paper equity stats", "get_etab", debug_equity)
    } else {
      mdebug("Generating several-paper equity stats", "get_etab", debug_equity)
      if ( (length(input$kpis_papers) > 1 | input$kpis_papers[1] != "All") & is.null(tp) )
        rtab <- rtab |> filter(paper %in% input$kpis_papers)

      if ( (length(input$kpis_years)  > 1 | input$kpis_years[1]  != "All") & is.null(ty) )
        rtab <- rtab |> filter(year %in% input$kpis_years)

      if ( (length(input$kpis_semesters)  > 1 | input$kpis_semesters[1]  != "All") & is.null(ts) )
        rtab <- rtab |> filter(semester %in% input$kpis_semesters)

      if ( (length(input$kpis_inc_maj) > 1 | input$kpis_inc_maj[1] != "All") )  {
        st <- paste(input$kpis_inc_maj, collapse = "*|")
        rtab <- rtab |> filter(grepl(st, paper))
      }

      if ( !input$kpis_inc_PG )
        rtab <- rtab |> filter(!grepl("[A-Z]{4}[4-5]", paper))

      if ( !input$kpis_inc_UG )
        rtab <- rtab |> filter(!grepl("[A-Z]{4}[1-3]", paper))

      if ( !input$kpis_inc_core )
        rtab <- rtab |> filter(!paper %in% BCom_core_papers,!grepl("BSNS1[0-9]{2}", paper))

      if ( input$kpis_inc_core & !input$kpis_inc_noncore )
        rtab <- rtab |> filter(paper %in% BCom_core_papers | grepl("BSNS1[0-9]{2}", paper))

      if ( !input$kpis_inc_noncore )
        rtab <- rtab |> filter(paper %in% BCom_core_papers | grepl("BSNS1", paper))

      if ( !input$kpis_inc_BEntr )
        rtab <- rtab |> filter(!grepl("ENTR", paper))
    }

    # Some combinations of choices will result in no data, so return early,
    # with a warning
    if (nrow(rtab) == 0) {
      if (offline) {
        mdebug("Your choices result in no data", "get_etabs", TRUE)
      } else {
        showNotification(p("Your choices result in no data"), duration =  20)
        return(NULL)
      }
    }

    if (offline) {
      stab <- get_tab("students") |> distinct(ID, .keep_all = TRUE)
    } else {
      stab <- get_students() |> distinct(ID, .keep_all = TRUE)
    }

    #### Add student fields ####
    etab <- rtab |>
      left_join(stab |> select(ID, Ethnicity, Gender,
                               Programme, Programme2,
                               Prog1Maj1, Prog1Maj2, Prog2Maj1, Prog2Maj2), by = "ID") |>
      filter(complete.cases(Gender)) |>
      mutate(Māori   = if_else(str_detect(Ethnicity, mao_rgx), "Māori",   "Non-Māori"),   .before = Ethnicity) |>
      mutate(Pacific = if_else(str_detect(Ethnicity, pac_rgx), "Pacific", "Non-Pacific"), .before = Ethnicity) |>
      mutate(MP      = if_else(Māori == "Māori" | Pacific == "Pacific",  "Māori or Pacific", "Neither Māori nor Pacific"), .after  = Pacific) |>
      mutate(Pass    = if_else(Mark > 49, TRUE, FALSE), .after  = Mark) |>
      distinct(ID, Assessment, paper, semester, year, .keep_all = TRUE)

    mdebug(elapsed("Generating rtab",  ge_s), "get_etab", force = debug_equity)

    if (mf)
      etab <- etab |> filter(Gender %in% c('Male', 'Female'))

    #### Summary stats for this instance vs. others this year ####
    if (is.null(tp)) {
      s <- Sys.time()
      gtab <- etab    |> group_by(Gender)  |> sum_stats(pr) |> mutate(Papers = "All papers", .before = 1) |> rename(SubGroup = 2)
      mtab <- etab    |> group_by(Māori)   |> sum_stats(pr) |> mutate(Papers = "All papers", .before = 1) |> rename(SubGroup = 2)
      ptab <- etab    |> group_by(Pacific) |> sum_stats(pr) |> mutate(Papers = "All papers", .before = 1) |> rename(SubGroup = 2)
      itab <- NULL
      mdebug(elapsed("Generating summary stats grouped by Gender, Māori and Pacific", s), "get_etab", force = debug_equity)
    } else {
      s <- Sys.time()
      inst <- paste(tp, ty, ts)
      onam <- "All others"

      onam_ty <- sprintf("%s in %s", onam, ty)
      etab_ty <- etab |>
        filter(year == ty) |>
        mutate(Group = if_else(paper == tp & semester == ts, inst, onam_ty))

      gtab <- etab_ty |> group_by(Gender,  Group) |> tmod("Gender")
      mtab <- etab_ty |> group_by(Māori,   Group) |> tmod("Māori")
      ptab <- etab_ty |> group_by(Pacific, Group) |> tmod("Pacific")

      ##### Gaps for this instance ####
      y_n <- etab_ty |>
        distinct(ID, Assessment, paper, semester) |>
        nrow()

      i_n <- etab_ty |>
        filter(paper == tp, semester == ts) |>
        distinct(ID, Assessment, paper, year, semester) |>
        nrow() |>
        fmt_n()

      a_n <- etab_ty |>
        filter(paper != tp) |>
        distinct(ID, Assessment, paper, year, semester) |>
        nrow() |>
        fmt_n()

      t_n <- as.numeric(str_remove(a_n, ",")) + as.numeric(str_remove(i_n, ","))

      if (y_n - t_n)
        mdebug(sprintf("Totals don't match: itab ns sum to %s but ytab b is %s, i.e. they differ by %s",
                       fmt_n(y_n), fmt_n(t_n), fmt_n(y_n - t_n)), "get_etab", TRUE)

      g_mgap <- get_gap(gtab, "Male",    "Female",      Median, inst, onam_ty)
      m_mgap <- get_gap(mtab, "Māori",   "Non-Māori",   Median, inst, onam_ty)
      p_mgap <- get_gap(ptab, "Pacific", "Non-Pacific", Median, inst, onam_ty)
      g_pgap <- get_gap(gtab, "Male",    "Female",      Pass,   inst, onam_ty)
      m_pgap <- get_gap(mtab, "Māori",   "Non-Māori",   Pass,   inst, onam_ty)
      p_pgap <- get_gap(ptab, "Pacific", "Non-Pacific", Pass,   inst, onam_ty)
      med_gaps <- tribble(~Papers, ~`Gender (m)`,    ~`Māori (m)`,     ~`Pacific (m)`,
                          inst,     g_mgap$instance,  m_mgap$instance,  p_mgap$instance,
                          onam_ty,  g_mgap$all_other, m_mgap$all_other, p_mgap$all_other)

      pr_gaps  <- tribble(~Papers, ~`Gender (p)`,    ~`Māori (p)`,     ~`Pacific (p)`,
                          inst,     g_pgap$instance,  m_pgap$instance,  p_pgap$instance,
                          onam_ty,  g_pgap$all_other, m_pgap$all_other, p_pgap$all_other)

      gaps_ns  <- tribble(~Papers, ~`Gender (n)`,    ~`Māori (n)`,     ~`Pacific (n)`,
                          inst,     g_pgap$i_n,       m_pgap$i_n,       p_pgap$i_n,
                          onam_ty,  g_pgap$a_n,       m_pgap$a_n,       p_pgap$a_n)
      itab <- med_gaps |>
        left_join(pr_gaps, by = "Papers") |>
        left_join(gaps_ns, by = "Papers") |>
        select(Papers, starts_with("G"), starts_with("M"), starts_with("P")) |>
        mutate(n = c(i_n, a_n))
      mdebug(elapsed("Getting gaps with get_gap()", s), "get_etab", force = debug_equity)
    }

    stab <- gtab |> add_row(mtab) |> add_row(ptab)
    mdebug(elapsed("Getting to the start of analysing gaps over time (from the start of this function)",  ge_s), "get_etab", force = debug_equity)
    #### Add Gaps over time ####
    s <- Sys.time()
    gt_m <- etab |> gt_f(Gender,  Mark, inst)
    mt_m <- etab |> gt_f(Māori,   Mark, inst)
    pt_m <- etab |> gt_f(Pacific, Mark, inst)

    if (DEBUG) {
      a <- etab |> filter(Pacific == "Pacific",     year == 2024) |> pull(Mark) |> median(na.rm = TRUE)
      b <- etab |> filter(Pacific == "Non-Pacific", year == 2024) |> pull(Mark) |> median(na.rm = TRUE)
      mdebug(sprintf("Pacific 2024 median mark: %3.1f; Non-Pacific: %3.1f", a, b), "get_etabs", TRUE)
    }

    gt_p <- etab |> gt_f(Gender,  Pass, inst)
    mt_p <- etab |> gt_f(Māori,   Pass, inst)
    pt_p <- etab |> gt_f(Pacific, Pass, inst)

    ns <- etab |> group_by(year) |> distinct(ID, Assessment, paper, semester) |> summarise(n = n())

    ytab <-     gt_m |> select(year, `Gender (m)`  = Gap) |>
      left_join(gt_p |> select(year, `Gender (p)`  = Gap, `Gender (n)` = n), by = "year") |>
      left_join(mt_m |> select(year, `Māori (m)`   = Gap), by = "year") |>
      left_join(mt_p |> select(year, `Māori (p)`   = Gap, `Māori (n)` = n), by = "year") |>
      left_join(pt_m |> select(year, `Pacific (m)` = Gap), by = "year") |>
      left_join(pt_p |> select(year, `Pacific (p)` = Gap, `Pacific (n)` = n), by = "year") |>
      add_column(n = fmt_n(ns$n)) |>
      arrange(desc(year))
    mdebug(elapsed("Getting gaps with gt_f()", s), "get_etab", force = debug_equity)

    mdebug(elapsed("Generating equity data", ge_s), "get_etab", force = debug_equity)

    return(invisible(list(edat = etab, stab = stab, itab = itab, ytab = ytab)))
  }


  pg_tab <- tibble()

  set_user <- function(uname = NULL) {
    if (is.null(uname)) {
      user      <- get_user()
      modalDialog(p(sprintf("User: %s", user)), title = "Debugging")
    } else {
      fake_user    <- input$fake_user
      cf$real_user <- user
      cf$fake_user <- fake_user
    }
  }

  db_do_restore <- function(fn) {
    # https://mariadb.com/kb/en/restoring-data-from-dump-files/
    cmd <- "mariadb" ;
    args <- c("marketing", "-u marketing", "-prhcb2233")

    if (on_server())
      args <- c(args, "-h mariadb.marketing-ebreporter")

    exmpl <- "mariadb -u marketing -prhcb2233 < backups/EB_datadump_2024_11_11_15_02.sql"
    mdebug(sprintf("Value of debug_restore: '%s'", input$debug), "db_do_restore", TRUE)
    if (input$debug) {
      mdebug("Running DB command to debug restore process", "db_do_restore", TRUE)
      rv <- system2(cmd, args = c(args, "-e'show tables'"), stderr = TRUE, stdout = TRUE)
    } else {
      mdebug(sprintf("Running cmd: '%s'", paste(cmd, paste(args, collapse = " "), fn)),
             caller = "db_do_restore", force = TRUE)
      rv <- system2(cmd, args = args, stdin = fn,           stderr = TRUE, stdout = TRUE)
    }

    err <- grep("ERROR", rv, value = TRUE)
    if (length(err) > 0)
      mdebug(err, caller = "db_do_restore", force = TRUE)

    if (length(rv) > 0)
      mdebug(sprintf("Result is: '\n%s'", paste(rv, collapse = "\n")), caller = "db_do_restore", force = TRUE)
  }

  save_user_access <- function(i, instances) {
    if ( input$is_emergency )
      return(NULL)

    debug_permissions <- TRUE
    code <- instances$instance[i]
    v    <- input[[paste0("access_", code)]]
    p    <- str_sub(code, 1, 7)
    y    <- input[["access_year"]]
    s    <- input[["access_sem" ]]

    old  <- instances |>
      dplyr::filter(paper == p, year == y, semester == s) |>
      replace_na(list(Coordinator = "")) |>
      slice(1) |>
      pull(Coordinator)

    itab <- get_instances() |>
      mutate(Coordinator = ifelse(paper == p & year == y & semester == s,
                                  v, Coordinator))
    new <- v
    if (old != new) {
      mdebug(sprintf("Only %s can access %s (%s %s) -- was '%s'",
                     new, p, y, s, old), "save_user_access", force = debug_permissions)

      set_tab("instances", itab)
    }
  }

  get_year <- function() {
    year <- get_ss(input$year_to_show)
    if (valid_year(year))
      year
    else
      str_sub(Sys.Date(), 1, 4)
  }

  get_sem <- function() {
    if (as.integer(str_sub(Sys.Date(), 6, 7)) > 7)
      "S2"
    else
      "S1"
  }

  valid_paper <- function(x) {
    is_valid(x, 7)
  }

  valid_sem <- function(x) {
    is_valid(x, 2)
  }

  valid_year <- function(x) {
    is_valid(get_ss(x), 4)
  }

  trace_dat <- FALSE
  if (DEBUG)
    trace_dat <- TRUE

  track_instance <- function(caller = 'caller unknown') {
    if(input$is_emergency)
      return(NULL)
    mdebug(sprintf("Getting instance: '%s' ('%s' '%s'); %d records",
                   rvs$instance$paper, rvs$instance$year, rvs$instance$semester,
                   nrow(rvs$instance$results)),
           caller)
  }

  data_path <- "Data/"

  get_title <- function(s) {
    paper    <- str_match(s, "^([A-Z]{4}[0-9]{3})" )[,2]
    semester <- str_match(s,  "\\(([SF][12SY])," )[,2]
    year     <- str_match(s,  "\\, ([0-9]{4})" )[,2]
    list(paper = paper, semester = semester, year = year)
  }

  set_title <- function(paper, year, semester, title = "") {
    s <- sprintf("%s %s (%s, %s)", paper, title, semester, year)
    mdebug(sprintf("(set_title): setting title: %s", s), "set_title", DEBUG)
    cf <- get_config()
    s_span <- ""
    if ( cf$fake_user != "none" & cf$fake_user != "" ) {
      users    <- get_users()
      user     <- users |> dplyr::filter(username == cf$fake_user)
      mdebug(sprintf("Impersonating %s", username), "set_title", force = TRUE)
      s_span   <- span(
        HTML(
          sprintf("%s — viewing as %s %s (%s)",
                  s,  user$user_f, user$user_s, cf$fake_user)),
        style = "color:red")
    }

    list(text = s, UI = s_span)
  }

  get_scaled <- function(dd, rf = 0) {
    if ( input$is_emergency )
      return(NULL)

    observe(input$scale_what)
    mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
           "get_scaled", force = debug_flicker | debug_scaling | DEBUG)

    if (length(input$scale_what) == 0)
      return(dd$results$Total)

    en              <- exam_name(dd)
    exam_weight     <- dd$exam_weight
    internal_weight <- 1 - exam_weight

    if (dd$exam_weight > 0 ) {
      exam <- dd$results |> pull(en)
    } else {
      exam <- rep(0, nrow(dd$results))
    }

    internal        <- dd$results |> pull(Internal)

    if (input$scale_what == "Nothing") {
      mdebug(sprintf("Not scaling anything -- '%s' ('%s' '%s')",
                     dd$paper, dd$year, dd$semester),
             "get_scaled", force = debug_flicker | debug_scaling | DEBUG)
      dd$results <- dd$results |> mutate(Scaled = Total)
    }

    if (input$scale_what == "Total") {
      mdebug(sprintf("Scaling total -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "get_scaled", force = (debug_flicker | debug_scaling | DEBUG))
      dd$results <- dd$results |> mutate(
        Scaled = ifelse(Total >= input$scale_thr,
                        mround(Total * input$scale_mul + input$scale_add),
                        Total))
    }

    if (input$scale_what == "Exam") {
      mdebug(sprintf("Scaling exam -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "get_scaled", force = debug_flicker | debug_scaling | DEBUG)
      dd$results <- dd$results |> mutate(
        Scaled = ifelse(exam >= input$scale_thr,
                        round(internal * internal_weight) + round(exam * exam_weight) * input$scale_mul + input$scale_add,
                        round(internal * internal_weight) + round(exam * exam_weight)
                        )
        )
    }

    if (input$scale_what == "Internal") {
      mdebug(sprintf("Scaling Internal -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "get_scaled", force = debug_flicker | debug_scaling | DEBUG)
      dd$results <- dd$results |> mutate(
        Scaled = ifelse(internal >= input$scale_thr,
                        round(internal * internal_weight * input$scale_mul + input$scale_add + exam * exam_weight, rf),
                        round(internal * internal_weight +                                     exam * exam_weight)), rf)
    }

    dd$results$Scaled <- round(dd$results$Scaled)

    mt <- mean(dd$results$Total)
    ms <- mean(dd$results$Scaled)
    if (TRUE) {
      mdebug(sprintf("Mean total : % 3.2f; Mean scaled: % 3.2f, Diff: %3.2f, Equal = %s",
                      mt, ms, ms - mt, mt == ms), "get_scaled", debug_flicker | debug_scaling | DEBUG)
      mdebug(sprintf("Exam weight: % 3.2f; What: %s, Threshold: %d, Add: %3.2f. Mul: %3.2f",
                      exam_weight,
                      input$scale_what, input$scale_thr,
                      input$scale_add, input$scale_mul), "get_scaled", debug_flicker | debug_scaling | DEBUG)
    }

    dd$results |> pull(Scaled)
  }

  #### Data setup ####

  paper_changed <- function(){
    req(input$paper_to_show, input$year_to_show, input$semester_to_show)

    # The very first time this function is called, the instance object is null
    p_i <- get_ss(rvs$instance$paper);
    y_i <- get_ss(rvs$instance$year);
    s_i <- get_ss(rvs$instance$semester)

    p_s <- get_ss(input$paper_to_show)
    y_s <- get_ss(input$year_to_show)
    s_s <- get_ss(input$semester_to_show)

    return(!(p_i == p_s & y_i == y_s & s_i == s_s))
  }

  #### !!! dat() !!! ####
  dat <- reactive({
    if (input$is_emergency)
      return(NULL)

    ### Attempt to load paper. On startup, the input controls are empty, and the
    ### instance object that holds the currently loaded paper is null.
    load_start <- Sys.time()
    s <- Sys.time()

    req(input$paper_to_show)
    req(input$year_to_show)
    req(input$semester_to_show)

    mdebug(sprintf("Firing -- input controls specify: '%s' ('%s' '%s')",
                   input$paper_to_show, input$year_to_show, input$semester_to_show),
           "dat", force = debug_flicker)

    # First: attempt to load data from specification from input controls
    paper     <- get_ss(input$paper_to_show)
    year      <- get_ss(input$year_to_show)
    semester  <- get_ss(input$semester_to_show)

    # The very first time this function is called, the instances object is null
    p_i       <- get_ss(rvs$instance$paper);
    y_i       <- get_ss(rvs$instance$year);
    s_i       <- get_ss(rvs$instance$semester)

    # So get default instance specification stored in database
    cf        <- get_config();
    p_d       <- get_ss(cf$default_paper);
    y_d       <- get_ss(get_year()); # Gets current year
    s_d       <- get_ss(cf$default_semester)

    mdebug(sprintf("Selections are now: '%s' ('%s' '%s')",  paper, year, semester),
           "dat", force = debug_load | debug_flicker)

    if (paper_changed()) {
      mdebug(sprintf("START: Instance does NOT match input selection: Selected = ('%s', '%s', '%s') != Instance = ('%s', '%s', '%s')",
                     paper, year, semester, p_i, y_i, s_i),
             "dat", force = debug_load | debug_flicker)
    } else {
      mdebug(sprintf("Data for: '%s' ('%s' '%s') already loaded", rvs$instance$paper, rvs$instance$year, rvs$instance$semester),
             "dat", force = debug_load | debug_flicker)
      return(rvs$instance)
    }

    # If input controls empty, try instance object (last loaded paper data)
    if (!valid_paper(paper))
      paper <- p_i

    if (!valid_paper(paper))
      paper <- p_d

    if (!valid_paper(paper))
      mdebug(sprintf("Instance selection failed! Value of paper is '%s'", get_ss(paper)), "dat", force = TRUE)

    if (!valid_year(year))
      year  <- y_i

    if (!valid_year(year))
      year  <- y_d

    if (!valid_year(year))
      year  <-  get_year()

    if (valid_year(year))
      mdebug(sprintf("Instance selection failed! Value of year is '%s'", get_ss(year)), "dat")

    if (!valid_sem(semester))
      semester <- s_i

    if (!valid_sem(semester))
      semester <- s_d

    if (!valid_sem(semester))
      semester <- "S1"

    # If getting values from instance unavailable get them from defaults.
    if (valid_paper(paper) & valid_year(year) & valid_sem(semester)) {
        # Defaults may not be latest available. How to handle this?
      # The request is SYNTATICALLY valid, but does the data exist?
      this_paper <- paper; this_year <- year; this_semester <- semester
      data_available <- get_results() |>
        filter(paper == this_paper, year == this_year, semester == this_semester) |>
        nrow() > 0

      if (data_available) {
        mdebug(sprintf("Data requested is available: '%s' ('%s', '%s')",
                       paper, year, semester), "dat", force = debug_load)
      } else {
        mdebug(sprintf("Data requested NOT available: '%s' ('%s', '%s'), so returning NULL",
                       paper, year, semester), "dat", force = debug_load)
        return(NULL)
      }
    } else {
      mdebug("This should never happen! Paper cannot be selected from input controls, instance object, or defaults",
             "dat", force = debug_load)
    }

    isolate({
      rvs$instance$paper    <- paper
      rvs$instance$year     <- year
      rvs$instance$semester <- semester
    })

    isolate(updateSelectInput(session, inputId = "paper_to_show",    selected = paper))
    isolate(updateSelectInput(session, inputId = "year_to_show",     selected = year))
    isolate(updateSelectInput(session, inputId = "semester_to_show", selected = semester))

    mdebug(sprintf("After updating, values of the selectInputs are: '%s' ('%s', '%s')",
                   get_ss(input$paper_to_show), get_ss(input$year_to_show), get_ss(input$semester_to_show)),
           "dat", force = debug_load)

    mdebug(sprintf("Requesting data: '%s' ('%s' '%s')", paper, year, semester),
           "dat", force = debug_load | debug_flicker)

    dd    <- load_data(paper, year, semester)
    #message("Assessments (dat)")
    #print(dd$assessments)
    paper <- dd$paper; year <- dd$year; semester <- dd$semester

    mdebug(sprintf("Data returned from load_data() for paper: '%s' ('%s' '%s') has %d rows",
                   get_ss(dd$paper), get_ss(dd$year), get_ss(dd$semester), get_n(nrow(dd$results))),
           "dat", force = debug_load | debug_flicker)

    # load_data will return the most recent instance if the requested instance
    # is not found. If this happens, the input controls need to be updated.
    # However, this does not happen until all dirty states have been resolved,
    # which is too late for the code below. Hence, we need a way to differentiate
    # cases where the request cannot be resolved because the data is unavailable.
    # as opposed to simply different to current.
    if (get_ss(dd$semester) != get_ss(input$semester_to_show)) {
      isolate(updateSelectInput(session, inputId = "semester_to_show", selected = semester))
      mdebug(sprintf("Returned data: '%s' ('%s' '%s') does not have the requested semester, so trying to update the input controls",
                     get_ss(dd$paper), get_ss(dd$year), get_ss(dd$semester)),
             "dat", force = debug_load)
    }
    if (get_ss(dd$year) != get_ss(input$year_to_show)) {
      isolate(updateSelectInput(session, inputId = "year_to_show",     selected = year))
      mdebug(sprintf("Returned data: '%s' ('%s' '%s') does not have the requested year, so trying to update the input controls",
                     get_ss(dd$paper), get_ss(dd$year), get_ss(dd$semester)),
             "dat", force = debug_load)
    }

    mr <- make_results(dd) # Spreads long format assessments to wide and calculates totals
    df <- mr$results
    exam_weight <- mr$exam_dat$exam_weight

    if (any(mr$assessments$Title %in% result_names)) {
      result_name_a <- mr$assessments$Title[which(mr$assessments$Title %in% result_names)]
      assessments_r <- unique(mr$results$Assessment)
      result_name_r <- assessments_r[assessments_r %in% result_names]
      if (sort(result_name_a)[1] == sort(result_name_r)[1]) {
        result_name <- result_name_a
      } else {
        result_name <- result_name_r
      }

      mdebug(sprintf("%s has overall result data: '%s'",
                     mr$paper, plist(result_name)), "dat", debug_load |TRUE)

      # FIXME: how to check that it's all internally assessed?
      # No (?) 400 level papers have exams, but also MART333 has no exam. But
      # obviously need a better way to deduce from data. Or provide an GUI
      # option on a per-paper basis?
      df <- df |> select(-all_of(result_name))
      int_only <- ifelse(all(df$Internal == df$Total), TRUE, FALSE)
    }

    if (mr$exam_dat$has_exam) {  #has_exam(dd)
      mdebug(sprintf("==> dat(): %s has an exam",  paper), "dat", DEBUG)
    } else {
      mdebug(sprintf("==> dat(): %s (%s %s) has no exam!", paper, year, semester), "dat", DEBUG | TRUE)
      df$Internal <- df$Total
    }

    dd$exam_weight <- exam_weight
    obj <- list(results      = df,
                results_tidy = dd$results,
                assessments  = dd$assessments,
                exam_weight  = dd$exam_weight,
                report       = dd$report,
                year         = dd$year,
                paper        = dd$paper,
                semester     = dd$semester)

    obj$results <-  obj$results |> mutate(Scaled = get_scaled(obj))  # was

    mdebug(sprintf("Getting instance (before update): '%s' ('%s', '%s'); %4.0d student records",
                   get_ss(rvs$instance$paper), get_ss(rvs$instance$year), get_ss(rvs$instance$semester),
                   get_n(nrow(rvs$instance$results))),
           "dat", force = debug_load)

    isolate({ # Tried <- and isolate; no effect.
      rvs$instance <<- obj
    })

    mdebug(sprintf("Getting instance (after update):  '%s' ('%s', '%s'); %4.0d student records",
                   get_ss(rvs$instance$paper), get_ss(rvs$instance$year), get_ss(rvs$instance$semester),
                   get_n(nrow(rvs$instance$results))),
           "dat", force = debug_load)

    if ( !paper_changed() ) {
      mdebug(sprintf("Instance DOES match input selection: Selected = %s %s %s == Instance = %s %s %s",
                   input$paper_to_show, input$year_to_show, input$semester_to_show,
                   rvs$instance$paper,  rvs$instance$year, rvs$instance$semester),
             "dat", force = debug_load)
    } else {
      mdebug(sprintf("Instance does NOT match input selection: Selected = %s %s %s == Instance = %s %s %s",
                     input$paper_to_show, input$year_to_show, input$semester_to_show,
                     rvs$instance$paper,  rvs$instance$year, rvs$instance$semester),
             "dat", force = debug_load)
    }
    if (debug_bz) {
      ft <- dd$results %>%
        dplyr::filter(Mark < 0, Assessment == "Overall paper result")

      mdebug(sprintf("Failed terms: %d", ft %>% nrow),"dat", force = debug_bz)
      mdebug("Here they are", "dat", force = debug_bz)
      print(ft)
      print(df)
    }

    e <- Sys.time()
    et <- format(e - s)
    mdebug(sprintf("Data for: '%s' ('%s' '%s') loaded (%d assessment result records in %s)",
                   obj$paper, obj$year, obj$semester, nrow(dd$results), et),
           "dat", force = debug_load)

    return(rvs$instance)
  })

  write_report <- function() {
    dd <- dat()

    update_report <- function(n, o, verbose = FALSE) {
      new <- get_ss(n)
      old <- get_ss(o)

      if ( length(new) > 0 ) {
        if (str_length(new) > 0) {
          if (!identical(old, new)) {
            if (verbose)
              message("Updating\n\t'", old, "'\nwith\n\t'", new, "'")
            return(new)
          }
        }
      }

      if (length(old) == 0)
        return("")
      else
        return(old)
    }

    if (length(get_ss(dd$paper)) != 0) {
      mdebug("(write_report): Updating report", force = F)
      mdebug(sprintf("(write_report): Coordinator from input control: '%s'",
                     get_ss(input$report_coordinator)), force = F)

      old_report <- get_report(dd$paper, dd$year, dd$semester)
      new_report <- tibble(
        paper             = dd$paper,
        year              = dd$year,
        semester          = dd$semester,
        title             = update_report(dd$title,                       old_report$title),
        scale_what        = update_report(input$scale_what,               old_report$scale_what),
        scale_add         = update_report(input$scale_add,                old_report$scale_add),
        scale_mul         = update_report(input$scale_mul,                old_report$scale_mul),
        scale_thr         = update_report(input$scale_thr,                old_report$scale_thr),
        coordinator       = update_report(input$report_coordinator,       old_report$coordinator),
        staff             = update_report(input$report_staff,             old_report$staff),
        group_work        = update_report(input$report_group_work,        old_report$group_work),
        exam_format       = update_report(input$report_exam_format,       old_report$exam_format),
        scaling           = update_report(input$report_scaling,           old_report$scaling),
        misconduct        = update_report(input$report_misconduct,        old_report$misconduct),
        feedback_channels = update_report(input$report_feedback_channels, old_report$feedback_channels),
        feedback_issues   = update_report(input$report_feedback_issues,   old_report$feedback_issues),
        feedback_actions  = update_report(input$report_feedback_actions,  old_report$feedback_actions),
        report            = update_report(input$report_report,            old_report$report),
        changes           = update_report(input$report_changes,           old_report$changes))

      new_report$year <- as.character(new_report$year )  # Stored as integer in database
#      print(new_report)

      # Now update report table
      db <- get_con()
      qry <- glue::glue_sql( "DELETE FROM reports WHERE paper = {dd$paper} AND year = {dd$year} AND semester = {dd$semester}", .con = db)
      rows_affected <- dbExecute(db, qry)
      dbDisconnect(db)
      set_tab("reports", new_report, overwrite = FALSE)

      if (FALSE) {
        mdebug(sprintf("(write_report): Deleted %d rows from existing report; added %d rows from new report",
                       rows_affected, nrow(new_report)),
               force = TRUE)
        mdebug(sprintf("(write_report): Coordinator from new_report: '%s'", new_report %>% pull(coordinator) ),
               force = TRUE)
        mdebug(sprintf("(write_report): Reading back data for '%s' ('%s', '%s')",
                       dd$paper, dd$year, dd$semester),
               force = TRUE)
        rt <- get_report(dd$paper, dd$year, dd$semester)
        mdebug(sprintf("(write_report): Coordinator from database: '%s'", rt %>% pull("coordinator")),
               force = TRUE)
      }
    } else {
      mdebug("===> (WARNING) Attempt to update report with bad data!", force = TRUE)
    }
  }

  onStop(function()
  {
    message("Session stopping at ", Sys.time())
  })


  #### System configuration functions ####
  # End dead code

  #### Data processing functions ####
  update_data <- function(new_dat, do_report = FALSE) {
    # This function is only called once, and is a function only for programmer
    # sanity, i.e. it could be inline code, in a huge function. However, import_data
    # could be called in the situation where existing data (from the CSV file)
    # needs to be replaced. This should NOT over-write any existing report data,
    # i.e. only the assessments and results tables need to be over-written if they
    # exist.
    debug_import <- input$debug_import

    if (is.null(new_dat$titles))
      new_dat$titles <- ""

    papers     <- str_replace_all(new_dat$papers,    "([A-Z]{4}[0-9]{3})", "'\\1'") |> plist()
    years      <- str_replace_all(new_dat$years,     "([0-9]{4})",         "'\\1'") |> plist()
    semesters  <- str_replace_all(new_dat$semesters, "([N|S]+[1-2]{1}|FY|SS)",  "'\\1'")  |> unique() |> plist()
    titles     <- new_dat$titles

    message("Adding cases to results table")
    print(new_dat)
    qry <- sprintf("SELECT * FROM results
                             WHERE paper    IN (%s)
                              AND  year     IN (%s)
                              AND  semester IN (%s)",
                   papers, years, semesters)

    message("Running query")
    print(qry)
    old_dat    <- get_qry(qry)

    new_res    <- new_dat$results
    new_ass    <- new_dat$assessments
    new_tot    <- new_dat$totals |> mutate(year = as.integer(as.numeric(year)),
                                           Mark = as.integer(mround(Mark)))
    new_ins    <- new_dat$results |>
      distinct(paper, year, semester) |>
      mk_instance() |>
      mutate(title = titles, Coordinator = '')

    new_res    <- new_dat$results
    new_ass    <- new_dat$assessments
    new_tot    <- new_dat$totals |> mutate(year = as.integer(as.numeric(year)),
                                           Mark = as.integer(mround(Mark)))
    #do_report <- FALSE;
    if (do_report)
      if ( str_length(input$report_staff) < 1) {
        new_rep <- tibble(
          paper             = new_dat$paper,
          year              = new_dat$year,
          semester          = new_dat$semester,
          title             = new_dat$title,
          coordinator       = "",
          staff             = "",
          group_work        = "",
          exam_format       = "",
          scaling           = "",
          misconduct        = "",
          feedback_channels = "",
          feedback_issues   = "",
          feedback_actions  = "",
          report            = "",
          changes           = ""
        )
      } else {
        new_rep <- tibble(
          paper             = dat$paper,
          year              = dat$year,
          semester          = dat$semester,
          title             = dat$title,
          coordinator       = input$report_coordinator,
          staff             = input$report_staff,
          group_work        = input$report_group_work,
          exam_format       = input$report_exam_format,
          scaling           = input$report_scaling,
          misconduct        = input$report_misconduct,
          feedback_channels = input$report_feedback_channels,
          feedback_issues   = input$report_feedback_issues,
          feedback_actions  = input$report_feedback_actions,
          report            = input$report_report,
          changes           = input$report_changes)
      }

    # Delete existing rows for this <paper, year, semester>
    tables <- c("results", "instances", "totals", "assessments")
    if (do_report)
      tables <- c(tables, "reports")

    # Delete existing data for each instance
    mdebug(sprintf("Deleting data in tables: '%s'", plist(tables)), "update_data", debug_import)
    db <- get_con()
    if (nrow(old_dat)) {
      for (i in 1:length(tables) ) {
        qry <- sprintf("DELETE FROM %s WHERE paper IN %s AND year IN %s AND semester IN %s",
               tables[i],
               enlist(unique(old_dat$paper)),
               enlist(unique(old_dat$year)),
               enlist(unique(old_dat$semester)))

        mdebug(sprintf("Executing SQL qry:\n%s", qry), "update_data", force = debug_import)
        rv <- dbExecute(db, qry)
        mdebug(sprintf("Return value indicates that %s records were deleted from %s", fmt_n(rv), tables[i]),
               "update_data", force = debug_flicker | debug_import | TRUE)
      }
    }
    dbDisconnect(db)

    # Insert the new rows
    mdebug("About to write to the database", "update_data", debug_import)
    set_tab("results",     new_res |> distinct(ID, paper, year, semester, Assessment, .keep_all = TRUE), overwrite = FALSE, debug_db = debug_import)
    set_tab("instances",   new_ins |> distinct(    paper, year, semester,             .keep_all = TRUE), overwrite = FALSE, debug_db = debug_import)
    set_tab("totals",      new_tot |> distinct(ID, paper, year, semester,             .keep_all = TRUE), overwrite = FALSE, debug_db = debug_import)
    set_tab("assessments", new_ass |> distinct(    paper, year, semester, Title,      .keep_all = TRUE), overwrite = FALSE, debug_db = debug_import)

    if (do_report)
      set_tab("reports",   new_rep, overwrite = FALSE)

    mdebug("Written to database", "update_data", debug_import|DEBUG)
  }

  import_data <- function(fn = NULL, update_db = TRUE, filename = NULL,
                          debug_import = FALSE, offline = FALSE, ftype = NULL) {
    #offline <- TRUE; fn <- NULL; update_db <- TRUE
    if (offline) {
      if ( length(grep("app", getwd())) == 0 )
        setwd("app")
      source("database_functions.R");
      source("utility_functions.R");
      rv        <- simulate_UI()
      input     <- rv$input
      do_report <- FALSE
      fn        <- NULL;
      update_db <- NULL;
      fn        <- NULL

      if (is.null(fn)) {
        fn  <- "~/Volumes/NVM/OneDrive/Service/School/TALC/Divisional Dashboard/Data/Students/CARPT002-EnrolmentandCourseApprovalStatusreport(byDivision).xlsx"; filename <- fn
        fn  <- "~/Dropbox (Maestral)/Attachments/BSNS113 S2 2024 results 20241115.xlsx"; filename <- fn
      }
      if (is.null(update_db)) {
        update_db    <- TRUE
        debug_import <- TRUE
      }
    } else {
      debug_import <- input$debug_import
    }

    if (input$dry_run | offline) {
      update_db    <- FALSE
      debug_import <- TRUE
      mdebug("Debugging import", "import_data", TRUE)
    }

    #print(fn)
    #print(filename)
    if (length(filename) > 1) {
      ftype <- "BB"
      mdebug(sprintf("Starting import of %s (source: %s)", fn[1], filename[1]),
             "import_data", force = debug_import|TRUE)
    } else {
      mdebug(sprintf("Starting import of %s (source: %s)", fn, filename),
             "import_data", force = debug_import|TRUE)
    }

    if (file.exists(fn) & is.null(ftype) ) {
      ftype <- sniff_contents(fn[1], filename) # , offline = TRUE
      mdebug(sprintf("File type is '%s'", ftype), "import_data", TRUE)
    } else {
      if (!file.exists(fn) ) {
        message(sprintf("File  '%s' does not exist!", fn))
        return(NULL)
      }
    }

    if (ftype %in% c("GC_csv", "GC_xlsx")) {
      if (ftype == "GC_csv") {
        nd <- read_GC_csv(fn, filename)
      } else {
        nd <- read_GC_xlsx(fn, filename)
      }

      mdebug(sprintf("Adding data for %s (%s %s)", nd$paper, nd$year, nd$semester),
             "import_data", TRUE)
      process_GC(nd)
      return(NULL)
    }

    message(sprintf("Importing data of type '%s'", ftype))
    if (ftype == "ENR") {
      message("===> Importing engagement data")
      return(read_enr(fn))
    }

    if (ftype == "BB") {
      fns <- fn
      for (fn in fns) {
        if (str_detect(fn, "csv")) {
          return(read_Echo(fn))
        } else {
          return(read_BB(fn))
        }
      }
    }


    if (ftype == "SD") {
      new_dat <- refresh_students(fn)
      return(NULL)
    }

    if (ftype == "CSV") {
      mdebug("Reading Template format", "import_data", debug_import)
      rd <- read_csv(fn, col_names = c("ID", "Name", "N1", "N2", "Mark", "Grade"))
    }

    if (ftype == "R1") {
      mdebug("Processing Results 1 data", "import_data", debug_import)
      new_dat <- read_R1(fn)
      assessments <- tibble(Title    = assessments,
                            Weight   = weights * 100,
                            paper    = papers,
                            year     = years,
                            semester = semesters)

      paper_code <- str_remove(papers, " ")
      paper_title <- ""
    }

    if (ftype == "Tp")
      rv <- import_Tp()

    if (ftype == "is_BO")
      results <- read_BO(fn)

    if (ftype == "SD") {
      mdebug(sprintf("Finished import of '%s' rows of Student Cohort data ",
                     fmt_n(nrow(new_dat))), "import_data", debug_import)
    } else {
      if (ftype == "BO")
        mdebug(sprintf("Finished import of '%s' ('%s' '%s'), constructing return object",
                       plist(papers), plist(years), plist(unique(semesters))),
               "import_data", debug_import)
    }

    if (update_db && !ftype %in% c("SD", "GC", "ENR")) {
      refresh_students(fn)
      #update_data(new_dat)
    }

    if (!ftype == "SD") {
      s_n <- new_dat$results |> distinct(ID) |> nrow()
      imp_msg <- sprintf("Finished importing '%s' '%s' '%s' (%s assessment results for %s students)",
                         plist(unique(new_dat$paper)),
                         plist(unique(new_dat$year)),
                         plist(unique(new_dat$semester)),
                         fmt_n(nrow(new_dat$results)),
                         fmt_n(s_n))
      if (!offline)
        shiny::showNotification(imp_msg, duration = 60)

      mdebug(imp_msg, "import_data", debug_import)

      return(invisible(new_dat))
    } else {
      return(NULL)
    }
  }

  grade_plot <- function(d, title, subtitle = "", more = NULL) {
    if (input$is_emergency)
      return(NULL)

    debug_plot <- FALSE

    subtitle <- str_replace(subtitle, "WARNING",
                            '<span style="color:red; font-weight:bold">WARNING</span>')
    subtitle <- str_replace(subtitle, "because","<br/>because")

    mdebug(sprintf("Firing: title, names and class(es) of d: '%s', '%s', '%s'",
                   title,
                   paste(names(d), collapse = ","),
                   paste(class(d), collapse = ", ")),
           "grade_plot", debug_plot)

    if (!nrow(d) ) {
      mdebug("No data supplied to grade_plot", "grade_plot", TRUE)
      print(nrow(d))
      print(d)
      return(NULL)
    } else {
      if (!is.null(more) & FALSE) {
        d <- d |> mutate(Total = more |> pull(1))
        d <- d |> pivot_longer(names_to  = "Scaling",
                               values_to = "Mark",
                               cols      = c("Scaled", "Total"))
      } else {
        names(d)[1] <- "Mark"
      }

      d <- d |> get_grades(Mark)

      if (!input$plot_negs) {
        mdebug("Filtering grades below zero", "grade_plot", debug_load)
        d <- d |> dplyr::filter(complete.cases(Mark), Mark >= 0)
      } else {
        mdebug("NOT filtering grades below zero", "grade_plot", debug_load|debug_plot)
       if (debug_load|debug_plot)
	       print(d |> arrange(desc(Mark)))
      }

      mdebug(sprintf("Lowest grades: %s",
               paste(as.character(d |>
                                    arrange(Mark) |>
                                    pull(Mark) |>
                                    head(20)), collapse = ", ")),
               "gradePlot", DEBUG)

      tab <- d |>
        group_by(G_cat) |>
        summarise( n = n()) |>
        mutate(`%` = round(100 * n / sum(n) )) |>
        select(Grade = G_cat, n, `%`) |>
        arrange(desc(Grade))

      tab <- tab |>
        add_row(Grade = 'Total', n = sum(tab$n)) |>
        mutate(`%` = as.character(`%`))

      tab$`%`[is.na(tab$`%`)] <- ''

      if (nrow(tab) > 0) {
        p2 <- tableGrob(tab,
                        rows  = NULL,
                        theme = ttheme_default(base_size = 16,
                                               base_family = "arial"))
      } else {
        message("WTF?")
      }

      mdebug("state of d before plotting", "grade_plot") # , title == "Final exam"
      if (DEBUG )
        print(d)

      if (is.null(more)) {
        p1 <- d |> ggplot(aes(x = Grade ))
      } else {
        p1 <- d |> ggplot(aes(x = Grade, fill = Scaling ))
        p1 <- d |> ggplot(aes(x = Grade))
      }

      mark <- d |> pull(Mark)

      if (length(subtitle) == 0)
        subtitle <- " "

      subtitle <- sprintf("Median: %3.0f, Mean: %3.0f (n: %d) %s",
                          median(mark), mean(mark), length(mark), get_ss(subtitle))

      p1 <- p1 +
        geom_bar(aes(y = after_stat(count)/sum(after_stat(count))),
                 position = "dodge") +
        scale_y_continuous(labels = scales::percent)  +
        labs(title    = title,
             x        = NULL,
             y        = NULL,
             subtitle = subtitle) +
        theme_bw(base_size = 14) +
        theme(text = element_text(),
              plot.title.position = "plot",
              plot.subtitle = element_markdown())

      if (nrow(tab) > 0) {
        rv <- gridExtra::grid.arrange(p1, p2, ncol = 2, widths = c(3, 1))

        if (debug_plot) { #  | title == "Final exam"
          message("Returning plot object comprised of the following structure")
          print(str(rv))
        }

        return(rv)

      } else {
        message("WTF again?")
        return(NULL)
      }
    }
  }

  #### Output object functions ####
  output$data_names <- renderUI({
    df <- rvs$instance$results
    radioButtons(inputId = "sort_by", label = "Sort by:", choices = names(df))
  })

  update_dat <- function() {
    req(rvs$instance)

    paper    <- rvs$instance$paper
    year     <- rvs$instance$year
    semester <- rvs$instance$semester

    (!(paper == input$paper_to_show & year == input$year_to_show & semester == input$semester_to_show))
  }

  output$current_paper <- renderUI({
    req(input$paper_to_show, input$year_to_show, input$semester_to_show)
    mdebug(sprintf("Firing -- '%s' ('%s' '%s')",
                   input$paper_to_show, input$year_to_show, input$semester_to_show),
           "current_paper", force = debug_flicker | debug_load)
    dd <- dat()
    mdebug(sprintf("Instance: '%s' ('%s' '%s')",
                   dd$paper, dd$year, dd$semester), "current_paper", force = debug_load)
    title    <- ifelse(str_length(dd$title) == 0, "", dd$title)
    return(p())
  })

  output$assessment_table <- renderUI({
    rd <- rvs$instance
    al <- rd$assessments %>%
      select(Title, Weight)

    tagList(
      h4("Assessment"),
      renderTable(al, digits = 0)
    )
  })



  #### Write data button ####
  output$write_csv <- renderUI({
    dd <- dat()

    paper    <- input$paper_to_show
    year     <- input$year_to_show
    semester <- input$semester_to_show

    if ( length(paper > 0 ) &
         length(year) > 0 &
         length(semester) > 0 &
         length(grep("Scaled", names(dd$results) )) > 0)
      fn  <- sprintf("Exports/%s_%s_%s_final_results.csv",
                     paper, year, semester)

    if (fn != "") {
      dd$results$Total[dd$results$Total < 0] <- 0
      dat <- dd$results %>%
        select(ID, Name, Total, Scaled)
    }
  })

  #### Download button ####
  output$download_data <- downloadHandler(
    filename = function() {
      if (length(input$paper_to_show)    > 0 &
          length(input$year_to_show)     > 0 &
          length(input$semester_to_show) > 0)
        sprintf("%s_%s_%s_final_results.csv",
                input$paper_to_show, input$year_to_show, input$semester_to_show)
      else
        "temp.csv"
    },
    content = function(file) {
      write.csv(read.csv(sprintf("Exports/%s_%s_%s.csv",
                                 input$paper_to_show,
                                 input$year_to_show,
                                 input$semester_to_show)),
                file,
                row.names = FALSE)

    },
    contentType = "text/csv"
  )
  #### Overview tab: plots ####
  output$InternalPlot <- renderUI({
    dd     <- dat()
    if (!is.null(dd)) {
      mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "InternalPlot", force = debug_flicker)
      rv <- renderPlot(grade_plot(dd$results |> select(Internal), "Internal Assessment"))
      #print(str(rv))
      return(rv)
    }
  })

  output$TotalPlot <- renderUI({
    dd     <- dat()
    if (!is.null(dd)) {
      mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "TotalPlot", force = debug_flicker)
      rv <- renderPlot(grade_plot(dd$results |> select(Total), "Total"))
      #print(str(rv))
      return(rv)
    }
  })

  output$ExamPlot <- renderUI({
    dd <- dat()
    if ( !is.null(dd) ) {
      mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "ExamPlot", force = debug_flicker)
      exam_n <- exam_name(dd)

      if ( !is.null(dd) & !is.null(exam_n) ) {
        mdebug(sprintf("Plotting '%s'", exam_n), "ExamPlot")

        exam <- dd$results |>
          select(exam = all_of(exam_n)) # |>  dplyr::filter(complete.cases(exam))

        absent <- nrow( dd$results |> dplyr::filter(exam_n == -3))


        rv <- renderPlot(grade_plot(exam, "Final exam", sprintf(", Absent: %d", absent)))
        #print(str(rv))
        return(rv)
        if (is.null(rv)) {
          h4(sprintf("No column found with a name that's recognised as final exam data (%s)", paste(exam_names, collapse = ", ")))
        } else {
          if (FALSE) {
            mdebug(sprintf("Got plot data"), "ExamPlot")
            print(str(rv))
          }
        }
      } else {
        mdebug(sprintf("No exam!"), "ExamPlot", TRUE)
      }
    }
  })

  output$ScaledPlot <- renderUI({
    # FIXME: This was the cause of flickering, i.e. plots being called twice:
    # there was a call to get_scaling() here, but scaling had already been
    # applied when dat() ran.

    dd <- dat()
    if (!is.null(dd)) {
      mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
             "ScaledPlot", force = debug_flicker  | debug_scaling | DEBUG)


      dd$results$Scaled <- get_scaled(dd)

      renderPlot(grade_plot(dd$results |> select(Scaled),
                 "Total (Scaled)",
                 subtitle = ifelse(input$scale_what != 'Nothing',
                                   sprintf("thr: %3.2f, mult %3.2f; add %3.2f; apply to: %s",
                                           input$scale_thr,
                                           input$scale_mul,
                                           input$scale_add,
                                           input$scale_what),
                                   ""),
                 more = dd$results |> select(Total)))
    }
  })

  #### Overview tab: Dynamic UI elements (sidebar) ####

  set_kpi_only <- function() {
    for (tab in user_tabs) {
      hideTab(inputId = "UI", target = tab)
    }
  }

  ##### Choose paper, year, semester #####
  get_authorised_instances <- reactive({
    if (input$is_emergency)
      return(NULL)

    authorised_instances <- NULL
    mdebug(sprintf("Firing"), "get_authorised_instances", debug_load | DEBUG)

    user  <- get_user()
    perms <- get_permissions(user)
    dept  <- get_dept(user)

    fusr  <- get_ss(input$fake_user)
    if (str_length(fusr) > 0 & fusr != user & fusr != 'none') {
      mdebug(sprintf("Setting user to %s", fusr), "get_authorised_instances", force = debug_load | debug_permissions)
      user <- fusr
    }

    all_instances <- get_instances() |> arrange(desc(year))

    if (dept == "Division") {
      updateSelectInput(  inputId = "set_dept",       selected = "BCom Core")
      updateCheckboxInput(inputId = "kpis_inc_noncore" , value = FALSE)
    } else {
      updateSelectInput(  inputId = "set_dept",       selected = dept)
    }

    if ( "user" %in% perms ) {
      authorised_instances <- all_instances |> dplyr::filter(Coordinator == user)
      mdebug(sprintf("user '%s' (dept: '%s') has 'user' permissions and access to these instances: %s",
                     user, dept, plist(authorised_instances$instance)),
             "get_authorised_instances", force = debug_load | TRUE )
    }

    if ( "all" %in% perms | "admin" %in% perms) {
      authorised_instances <- all_instances
    }

    if ( "kpis" %in% perms) {
     return(user)
    }

    if (is_null(authorised_instances)) {
      mdebug(sprintf("%s is not authorised to view any instances, (permissions are '%s'); returning NULL",
                     user, plist(perms)),
                     "get_authorised_instances", TRUE)
      return(user)
    } else {
      authorised_instances |>
        mutate(S = str_extract(semester, "[0-9]")) |>
        arrange(desc(year), desc(S))
    }
  })

  output$choose_dept <- renderUI({
    observe(input$default_dept)
    cf <- get_config()
    dl <- get_dept_list()

    selectInput("set_dept",
                "Department/Programme",
                choices  = dl$choices,
                selected = input$default_dept)
  })

  output$choose_paper <- renderUI({
    req(input$set_dept)
    if (input$is_emergency)
      return(NULL)

    mdebug(sprintf("Firing"), "choose_paper")

    cf     <- get_config()
    user   <- get_user()
    dept   <- get_dept(user)
    perms  <- get_permissions(user)

    dept_chosen <- input$set_dept

    if (dept_chosen != dept) {
      updateSelectInput(session, "set_dept", selected = dept_chosen)
      dept <- dept_chosen
    }
    codes       <- get_paper_codes(dept)

    mdebug(sprintf("User '%s' has permissions '%s'", user, plist(perms)), "choose_paper", debug_permissions)

    # Only show the papers that the user is authorised to see
    all_papers <- get_authorised_instances()

    if (length(all_papers) == 1) {
      mdebug(sprintf("User '%s' has permissions '%s'", user, plist(perms)), "choose_paper",  debug_permissions)
      if ("kpis" %in% perms) {
        set_kpi_only()
        return(NULL)
      } else {
        return(h4(sprintf("You are not authorised to view any papers. Please contact John Williams if this is a mistake.")))
      }
    }

    papers <- all_papers |> pull(paper) |> unique()
    mdebug(sprintf("User '%s' (dept: '%s') has permissions '%s' and can access %d papers",
                   user, dept, plist(perms), length(papers)), "choose_paper",  debug_permissions)

    if ("kpis" %in% perms)
      return(
          h3("Paper selection", style = sec_style),
          selectInput(inputId  = "paper_to_show",
                      label    = "Paper",
                      selected = "none",
                      choices  = NULL)
      )

    if (length(papers)) {
      papers <- unique(papers)
      papers <- papers[papers %in% codes]
      mdebug(sprintf("Firing -- %d papers available for user %s (perms '%s'): %s",
                     length(papers), user, plist(perms), plist(papers)), "choose_paper",
             force = debug_flicker | debug_permissions )

      if (perms == 'admin' | perms == 'all') {
        optimal_paper <- cf$default_paper
      } else {
        optimal_paper <- papers[1]
      }

      return(
        tagList(
          h3("Paper selection", style = sec_style),
          selectInput(inputId  = "paper_to_show",
                      label    = "Paper",
                      selected = optimal_paper,
                      choices  = sort(papers)))
      )
    } else {
      hide_tabs(c(user_tabs[-1], admin_tabs, kpi_tabs))
      return(
          h4("You are not authorised to view any papers. Please contact John Williams if this is a mistake."))
    }
  })

  get_periods <- function(period, paper) {
    this_paper <- get_ss(paper)

    inst_tab <- get_instances() |>
      dplyr::filter(paper == this_paper) |>
      arrange(desc(year))

    if (nrow(inst_tab)) {
      mdebug(sprintf("Data found for %s: %s", this_paper, plist(inst_tab$instance)),
             "get_periods", debug_load | debug_flicker)
      inst_tab |> pull(period)
    } else {
      mdebug(sprintf("(get_periods) No data found for %s", paper),
             "get_periods", debug_load | debug_flicker | debug_db)
      if (period == "year")
        return(get_year())
      else
        return("S1")
    }
  }

  output$choose_year <- renderUI({
    req(input$paper_to_show)
    paper <- input$paper_to_show
    mdebug(sprintf("Firing -- '%s'", paper), "choose_year", debug_flicker)

    if (!valid_paper(paper))
      mdebug(sprintf("Setting default paper: %s", paper), "choose_year")

    years <- get_periods("year", paper) |> unique()

    auth_years <- get_authorised_instances()
    if (length(auth_years) == 1)
      return(selectInput(inputId  = "year_to_show",
                         label    = "Year",
                         choices  = ''))

    auth_years <- auth_years |>
      filter(paper == paper) |> arrange(desc(year)) |> pull(year) |> unique()

    years <- years[years %in% auth_years]

    mdebug(sprintf("Years available: '%s'", plist(years)), "choose_year", debug_flicker)
    selectInput(inputId  = "year_to_show",
                label    = "Year",
                choices  = years,
                selected = input$default_year)
  })

  output$choose_semester <- renderUI({
    req(input$paper_to_show, input$year_to_show)
    paper <- input$paper_to_show
    year  <- input$year_to_show
    mdebug(sprintf("Firing -- '%s' ('%s')", paper, year), "choose_semester", debug_flicker)

    sems <- get_periods("semester", paper) |> unique()
    mdebug(sprintf("Semesters available: '%s'", plist(sems)), "choose_semester", debug_flicker)
    mdebug(sprintf("Current paper is '%s' ('%s', '%s')",
                   input$paper_to_show, input$year_to_show, sems[1]),
           "choose_semester", FALSE)
    selectInput(inputId  = "semester_to_show",
                      label    = "Semester",
                      #selected = sems[1],
                      choices  = sems)
  })

  get_instance <- function() {
    req(input$paper_to_show, input$year_to_show, input$semester_to_show)
    if (is.null(rvs$instance))
      dat()
    rvs$instance
  }

  ##### Scaling functions #####
  get_scaling <- function(field = NULL) {
    dd <- dat()
    mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester), "get_scaling", force = debug_flicker)

    tibble(scale_what = "Nothing",
                 scale_add    =  "0",
                 scale_mul    =  "1",
                 scale_thr    = "50",
                 change_to_50 = "49")
  }

  # FIXME: this function is probably the source GUI flickering due to loading
  # the same instance twice. When UI elements get their value from input fields
  # in other tabs, this causes problems. So use database or file IO for
  # inter-tab communication? (Or global variables?)


  output$scaling_controls <- renderUI({
    #observe(input$paper_to_show)
    dd <- dat()
    if (is.null(dd))
      return(NULL)

    mdebug(sprintf("Firing -- '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
           "scaling_controls", force = debug_flicker)

    rpt <- dd$report
    cfg <- get_config()

    scl_w <- get_ss(rpt$scale_what)
    scl_w <- ifelse(scl_w %in% c('Nothing', 'Total', 'Exam')  , scl_w, 'Nothing')

    load_end <- Sys.time()
    mdebug(sprintf("Total time to display UI: %s (%s, %s)" ,
                   format(load_end - load_start, digits = 2),
                   load_start, load_end),
           "scaling_controls", force = track_load_time | debug_flicker)

    mn <- "mark" # FIXME: What is the purpose of paramaterising this?
    tagList(
      h3("Scaling", style = sec_style),
      radioButtons(
        inline   = TRUE,
        inputId  = "scale_what",
        label    = "Apply scaling to:",
        choices  = c("Nothing", "Total", "Exam"),
        selected = scl_w
      ),
      sliderInput(inputId = "scale_add",
                  label   = sprintf("Add to %s:", mn),
                  min     = 0,
                  step    = 1,
                  max     = cfg$max_add,
                  value   = get_n(rpt$scale_add)
      ),
      sliderInput(inputId = "scale_mul",
                  label   = sprintf("Multiply %s by:", mn),
                  min     = 1,
                  step    = 0.01,
                  max     = cfg$max_mul,
                  value   = get_n(rpt$scale_mul, 1)
      ),
      sliderInput(inputId = "scale_thr",
                  label   = sprintf("Scale if %s is at least:", mn),
                  min     = 0,
                  step    = 1,
                  max     = cfg$max_thr,
                  value   = get_n(rpt$scale_thr, 50)
      ),
      sliderInput(inputId = "scale_fails",
                  label   = "Change marks to 50:",
                  min     = 45,
                  step    = 1,
                  max     = 50,
                  value   = 49),
      actionButton("scale_save", "Save scaling values", class = "btn")
    )
  })

  observeEvent(input$scale_save, {
    dd <- dat()
    mdebug(sprintf("Updating results table for '%s' ('%s' '%s')", dd$paper, dd$year, dd$semester),
           "scale_save", TRUE)
    #update_results(dd)
    showNotification("Data saved")
  })

  #### Internal Assessment tab ####
  output$populate_graphs <- renderUI({
    dd          <- get_instance()
    df          <- dd$results
    assessments <- dd$assessments
    en          <- exam_name(dd)

    if (is.null(en)) {
      assessment_names <- assessments$Title
    } else {
      assessment_names <- assessments$Title[assessments$Title != en ]
    }

    assessment_names <- assessments$Title[!assessments$Title %in% c(result_names, exam_names, internal_names)]

    if (length(assessment_names) > 1 ) {
      #message("Assessment names (populate_graphs)")
      #print(assessment_names)
      for (i in 1:length(assessment_names) ) {
        if (assessment_names[i] %in% names(df))
          mdebug(sprintf("Plotting '%s'", assessment_names[i]),
                 "populate_graphs")
          local({
            my_i <- i
            plotname <- paste0("plot_", my_i)
            output[[plotname]] <- renderPlot({
              item <- df |>
                select(assessment_names[my_i])
              item <- item |>
                dplyr::filter(complete.cases(item[, 1]))
              grade_plot(item, assessment_names[my_i])
            })
          })
      }
    }
  })

  output$InternalPlots <- renderUI({
    if(input$is_emergency)
      return(NULL)

    dd          <- dat()
    assessments <- dd$assessments
    df          <- dd$results
    debug_IP    <- FALSE

    mdebug(sprintf("Assessments for %s %s %s (InternalPlots)",
                    dd$paper, dd$semester, dd$year), "InternalPlots", debug_IP)
    #print(dd$assessments)
    assessment_names <- assessments |>
      dplyr::filter(!Title %in% c(exam_name(dd), result_names, internal_names)) |>
      pull(Title)

    assessment_names <- assessment_names[assessment_names %in% names(dd$results)]
    mdebug("Firing", "InternalPlots", debug_flicker | debug_IP)
    mdebug(sprintf("Assessments: %s", paste(assessment_names, collapse = ", ")),
	   "InternalPlots", force = debug_flicker | debug_IP)

    if (length(assessment_names) > 1 ) {
      plot_list <- lapply(1:length(assessment_names), function(i) {
        plotname <- paste0("plot_", i)
        column(4,
               shinycssloaders::withSpinner(plotOutput(plotname)),
        )
      })

      fluidPage(do.call(tagList, plot_list))
    }
  })

  #### Engagement tab ####
  filter_eng <- function(df) {
    if (is.null(df)) {
      return(NULL)
    } else {
      if (input$eng_include == "Māori")
        df <- df |> filter(Māori)
      if (input$eng_include == "Pacific")
        df <- df |> filter(Pacific)
      if (input$eng_include == "Māori or Pacific")
        df <- df |> filter(MP)

      if (input$eng_nz)
        df <- df |> filter(content > 0)

      if (input$eng_z)
        df <- df |> filter(content == 0)

      df
    }
  }

  output$eng_data_range <- renderUI({
    if(input$is_emergency)
      return(NULL)

    dates <- rvs$eng_dates
    psy   <- rvs$eng_psy
    emet <- get_tab("engagement_meta") |>
      filter(
        paper    == rvs$instance$paper,
        semester == rvs$instance$semester,
      )

    if (nrow(emet) == 0) { # Data has just been imported
      eng_tab <- get_tab("engagement_meta")
      psy <- str_split(rvs$eng_psy, "_")
      emet <- tibble(
        paper = psy[1], semester = psy[2], year = psy[3],
        last_update = today(),
        first_date  = smin(rvs$eng_dates),
        last_date   = smax(rvs$eng_dates))

      #eng_tab <- eng_tab |> add_case(emet)
      #set_tab("engagement_meta", eng_tab)
    } else {
      message("Engagement metadata")
      print(emet)
    }

    if (nrow(emet))
      tagList(
        strong("Current data covers:"),
        p(sprintf("%s to %s for %s (%s %s). Last updated: %s",
                  emet$first_date, emet$last_date,
                  emet$paper, emet$semester, emet$year,
                  emet$last_update))
      )
  })

  output$engagement_tab <- DT::renderDataTable({
    if(input$is_emergency)
      return(NULL)

    etab <- get_engagement() |>
      filter_eng() |>
      select(ID, where(is.numeric), -year, -Conditional)

    if (input$eng_sort)
      etab <- etab |> arrange(ES)

    DT::datatable(etab,  rownames = FALSE)
  })

  kaiawhina_students <- reactive({
    if(input$is_emergency)
      return(NULL)

    rows_sel <- input$engagement_tab_rows_selected
    if (is.null(rows_sel)) {
      return(NULL)
    } else {
      rvs$eng_tab |> slice(rows_sel)
    }
  })

  output$kaiawhina_students <- DT::renderDataTable({
    kaiawhina_students() |>
      select(across(where(is.numeric))) |>
      DT::datatable()
  })

  output$akoka_followup <- DT::renderDataTable({
    df <- kaiawhina_students()
    if (!is.null(df)) {
      #message("(akoka_followup) df:")
      #print(df)
      df |>
      select(ID, Mobile, Email) |>
      DT::datatable(
        rownames = FALSE,
        selection  = list(mode = 'single', target = 'row'),
        extensions = "Buttons",
        options = list(
          dom = "Bltip",
          buttons = list('copy', 'excel', 'pdf')))
    }
  })

  get_student_record <- function(srec) {
    tab <- get_results() |>
      dplyr::filter(ID == srec$ID) |>
      select(year, paper, Assessment, Grade, Mark) |>
      filter(!grepl("Total Paper Result", Assessment,
                    ignore.case = TRUE) ) |>
      arrange(desc(year), paper)

    if (input$only_final_sr | input$eng_gpa) {
      tab <- tab |> filter(grepl("Overall", Assessment))
      GPA <- round(mean(tab$Mark))
      tab <- tab |> add_row(tibble(Assessment = "<b>Mean</b>", Mark = GPA))
    }

    dtab <- DT::datatable(tab,
                          rownames = FALSE,
                          escape   = FALSE,
                          options  = dt_bare)

    eth_str <- ""
    if ("IWI" %in% names(srec)) {
      if ( !is.na(srec$IWI) & !srec$IWI == "NA" ) {
        eth_str <- sprintf("Ethnicity: %s; Iwi: %s",
                           srec$Ethnicity, srec$IWI)
      }
    }

    tagList(
      #h4(sprintf("Student record for %s (ID: %d)", srec$Name, srec$ID)),
      h4(sprintf("Student record for %s", srec$Name)),
      h5(eth_str),
      DT::renderDataTable(dtab)
    )
  }

  output$akoka_student_record <- renderUI({
    row_sel <- input$akoka_followup_rows_selected

    if (length(row_sel)) {
      kaiawhina_students <- kaiawhina_students()
      student_sel  <- kaiawhina_students[row_sel, ]

      list(
        h4("All assessment results in database", style = sec_style),
        get_student_record(student_sel)
      )
    }
  })

  output$anal_engagement <- renderUI({
    # 1. Re-think engagement metrics
    # 2. As soon as internal assessment data are available, correlate with engagement
    # 3. Ensure date range displays when data are displayed
    if(input$is_emergency)
      return(NULL)

    eng_tab <- get_engagement()

    if (nrow(eng_tab)) {
      if ("bb_hours" %in% names(eng_tab))
        eng_tab <-  eng_tab |> rename(BB_h = bb_hours, Echo_h = echo_hours)

      mbh <- eng_tab |> filter(content > 0) |> pull(content) |> median()

      et <- eng_tab |>
        filter_eng() |>
        rowwise() |>
        mutate(ES = round(sum(c_across(content:echo_pc))), .after = 1 )

      eng_stats <- tribble(
        ~Metric,            ~Median,     ~Max,       ~`Non-zero cases`,
        "Selected students", NA,          NA,          nrow(eng_tab),
        "Content hits",      mm(et, content), mx(et, content), zc(et, content),
        "Tools hits",        mm(et, tools),   mx(et, tools),   zc(et, tools),
        "BB hours",          mm(et, BB_h),    mx(et, BB_h),    zc(et, BB_h),
        "Echo hours",        mm(et, Echo_h),  mx(et, Echo_h),  zc(et, Echo_h),
        "Echo % viewed",     mm(et, echo_pc), NA,          NA,
        "Engagement score",  mm(et, ES),      NA,          NA
      ) |>
        mutate(`% any engagement` = round(100*`Non-zero cases`/nrow(eng_tab)))

      eng_stats[1, ncol(eng_stats)] <- NA

      rvs$eng_tab <- et |>
        mutate(ES_d = pc(ES, eng_stats$Median[nrow(eng_stats)]), .after = ID) |>
        ungroup()


      tagList(
        h4("Summary statistics", style = sec_style),
        DT::renderDataTable(DT::datatable(eng_stats, rownames = FALSE, options = dt_bare)))
    } else {
      tagList(
        h4(sprintf("No engagement data found for %s. Import data from the file browser in the sidebar. You can choose multiple files at once by holding down the Control key as you click on them.", rvs$instance$paper))
      )
    }
  })

  engagement_tab <- function(df, lu) {
    rv <- h4("No data")

    if (!is.null(df)) {
      df <- df |> arrange(Total)
      rv <- list( htmltools::h5(lu), DT::renderDataTable(DT::datatable(df)))
    } else {
      if (!is_null(rvs$bb_AUA))
        rv <- h4("No data here, but data in BB Content")
      if (!is_null(rvs$bb_SUA))
        rv <- h4("No data here, but data in BB Tools")
      if (!is_null(rvs$bb_CAO))
        rv <- h4("No data here, but data in BB Hours")
    }
    rv
  }

  output$engagement_BB_AUA <- renderUI({
    df <- rvs$bb_AUA
    lu <- sprintf("Covers: %s to %s", min(rvs$eng_dates), max(rvs$eng_dates))
    engagement_tab(df, lu)
  })

  output$engagement_BB_SUA <- renderUI({
    df <- rvs$bb_SUA
    lu <- sprintf("Last update: %s", format(rvs$bb_SUA_fi$mtime))
    engagement_tab(df, lu)
  })

  output$engagement_BB_CAO <- renderUI({
    df <- rvs$bb_CAO
    lu <- sprintf("Last update: %s", format(rvs$bb_CAO_fi$mtime))
    engagement_tab(df, lu)
  })

  output$engagement_Echo <- renderUI({
    df <- rvs$Echo360

    if (!is.null(df)) {
      df <- df |> arrange(Total)
      DT::renderDataTable(DT::datatable(df))
    } else {
      h4("No data")
    }
  })

  eng_rvs <- function(rv, offline = FALSE) {
    mdebug("Processing", "eng_rvs")
    df <- rv$df$dat
    fi <- rv$fi

    if (offline) {
      UI       <- simulate_UI()
      input    <- UI$input
      dd       <- UI$dd
    }

    if (is.null(names(df))) {
      message("No data supplied")
      print(names(rv))
      #print(names(df))
      #print(names(fi))
      return(NULL)
    }

    ftype <- sniff_eng(df)

    if (!is.null(rv$dates) ) {
      if (class(rv$dates[1]) == "Date") {
        message("Setting eng_dates (eng_rvs -- is.null(dates))")
        rvs$eng_dates <- rv$dates
      }
    }

    if (!is.null(rv$paper)) {
      if (!rv$paper == "") {
        message("Setting PSY (eng_rvs -- is.null(paper))")
        rvs$eng_psy <- paste(rv$paper, rv$semester, rv$year, collapse = "_")
        print(rvs$eng_psy)
      }
    }

    if (!is.null(rvs$eng_dates) && !is.null(rvs$eng_psy)) {
      message("Setting metadata")

      psy <- str_split(rvs$eng_psy, " ", simplify = TRUE)

      eng_meta <- get_tab("engagement_meta")
      exd <- eng_meta |>
        filter(! (paper == psy[1] &
               semester == psy[2] &
               year     == as.integer(psy[3])))

      nd <- tibble(
        paper = psy[1], semester = psy[2], year = as.integer(psy[3]),
        last_update = lubridate::today(),
        first_date  = smin(rvs$eng_dates),
        last_date   = smax(rvs$eng_dates))

      eng_meta <- exd |>
        add_case(nd) |>
        distinct(paper, year, semester, .keep_all = TRUE)

      set_tab("engagement_meta", eng_meta)
    }

    if ( ftype == "AUA") {
      #message("AUA")
      df <- df |>
        select(Student, Total,
               !matches("^[0-9]|2024|^[a-z]", ignore.case = FALSE)
               )
      if (!offline) {
        rvs$bb_AUA <- df |> filter(!grepl("wiljo00p", Student))
        rvs$bb_AUA_fi <- fi
      }
      return(NULL)
    }

    if (ftype == "SUA") {
      message("SUA")
      if ("Total.x" %in% names(df) )  {
        df <- df |> select(Student, Total = Total.x, announcements, `bb-rubric`)
      } else {
        df <- df |> select(Student, Total, announcements, `bb-rubric`)
      }

      if (!offline) {
        rvs$bb_SUA <- df |> filter(!grepl("wiljo00p", Student))
        rvs$bb_SUA_fi <- fi
      }
      return(NULL)
    }

    if (ftype == "CAO") { # Has course details, including total and active student count
      #message("CAO")
      df <- df |>
        select(Student, Username = `Student ID`, Total = `Course Activity in Hours`) |>
        mutate(Total = round(Total, 1))
      if (!offline) {
        rvs$bb_CAO <- df |> filter(!grepl("wiljo00p", Student))
        rvs$bb_CAO_fi <- fi
        message("Setting PSY (eng_rvs -- CAO)")
        rvs$eng_psy <- paste(rv$paper, rv$semester, rv$year, collapse = "_")
        print(rvs$eng_psy)
      }
      return(NULL)
    }

    if (ftype == "Echo") { # Echo student data
      #message("Echo360")
      df <- df |>
        select(Student = `Student Name`, matches("Video|Engagement")) |>
        group_by(Student) |>
        summarise(Total = sum(`Total Engagement`),
                  `%` = round(mean(`Video View %`)))
      if (!offline) {
        rvs$Echo360 <- df
        rvs$Echo360_fi <- fi
      }
      return(NULL)
    }

    if (offline)
      df
  }

  observeEvent(input$file_upload_BB, {
    dp         <- input$file_upload_BB$datapath
    for (fn in dp) {
      mdebug(sprintf("Importing %s", fn), "observe file_upload_BB")
      rv <- import_data(fn, filename = input$file_upload_BB$name)

      if ("dates" %in% names(rv)) {
        if (!is.null(rv$dates)) {
          rvs$eng_dates <- rv$dates
        }
      }

      mdebug("Sending object for processing", "observe file_upload_BB")
      eng_rvs(rv)
    }
  })

  #observeEvent(input$file_upload_Echo, {
  #  rv <- import_data(input$file_upload_Echo$datapath)
  #  eng_rvs(rv)
  #})

  #### Kaiawhina tabs ####

  filter_kaiawhina_dat <- function(d, code) {

    if (input$kai_include == "BCom only")
      d <- d |> filter(grepl("Bachelor of Commerce", Programme))

    if (input$kai_only_code)
      d <- d |>
        mutate(Papers = str_match_all(Papers, paste0(code, "[0-9]{3}")))

    if (input$kai_conditional)
      d <- d |> filter(Conditional == 1)

    if (!is.null(input$kai_levels)) {
      lvls <- paste(sprintf("[A-Z]{4}%s[0-9]{2}", str_sub(input$kai_levels,1,1)), collapse = "|")
      #print(lvls)
      d <- d |> filter(grepl(lvls, Papers))
    }

    updateCheckboxInput(session = getDefaultReactiveDomain(),
                        inputId = "kai_only_code",
                        label = sprintf("Only show %s papers", code))

    d
  }

  kaiawhina_dat <- function(paper_code) {
    ed <- get_enrolment()
    sd <- get_students()

    CODE <- rlang::as_name( enquo(paper_code) )

    kd <- ed |>
      left_join(sd, by = "ID") |>
      filter({{paper_code}}) |>
      distinct(ID, .keep_all = TRUE) |>
      filter_kaiawhina_dat(CODE)

    all_n <- nrow(kd)

    kd <- kd |>
      filter(Māori) |>
      select(ID, Name, Iwi = IWI, Email, Papers, Programme, Conditional)
    mao_n <- nrow(kd)

    if (DEBUG) {
      message(sprintf("Conditionals for paper code: '%s'", CODE))
      print(kd |> filter(Conditional == 1))
    }

    dt_layout <- list(
      topStart = NULL,
      #topStart         = 'buttons',
      #topStart    = 'pageLength',
      topEnd      = 'search',
      bottomStart = 'info',
      bottomEnd   = 'paging'
    )

    tagList(
      h4(sprintf("%s students meet the sidebar criteria; %d of them (%2.0f%%) whakapapa Māori",
                 fmt_n(all_n), mao_n, 100*mao_n/all_n)),
      kd |>  DT::datatable(rownames = FALSE,
                           extensions = "Buttons",
                           options = list(
                             dom = 'Blftip',
                             lengthMenu = list(
                               c(10, 50, 100, -1),
                               c("10", "50" , "100", "All")),
                             layout = dt_layout,
                             buttons = c("excel", 'pdf', 'copy'))) |>
        DT::renderDataTable()
    )
  }

  output$kaiawhina_bsns <- renderUI({
    kaiawhina_dat(BSNS)
  })

  output$kaiawhina_acct <- renderUI({
    kaiawhina_dat(ACCT)
  })

  output$kaiawhina_comp <- renderUI({
    kaiawhina_dat(COMP)
  })

  output$kaiawhina_econ <- renderUI({
    kaiawhina_dat(ECON)
  })

  output$kaiawhina_finc <- renderUI({
    kaiawhina_dat(FINC)
  })

  output$kaiawhina_info <- renderUI({
    kaiawhina_dat(INFO)
  })

  output$kaiawhina_mart <- renderUI({
    kaiawhina_dat(MART)
  })

  output$kaiawhina_mart <- renderUI({
      kaiawhina_dat(MART)
  })

  output$kaiawhina_mant <- renderUI({
      kaiawhina_dat(MANT)
  })


  output$kaiawhina_tour <- renderUI({
      kaiawhina_dat(TOUR)
  })

  #### Summary Statistics tab ####
  output$course_stats <- renderUI({
    req(rvs$instance)

    offline_testing <- FALSE

    if (offline_testing) {
      UI       <- simulate_UI()
      input    <- UI$input
      dd       <- UI$dd
    } else {
      dd       <- rvs$instance
    }

    paper    <- input$paper_to_show
    year     <- input$year_to_show
    semester <- input$semester_to_show

    rd       <- dd$results

    bz <- tibble()
    if (has_exam(dd))
      bz <- rd %>%
      select(ID, Name, Mark = exam_name(dd)) %>%
      dplyr::filter(Mark < 0)

    if (debug_bz) {
      mdebug(sprintf("Exam grades below zero for %s (%s %s): %d",
                     paper, year, semester, bz %>% nrow),
             "course_stats", force = debug_bz)

      if ( nrow(bz) ) {
        mdebug("Here they are", "course_stats", force = debug_bz)
        print(bz, n = Inf)
      }
    }

    n_n <- nrow(rd)

    s_n <- n_n - nrow(bz)
    if (has_exam(dd)) {
      w_n <- bz |> dplyr::filter(Mark %in% c(-1, -2))  |> nrow()
      a_n <- bz |> dplyr::filter(Mark == -3)           |> nrow()
      f_n <- bz |> dplyr::filter(Mark == -5)           |> nrow()
    } else {
      w_n <- 0
      a_n <- 0
      f_n <- 0
    }

    p_n <- rd |> dplyr::filter(Total >= 50)          |> nrow()
    s_u <- rd |> dplyr::filter(Total %in% c(48, 49)) |> nrow()

    f_c <- which(names(rd) == "Overall paper result")

    if (get_n(f_c) )
      f_n  <-  rd |> dplyr::filter(Total == -5) |> nrow()

    pr_a <- 100 * p_n / n_n
    pr_s <- 100 * p_n / s_n

    fmt <- "%3.0f"

    dat <- tribble(~Datum,             ~Value,
                   "Enrolled (n)",     sprintf(fmt, n_n),
                   "Sat exam (n)",     sprintf(fmt, s_n),
                   "WD & WDE",         sprintf(fmt, w_n),
                   "FT & FC",          sprintf(fmt, f_n),
                   "ABS",             sprintf(fmt, a_n),
                   "48 & 49",          sprintf(fmt, s_u),
                   "Pass rate (enrolled)",  sprintf("%3.0f%%", pr_a),
                   "Pass rate (sat exam)", sprintf("%3.0f%%", pr_s)
    )

    tagList(
      h4("Course data", style = sec_style),
      renderTable(dat, colnames = FALSE, align = c("r"))  # outputArgs = list(align=c("l", "r"))
    )
  })

  output$assessments <- renderUI({
    if(input$is_emergency)
      return(NULL)

    int_ass <- get_assessment_table()

    gp_prop <- int_ass |>
      dplyr::filter(Group == "Y") |>
      summarise(Prop = sum(Weight)) |>
      pull(Prop)

    gp_style <- "font-weight:bold"
    gp_style <- paste(gp_style, ifelse(gp_prop > 30, ";color:red", ""))
    gp_mess  <- ifelse(gp_prop == 0,
                       "No group work",
                       sprintf("Group work: %3.0f%%", gp_prop))

    tagList(h4("Assessment", style = sec_style),
            renderTable(int_ass %>% mutate(`%` = as.integer(Weight)) %>% select(-Weight)),
            span(gp_mess, style = gp_style))
  })

  output$report_scaling <- renderUI({

    dd <- dat()
    track_instance("report_scaling")

    scale_tab <- tibble(Parameter = c("Scaled", "Add", "Multiply", "Threshold"),
                        Value     = c(input$scale_what,
                                      input$scale_add,
                                      input$scale_mul,
                                      input$scale_thr))

    tagList(
      h4("Scaling", style = sec_style),
      renderTable(scale_tab)
    )
  })

  output$group_sum <- renderUI({
    dd <- dat()
    checked <- stringr::str_remove(input$group_work, " \\(.*\\)")
    if ( length(checked) ) {
      en <- exam_name(dd)
      if (is.null(en)) {
        int_ass <- dd$assessments
      } else {
        int_ass <- dd$assessments %>%
          dplyr::filter(Title != en)
      }

      int_ass_checked <- int_ass %>%
        dplyr::filter(Title %in% checked) %>%
        select(Title)
      if (DEBUG) {
        message("The checked items are:")
        print(int_ass_checked)
      }
      qry1 <- sprintf("UPDATE assessments SET is_group = 1
                       WHERE paper    = '%s'
                        AND  year     = '%s'
                        AND  semester = '%s'
                        AND  Title IN ('%s');",
                      dd$paper, dd$year, dd$semester,
                      paste(unlist(int_ass_checked), collapse = "', '"))
      qry2 <- sprintf("UPDATE assessments SET is_group = 0
                       WHERE paper    = '%s'
                        AND  year     = '%s'
                        AND  semester = '%s'
                        AND  NOT Title IN ('%s');",
                      dd$paper, dd$year, dd$semester,
                      paste(unlist(int_ass_checked), collapse = "', '"))
      db <- get_con()
      dbExecute(db, qry1)
      dbExecute(db, qry2)
      dbDisconnect(db)

      group_prop <- int_ass %>%
        dplyr::filter(Title %in% checked) %>%
        summarise(Prop = sum(Weight))

      strong(sprintf("Total group work: %2.0f%%", group_prop))
    } else{
      strong(sprintf("No group work"))
    }
  })

  eq_header <- function(g, cv = input$eq_flags) {
    g |> tab_spanner("Gender", columns = 2:4 ) |>
      tab_spanner("Maori",     columns = 5:7 ) |>
      tab_spanner("Pacific",   columns = 8:10) |>
      cols_label(ends_with("(m)") ~ "Median",
                 ends_with("(p)") ~ "Pass rate",
                 ends_with("(n)") ~ "n") |>
      tab_options(column_labels.font.weight = 'bold') |>
      tab_style_body(
        fn = function(x) {TRUE},
        columns = matches("(p)|(m)"),
        style   = list(
          cell_text(align = "center"))) |>
      tab_style_body(
        fn      = function(x) {abs(as.numeric(x)) > cv},
        columns = matches("\\(p\\)|\\(m\\)"),
        style   = list(
          cell_fill(color = "mistyrose"))) |>
      sub_missing(missing_text = "")
  }

  get_paper_equity <- reactive({
    mdebug(sprintf("Getting etab"), "output$equity", debug_equity)
    s <- Sys.time()
    dd    <- dat()

    etabs <- get_etab(tp = dd$paper, ty = dd$year, ts = dd$semester,
                      exam_only = input$eq_exam_only)
    e <- Sys.time()
    rvs$paper_equity <- etabs

    mdebug(sprintf("Got etab in %s", format(round(e - s, 2))), "output$equity", debug_equity)
  })

  output$equity_others <- render_gt({
    #itab <- gt(etabs$itab) |> eq_header()
    get_paper_equity()
    rvs$paper_equity$itab |> gt() |> eq_header()
  })

  output$equity_time <- render_gt({
    get_paper_equity()
    rvs$paper_equity$ytab |> gt() |> eq_header()
  })

  output$equity_deets <- renderDataTable({
    get_paper_equity()
    bdt(rvs$paper_equity$stab)
  })

  get_edat <- reactive({
    s <- Sys.time()
    etabs <- get_etab(exam_only = input$eq_exam_only)
    e <- Sys.time()
    mdebug(sprintf("Got etab in %s", format(round(e - s, 2))), "output$kpis_enrolment")

    if (is.null(etabs))
      return(h1("Choices resulted in no data"))
    etabs$edat
  })


  output$kpis_enrolment <- renderUI({
    if(input$is_emergency)
      return(NULL)
    edat <- get_edat()
    par_tab <- tibble(Paramater = c("Papers", "Years"),
                      Value     = c(plist(sort(unique(edat$paper))),
                                    plist(unique(edat$year))))

    tagList(
      fluidRow({
        papers <- plist(unique(edat |> pull(paper)) |> sort())
        h4(sprintf("Papers included: %s", papers))
      })
    )
  })

  output$kpis_enrolment_status <- render_gt({
    if(input$is_emergency)
      return(NULL)
    s <- Sys.time()
    edat <- get_edat()
    sts_tab <- edat |>
      mutate(Status = case_match(Mark,
                                 50:100 ~ "Pass",
                                 -1 ~ "WDN",
                                 -2 ~ "WDE",
                                 -3 ~ "ABS",
                                 -4 ~ "FC",
                                 -5 ~ "FT",
                                 .default = "Fail"),
             .after = Mark)  |>
      count(Status) |>
      mutate(`%` = 100*n/sum(n)) |>
      arrange(desc(n))
    rv <- gt(sts_tab) |> mt(pct_d = 1) |> tab_caption("Status counts")

    e <- Sys.time()
    mdebug(sprintf("gt object generated in %s", format(round(e - s))), "kpis_enrolment_status", TRUE)

    rv
  })

  # Notes: it appears Prog1Maj1, Prog2Maj1 contain multiple majors also Prog1Maj2 ands Prog2Maj2 are always empty
  # It's possible for Prog1Maj1 to be empty, but Prog2Maj1 non-empty

  output$kpis_enrolment_prg <- render_gt({
    if(input$is_emergency)
      return(NULL)

    prg_tab <- get_edat() |>
      filter(complete.cases(Programme2)) |>
      mutate(Programmes = sprintf("%s, %s", Programme, Programme2),
             Programmes = str_remove_all( Programmes, " \\(First Year\\)"),
             Programmes = str_replace_all(Programmes, "(.*), Bachelor of Commerce", "Bachelor of Commerce, \\1"),
             Programmes = str_replace_all(Programmes, "(.*), Bachelor of Laws",     "Bachelor of Laws, \\1")
      ) |>
      ptab(  Programmes, input$kpis_prog_min)
  })

  output$kpis_enrolment_maj <- render_gt({
    if(input$is_emergency)
      return(NULL)

    edat <- get_edat() |>
      select(ID, paper, year, semester, Mark, Programme, Programme2, Prog1Maj1, Prog2Maj1, year) |>
      mutate(Programmes = sprintf("%s, %s", Programme, Programme2),
             Majors = sprintf("%s, %s", Prog1Maj1, Prog2Maj1),
             Majors = str_replace(Majors, "Marketing Management", "Marketing"),
             Majors = str_replace(Majors, ", NA", ""),
             Majors = str_replace(Majors, "NA", ""),
             Majors = str_replace(Majors, "^, ", ""),
             Single_degree = ifelse(is.na(Programme2), TRUE, FALSE),
             Majors_n  = 1 + str_count(Majors, ","),
             Majors_n = if_else(Majors == "", 0, Majors_n),
             Single_major = Majors_n < 2)

    maj_tab <- edat |>
      filter(Majors != "") |>
      ptab(Majors, input$kpis_maj_min)

    mmaj_tab <- edat |>
      filter(Majors != "", Majors_n > 1) |>
      ptab(Majors, input$kpis_maj_min)

    gt(maj_tab) |> mt()          |> tab_caption("Major counts")
  })

  output$kpis_enrolment_mmaj <- render_gt({
    if(input$is_emergency)
      return(NULL)

    edat <- get_edat() |>
      select(ID, paper, year, semester, Mark, Programme, Programme2, Prog1Maj1, Prog2Maj1, year) |>
      mutate(Programmes = sprintf("%s, %s", Programme, Programme2),
             Majors = sprintf("%s, %s", Prog1Maj1, Prog2Maj1),
             Majors = str_replace(Majors, "Marketing Management", "Marketing"),
             Majors = str_replace(Majors, ", NA", ""),
             Majors = str_replace(Majors, "NA", ""),
             Majors = str_replace(Majors, "^, ", ""),
             Single_degree = ifelse(is.na(Programme2), TRUE, FALSE),
             Majors_n  = 1 + str_count(Majors, ","),
             Majors_n = if_else(Majors == "", 0, Majors_n),
             Single_major = Majors_n < 2)

    mmaj_tab <- edat |>
      filter(Majors != "", Majors_n > 1) |>
      ptab(Majors, input$kpis_maj_min)

    gt(mmaj_tab) |> mt()          |> tab_caption("Multiple Major counts")
  })

  my_theme <- theme_minimal() + theme(
    axis.title      = element_blank(),
    axis.text       = element_text(size = 16),
    plot.title      = element_text(size = 18),
    legend.text     = element_text(size = 16),
    legend.position = "bottom",
    plot.margin     = unit(c(0,0,20,0), "points")
  )

  output$kpis_grades <- renderUI({
    if(input$is_emergency)
      return(NULL)

    ph       <- round(round(as.numeric(input$dimension[2]) / 3) )
    fluidRow({
      req(input$kpis_inc_maj)
      papers <- plist(unique(get_edat() |> pull(paper)) |> sort())
      h4(sprintf("Papers included: %s", papers))
    })
  })

  output$kpis_grades_bps <- renderPlot({
    if(input$is_emergency)
      return(NULL)

    s <- Sys.time()
    rv <- get_edat() |>
      filter(Mark > 0) |>
      ggplot(aes(x = factor(year), y = Mark)) +
      geom_boxplot() +
      labs(title = "Central tendency and dispersion of overall mark over time") +
      my_theme

    e <- Sys.time()
    mdebug(sprintf("ggplot object generated in %s", format(round(e - s))), "kpis_grades_bps", TRUE)

    rv
  })

  output$kpis_grades_bar <- renderPlot({
    pdat <- get_edat() |>
      filter(Mark > 0) |>
      mutate(Grade = grades(Mark)) |>
      group_by(year) |>
      count(Grade)

    ggplot(pdat, aes(x = Grade, y = n, fill = factor(year))) +
      geom_bar(position = 'dodge', stat = 'identity') +
      labs(title = "Change in grades over time") +
      my_theme
  })

  output$kpis_grades_hst <- renderPlot({
    pdat <- get_edat() |>
      filter(Mark > 0) |>
      mutate(Grade = grades(Mark)) |>
      group_by(year) |>
      count(Grade) |>
      mutate(pc = 100*n/sum(n))

    p <- ggplot(pdat, aes(x = Grade, y = pc)) +
      geom_bar(stat = 'identity') +
      labs(title = "Distribution of grades by year") +
      my_theme + theme(strip.text       = element_text(size = 18, colour = 'black'),
                       strip.background = element_rect(           fill   = 'white'))

    if (length(unique(pdat$year)) > 1 )
      p <- p + facet_wrap(~year)

    p
  })

  output$kpis_grades_fct <- renderPlot({
    pdat <- get_edat() |>
      filter(Mark > 0) |>
      mutate(Grade = grades(Mark)) |>
      group_by(paper) |>
      count(Grade) |>
      mutate(pc = 100 * n / sum(n))

    p <- ggplot(pdat, aes(x = Grade, y = pc)) +
      geom_bar(stat = 'identity') +
      labs(title = "Distribution of grades by paper") +
      my_theme + theme(strip.text       = element_text(size = 18, colour = 'black'),
                       strip.background = element_rect(           fill   = 'white'))

    if (length(unique(pdat$paper)) > 1 )
      p <- p + facet_wrap(~paper, ncol = 3)

    p
  })

  gap_plot <- function(d, g, s, ymin = -10, ymax = 0, ph = 400) {
    metric <- ifelse(s == "p", "pass rate", "overall mark")

    d |>
      select(year, y = all_of(sprintf("%s (%s)", g, s))) |>
      ggplot(aes(x = year, y = as.numeric(y))) +
      geom_line() +
      scale_y_reverse(limits = c(ymax, ymin)) +
      labs(title = sprintf("Median %s gap: %s", metric, g)) +
      my_theme
  }

  eq_bps <- function(d, g, ph) {
    gname <- rlang::as_name(enquo(g))
    d |>
      filter(Mark > 0) |>
      mutate(year = factor(year)) |>
      ggplot(aes(x = year, y = Mark, color = {{ g }} ) ) +
      geom_boxplot() +
      labs(title = sprintf("Overall mark, by %s over time", gname)) +
      my_theme
  }

  eq_bpi <- function(d, g) {
    library(ggiraph)
    dat <- d |>
      filter(Mark > 0) |>
      mutate(year = factor(year))

    gname <- rlang::as_name(enquo(g))
    renderGirafe({
    p1 <- ggplot(data   = dat,
                mapping = aes(x = year, y = Mark, colour = {{g}}, data_id = Mark )) +
      geom_boxplot_interactive(
        aes(tooltip = after_stat(sprintf("%s scored median grade: %2.0f in %s)",
                                         .data$colour, .data$middle, # .data$ymin, .data$ymax, = Q1, Q4
                                         levels(dat$year)[.data$x])))) +
      labs(title = sprintf("Overall mark, by %s over time", gname)) +
      my_theme

    girafe(ggobj = p1)
  })}

  output$kpis_equity <- renderUI({
    if (FALSE) {
      s <- Sys.time()
      etabs <- get_etab(exam_only = input$eq_exam_only);
      e <- Sys.time()
      mdebug(sprintf("Got etab in %s", format(round(e - s, 2))), "output$kpis_equity", TRUE)

      if (is.null(etabs))
        return(h1("Choices produced no data"))

      pgap_min <- etabs$ytab |> select(contains("(p)")) |> unlist() |> as.numeric() |> min()
      mgap_min <- etabs$ytab |> select(contains("(m)")) |> unlist() |> as.numeric() |> min()
      pgap_max <- etabs$ytab |> select(contains("(p)")) |> unlist() |> as.numeric() |> max()
      mgap_max <- etabs$ytab |> select(contains("(m)")) |> unlist() |> as.numeric() |> max()

      ph <- round(round(as.numeric(input$dimension[2]) / 5) )

      if (DEBUG) {
        mdebug( sprintf( "Gender pgap max: %3.1f min: %3.1f; mgap max: %3.2f min: %3.2f",
                         pgap_min, pgap_max, mgap_min, mgap_max), "kpis_equity", TRUE)
        print(etabs$ytab)
      }
    }

    fluidRow({
      papers <- plist(unique(get_edat() |> pull(paper)) |> sort())
      h4(sprintf("Papers included: %s", papers))
    })
  })

  get_ytab <- reactive({
    etabs <- get_etab(exam_only = input$eq_exam_only);

    if (is.null(etabs))
      return(h1("Choices produced no data"))

    pgap_min <- etabs$ytab |> select(contains("(p)")) |> unlist() |> as.numeric() |> min()
    mgap_min <- etabs$ytab |> select(contains("(m)")) |> unlist() |> as.numeric() |> min()
    pgap_max <- etabs$ytab |> select(contains("(p)")) |> unlist() |> as.numeric() |> max()
    mgap_max <- etabs$ytab |> select(contains("(m)")) |> unlist() |> as.numeric() |> max()

    ph <- round(round(as.numeric(input$dimension[2]) / 5) )

    list(ytab = etabs$ytab, pgap_min = pgap_min, pgap_max = pgap_max,
         mgap_min = mgap_min, mgap_max = mgap_max, ph = ph)
  })

  output$kpis_gaps_gpl <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab, "Gender",  "p", yt$pgap_min, yt$pgap_max)
  })

  output$kpis_gaps_mpl <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab,   "Māori", "p", yt$pgap_min, yt$pgap_max, yt$ph)
  })

  output$kpis_gaps_ppl <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab, "Pacific", "p", yt$pgap_min, yt$pgap_max, yt$ph)
  })

  output$kpis_gaps_gml <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab,  "Gender", "m", yt$mgap_min, yt$mgap_max, yt$ph)
  })

  output$kpis_gaps_mml <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab,   "Māori", "m", yt$mgap_min, yt$mgap_max, yt$ph)
  })

  output$kpis_gaps_pml <- renderPlot({
    yt <- get_ytab()
    gap_plot(yt$ytab, "Pacific", "m", yt$mgap_min, yt$mgap_max, yt$ph)
  })

  output$kpis_gaps_gmb <- renderPlot({
    yt <- get_ytab()
    eq_bps(get_edat(), Gender,  yt$ph)
  })

  output$kpis_gaps_mmb <- renderPlot({
    yt <- get_ytab()
    eq_bps(get_edat(), Māori,   yt$ph)
  })

  output$kpis_gaps_pmb <- renderPlot({
    yt <- get_ytab()
    eq_bps(get_edat(), Pacific, yt$ph)
  })

  output$stats <- renderUI({
    if(input$is_emergency)
      return(NULL)
    offline_testing <- FALSE

    dd  <- dat()

    if (offline_testing) {
      UI    <- simulate_UI()
      input <- UI$input
      dd    <- UI$dd
      debug_stats <- TRUE
      input$set_dept <- "BCom Core"
    }

    en          <- exam_name(dd)
    ddr         <- dd$results |> select(-ID, -Name)
    comp_papers <- get_paper_codes(input$set_dept)

    afr <- get_totals() |>
      filter(paper %in% comp_papers)

    mdebug(sprintf("afr has %d rows, so filtering for papers in %s", nrow(afr), plist(comp_papers)),
           "stats", debug_stats)

    this_paper <- afr |> dplyr::filter(paper == dd$paper)
    mdebug(sprintf("This paper (%s %s %s) has %d records", dd$paper, dd$year, dd$semester, nrow(this_paper)),
           "stats", debug_stats)

    stab <- get_students() |>
      distinct(ID, .keep_all = TRUE)

    this_paper_equity <- this_paper |>
      left_join(stab, by = "ID") |>
      filter(complete.cases(Gender))

    if (!is.null(en)) {
      ei      <- ddr |> select(Internal, all_of(en))
      cor_ei  <- cor(ei, use = "pair", method = "spearman")
      cor_ei  <- cor_ei[1,2]
      cor_str <- sprintf("Correlation of Internal with Exam: %3.2f", cor_ei)
    } else {
      cor_str <- ""
    }

    if ( !input$inc_abs ) {
      if (!is.null(en)) {
        exam <- ddr |> select(all_of(en))
        ddr  <- ddr |> dplyr::filter(exam != -3)
      }
    }

    if ( !input$inc_fct ) {
        ddr  <- ddr |> dplyr::filter(exam > -4)
    }

    if ( !input$inc_wdn ) {
      if (!is.null(en)) {
        exam <- exam_name(dd)
        ddr  <- ddr |> dplyr::filter(
          .data[[exam]]  != -2,
          .data[[exam]]  != -1)
      }
    }

    ass_tab <- as_tibble(summary_stats(ddr, iz = input$inc_zer), rownames = "Assessment") |>
      dplyr::filter(Assessment != "Overall paper result") |>
      left_join(dd$assessments, by = c("Assessment" = "Title")) |>
      select(Assessment, Weight, everything(), -paper, -year, -semester, -is_group) |>
      mutate(Weight = as.character(Weight),
             Weight = ifelse(is.na(Weight), "", Weight)) |>
      rename(`%` = Weight)

    mdebug(sprintf("Papers in afr: %s", plist(unique(afr$paper))), "sum_stats", debug_stats)

    if (input$inc_pg == FALSE)
      afr <- afr |> dplyr::filter(!grepl("MART4", paper))
    if (input$inc_ug == FALSE)
      afr <- afr |> dplyr::filter( grepl("MART4", paper))
    if (input$inc_s1 == FALSE)
      afr <- afr |> dplyr::filter(semester != "S1")
    if (input$inc_s2 == FALSE)
      afr <- afr |> dplyr::filter(semester != "S2")

    mdebug(sprintf("papers in afr after filters applied: %s",
                   plist(unique(afr$paper))), "sum_stats", debug_stats)

    all_med   <- median(afr$Mark)
    comp_year <- as.numeric(dd$year)

    this_paper_sum <-   this_paper |>
      rename(sem = semester) |>
      dplyr::filter(Mark >= 0) |>
      mutate(Pass = if_else(Mark >= 50, TRUE, FALSE)) |>
      group_by(year, sem) |>
      summarise(Mean   = mean(  Mark),
                Med    = median(Mark),
                IQR    = IQR(   Mark),
                PR     = 100 * sum(Pass)/n(),
                .groups = "keep") |>
      arrange(desc(year), desc(sem))

    comp_stats <- afr |>
      dplyr::filter(year >= min(this_paper$year)) |>
      mutate(Pass = if_else(Mark >= 50, TRUE, FALSE)) |>
      rename(sem = semester)

    both_sems <- this_paper_sum |> ungroup() |> distinct(sem) |> nrow() == 2
    if (both_sems) {
      comp_stats <- comp_stats |>
        group_by(year, sem) |>
        filter(sem %in% c("S1", "S2"))
    } else {
      comp_stats <- comp_stats |>
        group_by(year)
    }

    comp_stats <- comp_stats |>
      summarise(Mean   = mean(Mark),
                Med    = median(Mark),
                IQR    = IQR(Mark),
                PR     = 100 * sum(Pass)/n(),
                .groups = "keep") |>
      arrange(desc(year))

    if (both_sems) {
      comb_stats <- this_paper_sum |> left_join(comp_stats, by = c("year", "sem"))
    } else {
      comb_stats <- this_paper_sum |> left_join(comp_stats, by = "year")
    }

    names(comb_stats)[3:10] <- c("Mean", "Med", "IQR", "PR", "Mean (all)", "Med (all)", "IQR (all)", "PR (all)")

    comb_stats <- comb_stats |>
      mutate(`Δ med` = Med - `Med (all)`,
             `Δ PR`  = PR  - `PR (all)`)

    bpt_stub <- "Distribution of Overall Result from all "
    bpt_subset  <- case_when(
      input$inc_pg == FALSE ~ "undergrad",
      input$inc_ug == FALSE ~ "postgrad",
      input$inc_s1 == FALSE ~ "S2",
      input$inc_s2 == FALSE ~ "S1",
      .default = ""
    )

    boxplot_title <- sprintf("%s %s %s papers in %s",
                             bpt_stub, bpt_subset, input$set_dept,
                             dd$year)
    tagList(
      fluidRow(
        column(5, h3("Assessment statistics",         style = sec_style)),
        column(7, h3("Comparisons of Overall Result", style = sec_style))
        ),
      fluidRow(
        column(5,
               h5(sprintf("Stats for %s asessments", dd$paper)),
               renderTable(ass_tab, digits = 0),
               p(cor_str)
        ),
        column(7,
               h5(sprintf("Stats for %s vs. all %d other %s papers %s to %s",
                          dd$paper,
                          length(unique(afr$paper)),
                          input$set_dept,
                          min(afr$year), max(afr$year)
                          )),
               DT::renderDataTable(DT::datatable(comb_stats,
                                             rownames   = FALSE,
                                             selection  = 'single',
                                             options = list(searching  = FALSE,
                                                            lengthMenu = list(
                                                              c(10, -1),
                                                              c("10", "All")))) |>
                                 formatRound(columns  = names(comb_stats), digits = 0) |>
                                 formatString(columns = c("year", "sem")) |>
                                 formatStyle(
                                     columns = c('year', 'sem'),
                                     target = 'row',
                                     backgroundColor = styleEqual(
                                       levels = dd$year,
                                       c('lightyellow'))
                                 )
               )
        )
      ),
      fluidRow(
        # Boxplots
        tagList(
          h3(boxplot_title, style = sec_style),
          #plotOutput(
            renderPlot(
              {
                Papers    <- dd$paper
                this_med  <- median(dd$results$Total)

                afr |>
                  mutate(focal_paper = ifelse(paper == dd$paper, TRUE, FALSE)) |>
                  dplyr::filter(as.numeric(year) == comp_year) |>
                  ggplot(aes(x = paper, y = Mark, fill = focal_paper)) +
                  geom_boxplot() +
                  scale_fill_manual(values = c("cornsilk", "goldenrod")) +
                  theme(legend.position = "none") +
                  theme(axis.text.x = element_text(angle = 315, size = 12)) +
                  labs(x = NULL, y = NULL) +
                  geom_hline(aes(yintercept = all_med,  colour = "All papers"))
              }
            ),
          p()
        )
      )
    )
  })

  #### Report tab ####
  output$report <- renderUI({
    dd <- dat();
    class_n <- nrow(dd$results)
    en <- exam_name(dd)

    if (!is.null(en)) {
      wwe_n   <- dd$results |> dplyr::filter(dd$results[, en] <   0) |> nrow()
      absent  <- dd$results |> dplyr::filter(dd$results[, en] == -3) |> nrow()
      exam_n  <- class_n - wwe_n
    } else {
      wwe_n  <- 0
      absent <- 0
      exam_n <- 0
    }

    pass_n  <- dd$results %>% dplyr::filter(dd$results[, "Total"      ] >= 50) %>% nrow

    pass_rate      <- round(100 * pass_n / class_n)
    attrition_rate <- round(100 * wwe_n  / class_n)

    tagList(
      p(strong("Paper"),          ":", dd$paper),
      p(strong("Enrolments"),     ":", class_n),
      p(strong("Sat exam"),       ":", exam_n),
      p(strong("Absent"),         ":", absent),
      p(strong("Pass rate"),      ":", pass_rate, "%"),
      p(strong("Attrition rate"), ":", attrition_rate, "%"),
      p("")
    )
  })

  get_assessment_table <- function() {
    dd <- dat()

    if (is.null(dd))
      return(NULL)

    mdebug(sprintf("Firing -- '%s' ('%s' '%s')",
           dd$paper, dd$year, dd$semester), "get_assessment_table", force = debug_flicker)
    ass_sel <- input$set_group_work

    at <- get_assessments() |>
      dplyr::filter(paper == dd$paper, year == dd$year, semester == dd$semester)

    en <- exam_name(dd)
    if (is.null(en)) {
      int_ass <- at
    } else {
      int_ass <- at |>
        dplyr::filter(!Title == en)
    }

    int_ass <-  int_ass |>
      dplyr::filter(!Title %in% result_names) |>
      mutate(`%` = as.integer(Weight),
             isGroup = ifelse(is_group, "Y", "N")) |>
      select(Assessment = Title, Weight, Group = isGroup)
  }

  set_assessment_table <- function() {
    dd <- dat()
    mdebug(sprintf("(set_assessment_table): firing for '%s' ('%s', '%s')",
                   dd$paper, dd$year, dd$semester), force = F)
    write_report()
    atab <- get_assessments()
    ntab <- atab |> dplyr::filter(  paper == dd$paper,  year == dd$year,  semester == dd$semester)
    atab <- atab |> dplyr::filter(!(paper == dd$paper & year == dd$year & semester == dd$semester))
    ntab <- ntab |> mutate(is_group = ifelse(Title %in% input$set_group_work, 1, 0))

    if (FALSE) {
      mdebug(sprintf("(set_assessment_table): value of input control: '%s'",
                     paste(input$set_group_work, collapse = ", ")))
      mdebug(sprintf("(set_assessment_table): value of coordinator: '%s'",
                     paste(input$report_coordinator, collapse = ", ")), force = TRUE)
      print(ntab)
    }

    set_tab("assessments", atab |> add_row(ntab))
  }

  output$build_report <- renderUI({
    req(rvs$instance)
    paper    <- input$paper_to_show
    year     <- input$year_to_show
    semester <- input$semester_to_show

    mdebug(sprintf("(build_report) firing: %s (%s, %s)", paper, year, semester), force = F)

    pwidth <- "100%"
    ph_coordinator <- "Who is the course course leader and author of this report?"
    ph_staff       <- "Who is the course administrator, and any other lecturers or tutors?"
    ph_group       <- "Which assessment items were group work? (Tick the boxes on the right.) How were the groups formed, and how were problems dealt with?"
    ph_exam        <- "Number of questions and format (essay, short answer, multi-choice), any compulsory? E.g. any 6 from 10 SA; 40 MC"
    ph_fbc         <- "How was student feedback obtained?"
    ph_scaling     <- "If scaling was performed, what was the rationale, and the details (criterion, addition and multiplication)?"
    ph_misconduct  <- "Were there any cases of academic misconduct, and if so, how were they dealt with and what was the result of each case?"
    ph_fbi         <- "What were the major positive and negative issues arising from student feedback?"
    ph_fba         <- "Have these issues been dealt with? Have students been informed of actions taken?"
    ph_report      <- "What went well with this paper? Identify any examples of innovation and good teaching & learning assessment practices, including any changes as a result of reflecting on the last delivery."
    ph_changes     <- "Have there been any other significant changes, and are you planning any in the future, to the paper? (e.g. teaching methods, staff, assessment methods, aims and objectives, course content, etc.) What are the reasons for the changes and have they been successful?"

    dd <- dat()
    rd <- get_report(dd$paper, dd$year, dd$semester)

    coordinator <- ifelse(length(rd$coordinator) == 0, "No-one", rd$coordinator)

    int_ass <- get_assessment_table()
    ass_sel <- int_ass$Assessment[int_ass$Group == "Y"]

    tagList(
      fluidRow(
        column(6,
               textAreaInput(inputId = "report_coordinator","Course leader",
                             value   = rd |> pull(coordinator),
                             width   = pwidth, placeholder = ph_coordinator),
               textAreaInput(inputId = "report_staff",  "Other teaching staff",
                             value   = rd |> pull(staff),
                             width   = pwidth, placeholder = ph_staff),

               textAreaInput(inputId = "report_exam_format", "Exam format",
                             value   = rd |> pull(exam_format),
                             width   = pwidth, placeholder = ph_exam),
               fluidRow(
                 column(9,
                        textAreaInput("report_group_work", "Assessment",
                                      value = rd |> pull(group_work),
                                      width = pwidth, placeholder = ph_group, height = "100%")),
                 column(3,
                        checkboxGroupInput("set_group_work", "Group work",
                                           choices  = int_ass$Assessment,
                                           selected = ass_sel),
                        style = "background-color:cornsilk;color:red"),
               ),
               textAreaInput("report_misconduct",  "Academic misconduct",
                             value = rd |> pull(misconduct),
                             width = pwidth, placeholder = ph_misconduct),
               textAreaInput("report_scaling",     "Scaling and justification",
                             value = rd |> pull(scaling),
                             width = pwidth, placeholder = ph_scaling),

               textAreaInput("report_feedback_channels", "Student feedback channels",
                             value = rd |> pull(feedback_channels),
                             width = pwidth, placeholder = ph_fbc),

               textAreaInput("report_feedback_issues", "Student feedback issues",
                             value = rd |> pull(feedback_issues),
                             width = pwidth, placeholder = ph_fbi),
               textAreaInput("report_feedback_actions", "Student feedback actions",
                             value = rd |> pull(feedback_actions),
                             width = pwidth, placeholder = ph_fba),
        ),
        column(6,
               textAreaInput("report_changes", "Any significant changes",
                             value = rd |> pull(changes),
                             rows = 10,  width = pwidth, placeholder = ph_changes),
               textAreaInput("report_report", "Coordinator's report",
                             value = rd |> pull(report),
                             rows = 27, width = pwidth,placeholder = ph_report))
      ))
  })

  output$render_report <- renderUI({
    if(input$is_emergency)
      return(NULL)

    dd         <- rvs$instance
    t_paper    <- input$paper_to_show
    t_year     <- input$year_to_show
    t_semester <- input$semester_to_show
    report     <- get_report(t_paper, t_year, t_semester)
    pwidth   <- 500

    mdebug(sprintf("Rendering report for: '%s' ('%s', '%s')", t_paper, t_year, t_semester))

    fmt_h <- function(s) {
      ss   <- deparse(substitute(s))
      fn   <- str_remove(ss, "input\\$report_")
      s_ed <- report |> pull(fn)
      s_nd <- get_ss(s)

      if ( length(s_nd) == 0)  {
        srv  <- report |> pull(fn)
      } else {
        s_ed <- get_ss(s_ed)
        if ( !identical(s_nd, s_ed) & str_length(get_ss(s_nd)) > 0)
          srv <- s_nd
        else
          srv <- s_ed
      }

      srv <- s_ed
      content <- srv |> str_replace_all("\n", "<br/>\n")
      content <- content |>  str_replace_all(c("â€¢" = "•", "â€œ" = '“', "â€" = '”'))
      HTML(paste("<div style = 'font-size:large'>", content, "</div><br/>"))
    }

    tagList(
      column(6,
             h3(sprintf("Facts about the paper (%s %s %s)", t_paper, t_year, t_semester), style = h3_style),
             h4("Coordinator",              style = h4_style),
             fmt_h(input$report_coordinator),
             h4("Staff",                    style = h4_style),
             fmt_h(input$report_staff),
             h4("Assessment",               style = h4_style),
             fmt_h(input$report_group_work),
             h4("Exam format",              style = h4_style),
             fmt_h(input$report_exam_format),
             h4("Scaling",                  style = h4_style),
             fmt_h(input$report_scaling),
             h4("Academic misconduct",      style = h4_style),
             fmt_h(input$report_misconduct),
             h4("Feedback channels",        style = h4_style),
             fmt_h(input$report_feedback_channels),
             h4("Feedback issues",          style = h4_style),
             fmt_h(input$report_feedback_issues),
             h4("Feedback actions",         style = h4_style),
             fmt_h(input$report_feedback_actions)
      ),
      column(6,
             h3("Reflection on the paper",  style = h3_style),
             h4("Report",                   style = h4_style),
             fmt_h(input$report_report),
             h4("Changes to course",        style = h4_style),
             fmt_h(input$report_changes)
      )
    )
  })

  toListen_report <- reactive({
    mdebug("Making input controls reactive.", "toListen_report", DEBUG)
    list(
      input$report_coordinator,       input$report_staff,
      input$report_group_work,        input$report_exam_format,
      input$report_scaling,           input$report_misconduct,
      input$report_feedback_channels, input$report_feedback_issues,
      input$report_feedback_actions,  input$report_report,
      input$report_changes)
  })

  toListen_scaling <- reactive({
    mdebug("Firing", "toListen_scaling", DEBUG)
    list(
      input$scale_what,               input$scale_multiply,
      input$scale_add,                input$scale_thr)
  })

  toListenGroupWork <- reactive({
    mdebug("Firing", "toListenGroupWork", DEBUG)
    list(input$set_group_work)
  })

  observeEvent(
    vals <- toListenGroupWork(),
    {
      set_assessment_table()
    }, ignoreInit = TRUE
  )

  observeEvent(
    vals <- toListen_report(),
    {
      mdebug(" firing: report has changed, writing report", "observeEvent", debug_RAM)
      dd  <- rvs$instance
      write_report()
    }, ignoreInit = TRUE)

  observeEvent(
    vals <- toListen_scaling(),
    {
      mdebug("firing: scaling has changed, writing values to report", "observeEvent", force = DEBUG)
      wha <- get_ss(input$scale_what)
      thr <- get_ss(input$scale_thr)
      add <- get_ss(input$scale_add)
      mul <- get_ss(input$scale_mul)

      pap <- get_ss(rvs$instance$paper)
      sem <- get_ss(rvs$instance$semester)
      yea <- get_ss(rvs$instance$year)

      vals <- c(wha, thr, add, mul, pap, sem, yea)
      if ( all(vals != '' ) ) {
        mdebug(sprintf("(line 3075) Scaling for %s %s %s changed: what: '%s', threshold: '%s', add: '%s', multiply: '%s'",
                        pap, yea, sem, wha, thr, add, mul), "observeEvent")
        qry1 <- sprintf("UPDATE reports
                          SET scale_what = '%s',
                              scale_thr  = '%s',
                              scale_add  = '%s',
                              scale_mul  = '%s'
                          WHERE paper    = '%s'
                            AND year     = '%s'
                            AND semester = '%s'
                        ;",
                        wha, thr, add, mul,
                        pap, yea, sem)
        db <- get_con()
        rows_affected <- dbExecute(db, qry1)
        dbDisconnect(db)
      }
    }, ignoreInit = TRUE)

  output$get_year_list_control <- renderUI({
    years <- get_totals() |>
      filter(complete.cases(year)) |>
      pull(year) |> as.numeric()
    miny <- min(years)
    maxy <- max(years, na.rm = TRUE)
    years <- rev(sort(unique(years)))
    selectInput("students_year", "Year (latest)", years, selected = maxy )
  })

  #### All Students tab  ####
  filter_final_results <- reactive({
    mdebug("Firing", "filter_final_results", DEBUG)
    tab <- get_totals()

    if (FALSE) {
      lvls      <- c(100, 200, 300, 400, 500)
      paper_g_c <- 70
      paper_n_c <- 2
      t_year    <- get_year()
      fas       <- TRUE
      fbcom     <- FALSE
      fmart     <- FALSE
    } else {
      lvls      <- input$filter_levels
      paper_g_c <- input$students_grade
      paper_n_c <- input$students_papers
      t_year    <- as.numeric(input$students_year)
      t_year    <- ifelse(length(t_year) == 0, as.numeric(get_year()), t_year)
      fas       <- input$filter_all_students == 'Yes'
      fbcom     <- input$filter_bcom
      fmart     <- input$filter_mart
    }

    mdebug(sprintf("The value of t_year is: '%d'", t_year))

    if (!is.na(lvls[1])) {
      stab <- tab |> slice(1)
      for (lvl in lvls) {
        ltab <- tab |> dplyr::filter(str_sub(paper, 5, 5) %in% str_sub(lvl, 1, 1))
        stab <- stab |> add_row(ltab)
      }
      tab <- stab
    }

    tab <- tab |>
      dplyr::filter(year == t_year) |>
      group_by(Name, ID) |>
      summarise(Mean = round(mean(Mark)), Papers = n(), .groups = "keep") |>
      arrange(desc(Mean))

    if (fas) {
      tab <- tab |>
        dplyr::filter(Mean >= paper_g_c, Papers >= paper_n_c)
    }

    stab <- get_students()
    mdebug(sprintf("Names of stab: %s", plist(names(stab))), "filter_final_results")

    tab <- tab |>
      group_by(ID) |>
      summarise(Mean = mean(Mean)) |>
      left_join(stab, by = "ID")

    mdebug(sprintf("Names of tab: %s", plist(names(tab))), "filter_final_results")
    if (fbcom)
      tab <- tab |> dplyr::filter(grepl("Commerce", Programme) | grepl("Commerce", Programme2))

    if (fmart)
      tab <- tab |> dplyr::filter(grepl("Marketing", Prog1Maj1))

    tab <- tab |>
      select(ID, Firstname, Surname, Mean) |>
      mutate(Name = sprintf("%s %s", Firstname, Surname), .after = "ID") |>
      select(-Firstname, -Surname) |>
      distinct() |>
      arrange(desc(Mean))

    rvs$top_students <- tab
    tab
  })



  output$top_students <- DT::renderDataTable({
    DT::datatable(filter_final_results(),
                  rownames = TRUE,
                  options = list(lengthMenu = list(c( 20,   50,   100,   -1),
                                                   c('20', '50', '100', 'All'))))
  })

  output$selected_students <- DT::renderDataTable({
    selected_students() |>
      DT::datatable(rownames = FALSE,
                    extensions = "Buttons",
                    selection  = list(mode = 'single', target = 'row'),
                    options    = list(paging = FALSE,
                                      searching = FALSE,
                                      dom = "tB",
                                      buttons = c("excel", 'pdf', 'copy')))
  })

  selected_students <- reactive({
    rows_sel <- input$top_students_rows_selected
    if (is.null(rows_sel)) {
      return(NULL)
    } else {
      stab <- get_students() |> select(-Name)
      rvs$top_students |>
        slice(rows_sel) |>
        left_join(stab, by = "ID") |>
        select(ID, Name, Email, Mobile)
    }
  })

  output$top_student_record <- renderUI({
    row_sel <- input$selected_students_rows_selected

    if (length(row_sel)) {
      selected_students <- selected_students()
      student_sel  <- selected_students[row_sel, ]
      get_student_record(student_sel)
    }
  })


  output$contents <- renderUI(
    {
      req(input$paper_to_show, input$year_to_show, input$semester_to_show)
      dd <- dat()$results

      if ("Overall paper result" %in% colnames(dd) )
        dd <- dd %>% relocate(`Overall paper result`, .after = last_col()) %>%
          mutate(Delta = `Overall paper result` - Total)

      tagList(
         h4(sprintf("%s (%s, %s)", rvs$instance$paper, rvs$instance$semester, rvs$instance$year)),
         DT::datatable(dd,
                    options = list(lengthMenu = list(c(20, 50, 100, -1),
                                                     c(' 20', ' 50', '100', 'All'))))
	 )

    }
  )

  output$special_students <- DT::renderDataTable({
    req(rvs$instance)
    dd <- dat()
    dr <- dd$results
    en <- exam_name(dd)

    dr <- dr |>
      #across(where(is.numeric), filter(. < input$filter_specials))
      filter_at(vars(-ID, -Name), any_vars(. < input$filter_specials))

    if (!input$inc_wdn_s)
      dr <- dr |> filter(!if_any(is.numeric, ~ . %in% c(-1, -2)))
    if (!input$inc_abs_s)
      dr <- dr |> filter(!if_any(is.numeric, ~ . == -3))
    if (!input$inc_fc_s)
      dr <- dr |> filter(!if_any(is.numeric, ~ . %in% c(-4, -5)))

    rns <- result_names[result_names != "Total"]
    if (any(rns %in% colnames(dr)) ) {
      rn <- rns[which(rns %in% colnames(dr))]
      dr <- dr |>
        relocate(1, .after = last_col()) |>
        select(Result = all_of(rn), everything()) |>
        mutate(Delta = Result - Total) |>
        arrange(Result) |>
        select(ID, Name, everything())
    } else {
      if (!is.null(en))
        dr <- dr |>
          arrange(Total) |>
          mutate(Status = case_when(
            !!as.name(en) == -1 ~ "WD",
            !!as.name(en) == -2 ~ "WDE",
            !!as.name(en) == -3 ~ "ABS",
            !!as.name(en) == -4 ~ "FC",
            !!as.name(en) == -5 ~ "FT",
            Total         <  50 ~ "Failed",
            TRUE                ~ "")) |>
          select(Status, everything()
          )
    }

    DT::datatable(dr,
                  options = list(lengthMenu = list(c(20, 50, 100, -1),
                                                   c(' 20', ' 50', '100', 'All')))) |>
      formatStyle(columns = "Name",
                  valueColumns = "Scaled",
                  backgroundColor = styleEqual( -5:49, "wheat")) |>
      formatStyle(columns  = 4:ncol(dr),
                  backgroundColor = styleEqual( -5:0, "mistyrose"))
  })

  ##### Utility functions ####

  observeEvent(input$file_upload, {
    #message("Value of file upload changed")
    import_data(input$file_upload$datapath, filename = input$file_upload$name)
  })

  observeEvent(input$bulk_upload, {
    #message("Bulk importing data")
    showModal(modalDialog(em("Not implemented yet, sorry."), title = NULL, footer = modalButton("Dismiss"),
                          size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE))
    bulk_import("Data/")

  })

  compare_gt <- function() {
    require(tidyverse)
    mine <- as_tibble(read.csv("Results/MART308_Integrated_Digital_Marketing_final_results.csv"))
    ground_truth <- readxl::read_excel("Results/Data/MART308_ground_truth.xlsx",
                                       col_names = c("ID", "Name", "n1", "n2", "Truth"),
                                       skip = 1)
    ground_truth$ID <- as.numeric(ground_truth$ID)
    errors <- which(mine$Total != ground_truth$Truth)
    cmp <- left_join(mine, ground_truth, by = "ID") %>% select(ID, Name = Name.x, Total, Truth)
    dd <- load_data(paper, year, semester)
    df <- tidyr::spread(dd$results %>% select(-Grade), Assessment, Mark)
    raw <- as.matrix(ass_scores) %*% as.matrix(ass_weights)
    new <- ass_scores %>% mutate(T1 = round(`Test One`   * 0.03),
                                 T2 = round(`Test Two`   * 0.03),
                                 T3 = round(`Test Three` * 0.03),
                                 T4 = round(`Test Four`  * 0.03),
                                 T5 = round(`Test Five`  * 0.03),
                                 CLE =  round(`Computer Lab Exercises` * 0.15),
                                 MP = round(`Mimic Pro Project`        * 0.20),
                                 Exam = round(`Final Examination`      * 0.50),
                                 Total = round(T1 + T2 + T3 + T4 + T5 + CLE + MP + Exam))
    print(cmp[errors, ])
    print(df[errors,])
    print(ass_scores[errors,])
    print(raw[errors,], digits = 5)
    print(round(raw[errors,]))
    print(ceiling(raw[errors,]))
    errors <- which(new$Total != ground_truth$Truth)
    print(new[errors,])
  }

  #### Admin ####

  output$config_defaults_inputs <- renderUI({
    if(input$is_emergency)
      return(NULL)
    mdebug(sprintf("Setting default controls"),  "config_defaults_inputs", DEBUG)

    cf <- get_config()

    tagList(
      selectInput("default_dept", "Default department/programme",
                  choices = get_dept_list()$choices,
                  selected =  cf$default_dept,
                  width = "12em"),
      selectInput("default_paper", "Default paper",
                  choices = get_paper_list()$choices,
                  selected =  cf$default_paper,
                  width = "12em"),
      selectInput("default_year", "Default year",
                  choices = get_years(),
                  width = "6em"),
      selectInput(inputId  = "default_semester",
                  label    = "Default semester",
                  choices  = c("S1", "S2")),
      numericInput( "max_add",   "Maximum to add to raw grade:",
                    rvs$config$max_add),
      numericInput( "max_mul",   "Maximum to multiply raw grade by:",
                    rvs$config$max_mul),
      numericInput( "max_thr",   "Maximum threshold for applying scaling:",
                    rvs$config$max_thr),
      textInput(    "data_path", "Data path:",
                    getwd()),
      textInput(    "app_path", "App path:",
                    getwd()),

      selectInput("fake_user", "Act as",
                  choices  = get_users() |> arrange(username) |> pull(username),
                  selected = 'wiljo00p'
      )
    )
  })

  output$admin <- renderUI({
    if ( on_server() ) {
      if ( "admin" %in% get_permissions(get_session_user()))
        return(ui)
      else {
        name <- get_users() |>
          dplyr::filter(username == get_user()) |>
          select(user_f)
        tagList(h4(sprintf("Sorry %s, but you are not authorised to view this tab.", name)))
      }
    } else {
      return(ui)
    }
  })

  observeEvent(input$fake_user, {
    if (input$is_emergency) {
      emergency_shell()
      return(NULL)
    }
    mdebug("input$fake_user changed", "observeEvent", debug_load|DEBUG)

    fu    <- input$fake_user
    perms <- get_permissions(fu)
    if ( !("admin" %in% perms) ) {
      for (tab in admin_tabs) {
        hideTab(inputId = "UI", target = tab)
      }
    }

    showNotification(sprintf("Acting as %s", fu))
  })

  output$stop_fake <- renderUI({

    if (input$is_emergency)
      return(NULL)
    mdebug("Firing", "stop_fake", debug_load | debug_permissions)

    fu <- get_ss(input$fake_user)
    ru <- get_user()

    mdebug(sprintf("Real user is: '%s'", ru), "stop_fake",  debug_permissions)

    ud <- get_users() |> dplyr::filter(username == fu)
    mdebug(sprintf("user data is: '%s'", plist(ud)), "stop_fake",  debug_permissions)
    #print(ud)
    if ( length(ud$username) == 0 ) {
      ud <- get_users() |> dplyr::filter(username == ru)
      #print(ud)
      fu <- ru
    }

    showNotification(p(HTML(sprintf("Kia ora, %s %s (%s). You have <strong>%s</strong> permissions.",
                                    ud$user_f, ud$user_s, fu, ud$permissions))), duration = 10)
    if (fu != ru)
      actionButton("stop_being_fake", "Stop faking", class = "btn btn-warning")

  })

  observeEvent(input$stop_being_fake, {
    fu <- get_ss(input$fake_user)

    isolate(updateSelectInput(session, inputId = "fake_user", selected = "wiljo00p"))

    cf <- get_config()
    cf$fake_user <- 'none'
    set_tab("config", cf)

    for (tab in c(user_tabs, admin_tabs, kpi_tabs)) {
      showTab(inputId = "UI", target = tab)
    }

    mdebug(sprintf("Unfaking from %s. In the config table, fake_user is now '%s'", fu, cf$fake_user),
           "stop_being_fake", debug_permissions)
  })

  output$show_database_meta <- DT::renderDataTable({

    mdebug(sprintf("Fetching tables for DB metadata display"), "show_database_meta", DEBUG)
    itab <- get_instances() |> mutate(Instance = sprintf("%s_%s_%s", paper, year, semester))
    rtab <- get_results()   |> mutate(Instance = sprintf("%s_%s_%s", paper, year, semester))
    ptab <- get_reports()   |> mutate(Instance = sprintf("%s_%s_%s", paper, year, semester))

    mdebug(sprintf("Fetched tables for DB metadata display"), "show_database_meta", debug_RAM)

    op_lst <- ptab$Instance[!ptab$Instance %in% rtab$Instance]
    oi_lst <- itab$Instance[!itab$Instance %in% ptab$Instance]
    or_lst <- rtab$Instance[!rtab$Instance %in% itab$Instance] |> unique()

    #plist <- function(l) {
    #  paste(sort(l), collapse = ", ")
    #}

    dd <- tibble(Metadata = c("Assessment records in results table",
                              "Distinct students",
                              "Instances of papers",
                              "Paper, Year, Semester results",
                              "Examiner reports",
                              "Years",
                              sprintf("Reports with no results (%d):",  length(op_lst)),
                              #sprintf("Instances with no reports (%d)", length(oi_lst)),
                              sprintf("Results with no instance (%d)",  length(or_lst))),

                 Value     = c(rtab                                       |> nrow() |> fmt_n(),
                               rtab |> distinct(ID)                       |> nrow() |> fmt_n(),
                               get_instances()                            |> nrow() |> fmt_n(),
                               rtab |> distinct(Instance)                 |> nrow() |> fmt_n(),
                               ptab |> filter(report != "")               |> nrow(),
                               paste(rev(sort(unique(rtab$year))), collapse = ", " ),
                               plist(op_lst),
                               #plist(oi_lst),
                               plist(or_lst)
                 )
    )

    mdebug(sprintf("There are %s rows in the database summary report", nrow(dd)), "show_database_meta")
    #print(dd)
    DT::datatable(dd, rownames = FALSE, options = dt_bare)
  })

  ##### Assign users to instances #####
  output$make_user_access_list <- renderUI({
    if (input$is_emergency)
      return(NULL)

    req(input$access_year)

    users <- get_users()

    if (!input$include_old_staff) {
      user_list <- user_list |> filter(current == 1)
    } else {
      user_list <- users
    }

    paper_list <- get_instances() |>
      dplyr::filter(year     == input$access_year,
                    semester == input$access_sem) |>
      arrange(paper)

    mdebug(sprintf("There are %d papers in the database for this semester (%s %s): %s",
                   nrow(paper_list),
                   input$access_sem, input$access_year,
                   paste(paper_list |> pull(paper), collapse = ", ")), "make_user_access_list", debug_load)

    ui <- h4("No papers that meet these criteria found.")

    coords <- unique(c(user_list$username, paper_list$Coordinator))

    if (nrow(paper_list) > 0) {
      paper_inputs_list <- lapply(1:nrow(paper_list), function(i) {
        choices         <- c('', coords)
        names(choices)  <- c("Choose staff ...",
                             lapply(unlist(choices[-1]), get_users_name, users))
        column(2,
               selectizeInput(sprintf("access_%s", paper_list$instance[i]),
                           label    = paper_list$paper[i],
                           choices  = choices,
                           selected = paper_list$Coordinator[i],
                           options  = list(placeHolder = "Choose staff ...",
                                           width = 100)
               )
        )
      })

      ui <- do.call(tagList, paper_inputs_list)

      mdebug("Returning", "make_user_access_list")
      observeEvent(paper_inputs_list,{
        for (ii in 1:length(paper_inputs_list)) {
          local({
            i <- ii
            observeEvent(eventExpr = input[[sprintf("access_%s",
						    paper_list$instance[i])]],
                         handlerExpr = {
                           save_user_access(i, paper_list)
                         }, ignoreInit = TRUE)
          })
        }
      })
    }

    ui
  })

  ##### Results/Instance table #####
  ###### Edit cell #####
  # 1. enable the editable option
  # 2. make the underlying data globally visible and reactive
  # 3. make a DT proxy object
  # 4. observe cell_edit events, and withing that function, set_tab() the underlying data

  observe({
    rvs$instance_tab <<- get_instance_table()
  })
  output$show_database_recs <- DT::renderDataTable({
    instance_tab <- get_instance_table()
    DT::datatable(instance_tab, rownames = FALSE, editable = TRUE)
  })
  it_proxy <- dataTableProxy("show_database_recs")
  observe({
    DT::replaceData(it_proxy, rvs$instance_tab,
                    rownames = FALSE, resetPaging = FALSE)
  })
  observeEvent(input$show_database_recs_cell_edit, {
    rvs$instance_tab <<- editData(rvs$instance_tab, input$show_database_recs_cell_edit,
                                  rownames = FALSE)
    new_dat <- rvs$instance_tab |>
      select(paper, year, semester, instance, coordinator = Coordinator, title) |>
      arrange(desc(paper), desc(year), desc(semester))

    if (debug_edit) {
      mdebug("editing instance_tab cell", "instance_cell_edit", force = TRUE)
      old_dat <- get_instances() |>
        arrange(desc(paper), desc(year), desc(semester))
      print(old_dat)
      print(new_dat)
    } else {
      set_tab("instances", new_dat)
    }
  })

  ###### Delete row #####
  # https://shiny.posit.co/r/articles/build/action-buttons/
  output$data_instance_del <- renderUI({
    req(input$show_database_recs_rows_selected)
    actionButton("instance_del", "Delete selected rows", class = "btn btn-danger")
  })

  perform_results_delete <- function() {
    sc      <- input$show_database_recs_rows_selected
    sd      <- instance_tab[sc, ]

    old_dat <- get_results()
    new_dat <- old_dat |> filter(!(paper == sd$paper & year == sd$year & semester == sd$semester))
    del_dat <- old_dat |> filter(  paper == sd$paper & year == sd$year & semester == sd$semester)
    dif_dat <- nrow(old_dat) - nrow(new_dat)
    san_dat <- dif_dat == nrow(del_dat)

    san_mes <- ifelse(san_dat, "which makes sense",
                      sprintf("which is a BIG MISTAKE!!! It's a difference of %s but it should be %s",
                              fmt_n(dif_dat), fmt_n(nrow(del_dat))))

    rv_txt <- sprintf("This would reduce the results table from %s rows to %s rows, i.e. remove %s rows, %s",
              fmt_n(nrow(old_dat)), fmt_n(nrow(new_dat)), fmt_n(nrow(del_dat)), san_mes)

    return(list(new_dat = new_dat, message = rv_txt))
  }

  observeEvent(input$instance_del, {
    rv <- perform_results_delete()
    sc <- input$show_database_recs_rows_selected
    sd <- instance_tab[sc, ]

    showModal(modalDialog(
      paste(sprintf("Delete selected row(s)? (%s) %s", paste(sc, collapse = ","), rv$message),
            sprintf("\nThis is for paper: %s, year: %s, semester: %s", sd$paper, sd$year, sd$semester)),
      title = "Really delete?",
      footer = tagList(actionButton("confirmDelete", "Delete", class = "btn btn-danger"),
                       actionButton("confirmCancel", "Cancel", class = "btn btn-success"))))
      })

  observeEvent(input$confirmDelete, {
    rv <- perform_results_delete()
    shiny::showNotification(sprintf("Delete actioned: results table now has %s rows", nrow(rv$new_dat)))
    #print(rv$new_dat |> arrange(desc(year)))
    set_tab("results", rv$new_dat)
    removeModal()
  })

  observeEvent(input$confirmCancel, {
    shiny::showNotification("Delete cancelled")
    removeModal()
  })


  ##### User list #####
  # To make cell edits possible:
  # 1. enable the editable option
  # 2. make the underlying data globally visible and reactive
  # 3. make a DT proxy object
  # 4. observe cell_edit events, and within that function, set_tab() the underlying data

  reactive({
    slist                <- get_users()
    observe(rvs$users    <- slist)
  })

  output$display_staff_list <- renderDT({
    DT::datatable(rvs$users,
    selection = 'none',
    editable  = list(target = 'all'),
    options   = list(pageLength = 30, dom = 't' ))
  })

  sl_proxy <- dataTableProxy("display_staff_list")

  observeEvent(input$display_staff_list_cell_edit, {

    new_dat <- as_tibble(input$display_staff_list_cell_edit ) |>
      pivot_wider(values_from = value,id_cols = row,names_from = col) |>
      select(-c(1:2))

    names(new_dat) <- names(rvs$users)

    new_dat <- new_dat |>
      mutate(current = as.integer(current))

    rvs$users <- new_dat

    DT::replaceData(sl_proxy, new_dat, resetPaging = FALSE)

    if (check_fields(rvs$users) ) {
      if (debug_editing) {
        print(rvs$users, n = Inf)
      } else {
        set_tab("users", rvs$users |> arrange(permissions, department, current, user_f))
      }
    } else {
      showNotification("One or more values are too long. First names and surnames must be 20 characters or less,
                       usernames and permissions must be 8 characters or less, and current must be either 0 or 1",
                       duration = 20)
    }
    mdebug("Finished editing staff cell", "staff_cell_edit", debug_editing)
  })

  output$display_staff_list2 <- renderDT({
    if (input$is_emergency)
      return(NULL)

    mdebug("Firing", "display_staff_list", debug_editing)

    if (debug_editing & FALSE) {
      slist <- slist |>
        mutate(permissions = sprintf("
    <select id = '%s_p'>
      <option>%s </option>
      <option>user </option>
      <option>all  </option>
      <option>admin</option>
    </select>", username, permissions),
               current = sprintf(
                 "<input type='checkbox' id ='%s_c' value=%s %s/>",
                 username, current, if_else(current == 1, "checked", "")),
        )
    }

    DT::datatable(
      isolate(slist),
      editable = list(target = "all"),
      #selection = 'single',
      escape = FALSE
    )
  },
  server = FALSE,
  rownames = FALSE,
  options = list(pageLength = 30, dom = 't' ),
  callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('name', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());"))

  ###### Staff edit #####
  # https://yihui.shinyapps.io/DT-edit/
  ####### Edit cells #######


  ###### Delete row ######
  output$staff_list_del <- renderUI({
    req(input$staff_list_rows_selected)
    actionButton("staff_del", "Delete selected row", class = "btn btn-danger")
  })
  observeEvent(input$staff_del,
               showModal(
                 modalDialog("Are you sure?",
                             title = "Really delete?",
                             footer = tagList(actionButton("staff_del_OK",
                                                           "Delete", class = "btn btn-danger"),
                                              actionButton("staff_del_Cancel",
                                                           "Cancel", class = "btn btn-success"))))
  )
  observeEvent(input$staff_del_OK, {
    #rv <- perform_results_delete()
    showNotification(sprintf("Staff delete actioned: results table now has %s rows", fmt_n(0)))
    removeModal()
  })
  observeEvent(input$staff_del_Cancel, {
    shiny::showNotification("Staff Delete cancelled")
    removeModal()
  })

  ###### Add row ######
  output$staff_list_add <- renderUI({
    tagList(
      p(),
      actionButton("staff_add", "Add a row", class = "btn btn-success")
    )
  })
  observeEvent(input$staff_add, {
    showModal(
      modalDialog("Are you sure?",
                  title = "Really add a row?",
                  footer = tagList(actionButton("staff_add_OK",
                                                "Add", class = "btn btn-danger"),
                                   actionButton("staff_add_Cancel",
                                                "Cancel", class = "btn btn-success"))))
  })

  observeEvent(input$staff_add_OK, {
    new_row <- tribble(~user_f, ~user_s, ~username, ~current,
                       '',       '',      '',    0)

    if (FALSE) {
      print(rvs$users)
    } else {
      rvs$users <<- rvs$users |>
        #filter(complete.cases(username), username != '') |>
        add_row(new_row)

      smax <- function(x) {
        rv <- max(x, na.rm = TRUE)
        if (is.na(rv))
          rv <- 0
        rv
      }

      fname_OK <- ifelse(smax(str_length(rvs$users$user_f))      < 21, TRUE, FALSE)
      sname_OK <- ifelse(smax(str_length(rvs$users$user_s))      < 21, TRUE, FALSE)
      uname_OK <- ifelse(smax(str_length(rvs$users$username))    <  9, TRUE, FALSE)
      perms_OK <- ifelse(smax(str_length(rvs$users$permissions)) <  9, TRUE, FALSE)
      crrnt_OK <- ifelse(smax(str_length(rvs$users$current))     <  2, TRUE, FALSE)

      if (uname_OK & fname_OK & sname_OK & perms_OK & crrnt_OK ) {
        set_tab("users", rvs$users)
        showNotification(sprintf("Staff Add row actioned, table now has %s rows", fmt_n(nrow(rvs$users))))
      } else {
        showNotification("One or more values are too long. First names and surnames must be 20 characters or less,
                       usernames and permissions must be 8 characters or less, and current must be either 0 or 1",
                         duration = 20)
      }
    }
    removeModal()
  })

  observeEvent(input$staff_add_Cancel, {
    showNotification("Staff Add row cancelled")
    removeModal()
  })

  #### DB backup & restore ####
  ##### UI #####
  output$db_backups <- renderUI({
    mc <- "btn btn-success"

    tab <- as_tibble(dir(paste0(getwd(), "/backups/")))

    tagList(
      column(12,
             h3("backups", style = sec_style)
      ),
      column(4,
             verticalLayout (
               strong("Just here to take up space"),
               actionButton("db_backup", "Write backup file", class = mc),
               strong("Existing backup files"),
               DT::renderDataTable(DT::datatable(tab, options = dt_bare))
             )
      ),
      column(4,
             selectInput(inputId  = "db_choose_server_backup",
                         label    = "Select backup on server",
                         choices  = c("Choose backup ...", dir(paste0(getwd(), "/backups/"))),
                         selected =   "Choose backup ..."
             ),
             actionButton(  "db_restore_from_server",  "Restore from backup file", class = mc),
             h3(""),
             downloadButton("db_download",             "Download backup file",     class = mc),

      ),
      column(4,
             fileInput(inputId = "db_choose_local_backup",
                       label   = "Restore from backup elsewhere",
             )
      )
    )
  })

  ##### Backup #####
  ###### Make backup #####

  observeEvent(input$db_backup, {
    # FIXME: needs MariaDB root password on server (?)
    if (!dir.exists("backups")) {
      rv <- dir.create("backups")
      mdebug(sprintf("Attempt to create missing backup directory has result: '%s'", rv),
             "db_backup", debug_db)
    } else {
      mdebug(sprintf("Backup directory has %d files", length(dir("backups/"))),
             "db_backup", debug_db)
    }

    h <- get_db_server()
    s <- sprintf("mariadb-dump -h %s -u marketing -prhcb2233 --lock-tables --database marketing > backups/EB_datadump_%s.sql",
                 #mariadb-dump -h    -u marketing -prhcb2233 --lock-tables --database marketing > backups/dbs.sql
                 #                mariadb.marketing-ebreporter
                   h, format(Sys.time(), format = "%Y_%m_%d_%H_%M"))

    rv <- system(s)
    mdebug(sprintf("Running backup command '%s'", s), "db_backup", debug_db | TRUE)
    mdebug(sprintf("Return code: '%s'", rv),          "db_backup", debug_db | TRUE)
  })

  ###### Download backup #####

  get_dump <- reactive({
    read_lines(paste0(getwd(), "/backups/", input$db_choose_server_backup))
  })

  output$db_download <- downloadHandler(
    filename = function() {
      paste0(getwd(), "/backups/", input$db_choose_server_backup)
    },
    content = function(file) {
      mdebug(sprintf("Writing lines from '%s'", file), "db_download", TRUE)
      write_lines(get_dump(), file)
    }
  )

  ##### Restore from server #####
  observeEvent(input$db_restore_from_server, {
    fn <- input$db_choose_server_backup
    if (fn == "Choose backup ...") {
    } else {
      db_do_restore(paste0(getwd(), "/backups/", fn))
    }
  })

  ##### Restore from client #####
  observeEvent(input$db_choose_local_backup, {
    fn <- input$db_choose_local_backup$datapath
    db_do_restore(fn )
  })

  observeEvent(input$sql_submit, {
    mdebug("Button pressed", "sql_submit", TRUE)
  })

  output$sql_output <- renderUI({
    rv <- NULL
    if (str_detect(input$sql_input, ";$"))
      rv <- run_qry(input$sql_input)

    if (is.null(rv)) {
      "No query submitted."
    } else {
      if (DEBUG) {
        mdebug("Result is", "sql_output")
        print(rv)
      }
    }
    tagList(
      h4("Server response"),
      div(renderTable(rv)) |>
        tagAppendAttributes( style = "color:lime;background:black")
    )
  })

  output$shell_output <-  renderUI({
    if (input$shell_command != "") {
      rv <- try(robust.system(input$shell_command),silent = TRUE)
      #s <- capture.output(system(input$shell_command, intern = TRUE))
      tagList(
        h4("Shell response"),
          pre(paste(c(rv$stdout, rv$stderr), collapse = "\n")) |>
          tagAppendAttributes( style = "color:lime;background:black"),
      )
    } else {
      h4("No response from shell")
    }
  })


  #### Debugging info (Admin sidebar) ####
  output$debugging_info <-  renderUI({
    if (input$debug) {
      s <- capture.output(system("g++ --version", intern = TRUE))[1]
      tagList(
        h4("Environment"),
        p(s))
    } else {
      p("")
    }
  })


  ### Postgrad table ###
  output$postgrad_table <- renderDT({
    th_style <- "'font-weight:normal;'"

    pg_tab <<- postgrad_sheet()
    paper_cols <- grep("MART", names(pg_tab)) - 1

    pg_tab <<- pg_tab |>
      dplyr::filter(Papers %in% as.numeric(input$pg_num_papers)) |>
      rename_with(~str_extract(.x, "[0-9]+"), starts_with("MART")) |>
      select(-ID) |>
      rename(Degree = Programme)

    get_mean <- function(x) {
      if (length(x[complete.cases(x)])  == 0)
        return("")
      if (is.numeric(x)) {
        if (max(x, na.rm = TRUE) > 10)
          rv <- as.character(round(mean(x, na.rm = TRUE)))
        else
          rv <- ""
      } else {
        if (pg_tab$Surname[1] == x[1])
          rv <- "Means"
        else
          rv <- ""
      }
      sprintf("<span style=%s>%s</span>", th_style, rv)
    }

    th <- htmltools::withTags(table(
      tableHeader(pg_tab),
      tableHeader(pg_tab |> summarise(across(where(is.numeric), get_mean)) |>
                    unlist(),
                  escape = FALSE)
    ))

    pg_dt  <- datatable(
      pg_tab,
      editable     = "cell",
      extensions   = c("Buttons"),
      rownames     = FALSE,
      options = list(
        dom          = 'tB',
        buttons      = c('excel', 'copy'),
        scrollX      = TRUE,
        scrollY      = TRUE,
        pageLength   = -1,
        autowidth    = TRUE,
        fixedColumns = list(leftColumns = 1),
        initComplete = JS(
          "function(settings, json) {",
          "$('th:not(:nth-child(-n+4))').css({'textAlign': 'center'});",
          "$('td:not(:nth-child(-n+4))').css({'textAlign': 'center'});",
          "}")
      )
    )

    pg_dt |> formatStyle("Mean", backgroundColor = styleEqual(max(pg_tab$Mean), "beige"))
  })

  observeEvent(input$postgrad_table_cell_edit, {
    ndat <- input$postgrad_table_cell_edit
    pg_tab <<- editData(pg_tab, ndat, 'postgrad_table', rownames = FALSE)

    ndat$col   <- 4
    ndat$value <- pg_tab %>%
      slice(ndat$row) %>%
      select(starts_with("4")) %>%
      t %>%
      as.numeric %>%
      mean(na.rm = TRUE) %>%
      round
    pg_tab <<- editData(pg_tab, ndat, 'postgrad_table', rownames = FALSE)
  })

  #### Packages tab ####
  output$packages <- renderDT ({
    DT::datatable(as_tibble(installed.packages()) |> select(Package, Version))
  })

  #### Log tab ####
  output$log <- renderDT({
    if (is.null(rvs$log)) {
      fn <- if_else(on_server(), "/opt/app/debug.log", "debug.log")
      rvs$log <- as_tibble(read_lines(fn))
    }

    tab <- rvs$log |>
      transmute(caller = str_extract(value, "\\(([^\\)]+)\\)", group = 1),
             when   = lubridate::as_datetime(str_extract(value, "\\[([^\\]]+)\\]", group = 1)),
             mess   = str_extract(value, "\\] (.*)", group = 1),) |>
      filter(complete.cases(when)) |>
      arrange(desc(when))


    DT::datatable(tab,
                  options = list(pageLength = 30, lengthMenu = c("30", "60", "90"))) |>
      formatDate(2, method = 'toUTCString')
  })

  observeEvent(input$log_reload, {
    mdebug("Reloading log", "log_reload", force = TRUE)
    fn <- if_else(on_server(), "/opt/app/debug.log", "debug.log")
    rvs$log <- as_tibble(read_lines(fn))
  })

  #### Admin: System parameters  ####
  observeEvent( input$max_add, {
    update_config("max_add", input$max_add)
  })

  observeEvent( input$max_mul, {
    update_config("max_mul", input$max_mul)
  })

  observeEvent( input$max_thr, {
    update_config( "max_thr", input$max_thr )
  })

  observeEvent( input$default_semester, {
    mdebug(sprintf("(input$default_semester): Setting default semester: %s ",
                   input$default_semester), "observeEvent")
    update_config( "default_semester", input$default_semester)
  })

  observeEvent( input$default_dept, {
    update_config( "default_dept", input$default_dept)
  })

  observeEvent( input$default_paper, {
    mdebug(sprintf("(input$default_paper): Setting default paper: %s",
                   input$default_paper), "observeEvent", TRUE)
    update_config("default_paper", input$default_paper)
    if (input$is_emergency)
      emergency_shell
  })

  # End of server definition
})
