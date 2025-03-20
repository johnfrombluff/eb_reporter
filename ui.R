#### Define UI ####
source("utility_functions.R", local = TRUE)
source("database_functions.R", local = TRUE)

library(gt)
#library(gdtools)
# shinyFiles::shinyDirButton(
#id    = "eng_dir",
#label = "Upload data",
#title = "Please select a folder")

ph <- '300px'

fluidPage(
  #addGFontHtmlDependency(family = c("Open Sans")),
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    th {
      test-align: right;
    }
                    ")
    ),
    tags$script('
    // From https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
    // https://stackoverflow.com/a/37060206
	var dimension = [0, 0];
	$(document).on("shiny:connected", function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
	$(window).resize(function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
  ')
  ),
  #uiOutput("current_paper"),
  tabsetPanel(type = "tabs",
              id = "UI",
              tabPanel("Overview",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           uiOutput("choose_dept"),
                           uiOutput("choose_paper"),
                           uiOutput("choose_year"),
                           uiOutput("choose_semester"),
                           checkboxInput("plot_negs",
                                         "Include WDN, WDE, FT, FC & ABS",
                                         value = TRUE),
                           uiOutput("scaling_controls"),
                         ),
                         mainPanel(
                           width = 10,
                           fluidRow(
                             column(12,
                                    br(),
                                    h3("Grade distributions",
                                       style = sec_style)),
                             column(6, shinycssloaders::withSpinner(uiOutput("TotalPlot"))),
                             column(6, shinycssloaders::withSpinner(uiOutput("InternalPlot"))),
                             column(6, shinycssloaders::withSpinner(uiOutput("ScaledPlot"))),
                             column(6, shinycssloaders::withSpinner(uiOutput("ExamPlot")))
                           ), style = "height:100%"
                         )
                       ), style = "height:100%"
              ),
              tabPanel("Report",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           uiOutput("course_stats"),
                           uiOutput("assessments"),
                           #uiOutput("group_sum"),
                           uiOutput("report_scaling")
                         ),
                         mainPanel(width = 10,
                                   verticalLayout(
                                     uiOutput("render_report")
                                   )
                         )
                       )
              ),
              tabPanel("Internal assessment",
                         fluidRow(
                           uiOutput("InternalPlots"),
                           uiOutput("populate_graphs")
                         )
              ),
              tabPanel("Summary statistics",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           fluidRow(
                             h4("Table options", style = sec_style),
                             checkboxInput("inc_wdn", "Include Widthdrawn",                TRUE),
                             checkboxInput("inc_abs", "Include Absent",                    TRUE),
                             checkboxInput("inc_fct", "Include Failed compulsory & terms", TRUE),
                             checkboxInput("inc_zer", "Include Zero scores",               TRUE)),
                           helpText("Numeric codes for results are as follows:", br(), "WD = -1; WDE = -2; ABS = -3; FC = -4"),
                           fluidRow(
                             h4("Plot options", style = sec_style),
                             checkboxInput("inc_ug",
                                           "Include Undergrad papers", TRUE),
                             checkboxInput("inc_pg",
                                           "Include Postgrad papers",  TRUE),
                             checkboxInput("inc_s1",
                                           "Include S1",               TRUE),
                             checkboxInput("inc_s2",
                                           "Include S2",               TRUE)
                           ),
                           fluidRow(
                             h4("Equity analysis options", style = sec_style),
                             checkboxInput("eq_exam_only",
                                           "Include exam results only",    FALSE),
                             checkboxInput("eq_final_only",
                                           "Include overall results only", TRUE),
                             sliderInput("eq_flags",
                                         "Highlight pass rate gaps more than",
                                         min = 0,
                                         max = 20,
                                         value = 10,
                                         step = 1),
                           ),
                         ),
                         mainPanel(
                           width = 10,
                           tabsetPanel(id = "sum_stats", type = "tabs",
                                       tabPanel("Overall stats",      tableOutput("stats")),
                                       tabPanel("Equity stats",
                                                fluidRow(
                                                  column(6,
                                                         h3("Median mark and pass rate gaps", style = sec_style),
                                                         p(HTML("<strong>Note</strong>: n = number of students")),
                                                         shinycssloaders::withSpinner(gt_output("equity_others"))),
                                                  column(6,
                                                         h3("Gaps in median mark and pass rate gaps over time (all papers)", style = sec_style),
                                                         shinycssloaders::withSpinner(gt_output("equity_time")))
                                                  ),
                                                  fluidRow(
                                                    column(12,
                                                         p(HTML("<strong>Note</strong>: n = number of items of assessment")),
                                                         shinycssloaders::withSpinner(DT::dataTableOutput("equity_deets")))
                                                )
                                       )
                           )
                         )
                       )
              ),
              tabPanel("Divisional KPIs",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                             h4("Data display options", style = sec_style),
                            uiOutput("choose_kpi_papers"),
                           checkboxInput("kpis_inc_core",
                                         "Include BCom core papers",
                                         value = TRUE),
                           checkboxInput("kpis_inc_noncore",
                                         "Include non-BCom core papers",
                                         value = FALSE),
                           checkboxInput("kpis_inc_BEntr",
                                         "Include BEntr papers",
                                         value = TRUE),
                           checkboxInput("kpis_inc_UG",
                                         "Include undergrad papers",
                                         value = TRUE),
                           checkboxInput("kpis_inc_PG",
                                         "Include postgrad papers",
                                         value = TRUE),
                             selectInput("kpis_semesters",
                                         "Include semesters",
                                         choices = c("All","S1", "S2", "FY", "SS"),
                                         selected = "All",
                                         multiple = TRUE),
                           uiOutput("get_kpis_years"),
                           selectInput("kpis_inc_maj",
                                       "Include majors",
                                       choices = c("All"  ,"ACCT", "ECON", "FINC",
                                                   "HURM", "IBUS", "INTB", "INFO",
                                                   "MANT", "MART", "PHPE", "TOUR"),
                                       selected = "All",
                                       multiple = TRUE),
                           selectInput("kpis_inc_prg",
                                       "Include programmes",
                                       choices = c("All","BCom", "BEntr", "BACom", "BComSc",
                                                   "DipCom", "DipGrad", "MMart",
                                                   "Other undergrad",  "Other postgrad"),
                                       selected = "All",
                                       multiple = TRUE),
                           sliderInput("kpis_min", "Filter marks below",
                                       min = 0, max = 50, step = 1, value = 0),
                           sliderInput("kpis_prog_min", "Filter programmes with % below",
                                       min = 0, max = 10, step = 1, value = 4),
                           sliderInput("kpis_maj_min", "Filter majors with % below",
                                       min = 0, max = 10, step = 1, value = 2)
                         ),
                         mainPanel(
                           width = 10,
                           tabsetPanel(id = "kpis", type = "tabs",
                                       tabPanel("Enrolment analysis",
                                                fluidRow(
                                                  column(3, shinycssloaders::withSpinner(gt_output("kpis_enrolment_status"))),
                                                  column(3, shinycssloaders::withSpinner(gt_output("kpis_enrolment_prg"))),
                                                  column(3, shinycssloaders::withSpinner(gt_output("kpis_enrolment_maj"))),
                                                  column(3, shinycssloaders::withSpinner(gt_output("kpis_enrolment_mmaj")))
                                                ),
                                                uiOutput("kpis_enrolment")

                                       ),
                                       tabPanel("Grade analysis",
                                                fluidRow(
                                                  column(6, shinycssloaders::withSpinner(plotOutput("kpis_grades_bps"))),
                                                  column(6, shinycssloaders::withSpinner(plotOutput("kpis_grades_bar")))
                                                ),
                                                fluidRow(
                                                  column(6, shinycssloaders::withSpinner(plotOutput("kpis_grades_hst"))),
                                                  column(6, shinycssloaders::withSpinner(plotOutput("kpis_grades_fct")))
                                                ),
                                                uiOutput("kpis_grades")

                                       ),
                                       tabPanel("Equity analysis",
                                                fixedRow(
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_gpl", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_mpl", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_ppl", height = ph)))
                                                ),
                                                fluidRow(
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_gml", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_mml", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_pml", height = ph)))
                                                ),
                                                fluidRow(
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_gmb", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_mmb", height = ph))),
                                                  column(4, shinycssloaders::withSpinner(plotOutput("kpis_gaps_pmb", height = ph)))
                                                ),
                                                uiOutput("kpis_equity")
                                       )
                           ),
                         ),
                       ),
              ),
              tabPanel("Report Editor",
                       mainPanel(width = 10,
                                 verticalLayout(
                                   uiOutput("build_report")
                                 )
                       )
              ),
              tabPanel("Data",
                       uiOutput("contents")
              ),
              tabPanel("Notable results",
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      h3("Table options", style = sec_style),
                                      sliderInput("filter_specials",
                                                  "Show grades less than:",
                                                  min = 0, value = 50, max = 100, step = 5),
                                      checkboxInput("inc_wdn_s",
                                                    "Include Widthdrawn",  TRUE),
                                      checkboxInput("inc_abs_s",
                                                    "Include Absent",      TRUE),
                                      checkboxInput("inc_fc_s",
                                                    "Include Failed compulsory & terms", TRUE),
                                      h3("Numeric codes", style = sec_style),
                                      HTML("
                                      <table>
                                      <tr><td> WD  </td><td> = </td><td> -1 </td></tr>
                                      <tr><td> WDE </td><td> = </td><td> -2 </td></tr>
                                      <tr><td> ABS </td><td> = </td><td> -3 </td></tr>
                                      <tr><td> FC  </td><td> = </td><td> -4 </td></tr>
                                      <tr><td> FT  </td><td> = </td><td> -5 </td></tr>
                                      </table>
                                           ")
                         ),
                         mainPanel(
                           width = 10,
                           fluidRow(
                             column(width = 12,
                                    h3("Students who have scored 0, WD, FT, FC etc.", style = sec_style),
                                    DT::dataTableOutput("special_students")
                             )
                           )
                         )
                       )
              ),
              tabPanel("All students",
                       sidebarLayout(
                         sidebarPanel(width = 2,
                                      radioButtons("filter_all_students",
                                                   "Apply filter?",
                                                   c("Yes", "No")),
                                      sliderInput("students_grade",
                                                  "Grade at least",
                                                  min = 70, max = 100, value = 70, step = 5),
                                      sliderInput("students_papers",
                                                  "For at least ? papers",
                                                  min = 1, max = 8, value = 2, step = 1),
                                      checkboxGroupInput("filter_levels", "Include levels",
                                                         c(100, 200, 300, 400), c(100, 200, 300),
                                                         inline = TRUE),
                                      uiOutput("get_year_list_control"),
                                      checkboxInput("filter_bcom",    "Only BCom students"),
                                      checkboxInput("filter_mart",    "Only Marketing majors"),
                                      checkboxInput("only_final_sr",  "Only final results (student record)")
                         ),
                         mainPanel(width = 10,
                                   fluidRow(
                                     column(6,
                                            h4("Top students"),
                                            h5("Average final result"),
                                            DT::dataTableOutput("top_students")),
                                     column(6,
                                            h4("Selected students"),
                                            DT::dataTableOutput("selected_students"),
                                            uiOutput("top_student_record")
                                     )
                                   )
                         )
                       )
              ),
              tabPanel("Engagement",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           radioButtons(
                             "eng_include",
                             "Show only these groups",
                             choices = c("Everyone",
                                         "Māori",
                                         "Pacific",
                                         "Māori or Pacific"),
                             selected = "Everyone"
                           ),
                           strong("Other data options"),
                           checkboxInput(
                             "eng_nz",
                             "Exclude zero engagement",
                             FALSE),
                           checkboxInput(
                             "eng_z",
                             "Exclude non-zero engagement",
                             FALSE),
                           checkboxInput(
                             "eng_sort",
                             "Sort ascending",
                             FALSE),
                           strong("Followup options"),
                           checkboxInput(
                             "eng_gpa",
                             "Only include final mark",
                             FALSE),
                           fileInput("file_upload_BB",
                                     "Import data",
                                     multiple = TRUE,
                                     accept = c("xls", "xlsx")),
                           uiOutput("eng_data_range")
                         ),
                         mainPanel(
                           width = 10,
                           column(7,
                                  uiOutput("anal_engagement"),
                                  h4("Data", style = sec_style),
                                  DT::dataTableOutput("engagement_tab"),
                           ),

                           column(5,
                              h4("For following up", style = sec_style),
                              DT::dataTableOutput("akoka_followup"),
                              uiOutput("akoka_student_record")

                           )
                         )
                       )

              ),
              tabPanel("Kaiāwhina",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           radioButtons(
                             "kai_include",
                             "Include programmes",
                             choices = c("All",
                                         "BCom only",
                                         "BCom and BEntr"
                             ),
                             selected = "BCom only"
                           ),
                           checkboxInput("kai_only_code",
                                           "Only show these papers",
                                           value = TRUE
                           ),
                           checkboxInput("kai_conditional",
                                         "Only show conditional enrolments",
                                         value = FALSE
                           ),
                           checkboxGroupInput("kai_levels",
                                              "Choose paper levels",
                                              choices = c("100", "200", "300", "400", "500")
                           ),
                         ),
                         mainPanel(
                           width = 10,
                           tabsetPanel(
                             tabPanel("BSNS", width = 12,
                                      h3("BSNS", style = sec_style),
                                      uiOutput("kaiawhina_bsns")
                             ),
                             tabPanel("ACCT", width = 12,
                                      h3("ACCT", style = sec_style),
                                      uiOutput("kaiawhina_acct")
                             ),
                             tabPanel("COMP", width = 12,
                                      h3("COMP", style = sec_style),
                                      uiOutput("kaiawhina_comp")
                             ),
                             tabPanel("FINC", width = 12,
                                      h3("FINC", style = sec_style),
                                      uiOutput("kaiawhina_finc")
                             ),
                             tabPanel("ECON", width = 12,
                                      h3("ECON", style = sec_style),
                                      uiOutput("kaiawhina_econ")
                             ),
                             tabPanel("INFO", width = 12,
                                      h3("INFO", style = sec_style),
                                      uiOutput("kaiawhina_info")
                             ),
                             tabPanel("MART", width = 12,
                                      h3("MART", style = sec_style),
                                      uiOutput("kaiawhina_mart")
                             ),
                             tabPanel("MANT", width = 12,
                                      h3("MANT", style = sec_style),
                                      uiOutput("kaiawhina_mant")
                             ),
                             tabPanel("TOUR",  width = 12,
                                      h3("TOUR", style = sec_style),
                                      uiOutput("kaiawhina_tour")
                             )
                           )
                         )
                       ),

              ),
              tabPanel("Postgraduate sheet",
                       sidebarLayout(
                         sidebarPanel(width = 1,
                                      checkboxGroupInput(inputId  = "pg_num_papers",
                                                         label    = "Papers",
                                                         choices  = c(1, 2, 3, 4, 5, 6, 7),
                                                         selected = c(      3, 4, 5, 6, 7))
                         ),
                         mainPanel(width = 11,
                                   column(12,
                                          DT::DTOutput("postgrad_table")
                                   )
                         )
                       )
              ),
              tabPanel("Help",
                       mainPanel(
                         fluidPage(h3("Help for this application"),
                                   helpText("This is a web application, which you can refer to as EB Reporter, is built using ",
                                            a(href = "https://www.r-project.org/", "R", target = "_blank"),
                                            "(a programming language for statistical computing and graphics) and ",
                                            a(href = "https://shiny.rstudio.com/", "Shiny", target = "_blank"),
                                            " (an ",
                                            strong("R"),
                                            " dialect for building web applications).
                                   It gets data from Business Objects, so if you want to add some data, check that
                                     it's available from there. My hope is that the UI is so well designed that you
                                     won't need any help acheiving the tasks you want to do, however there may be some
                                     things you wish it could do but can't see how; in that case, email me at "),
                                   a(href = "mailto:john.williams@otago.ac.nz?subject=Your Marks Web App",
                                     "john.williams@otago.ac.nz"),
                                   h3("Importing data from Blackboard (for admins only"),
                                   helpText("(I said ignore it, but I'm leaving it here just in case things change.) If your data is managed by eVision and available on Business Objects:",
                                            withTags(
                                              ul(
                                                li("Download the EXRPT11 file from",
                                                   a("Business Objects",
                                                     href = "https://its-boxi.otago.ac.nz/OUBizObjWebApp/logonform.jsp",
                                                     target = "_blank"),
                                                   "in Excel format (Choose",
                                                   strong("Study, Results & Academic Records"),
                                                   "and the report you want is third from the top)"),
                                                li("Choose \"Summary\", not \"Tabular\". (It says that Summary must run in PDF, but that's a lie ;-)"),
                                                li("Choose data type SAT", strong("and"), "SMR"),
                                                li("Choose paper and teaching period"),
                                                li("Leave everything else at its default value (but choose Excel at the bottom)")))),
                                   hr(),
                                   HTML("
                                   <div style='color:#737373;'>
                                   <h3>Bugs</h3>
                                   <ul>
                                   <li>No actual bugs that I'm aware of. If you spot one, please let me know!</<li>
                                   </ul>
                                   <h3>Future plans</h3>
                                   <ul>
                                   <li>
                                    I have been trying, unsucessfully, for years to convince the powers-that-be to allow me to access Business Objects/eVision data programatically, i.e. by code in the background of this app. This has been mostly achieved but there is still some manual work involved (someone has to set up an automated Business Objects report each semester). There is no technical reason why it's not possible to fully automate this process; it's entirely petty beauracraticpolitics. So if you have any influence, please help me make your life easier!
                                   </<li>
                                   </ul>
                                   <h3>Version history</h3>
                                   <ul>
                                    <li>v.13 (February, 2025) Added engagement stats (data from Blackboard and Echo360 for pastoral care)
                                    <li>v.12 (January, 2025) Added Divisional KPIs</li>
                                    <li>v.11 (Sep 11<sup>th</sup>, 2024): Added pass rates to stats tab. Moved to OU-supported site and integration with SSO. Implemented permissions, such that most users can only see the papers they are reporting on. Added equity statistics.</li>
                                    <li>v.10 (Nov 11<sup>th</sup>, 2021): Improved report editing and display; fully automated data import for papers with data in Business Objects, improved support for papers adminsistered by Results 1 (eVision).</li>
                                    <li>v.09 (Nov 16<sup>th</sup>, 2020): Added Report Page tab; added support for papers with only 1 assessment</li>
                                    <li>v.08 (Nov 15<sup>th</sup>, 2020): Added support for papers with no final exam</li>
                                    <li>v.07 (Nov 14<sup>th</sup>, 2020): Added option to exclude WDN etc. on Overview tab</li>
                                    <li>v.06 (Nov 13<sup>th</sup>, 2020): Added capability to process Overall paper result from SMR data (to get FT, FC etc)</li>
                                    <li>v.05 (Nov 12<sup>th</sup>, 2020): Added Notable results tab</li>
                                    <li>v.04 (Nov 11<sup>th</sup>, 2020): Removed save button in admin interface (changes are autosaved)</li>
                                    <li>v.03 (Nov 10<sup>th</sup>, 2020): Slight speedup by detecting whether data is already loaded before attempting load</li>
                                    <li>v.02 (Nov  9<sup>th</sup>, 2020): Bug fixes</li>
                                    <li>v.01 (Nov  8<sup>th</sup>, 2020): First public beta, although development started many months prior.</<li>
                                   </ul>
                                   </div>
                                   ")                                )
                       ),
                       uiOutput("stop_fake")
              ),
              tabPanel("Admin",
                       sidebarLayout(
                         sidebarPanel(
                           width = 2,
                           h4("System parameters"),
                           uiOutput("config_defaults_inputs"),
                           checkboxInput("debug", "Debug mode", FALSE),
                           uiOutput("debugging_info"),
                           checkboxInput("is_emergency", "Emergency!", value = TRUE),
                         ),
                         mainPanel(width = 10,
                                   tabsetPanel(id = "AdminTables", type = "tabs",
                                               tabPanel("Import data & Assign Coordinators",
                                                        verticalLayout(   #FIXME: get list of semesters from data
                                                          h4("Manage user access for papers"),
                                                          fluidRow(
                                                            column(2,
                                                                   uiOutput("coord_years")),
                                                            column(2,
                                                                   selectInput("access_sem",  "Semester",
                                                                               choices = c("S1", "S2", "SS", "FY"),
                                                                               selected = '',
                                                                               width = "6em")),
                                                            column(2,
                                                                     fileInput("file_upload",
                                                                               "Import spreadsheet"),
                                                            ),
                                                            column(1,
                                                                   checkboxInput("dry_run",
                                                                                 "Test run only",
                                                                                 FALSE),
                                                                   checkboxInput("debug_import",
                                                                                 "Write import debugging to log",
                                                                                 FALSE)
                                                            ),
                                                            column(2, textInput("r1_rgx", "Regex for R1 files",
                                                                                value = "MART.*4[0-9]{2}"))
                                                          ),
                                                          hr(),
                                                          h4("Coordinators (report authors)"),
                                                          checkboxInput("include_old_staff",
                                                                        "Include only current staff",
                                                                        value = TRUE),
                                                          uiOutput("make_user_access_list")
                                                        )
                                               ),
                                               tabPanel("Staff maintenance",
                                                        h3("Click in cell to edit; Ctrl-Entr to save"),
                                                        fluidRow(
                                                          column(2, uiOutput("staff_list_add")),
                                                          column(2),
                                                          column(2, uiOutput("staff_list_save")),
                                                          column(2),
                                                          column(2, uiOutput("staff_list_del"))),
                                                        DT::dataTableOutput("display_staff_list")
                                               ),
                                               tabPanel("Database maintenance",
                                                        h4("Instances"),
                                                        DT::dataTableOutput("show_database_meta"),
                                                        h4("Records"),
                                                        DT::dataTableOutput("show_database_recs"),
                                                        uiOutput("data_instance_del"),
                                                        fluidRow(
                                                               uiOutput("db_backups")
                                                        ),
                                               ),
                                               tabPanel("Packages",
                                                        DT::DTOutput("packages")),
                                               tabPanel("Log",
                                                        actionButton("log_reload", "Reload log file"),
                                                        DT::DTOutput("log"))
                                   )
                         )
                       )
              ),
              tabPanel("Database terminal",
                       fluidRow(
                         column(6, textAreaInput("sql_input",     "SQL Input",     "", width = "100%", height = "200px")),
                         column(6, textAreaInput("shell_command", "Shell command", "", width = "100%", height = "200px")),
                       ),
                       fluidRow(
                         column(6, uiOutput("sql_output"),   height = "600px"),
                         column(6, uiOutput("shell_output"), height = "600px")
                       ),
                       checkboxInput("is_emergency",
                                     "Emergency!",
                                     value = get_emergency_status())

              ),
  ), style = "height:100%"
)
