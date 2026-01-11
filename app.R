library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinyvalidate)
library(shinydashboard)
# rm(list = ls())

# setwd("/Users/jeremydumalig/Desktop/Productivity/HOPES")

Sys.setenv(TZ='CST6CDT')

source("js.R")
source("Database.R")
source("Dashboard.R")
source("setSliderColor.R")

ui <- dashboardPage(
  dashboardHeader(# title=HTML("<b>4 Foundations RSO (University of Chicago)</b>"),
    title = "4 Foundations RSO (University of Chicago)",
    titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem(HTML("<b>Coach Session Log</b>"), tabName="coach_tab"),
      menuItem(HTML("<b>Admin Dashboard</b>"), tabName="admin_tab"),
      menuItem(HTML("<b>Settings</b>"), tabName="settings_tab"))
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(
        tabName="admin_tab",
        hidden(
          div(
            class = "page",
            id = "admin_p0"
          ),
          div(
            class = "page",
            id = "admin_p1",
            h2("Admin Dashboard"),
            br(),
            uiOutput("dashboard_selections"),
            br(),
            gt_output("sessions"),
            br(),
            gt_output("cld_log"),
            br(),
            br(),
            plotOutput("cld_sessions",
                       height="600px"),
            br(),
            gt_output("cld_table"),
            br(),
            br(),
            fluidPage(
              fluidRow(
                column(6, plotOutput("khan_progress")),
                column(6, plotOutput("mms_progress"))
              )
            ),
            br(),
            br(),
            fluidRow(
              column(1),
              column(5, gt_output("coach_hours")),
              column(5, gt_output("student_hours")),
              column(1)
            ),
            br(),
            downloadButton("report", "Generate Report")
          ),
          div(
            class = "page",
            id = "admin_p2",
            h2("You do not have access to this page."),
          )
        )
      ),
      tabItem(
        tabName="settings_tab",
        hidden(
          div(
            class = "page",
            id = "settings_p0"
          ),
          div(
            class = "page",
            id = "settings_p1",
            h2("Settings"),
            p("View active coaches, students, and subjects below."),
            br(),
            uiOutput("all_settings")
          ),
          div(
            class = "page",
            id = "settings_p2",
            h2("You do not have access to this page.")
          )
        )
      ),
      tabItem(
        tabName="coach_tab",
        hidden(
          div(
            class = "page",
            id = "page0",
            h2("Make Selections"),
            br(),
            uiOutput("selections"),
            fluidRow(
              column(1, actionButton("make_selections_back", "Back")),
              column(1, actionButton("review_submission", "Next")),
              column(10)
            )
          ),
          div(
            class = "page",
            id = "pagealready",
            h2("Existing Sessions"),
            br(),
            uiOutput("continue_selections"),
            actionButton("existing_back", "Back"),
            actionButton("continue", "Continue")
          ),
          div(
            class = "page",
            id = "page2",
            h2(HTML("<b>Your Selections</b>")),
            br(),
            br(),
            uiOutput("your_selections"),
            br(),
            fluidRow(
              column(6, p(class="text20", "My selections above are correct. If not,")),
              column(1, actionButton("check_in_back", "Go Back")),
              column(5)
            ),
            br(),
            fluidRow(
              column(6, p(class="text20", "The student is ready to start the session.")),
              column(1, actionButton("check_in", "Check-In")),
              column(5)
            )
          ),
          div(
            class = "page",
            id = "page3",
            uiOutput("your_selections2"),
            # br(),
            div( # style = "margin-top: -25px;",
              p(HTML("<b>Coach...</b>"), class = "text20",
                style = "position: relative; bottom: -20px;"),
              awesomeCheckboxGroup(
                inputId = "checked",
                label = "", 
                choices = c("started with a brief chat.",
                            "skipped the brief chat."),
                width='100%'
              )
            ),
            # br(),
            conditionalPanel(
              condition = 'input.select_subject == "Science" || input.select_subject == "Gifted Education" || input.select_subject == "Test Prep" || input.select_subject == "Analytic and Problem-Solving Skills"',
              p(HTML("<b>Comments?</b>"), class = "text20",
                style = "position: relative; bottom: -20px;"),
              textInput(inputId="science_comments", 
                        label="", 
                        width='100%', 
                        placeholder = "Open Script..."),
              br(),
              div(align='center',
                  fluidRow(
                    column(5),
                    column(1, actionButton("back_science", "Back")),
                    column(1, actionButton("submit_science", "Submit")),
                    column(5)
                  )
              )
            ),
            conditionalPanel(
              condition = 'input.select_subject == "Math"',
              uiOutput("previous_session_math"),
              br(),
              div(align='center',
                  fluidRow(
                    column(5),
                    column(1, actionButton("back_math", "Back")),
                    column(1, actionButton("check_next_math", "Next")),
                    column(5)
                  )
              )
            ),
            conditionalPanel(
              condition = 'input.select_subject == "Language"',
              uiOutput("previous_session_language"),
              br(),
              uiOutput("brain_book_previous"),
              div(align='center',
                  style = "position: relative; bottom: 300px;",
                  fluidRow(
                    column(5),
                    column(1, actionButton("back_language", "Back")),
                    column(1, actionButton("check_next_language", "Next")),
                    column(5)
                  )
              )
            )
          ),
          div(
            class = "page",
            id = "page4",
            br(),
            uiOutput("today_summary"),
            br(),
            uiOutput("input_khan"),
            br(),
            conditionalPanel(
              condition = 'input.select_subject == "Math"',
              uiOutput("ui_math_core"),
              br(),
              uiOutput("pre_checkout"),
              fluidRow(
                column(1,
                       actionButton("checkout_review", "Review Checkout")
                ),
                column(11)
              ),
              br(),
              fluidRow(
                column(1,
                       actionButton("checkout_back", "Back")
                ),
                column(11)
              )
            ),
            conditionalPanel(
              condition = 'input.select_subject == "Language"',
              uiOutput("ui_language_core"),
              br(),
              uiOutput("pre_checkout_language"),
              div(style="position: relative; bottom: 300px;",
                  fluidRow(
                    actionButton("checkout_review_language", "Review Checkout")
                  ),
                  br(),
                  fluidRow(
                    actionButton("checkout_back_language", "Back")
                  ))
            )
          ),
          div(
            class = "page",
            id = "pagecheckout_review",
            uiOutput("student_review_checkout_title"),
            br(),
            uiOutput("checkout_submission"),
            br(),
            p(HTML("<b>STOP: Confirm students have signed out of Zoom before proceeding.</b>"),
              class = "textblue text20"),
            br(),
            fluidRow(
              column(2, actionButton("checkout_review_back", "Back")),
              column(1, actionButton("done", "Complete Student Checkout")),
              column(10)
            )
          ),
          div(
            class = "page",
            id = "page5",
            h2("Post-Session Survey"),
            br(),
            p(HTML("<b>Student(s)...</b>"),
              class = "textblue text20",
              style = "position: relative; bottom: -20px;"),
            awesomeCheckboxGroup(
              inputId = "completed",
              label = "", 
              choices = c("had a completed and well-done assignment.",
                          "did not totally complete weekly assignment."),
              width='100%'
            ),
            p(HTML("<b>Student(s)...</b>"),
              class = "textblue text20",
              style = "position: relative; bottom: -20px;"),
            awesomeCheckboxGroup(
              inputId = "focused",
              label = "", 
              choices = c("stayed focused on the content and did not get distracted.",
                          "spent time unfocused, leading away from the math or language content."),
              width='100%'
            ),
            br(),
            uiOutput("math_focused_question"),
            br(),
            p(HTML("<b>Student(s)...</b>"),
              class = "textblue text20",
              style = "position: relative; bottom: -20px;"),
            awesomeCheckboxGroup(
              inputId = "prepared",
              label = "", 
              choices = c("was fully prepared for the session.",
                          "was missing paper, pencil, white board, and/or lesson materials.",
                          "computer was not fully charged.",
                          "was not at proper desk for the session.",
                          "used up session time to look for and discuss the start content.",
                          "used up session time to discuss Student’s unpreparedness."),
              width='100%'
            ),
            br(),
            p(HTML("<b>Student(s)...</b>"),
              class = "textblue text20",
              style = "position: relative; bottom: -20px;"),
            awesomeCheckboxGroup(
              inputId = "distracted",
              label = "", 
              choices = c("had no noise/environmental issues.",
                          "had poor or erratic internet connection.",
                          "forgot to click “share sound” when sharing screen and Student had to stop and start again.",
                          "appeared tired (could be due to lack of sleep, lack of food, stress, mood).",
                          "There were loud noises in the background (e.g., loud TV, others arguing or playing, parent cleaning around desk, etc.)."),
              width='100%'
            ),
            br(),
            # uiOutput("validate"),
            # br(),
            uiOutput("assignment_goal_offline"),
            br(),
            actionButton("review", "Review Submission")
          ),
          div(
            class = "page",
            id = "page6",
            uiOutput("your_submission"),
            br(),
            uiOutput("user_submission"),
            br(),
            fluidPage(
              fluidRow(
                column(1, actionButton("back", "Back")),
                column(1, actionButton("submit", "Submit")),
                column(10)
              )
            )
          ),
          div(
            class = "page",
            id = "page7",
            h3("You have completed today’s session. Thank you!")
          )
        )
      )
    ),
    tags$head(tags$style('.required {color:red;}
                         .main-header .logo {font-weight: bold; }
                         .checkbox-group-buttons .btn {font-size: 16px;}
                         #checkbox :after, #checkbox :before{background-color: #778EC7;}
                         .purpletext {color: #7030a0;}
                         .textblue {color: #225e9a;}
                         #check_in {background-color: #778EC7; color: white;}
                         #check_in_back {background-color: #778EC7; color: white;}
                         #make_selections_back {background-color: #778EC7; color: white;}
                         #checkout_back {background-color: #778EC7; color: white;}
                         #checkout_review_back {background-color: #778EC7; color: white;}
                         #back {background-color: #778EC7; color: white;}
                         #submit {background-color: #778EC7; color: white;}
                         #review {background-color: #778EC7; color: white;}
                         #done {background-color: #778EC7; color: white;}
                         #review_submission {background-color: #778EC7; color: white;}
                         #back_science {background-color: #778EC7; color: white;}
                         #submit_science {background-color: #778EC7; color: white;}
                         #back_math {background-color: #778EC7; color: white;}
                         #check_next_math {background-color: #778EC7; color: white;}
                         #back_language {background-color: #778EC7; color: white;}
                         #check_next_language {background-color: #778EC7; color: white;}
                         #checkout_review {background-color: #778EC7; color: white;}
                         #checkout_review_language {background-color: #778EC7; color: white;}
                         #checkout_back_language {background-color: #778EC7; color: white;}
                         textarea {font-weight: normal;}
                         #select_coach :before{ background-color: lightgray; }
                         #select_coach :after{ background-color: #778EC7;} 
                         #select_coach {font-size: 16px;}
                         #language_reading {font-size: 20px;}
                         #select_student :before{ background-color: lightgray; }
                         #core_topics :before{ background-color: lightgray; }
                         #select_student :after{ background-color: #778EC7;} #select_student {font-size: 16px;}
                         #select_subject :before{ background-color: lightgray; }
                         #select_subject :after{ background-color: #778EC7;} #select_subject {font-size: 16px;}
                         #review :before{ background-color: lightgray; }
                         #review :after{ background-color: #778EC7;} #review {font-size: 16px;}
                         #focused :before{ background-color: lightgray; }
                         #focused :after{ background-color: #778EC7;} #focused {font-size: 16px;}
                         #math_focused :before{ background-color: lightgray; }
                         #math_focused :after{ background-color: #778EC7;} #math_focused {font-size: 16px;}
                         #prepared :before{ background-color: lightgray; }
                         #prepared :after{ background-color: #778EC7;} #prepared {font-size: 16px;}
                         #distracted :before{ background-color: lightgray; }
                         #distracted :after{ background-color: #778EC7;} #distracted {font-size: 16px;}
                         #final_checkout :before{ background-color: lightgray; }
                         #final_checkout :after{ background-color: #778EC7;} #final_checkout {font-size: 16px;}
                         #core_topics :after{ background-color: #778EC7;}
                         #grammar_input :before{ background-color: lightgray; }
                         #grammar_input :after{ background-color: #778EC7;} #grammar_input {font-size: 16px;}
                         #speaking_input :before{ background-color: lightgray; }
                         #speaking_input :after{ background-color: #778EC7;} #speaking_input {font-size: 16px;}
                         #writing_input :before{ background-color: lightgray; }
                         #writing_input :after{ background-color: #778EC7;} #writing_input {font-size: 16px;}
                         #comprehension_input :before{ background-color: lightgray; }
                         #comprehension_input :after{ background-color: #778EC7;} #comprehension_input {font-size: 16px;}
                         #comprehension_material :before{ background-color: lightgray; }
                         #comprehension_material :after{ background-color: #778EC7;} #comprehension_material {font-size: 16px;}
                         
                         #personal_book_finished :before{ background-color: lightgray; }
                         #personal_book_finished :after{ background-color: #778EC7;} #personal_book_finished {font-size: 16px;}
                         #personal_book_name :before{ background-color: lightgray; }
                         #personal_book_name :after{ background-color: #778EC7;} #personal_book_name {font-size: 16px;}
                         
                         #school_book_finished :before{ background-color: lightgray; }
                         #school_book_finished :after{ background-color: #778EC7;} #school_book_finished {font-size: 16px;}
                         #school_book_name :before{ background-color: lightgray; }
                         #school_book_name :after{ background-color: #778EC7;} #school_book_name {font-size: 16px;}
                         
                         #reading_input :before{ background-color: lightgray; }
                         #reading_input :after{ background-color: #778EC7;} #reading_input {font-size: 16px;}
                         #brain_book_input :before{ background-color: lightgray; }
                         #brain_book_input :after{ background-color: #778EC7;} #brain_book_input {font-size: 16px;}
                         #grammar_input :before{ background-color: lightgray; }
                         #grammar_input :after{ background-color: #778EC7;} #grammar_input {font-size: 16px;}
                         #speaking_input :before{ background-color: lightgray; }
                         #speaking_input :after{ background-color: #778EC7;} #speaking_input {font-size: 16px;}
                         #writing_input :before{ background-color: lightgray; }
                         #writing_input :after{ background-color: #778EC7;} #writing_input {font-size: 16px;}
                         #comprehension_material :before{ background-color: lightgray; }
                         #comprehension_material :after{ background-color: #778EC7;} #comprehension_material {font-size: 16px;}
                         #personal_book_finished :before{ background-color: lightgray; }
                         #personal_book_finished :after{ background-color: #778EC7;} #personal_book_finished {font-size: 16px;}
                         #personal_book_name :before{ background-color: lightgray; }
                         #personal_book_name :after{ background-color: #778EC7;} #personal_book_name {font-size: 16px;}
                         #checked :before{ background-color: lightgray; }
                         #checked :after{ background-color: #778EC7;} #checked {font-size: 16px;}
                         #language_grammar :before{ background-color: lightgray; }
                         #language_grammar :after{ background-color: #778EC7;} #language_grammar {font-size: 20px;}
                         #language_speaking :before{ background-color: lightgray; }
                         #language_speaking :after{ background-color: #778EC7;} #language_speaking {font-size: 20px;}
                         #language_writing :before{ background-color: lightgray; }
                         #language_writing :after{ background-color: #778EC7;} #language_writing {font-size: 20px;}
                         #language_comprehension :before{ background-color: lightgray; }
                         #language_comprehension :after{ background-color: #778EC7;} #language_comprehension {font-size: 20px;}
                         #language_reading :before{ background-color: lightgray; }
                         #language_reading :after{ background-color: #778EC7;} #language_reading {font-size: 20px;}
                         #completed :before{ background-color: lightgray; }
                         #completed :after{ background-color: #778EC7;} #completed {font-size: 16px;}
                         .text20 {font-size: 20px;}
                         .text24 {font-size: 24px;}
                         .text32 {font-size: 32px;}
                         .text36 {font-size: 36px;}
                         #khan_summary1, #khan_summary2, #non_khan_summary1, #non_khan_summary2, 
                         #ml_assignment, #assignment, #goal, #offline, #brain_book_input, #language_notes, #language_assignment {font-size: 20px; font-weight: normal;}
                         #check_in_back, #back, #submit, #check_in, #back_language, #check_next_language, #submit_science, #back_science, #back_math, #check_next_math, #checkout_back, #checkout_review, #checkout_back_language, #checkout_review_language, #checkout_review_back, #done, #review, #brain_next {font-size: 20px;}
                         .checkbox label, .checkbox choices {font-size: 20px}
                         .skin-blue .main-header .logo {background-color: #778EC7;}
                         .skin-blue .main-header .logo:hover {background-color: #b4c4ed;}
                         .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #b4c4ed;}
                         .skin-blue .main-header .navbar {background-color: #778EC7;}
                         .content-wrapper, .right-side {background-color: #ffffff;}
                         .nav-tabs>li>a {color: #778EC7;}
                         .skin-blue .sidebar-menu > li.active > a {border-left-color: #778EC7;}
                         .skin-blue .sidebar-menu > li:hover > a {border-left-color: #778EC7;}
                         h3 {font-weight: bold;}
                         .btn-group .btn {
                         background-color: lightgray !important; /* Unselected color */
                         color: black;}
                         .btn-group .btn.active {
                         background-color: darkgray !important; /* Selected color */
                         color: white;}
                         ')),
  )
)

server <- function(input, output, session) {
  Sys.setenv(TZ='CST6CDT')
  
  database = reactiveValues(coaches = data.frame(Coach=get_coaches()),
                            students = data.frame(Student=get_students()),
                            subjects = data.frame(Subject=get_subjects()),
                            accounts = get_valid_users(),
                            pairings = get_pairings())
  
  coaches <- reactive({get_coaches()})
  students <- reactive({get_students()})
  subjects <- reactive({get_subjects()})
  
  rv <- reactiveValues(page = 0,
                       admin_page = 0,
                       settings_page = 0)
  
  observe({
    shinyjs::hide(selector = ".page")
    shinyjs::show(paste0("page", rv$page))
    shinyjs::show(paste0("admin_p", rv$admin_page))
    shinyjs::show(paste0("settings_p", rv$settings_page))
  })
  
  output$selections <- renderUI({
    div(
      awesomeRadio(
        inputId = "select_coach",
        label = "Coach Name", 
        choices = c(information$name),
        status = "default",
        selected = information$name
      ),
      awesomeCheckboxGroup(
        inputId = "select_student",
        label = "Student Name(s)", 
        choices = filter(get_pairings(), coach == information$name)$student,
        status = "default"
      ),
      awesomeRadio(
        inputId = "select_subject",
        label = "Subject", 
        choices = database$subjects$Subject,
        status = "default"
      )
    )
  })
  
  output$during_session <- renderUI({
    div(
      h2(paste(information$student, collapse=", ")),
      h4(paste0("Coach: ", information$coach), 
         style = "color: red;"),
      h4(paste0("Grade(s): ", get_grade(information$student), " | ", "Subject: ", information$subject))
    )
  })
  
  # output$validate <- renderUI({
  #   if (information$subject == "Math") {
  #     if (any(sapply(list(input$completed,
  #                         input$focused,
  #                         input$math_focused,
  #                         input$prepared,
  #                         input$distracted,
  #                         input$core_topics), is.null))) {
  #       div(class="required",
  #           p("Please fill out all required fields above."))
  #     }
  #   } else if (information$subject == "Language") {
  #     
  #   } else {
  #     if (any(sapply(list(input$completed,
  #                         input$focused,
  #                         input$prepared,
  #                         input$distracted), is.null))) {
  #       div(class="required",
  #           p("Please fill out all required fields above."))
  #     }
  #   }
  # })
  
  checkin_time <- NULL
  checkout_time <- NULL
  time_zone <- NULL
  date <- NULL
  day_of_week <- NULL
  
  information <- reactiveValues(
    student = NULL,
    coach = NULL,
    subject = NULL,
    name = NULL
  )
  
  output$checkin_window <- renderUI({
    p( paste0("Student checked in at: ", 
              day_of_week, ", ", 
              date, ", ",
              substr(as.character(checkin_time), 12, 16), " (CST)") )
  })
  output$check_inout_times <- renderUI({
    div(
      p( paste0("Student checked in at: ", 
                day_of_week, ", ", 
                date, ", ",
                substr(as.character(checkin_time), 12, 16), " (CST)") ),
      p( paste0("Student checked out at: ", 
                day_of_week, ", ", 
                date, ", ",
                substr(as.character(checkout_time), 12, 16), " (CST)") )
    )
  })
  
  returnPage <- function() {
    rv$page <- 1
  }
  
  navPage <- function(check_in=FALSE, check_out=FALSE) {
    rv$page <- rv$page + 1
    
    Sys.setenv(TZ='CST6CDT')
    time <- as.character(Sys.time())
    time_zone <<- Sys.timezone()
    
    if (check_in) {
      checkin_time <<- time
      date <<- Sys.Date()
      day_of_week <<- weekdays(date)
      
      clock(checkin_time, time_zone, information$coach, information$student, information$subject, check_in=TRUE)
    } else if (check_out) {
      checkout_time <<- time
      
      clock(checkout_time, time_zone, information$coach, information$student, information$subject, check_out=TRUE)
    }
  }
  
  observeEvent(input$review_submission, {
    if (length(input$select_student) > 0) {
      rv$page <- 2
      
      information$student = input$select_student
      information$coach = input$select_coach
      information$subject = input$select_subject
    }
  })
  observeEvent(input$done, 
               if (((information$subject == "Math") & (!is.null(input$final_checkout))) | 
                   ((information$subject != "Math") & (!is.null(input$final_checkout)))) {
                 rv$page = 4
                 navPage(check_out=TRUE)
               })
  observeEvent(input$review, {
    navPage()
  })
  observeEvent(input$back, {rv$page <- rv$page - 1})
  observeEvent(input$submit, {
    clock_submit(as.character(Sys.time()), time_zone, 
                 information$coach, information$student, information$subject)
    
    if ((information$subject == "Math") & (length(information$student) > 1)) {
      
      submit_session(checkin_time, checkout_time,
                     information$coach, information$student, information$subject,
                     input$completed, input$focused, input$checked, input$prepared, input$distracted,
                     goal=input$goal, offline=input$offline,
                     math_focused=input$math_focused, khan1=input$khan1, khan2=input$khan2, 
                     mms=input$math_mastered_skills,
                     assignment=input$ml_assignment,
                     khan_summary1=input$khan_summary1, khan_summary2=input$khan_summary2,
                     non_khan_summary1=input$non_khan_summary1, non_khan_summary2=input$non_khan_summary2,
                     core_topics=c((if ("Other" %in% input$core_topics) input$core_topics[-1] else sort(input$core_topics)),  
                                   (if (input$core_choices_other == "") c() else input$core_choices_other)))
      
    } else if (information$subject == "Math") {
      
      submit_session(checkin_time, checkout_time,
                     information$coach, information$student, information$subject,
                     input$completed, input$focused, input$checked, input$prepared, input$distracted,
                     goal=input$goal, offline=input$offline,
                     math_focused=input$math_focused, khan1=input$khan1,
                     mms=input$math_mastered_skills,
                     assignment=input$ml_assignment,
                     khan_summary1=input$khan_summary1, khan_summary2=input$khan_summary2,
                     non_khan_summary1=input$non_khan_summary1, non_khan_summary2=input$non_khan_summary2,
                     core_topics=c((if ("Other" %in% input$core_topics) input$core_topics[-1] else sort(input$core_topics)),
                                   (if (input$core_choices_other == "") c() else input$core_choices_other)))
      
    } else if (information$subject == "Language") {
      submit_session(checkin_time, checkout_time,
                     information$coach, information$student, information$subject,
                     input$completed, input$focused, input$checked, input$prepared, input$distracted,
                     language_reading=input$language_reading, 
                     language_comprehension=input$language_comprehension, 
                     language_writing=input$language_writing, 
                     language_speaking=input$language_speaking, 
                     language_grammar=input$language_grammar, 
                     language_notes=input$language_notes, 
                     language_assignment=input$language_assignment,
                     reading_input=(if (length(input$language_reading) != 0) input$reading_input else c()),
                     
                     brain_book_input=input$brain_book_input, 
                     brain_book_chapter=input$brain_book_chapter,
                     brain_book_pages=input$brain_book_pages,
                     
                     personal_book_name=(if ((length(input$language_reading) != 0) & ("Personal Book" %in% input$reading_input)) input$personal_book_name else ""),
                     personal_book_pages=(if ((length(input$language_reading) != 0) & ("Personal Book" %in% input$reading_input)) input$personal_book_pages else c()),
                     personal_book_finished=(if ((length(input$language_reading) != 0) & ("Personal Book" %in% input$reading_input)) input$personal_book_finished else c()),
                     school_book_name=(if ((length(input$language_reading) != 0) & ("School Book" %in% input$reading_input)) input$school_book_name else ""),
                     school_book_pages=(if ((length(input$language_reading) != 0) & ("School Book" %in% input$reading_input)) input$school_book_pages else c()),
                     school_book_finished=(if ((length(input$language_reading) != 0) & ("School Book" %in% input$reading_input)) input$school_book_finished else c()),
                     
                     comprehension_material=(if (length(input$language_comprehension) != 0) input$comprehension_material else ""),
                     comprehension_input=(if (length(input$language_comprehension) != 0) input$comprehension_input else c()),
                     writing_input=(if (length(input$language_writing) != 0) input$writing_input else ""),
                     speaking_input=(if (length(input$language_speaking) != 0) input$speaking_input else ""),
                     grammar_input=(if (length(input$language_grammar) != 0) input$grammar_input else ""),
                     initial1=input$initial1,
                     initial2=input$initial2,
                     initial3=input$initial3,
                     initial4=input$initial4,
                     initial5=input$initial5,
                     initial6=input$initial6,
                     initial7=input$initial7,
                     initial8=input$initial8,
                     initial9=input$initial9,
                     initial10=input$initial10,
                     initial11=input$initial11,
                     chapter1=input$chapter1,
                     chapter2=input$chapter2,
                     chapter3=input$chapter3,
                     chapter4=input$chapter4,
                     chapter5=input$chapter5,
                     chapter6=input$chapter6,
                     chapter7=input$chapter7,
                     chapter8=input$chapter8,
                     chapter9=input$chapter9,
                     chapter10=input$chapter10,
                     chapter11=input$chapter11,
                     goal=input$goal, offline=input$offline # 11/22
      )
    } else {
      submit_session(checkin_time, checkout_time,
                     information$coach, information$student, information$subject,
                     input$completed, input$focused, input$checked, input$prepared, input$distracted,
                     goal=input$goal, offline=input$offline,
                     other_comments=input$science_comments)
    }
    
    navPage()
  })
  
  observeEvent(input$submit_science, {
    Sys.setenv(TZ='CST6CDT')
    checkout_time <<- as.character(Sys.time())
    
    clock(checkout_time, time_zone, information$coach, information$student, information$subject, check_out=TRUE)
    
    submit_session(checkin_time, checkout_time,
                   information$coach, information$student, information$subject,
                   "", "", "", "", "", other_comments=input$science_comments)
    
    rv$page <- 7
  })
  
  output$input_khan <- renderUI({
    if (information$subject == "Math") {
      div(
        p(HTML(paste0("<b>A) ", information$student[1], "\'s total number of Mastered Concepts:</b>")),
          style = "position: relative; bottom: -20px",
          class = "textblue text20"),
        numericInput(inputId="math_mastered_skills",
                     label = "", 
                     min=0,
                     max=80,
                     value = 0,
                     width="100%"),
        p(HTML(paste0("<b>B) ", information$student[1], "\'s previous Khan Percent (between 0 and 100):</b>")),
          style = "position: relative; bottom: -20px",
          class = "textblue text20"),
        numericInput(inputId="khan1", 
                     label = "", 
                     min=0,
                     max=100,
                     value = 0,
                     width="100%"),
        if (length(information$student) > 1) {
          div(
            p(HTML(paste0("<b>B) ", information$student[2], "\'s previous Khan Percent (between 0 and 100):</b>")),
              style = "position: relative; bottom: -20px",
              class = "textblue text20"),
            numericInput(inputId="khan2", 
                         label = "", 
                         min=0,
                         max=100,
                         value = 0,
                         width="100%")
          )
        }
      )
    }
  })
  
  output$user_submission <- renderUI({
    div(
      p(HTML(paste0("<b>Student(s)...</b> ", input$completed)),
        class = "textblue text20"), br(),
      p(HTML(paste0("<b>Student(s)...</b> ", paste(input$focused, collapse=".. "))),
        class = "textblue text20"), br(),
      if (information$subject == "Math") {
        p(HTML(paste0("<b>For math, student(s)...</b> ", input$math_focused)),
          class = "textblue text20")
      }, br(), 
      p(HTML(paste0("<b>Coach...</b> ", input$checked)),
        class = "textblue text20"), br(),
      p(HTML(paste0("<b>Student(s)...</b> ", paste(input$prepared, collapse=".. "))),
        class = "textblue text20"), br(),
      p(HTML(paste0("<b>Student(s)...</b> ", paste(input$distracted, collapse=".. "))),
        class = "textblue text20"), br(),
      div(
        p(HTML(paste0("<b>Goals for ", information$student, ":</b> ", input$goal)),
          class = "textblue text20"), br(),
        p(HTML(paste0("<b>Any Offline Issues for ", information$student, ":</b> ", input$offline)),
          class = "textblue text20")
      )
    )
  })
  
  output$all_settings <- renderUI({
    div(
      setSliderColor(c("#778EC7"), c(1)),
      fluidPage(
        fluidRow(
          column(4,
                 h3("Coaches"),
                 data.frame(`Name` = (database$coaches)$Coach) %>% 
                   gt() %>% 
                   opt_interactive(page_size_default=5),
                 br(),
                 textInput(inputId="new_coach", 
                           label="Add new coach:", 
                           width='100%', 
                           placeholder = "Name"),
                 actionButton("add_coach_button", "Add"),
                 pickerInput(
                   inputId = "delete_coach",
                   label = "Remove coach:", 
                   choices = (database$coaches)$Coach,
                   options = list(title = "Name")),
                 actionButton("remove_coach_button", "Remove")),
          column(4,
                 h3("Students"),
                 gt_output("student_selections"),
                 br(),
                 textInput(inputId="new_student", 
                           label="Add new student:", 
                           width='100%', 
                           placeholder = "Name"),
                 sliderTextInput(
                   inputId = "new_student_grade",
                   label = "Current Grade:", 
                   choices = c("K", seq(1:12)),
                   grid=TRUE),
                 actionButton("add_student_button", "Add"),
                 pickerInput(
                   inputId = "delete_student",
                   label = "Remove student:", 
                   choices = database$students$Student,
                   options = list(title = "Name")),
                 actionButton("remove_student_button", "Remove")),
          column(4,
                 h3("Subjects"),
                 data.frame(`Name` = database$subjects$Subject) %>% 
                   gt() %>% 
                   opt_interactive(page_size_default=5),
                 br(),
                 textInput(inputId="new_subject", 
                           label="Add new subject:", 
                           width='100%', 
                           placeholder = "Name"),
                 actionButton("add_subject_button", "Add"),
                 pickerInput(
                   inputId = "delete_subject",
                   label = "Remove subject:", 
                   choices = database$subjects$Subject,
                   options = list(title = "Name")),
                 actionButton("remove_subject_button", "Remove"))
        ),
        br(),
        br(),
        fluidRow(
          column(6,
                 h3("Accounts"),
                 database$accounts %>% 
                   gt() %>% 
                   opt_interactive(page_size_default=5),
                 br(),
                 textInput(inputId="new_account_name", 
                           label="New account name:", 
                           width='100%', 
                           placeholder = "Name"),
                 textInput(inputId="new_account_username", 
                           label="New account username:", 
                           width='100%', 
                           placeholder = "Username"),
                 textInput(inputId="new_account_password", 
                           label="New account password:", 
                           width='100%', 
                           placeholder = "Password"),
                 awesomeRadio(
                   inputId = "new_account_admin",
                   label = "New account admin:", 
                   choices = c("TRUE", "FALSE"),
                   status = "default",
                 ),
                 actionButton("add_account_button", "Add"),
                 pickerInput(
                   inputId = "delete_account",
                   label = "Remove account:", 
                   choices = get_valid_users()$username,
                   options = list(title = "Username")),
                 actionButton("remove_account_button", "Remove")),
          column(6,
                 h3("Coach-Student Pairings"),
                 database$pairings %>% 
                   gt() %>% 
                   opt_interactive(page_size_default=5),
                 br(),
                 pickerInput(inputId="new_pairing_coach", 
                             label="New pairing coach:", 
                             choices = get_coaches(),
                             options = list(title = "Coach")),
                 pickerInput(inputId="new_pairing_student", 
                             label="New pairing student:", 
                             choices = get_students(),
                             options = list(title = "Student")),
                 actionButton("add_pairing_button", "Add"),
                 pickerInput(
                   inputId = "delete_pairing",
                   label = "Remove pairing:", 
                   choices = mutate(get_pairings(), pairing = paste(coach, student, sep=" - "))$pairing,
                   options = list(title = "Pairing")),
                 actionButton("remove_pairing_button", "Remove"))
        )
      )
    )
  })
  
  output$pre_checkout <- renderUI({
    div(
      awesomeCheckboxGroup(
        inputId = "final_checkout",
        label = "", 
        choices = c("I am ready to check out."),
        width='100%')
    )
  })
  output$pre_checkout_language <- renderUI({
    div(style = "position: relative; bottom: 300px;",
        awesomeCheckboxGroup(
          inputId = "final_checkout",
          label = "", 
          choices = c("I am ready to check out."),
          width='100%')
    )
  })
  
  observeEvent(input$add_coach_button, 
               if (input$new_coach != "") {
                 add_coach(input$new_coach)
                 database$coaches <- rbind(database$coaches, data.frame(Coach=input$new_coach)) %>%
                   unique()
               })
  observeEvent(input$add_student_button, 
               if (input$new_student != "") {
                 add_student(input$new_student, input$new_student_grade) 
                 database$students <- rbind(database$students, data.frame(Student=input$new_student)) %>%
                   unique()
               })
  observeEvent(input$add_subject_button, 
               if (input$new_subject != "") {
                 add_subject(input$new_subject)
                 database$subjects <- rbind(database$subjects, data.frame(Subject=input$new_subject)) %>%
                   unique()
               })
  observeEvent(input$add_account_button, {
    if ((!is.null(input$new_account_name)) & (!is.null(input$new_account_username)) & (!is.null(input$new_account_password))) {
      add_valid_user(input$new_account_name, input$new_account_username, input$new_account_password, admin=(input$new_account_admin == "TRUE"))
      database$accounts <- rbind(database$accounts, 
                                 data.frame(name=input$new_account_name, 
                                            username=input$new_account_username, 
                                            password=input$new_account_password, 
                                            admin=(input$new_account_admin == "TRUE"))) %>%
        unique()
    }
  })
  observeEvent(input$add_pairing_button, 
               if ((!is.null(input$new_pairing_coach)) & (!is.null(input$new_pairing_student))) {
                 add_pairing(input$new_pairing_coach, input$new_pairing_student)
                 database$pairings <- rbind(database$pairings, 
                                            data.frame(coach=input$new_pairing_coach, 
                                                       student=input$new_pairing_student)) %>%
                   unique()
               })
  
  observeEvent(input$remove_coach_button, {
    remove_coach(input$delete_coach)
    database$coaches <- filter(database$coaches, Coach != input$delete_coach)
  })
  observeEvent(input$remove_student_button, {
    remove_student(input$delete_student)
    database$students <- filter(database$students, Student != input$delete_student)
  })
  observeEvent(input$remove_subject_button, {
    remove_subject(input$delete_subject)
    database$subjects <- filter(database$subjects, Subject != input$delete_subject)
  })
  observeEvent(input$remove_account_button, {
    remove_valid_user(input$delete_account)
    database$accounts <- filter(database$accounts, username != input$delete_account)
  })
  observeEvent(input$remove_pairing_button, {
    delete_coach = strsplit(input$delete_pairing, " - ")[[1]][1]
    delete_student =strsplit(input$delete_pairing, " - ")[[1]][2]
    
    remove_pairing(delete_coach, delete_student)
    database$pairings <- filter(database$pairings, 
                                ((coach != delete_coach) & (student != delete_student)))
  })
  
  output$dashboard_selections <- renderUI({
    earliest <- as.Date(substr(min(get_all_sessions()$checkin, na.rm=T), 0, 10), "%Y-%m-%d")
    latest <- as.Date(substr(max(get_all_sessions()$checkin, na.rm=T), 0, 10), "%Y-%m-%d")
    
    current_year <- as.integer(substr(Sys.Date(), 0, 4))
    most_years_in_program <- current_year - as.integer(substr(min(get_students_grades()$entrance, na.rm=T), 0, 4)) + 1
    
    fluidPage(
      fluidRow(
        column(6,
               checkboxGroupButtons(
                 inputId="coach_dash",
                 label="Select coach(es):", 
                 choices=c(sort((database$coaches)$Coach), "All"),
                 selected="All",
                 status="default")),
        column(6, 
               checkboxGroupButtons(
                 inputId="student_dash",
                 label="Select student(s):", 
                 choices=c(sort((database$students)$Student), "All"),
                 selected="All"))
      ),
      fluidRow(
        column(6,
               checkboxGroupButtons(
                 inputId="subject_dash",
                 label="Select subject(s):", 
                 choices=c(sort((database$subjects)$Subject), "All"),
                 selected="All")),
        column(6, 
               checkboxGroupButtons(
                 inputId="grade_dash",
                 label="Select grade(s):", 
                 choices=c("K", seq(1:12), "All"),
                 selected="All",
                 status="default"))
      ),
      fluidRow(
        setSliderColor(c("#778EC7"), c(1)),
        column(1),
        column(5, align='center',
               sliderInput(inputId="session_dash",
                           "Select time range for sessions:",
                           min = earliest,
                           max = latest,
                           value=c(earliest, latest),
                           timeFormat="%Y-%m-%d")
        ),
        column(2, align='center',
               downloadButton("export_session_log", "Export Session Log")),
        column(1),
        column(2, align='center',
               downloadButton("export_cld", "Export CLD Log")),
        column(1)
      )
    )
  })
  
  output$sessions <- render_gt({
    get_sessions(input$coach_dash, input$student_dash, input$subject_dash, s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
      session_log()
  })
  output$cld_log <- render_gt({
    get_sessions(input$coach_dash, input$student_dash, input$subject_dash, s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
      cld_log()
  })
  output$khan_progress <- renderPlot({
    if (("Math" %in% input$subject_dash) | ("All" %in% input$subject_dash)) {
      get_sessions(input$coach_dash, input$student_dash, c("Math"), s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
        khan_plot()
    }
  })
  output$mms_progress <- renderPlot({
    if (("Math" %in% input$subject_dash) | ("All" %in% input$subject_dash)) {
      get_sessions(input$coach_dash, input$student_dash, c("Math"), s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
        mms_plot()
    }
  })
  output$cld_sessions <- renderPlot({
    get_sessions(input$coach_dash, input$student_dash, input$subject_dash, s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
      cld_plot()
  })
  output$coach_hours <- render_gt({
    get_coach_hours() %>%
      gt() %>%
      tab_header(title=md("**Analytics by Coach**"))
  })
  
  output$student_hours = render_gt({
    get_student_hours() %>%
      gt() %>%
      tab_header(title=md("**Analytics by Student**"))
  })
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(coaches=input$coach_dash,
                     students=input$student_dash, 
                     subjects=input$subject_dash,
                     grades=input$grade_dash)
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$your_selections <- renderUI({
    div(
      h2(paste(information$student, collapse=", ")),
      h4(paste0("Coach: ", information$coach), 
         style = "color: red; font-size: 24px;"),
      h4(paste0("Grade(s): ", get_grade(information$student), " | ", "Subject: ", information$subject),
         style = "font=size: 24px;")
    )
  })
  output$your_selections2 <- renderUI({
    div(style = "margin-top: -25px;",
      h2(paste(information$student, collapse=", ")),
      h4(paste0("Coach: ", information$coach), 
         style = "color: red; font-size: 24px;"),
      h4(paste0("Grade(s): ", get_grade(information$student), " | ", "Subject: ", information$subject),
         style = "font=size: 24px;")
    )
  })
  output$previous_session_math <- renderUI({
    if (length(information$student) == 1) {
      data <- get_sessions("All", information$student, information$subject, prev=TRUE)
    } else {
      data <- get_previous_multiple(information$student, information$subject, prev=TRUE)
    }
    
    if (nrow(data) == 0) {
      return( data.frame() )
    }
    
    time_in_session =
      difftime(data$checkout, data$checkin, units="mins") %>%
      as.numeric() %>%
      round()
    
    div(
      h2(HTML(paste0("<b>Summary of ", information$student, "'s Previous Session</b>")),
         class="textblue text36",
         style = "text-align: center;"),
      br(),
      p( HTML( paste0("<b>", information$student, "'s Previous Session:</b> ", substr(as.character(data$checkout), 0, 10)) ),
         class = "textblue text20" ),
      if (information$subject == "Math") {
        div(
          p(HTML(paste0("<b>", information$student[1], "\'s Previous # of Mastered Concepts:</b> ", data$mms)),
            class = "textblue text20"),
          p(HTML(paste0("<b>", information$student[1], "\'s Previous Khan Percent:</b> ", data$khan1)),
            class = "textblue text20")
        )
      },
      if ((information$subject == "Math") & (length(information$student) > 1)) {
        div(
          p(HTML(paste0("<b>", information$student[2], "\'s Previous # of Mastered Concepts:</b> ", data$mms)),
            class = "textblue text20"),
          p(HTML(paste0("<b>", information$student[2], "\'s Previous Khan Percent:</b> ", data$khan2)),
            class = "textblue text20")
        )
      },
      if (information$subject == "Math") {
        split_topics = str_split(data$core_topics, " - ")[[1]]
        
        topic1 = split_topics[1]
        topic2 = if (length(split_topics) > 1) (split_topics[2]) else ""
        
        div(
          p(HTML(paste("<b>", information$student[1], "\'s Core Topic 1</b>: ", topic1)),
            class = "textblue text20"),
          p(HTML(paste("<b>Khan Summary 1 (first core topic selected), </b>: ", data$khan_summary1)),
            class = "textblue text20"),
          p(HTML(paste("<b>Non-Khan Summary 1</b>: ", data$non_khan_summary1)),
            class = "textblue text20"),
          p(HTML(paste("<b>", information$student[1], "\'s Core Topic 2</b>: ", topic2)),
            class = "textblue text20"),
          p(HTML(paste("<b>Khan Summary 2 (if applicable, second core topic selected)</b>: ", data$khan_summary2)),
            class = "textblue text20"),
          p(HTML(paste("<b>Non-Khan Summary 2</b>: ", data$non_khan_summary2)),
            class = "textblue text20"),
          p(HTML(paste("<b>Pre-Session Assignment Due Today:</b> ", data$assignment)),
            class = "textblue text20"),
          # p(HTML(paste("<b>Goal of Today’s Session:</b> ", data$goal)),
          #   class = "textblue text20")
        )
      } else {
        div(p(HTML(paste("<b>Pre-Session Assignment Due Today:</b> ", data$assignment)),
              class = "textblue text20"),
            # p(HTML(paste("<b>Goal of Today’s Session:</b> ", data$goal))),
            class = "textblue text20")
      },
      p(HTML("<b>Time in Session:</b> ", if (nrow(data) != 0) paste0(time_in_session, " min") else ""),
        class = "textblue text20")
    )
  })
  
  # output$checkin_validate2 <- renderUI({
  #   if (is.null(input$checked)) {
  #     div(class="required",
  #         p("Please review your selections."))
  #   }
  # })
  observeEvent(input$check_in, {
    navPage(check_in=TRUE)
    
    information$student = input$select_student
    information$coach = input$select_coach
    information$subject = input$select_subject
  })
  
  output$continue_selections <- renderUI({
    df <- 
      get_all_clocks(unmatched=TRUE) %>%
      filter(coach == information$name) %>%
      tail(5)
    
    if (nrow(df) > 0) {
      div(
        radioGroupButtons(
          inputId="current_selection",
          label="Select your current session:", 
          choices=df$Selections,
          direction="vertical",
          status="default")
      )
    } else {
      div()
    }
  })
  
  observeEvent(input$check_in_back, {
    rv$page <- 0
  })
  observeEvent(input$existing_back, {
    rv$page <- 0
  })
  observeEvent(input$make_selections_back, {
    rv$page <- 0
  })
  observeEvent(input$continue, {
    rv$page <- 4
    
    prev_students = strsplit(input$current_selection, " \\| ")[[1]][4]
    
    information$student = strsplit(prev_students, ", ")[[1]]
    information$coach = strsplit(input$current_selection, " \\| ")[[1]][3]
    information$subject = strsplit(input$current_selection, " \\| ")[[1]][5]
    
    checkin_time <<- paste0("           ", strsplit(input$current_selection, " \\| ")[[1]][2])
    time_zone <<- "CST"
    date <<- strsplit(input$current_selection, " \\| ")[[1]][1]
    day_of_week <<- weekdays(as.Date(date))
  })
  
  output$math_focused_question <- renderUI({
    if (information$subject == "Math") {
      div(
        p(HTML("<b>For math sessions, student(s)...</b>"),
          class = "textblue text20",
          style = "position: relative; bottom: -20px;"),
        awesomeCheckboxGroup(
          inputId = "math_focused",
          label = "", 
          choices = c("read aloud each question entirely including the title.",
                      "did NOT read aloud each question entirely including the title."),
          width='100%'
        )
      )
    }
  })
  
  output$cld_table <- render_gt({
    get_sessions(input$coach_dash, input$student_dash, input$subject_dash, s=input$session_dash, e=input$entrance_dash, select_grades=input$grade_dash) %>%
      cld_plot(table=TRUE)
  })
  
  observeEvent(input$check_button, {
    rv$page <- "check"
  })
  observeEvent(input$already_button, {
    rv$page <- "already"
  })
  
  output$ui_math_core <- renderUI({
    if (information$subject == "Math") {
      core_subject = "Math"
      grades = get_grade(information$student)
    } else {
      return()
    }
    
    core_choices = 
      (multiple_select_options %>%
         filter(Grade %in% grades,
                Subject == core_subject))$Options
    
    div(
      fluidRow(
        column(4,
               div(
                 p(HTML("<b>C) Core Concepts worked on in today’s session:</b>"),
                   style = "position: relative; bottom: -20px;",
                   class = "textblue text20"),
                 awesomeCheckboxGroup(
                   inputId = "core_topics",
                   label = "", 
                   choices = c(core_choices, "Other"),
                   width='100%'
                 ),
                 conditionalPanel(
                   condition = "input.core_topics.includes('Other')",
                   textInput("core_choices_other", "Other core topics:")
                 )
               )),
        column(8,
               br(),
               br(),
               p(HTML("<b>D) Please describe the Khan activities completed during the session. Please relate these activities to the core math concepts to the left:</b>"),
                 class = "textblue text20"),
               div(
                 p(HTML(paste0("<b>Khan Summary 1 (first core topic selected):</b>")),
                   class = "textblue text20"),
                 textInput(inputId="khan_summary1", 
                           label="", 
                           width='100%', 
                           placeholder = "Open Script..."),
                 p(HTML(paste0("<b>Khan Summary 2 (if applicable, second core topic selected)</b>")),
                   class = "textblue text20"),
                 textInput(inputId="khan_summary2", 
                           label="",
                           width='100%', 
                           placeholder = "Open Script...")
               ))
      ),
      br(),
      p(class = "textblue text20",
        HTML("<b>E) Please describe the non-Khan activities (if any) completed during the session. Please relate these activities to the core math concepts described above:</b>"),
        style = "position: relative; bottom: -20px;"),
      fluidRow(
        column(6,
               p(class = "textblue text20",
                 HTML("<b>Non-Khan Summary 1 (first core topic selected):</b>"),
                 style = "position: relative; bottom: -20px;"),
               textInput(inputId="non_khan_summary1",
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")
        ),
        column(6,
               p(class = "textblue text20",
                 HTML("<b>Non-Khan Summary 2 (second core topic selected):</b>"),
                 style = "position: relative; bottom: -20px;"),
               textInput(inputId="non_khan_summary2",
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")       
        )
      ),
      br(),
      p(class = "textblue text20",
        HTML( paste0("<b>F) Assignment for ", information$student, "'s Next Session:<b>") ),
        style = "position: relative; bottom: -20px;"),
      textInput(inputId="ml_assignment", # ml = math/language
                label="", 
                width='100%', 
                placeholder = "Open Script...")
    )
  })
  
  dataModal <- function() {
    modalDialog(
      tags$head(tags$script(HTML(jscode))),
      textInput("username", "Username:"),
      tagAppendAttributes(
        passwordInput("password", "Password:"),
        `data-proxy-click` = "login_button"
      ),
      footer = tagList(
        actionButton("login_button", "LOGIN")
      )
    )
  }
  
  login_modal <- observe({
    showModal(dataModal())
  })
  
  observeEvent(input$login_button, {
    valid_users = database$accounts
    
    if (nrow(filter(valid_users, username == input$username)) > 0) {
      if (filter(valid_users, username == input$username)$password == input$password) {
        
        login_modal$suspend()
        removeModal()
        
        information$name = filter(get_valid_users(), username == input$username)$name
        
        if (filter(valid_users, username == input$username)$admin) {
          rv$admin_page <- rv$admin_page + 1
          rv$settings_page <- rv$settings_page + 1
        } else {
          rv$admin_page <- rv$admin_page + 2
          rv$settings_page <- rv$settings_page + 2
        }
      }   
    }
  })
  
  observeEvent(input$checkout_review, {
    if (!(is.null(input$final_checkout))) {
      
      rv$page <- "checkout_review"
      
    }
  })
  observeEvent(input$checkout_review_language, {
    if (!(is.null(input$final_checkout))) {
      
      rv$page <- "checkout_review"
      
    }
  })
  
  output$checkout_submission <- renderUI({
    if (information$subject == "Language") {
      uiOutput("language_review_submission")
    } else {
      div(
        if (information$subject == "Math") {
          HTML(paste0("<b>A) ", information$student[1], "\'s total number of Mastered Concepts:</b> ", input$math_mastered_skills)) %>%
            p(class = "textblue text20")
        }, 
        br(),
        if (information$subject == "Math") {
          HTML(paste0("<b>B) ", information$student[1], "\'s previous Khan Percent:</b> ", input$khan1)) %>%
            p(class = "textblue text20")
        },
        if ((information$subject == "Math") & (length(information$student) > 1)) {
          HTML(paste0("<b>B) ", information$student[2], "\'s previous Khan Percent:</b> ", input$khan2)) %>%
            p(class = "textblue text20")
        }, br(),
        if (information$subject %in% c("Math")) {
          HTML(paste0("<b>C) Core Concepts worked on in today’s session:</b> ", 
                      paste(c((if ("Other" %in% input$core_topics) input$core_topics[-1] else sort(input$core_topics)), 
                              (if ("Vocabulary (BRAIN Book, etc.)" %in% input$core_topics) paste0(input$core_choices_vocab, " (BRAIN book)") else c()),
                              (if (input$core_choices_other == "") c() else input$core_choices_other)), collapse=".. "))) %>%
            p(class = "textblue text20")
        }, br(),
        if (information$subject %in% c("Math")) {
          topic1 = sort(input$core_topics)[1]
          topic2 = if (length(input$core_topics) > 1) (sort(input$core_topics)[2]) else ""
          
          div(
            HTML("<b>D)	Please describe the Khan activities (if any) completed during the session. Please relate these activities to the core math concepts below:</b>") %>%
              p(class = "textblue text20"),
            br(),
            if (!is.null(topic1)) {
              div(
                HTML(paste0("<b>", topic1, ":</b> ", input$khan_summary1)) %>%
                  p(class = "textblue text20"), 
                br()
              )
            },
            if (topic2 != "") {
              div(
                HTML(paste0("<b>", topic2, ":</b> ", input$khan_summary2)) %>%
                  p(class = "textblue text20"),
                br(), br()
              )
            },
            HTML("<b>E)	Please describe the non-Khan activities (if any) completed during the session. Please relate these activities to the core math concepts described above:</b>") %>%
              p(class = "textblue text20"),
            br(),
            div(
              HTML(paste0("<b>Non-Khan Summary 1:</b> ", input$non_khan_summary1)) %>%
                p(class = "textblue text20"),
              br()
            ),
            div(
              HTML(paste0("<b>Non-Khan Summary 2:</b> ", input$non_khan_summary2)) %>%
                p(class = "textblue text20"),
              br()
            ),
            HTML(paste0("<b>F) Assignment for ", information$student, "'s Next Session:</b> ", input$ml_assignment)) %>%
              p(class = "textblue text20")
          )
        }
      ) 
    }
  })
  
  observeEvent(input$checkout_review_back, {
    rv$page = 4
  })
  
  observe({
    if(length(input$core_topics) > 2){
      updateCheckboxGroupInput(session=session, inputId="core_topics", selected=tail(input$core_topics, 2))
    }
  })
  
  output$assignment_goal_offline = renderUI({
    div(
      p(HTML(paste0("<b>Would you say your Student is completing your content goals quickly? Elaborate on next session’s content goal here.</b>")),
        class = "textblue text20"),
      textInput(inputId="goal", 
                label="", 
                width='100%', 
                placeholder = "Open Script..."),
      p(HTML(paste0("<b>Is your student experiencing any non-academic circumstances the admin should know about? Please elaborate here.</b>")),
        class = "textblue text20"),
      textInput(inputId="offline", 
                label="", 
                width='100%', 
                placeholder = "Open Script...")
    )
  })
  
  output$student_selections = render_gt({
    current_month = as.numeric(substr(Sys.Date(), 6, 7))
    
    get_students(df=T) %>%
      mutate(Name = student,
             Grade = grade
             # Grade = 2032 - graduation + (current_month >= 6)
             ) %>%
      select(Name, Grade) %>% 
      gt() %>%
      opt_interactive(page_size_default=5)
  })
  
  observeEvent(input$check_next_language, {
    if (length(input$checked) != 0) {
      rv$page <- 4
    }
  })
  observeEvent(input$check_next_math, {
    if (length(input$checked) != 0) {
      rv$page <- 4
    }
  })
  
  output$time_in_session_ui = renderUI({
    time_in_session =
      difftime(as.character(checkout_time), as.character(checkin_time), units="mins") %>%
      as.numeric() %>%
      round()
    
    p(HTML("<b>Time in Session:</b> ", time_in_session, " min"),
      class = "textblue text20")
  })
  
  output$ui_language_core = renderUI({
    div(
      awesomeCheckboxGroup("language_reading",
                           label="",
                           choices=c("Reading")),
      uiOutput("reading_conditional"),
      
      awesomeCheckboxGroup("language_comprehension",
                           label="",
                           choices=c("Comprehension")),
      uiOutput("comprehension_conditional"),
      
      awesomeCheckboxGroup("language_writing",
                           label="",
                           choices=c("Writing")),
      uiOutput("writing_conditional"),
      
      awesomeCheckboxGroup("language_speaking",
                           label="",
                           choices=c("Speaking")),
      uiOutput("speaking_conditional"),
      
      awesomeCheckboxGroup(inputId="language_grammar",
                           label="",
                           choices=c("Grammar")),
      uiOutput("grammar_conditional"),
      
      br(),
      br(),
      
      p(HTML("<b>Brain Book Extension</b> (add these words to my student)"),
        class = "textblue text20"),
      fluidRow(
        column(4,
               textInput(inputId="brain_book_input", 
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")),
        column(2,
               br(),
               p(HTML("Brain Book Chapter Number:"),
                 class = "textblue text20"),
               
        ),
        column(2,
               textInput(inputId="brain_book_chapter", 
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")
        ),
        column(2,
               br(),
               p(HTML("Brain Book Page Number (up to):"),
                 class = "textblue text20"),
               
        ),
        column(2,
               textInput(inputId="brain_book_pages", 
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")
        )
      ),
      p(paste0(information$student, "'s assignment for next session:"),
        class = "textblue text20",
        style = "position: relative; bottom: -20px;"),
      textInput(inputId="language_assignment",
                label="", 
                width='100%', 
                placeholder = "Open Script..."),
      
      br(),
      uiOutput("brain_book_current")
    )
  })
  
  output$reading_conditional = renderUI({
    div(
      fluidRow(
        column(1),
        column(2,
               if (length(input$language_reading) != 0) {
                 awesomeCheckboxGroup(inputId="reading_input", 
                                      label="", 
                                      choices=c("School Book", "Personal Book"),
                                      selected=c())
               }
        ),
        column(9,
               br(),
               conditionalPanel(
                 condition = "input.language_reading.length == 1",
                 conditionalPanel(
                   condition = "input.reading_input.includes('School Book')",
                   uiOutput("reading_conditional_school")
                 ),
                 conditionalPanel(
                   condition = "input.reading_input.includes('Personal Book')",
                   uiOutput("reading_conditional_personal")
                 )
               )
        )
      )
    )
  })
  output$reading_conditional_personal = renderUI({
    fluidRow(
      column(2,
             br(),
             p(HTML("Name of personal book(s):"),
               class = "textblue text20"),
             
      ),
      column(3,
             textInput(inputId="personal_book_name", 
                       label="", 
                       width='100%', 
                       placeholder = "Open Script...")
      ),
      column(2,
             br(),
             p(HTML("Up to page number(s)”, :"),
               class = "textblue text20"),
             
      ),
      column(3,
             textInput(inputId="personal_book_pages", 
                       label="", 
                       width='100%', 
                       placeholder = "Open Script...")
      ),
      column(1,
             br(),
             awesomeCheckboxGroup(inputId="personal_book_finished", 
                                  label="", 
                                  choices=c("Finished?"))
      )
    )
  })
  output$reading_conditional_school = renderUI({
    fluidRow(
      column(2,
             br(),
             p(HTML("Name of school book(s):"),
               class = "textblue text20"),
             
      ),
      column(3,
             textInput(inputId="school_book_name", 
                       label="", 
                       width='100%', 
                       placeholder = "Open Script...")
      ),
      column(2,
             br(),
             p(HTML("Up to page number(s)”, :"),
               class = "textblue text20"),
             
      ),
      column(3,
             textInput(inputId="school_book_pages", 
                       label="", 
                       width='100%', 
                       placeholder = "Open Script...")
      ),
      column(1,
             br(),
             awesomeCheckboxGroup(inputId="school_book_finished", 
                                  label="", 
                                  choices=c("Finished?"))
      )
    )
  })
  
  output$comprehension_conditional = renderUI({
    if (length(input$language_comprehension) != 0) {
      fluidRow(
        column(1),
        column(3,
               awesomeCheckboxGroup(inputId="comprehension_input", 
                                    label="", 
                                    choices=c("Main Idea", 
                                              "Summarizing", 
                                              "Sequencing (Chronology)", 
                                              "Inferential", 
                                              "Critical Thinking", 
                                              "Others (define in Summary)"))
        ),
        column(4,
               p(HTML("Material:"),
                 class = "textblue text20"),
               textInput(inputId="comprehension_material", 
                         label="", 
                         width='100%', 
                         placeholder = "Open Script...")),
        column(4)
      )
    }
  })
  
  output$writing_conditional = renderUI({
    if (length(input$language_writing) != 0) {
      textInput(inputId="writing_input", 
                label="", 
                width='100%', 
                placeholder = "Open Script...")
    }
  })
  
  output$speaking_conditional = renderUI({
    if (length(input$language_speaking) != 0) {
      textInput(inputId="speaking_input", 
                label="", 
                width='100%', 
                placeholder = "Open Script...")
    }
  })
  
  output$grammar_conditional = renderUI({
    if (length(input$language_grammar) != 0) {
      textInput(inputId="grammar_input", 
                label="", 
                width='100%', 
                placeholder = "Open Script...")
    }
  })
  
  output$previous_session_language = renderUI({
    if (length(information$student) == 1) {
      data <- get_sessions("All", information$student, "Language", prev=TRUE) %>%
        mutate(across(where(is.character), ~replace(., is.na(.), "")))
    } else {
      data <- get_previous_multiple(information$student, "Language", prev=TRUE) %>%
        mutate(across(where(is.character), ~replace(., is.na(.), "")))
    }
    
    div(
      h2(HTML(paste0("<b>Summary of ", information$student, "'s Previous Session</b>")),
         class = "textblue text36",
         style = "text-align: center;"),
      br(),
      p( HTML( paste0("<b>", information$student, "'s Previous Session:</b> ", substr(as.character(data$checkout), 0, 10)) ),
         class = "textblue text20" ),
      if (nrow(data) > 0) {
        div(
          div(
            if (data$reading_input == "School Book") {
              p(HTML(paste0("<b>Reading: </b>", data$reading_input, " (", data$school_book_name, ", up to page: ", data$school_book_pages, ", ", (if (data$school_book_finished == "Finished?") "finished" else "unfinished"), ")")),
                class = "textblue text20")
            } else if (data$reading_input == "Personal Book") {
              p(HTML(paste0("<b>Reading: </b>", data$reading_input, " (", data$personal_book_name, ", up to page: ", data$personal_book_pages, ", ", (if (data$personal_book_finished == "Finished?") "finished" else "unfinished"), ")")),
                class = "textblue text20")
            } else if (grepl(" - ", data$reading_input)) {
              p(HTML(paste0("<b>Reading: </b>", 
                            "School Book", " (", data$school_book_name, ", up to page: ", data$school_book_pages, ", ", (if (data$school_book_finished == "Finished?") "finished" else "unfinished"), ")",
                            ", Personal Book", " (", data$personal_book_name, ", up to page: ", data$personal_book_pages, ", ", (if (data$personal_book_finished == "Finished?") "finished" else "unfinished"), ")")),
                class = "textblue text20")
            } else {
              p(HTML("<b>Reading:</b>"),
                class = "textblue text20")
            }
          ),
          p(HTML(paste0("<b>Comprehension: </b>", data$comprehension_input, ", Material: ", data$comprehension_material)),
            class = "textblue text20"),
          p(HTML(paste0("<b>Writing: </b>", data$writing_input)),
            class = "textblue text20"),
          p(HTML(paste0("<b>Speaking: </b>", data$speaking_input)),
            class = "textblue text20"),
          p(HTML(paste0("<b>Grammar: </b>", data$grammar_input)),
            class = "textblue text20")
        )
      },
      p(HTML(paste0("<b>Brain Book Extension: </b>", data$brain_book_input)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Brain Book Chapter: </b>", data$brain_book_chapter)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Brain Book Pages (up to): </b>", data$brain_book_pages)),
        class = "textblue text20"),
      p(HTML(paste0("<b>", information$student, "'s Pre-Session Assignment Due Today: </b>", data$language_assignment)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Goals for Today’s Session: </b>", data$goal)),
        class = "textblue text20")
    )
    
    
  })
  
  output$language_review_submission = renderUI({
    div(
      div(
        if ((length(input$reading_input) == 1) & ("School Book" %in% input$reading_input)) {
          p(HTML(paste0("<b>Reading: </b>", input$reading_input, " (", input$school_book_name, ", ", input$school_book_pages, ", ", (if (length(input$school_book_finished) != 0) "finished" else "unfinished"), ")")),
            class = "textblue text20")
        } else if ((length(input$reading_input) == 1) & ("Personal Book" %in% input$reading_input)) {
          p(HTML(paste0("<b>Reading: </b>", input$reading_input, " (", input$personal_book_name, ", ", input$personal_book_pages, ", ", (if (length(input$personal_book_finished) != 0) "finished" else "unfinished"), ")")),
            class = "textblue text20")
        } else if (length(input$reading_input) == 2) {
          p(HTML(paste0("<b>Reading: </b>", 
                        "School Book", " (", input$school_book_name, ", ", input$school_book_pages, ", ", (if (length(input$school_book_finished) != 0) "finished" else "unfinished"), ")",
                        ", Personal Book", " (", input$personal_book_name, ", ", input$personal_book_pages, ", ", (if (length(input$personal_book_finished) != 0) "finished" else "unfinished"), ")")),
            class = "textblue text20")
        } else {
          p(HTML("<b>Reading:</b>"),
            class = "textblue text20")
        }
      ),
      p(HTML(paste0("<b>Comprehension: </b>", paste(input$comprehension_input, collapse=", "), " (Material: ", input$comprehension_material, ")")),
        class = "textblue text20"),
      p(HTML(paste0("<b>Writing: </b>", input$writing_input)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Speaking: </b>", input$speaking_input)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Grammar: </b>", input$grammar_input)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Brain Book Extension: </b>", input$brain_book_input)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Brain Book Chapter: </b>", input$brain_book_chapter)),
        class = "textblue text20"),
      p(HTML(paste0("<b>Brain Book Pages (up to): </b>", input$brain_book_pages)),
        class = "textblue text20"),
      p(HTML(paste0("<b>", information$student, "'s Next Assignment: </b>", input$language_assignment)),
        class = "textblue text20")
    )
  })
  
  output$brain_book_previous = renderUI({
    
    if (length(information$student) == 1) {
      data <- get_sessions("All", information$student, "Language")
    } else {
      data <- get_previous_multiple(information$student, "Language")
    }
    
    div(
      h2(HTML("<b>Brain Book - Spaced Repetition</b>")),
      br(),
      
      fluidRow(style = "position: relative; bottom: 30px;",
               column(4,
                      br(),
                      p(HTML("<b>1. Beginner's Wonderland</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial1", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial1)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter1",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter1),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 60px;",
               column(4,
                      br(),
                      p(HTML("<b>2. From Earth to Space</b>"),
                        style = "font-size: 20px; color: #ce6e01;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial2", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial2)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter2",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter2),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 90px;",
               column(4,
                      br(),
                      p(HTML("<b>3. Journey to the Woods</b>"),
                        style = "font-size: 20px; color: #997954;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial3", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial3)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter3",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter3),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 120px;",
               column(4,
                      br(),
                      p(HTML("<b>4. Mix It Up</b>"),
                        style = "font-size: 20px; color: #00ae3c;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial4", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial4)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter4",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter4),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 150px;",
               column(4,
                      br(),
                      p(HTML("<b>5. Nature's Beauty</b>"),
                        style = "font-size: 20px; color: #215c9a;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial5", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial5)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter5",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter5),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 180px;",
               column(4,
                      br(),
                      p(HTML("<b>6. Mysteries of Water</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial6", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial6)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter6",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter6),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 210px;",
               column(4,
                      br(),
                      p(HTML("<b>7. Urban Life</b>"),
                        style = "font-size: 20px; color: #ce6e01;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial7", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial7)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter7",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter7),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 240px;",
               column(4,
                      br(),
                      p(HTML("<b>8. Animal Kingdom</b>"),
                        style = "font-size: 20px; color: #997954;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial8", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial8)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter8",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter8),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 270px;",
               column(4,
                      br(),
                      p(HTML("<b>9. In The Kitchen</b>"),
                        style = "font-size: 20px; color: #00ae3c;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial9", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial9)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter9",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter9),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 300px;",
               column(4,
                      br(),
                      p(HTML("<b>10. Seasons And Weather</b>"),
                        style = "font-size: 20px; color: #215c9a;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial10", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial10)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter10",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter10),
                        justified = TRUE))
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 330px;",
               column(4,
                      br(),
                      p(HTML("<b>11. At School</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      disabled(checkboxGroupButtons(inputId="disabled_initial11", 
                                                    label="", 
                                                    choices=c("Initial Session"),
                                                    selected=(if ("Initial Session" %in% unique(data$initial11)) c("Initial Session") else c())))
               ),
               column(6,
                      disabled(checkboxGroupButtons(
                        inputId = "disabled_chapter11",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter11),
                        justified = TRUE))
               )
      )
      
    )
    
  })
  
  output$brain_book_current = renderUI({
    
    if (length(information$student) == 1) {
      data <- get_sessions("All", information$student, "Language")
    } else {
      data <- get_previous_multiple(information$student, "Language")
    }
    
    div(
      h2(HTML("<b>Brain Book - Spaced Repetition</b>")),
      br(),
      
      fluidRow(style = "position: relative; bottom: 30px;",
               column(4,
                      br(),
                      p(HTML("<b>1. Beginner's Wonderland</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial1", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial1)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter1",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter1),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 60px;",
               column(4,
                      br(),
                      p(HTML("<b>2. From Earth to Space</b>"),
                        style = "font-size: 20px; color: #ce6e01;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial2", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial2)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter2",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter2),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 90px;",
               column(4,
                      br(),
                      p(HTML("<b>3. Journey to the Woods</b>"),
                        style = "font-size: 20px; color: #997954;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial3", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial3)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter3",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter3),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 120px;",
               column(4,
                      br(),
                      p(HTML("<b>4. Mix It Up</b>"),
                        style = "font-size: 20px; color: #00ae3c;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial4", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial4)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter4",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter4),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 150px;",
               column(4,
                      br(),
                      p(HTML("<b>5. Nature's Beauty</b>"),
                        style = "font-size: 20px; color: #215c9a;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial5", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial5)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter5",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter5),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 180px;",
               column(4,
                      br(),
                      p(HTML("<b>6. Mysteries of Water</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial6", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial6)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter6",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter6),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 210px;",
               column(4,
                      br(),
                      p(HTML("<b>7. Urban Life</b>"),
                        style = "font-size: 20px; color: #ce6e01;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial7", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial7)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter7",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter7),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 240px;",
               column(4,
                      br(),
                      p(HTML("<b>8. Animal Kingdom</b>"),
                        style = "font-size: 20px; color: #997954;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial8", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial8)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter8",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter8),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 270px;",
               column(4,
                      br(),
                      p(HTML("<b>9. In The Kitchen</b>"),
                        style = "font-size: 20px; color: #00ae3c;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial9", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial9)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter9",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter9),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 300px;",
               column(4,
                      br(),
                      p(HTML("<b>10. Seasons And Weather</b>"),
                        style = "font-size: 20px; color: #215c9a;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial10", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial10)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter10",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter10),
                        justified = TRUE)
               )
      ),
      
      fluidRow(style = "position: relative; bottom: 330px;",
               column(4,
                      br(),
                      p(HTML("<b>11. At School</b>"),
                        style = "font-size: 20px; color: #a1136f;")
               ),
               column(2,
                      checkboxGroupButtons(inputId="initial11", 
                                           label="", 
                                           choices=c("Initial Session"),
                                           selected=(if ("Initial Session" %in% unique(data$initial11)) c("Initial Session") else c()))
               ),
               column(6,
                      checkboxGroupButtons(
                        inputId = "chapter11",
                        label = "",
                        choices = c("Practice 1", 
                                    "Practice 2",
                                    "Practice 3"),
                        selected = chapter_helper(data$chapter11),
                        justified = TRUE)
               )
      )
      
    )
    
  })
  
  output$today_summary = renderUI({
    p(class = "textblue text36", HTML(paste0("<b>Summary of ", information$student, "'s Session Today</b>")),
      style = "text-align: center;")
  })
  
  output$your_submission = renderUI({
    h3( paste0("Your Submission for ", information$student) )
  })
  
  output$student_review_checkout_title = renderUI({
    h3( paste0("Review ", information$student, "'s Checkout") )
  })
  
  output$export_session_log = downloadHandler(
    filename = "export_session_log.csv",
    content = function(file) {
      write.csv(get_sessions(input$coach_dash, input$student_dash, input$subject_dash, 
                             s=input$session_dash, e=input$entrance_dash, 
                             select_grades=input$grade_dash) %>%
                  session_log(range=T), 
                file, 
                row.names=F)
    }
  )
  output$export_cld = downloadHandler(
    filename = "export_cld.csv",
    content = function(file) {
      write.csv(get_sessions(input$coach_dash, input$student_dash, input$subject_dash, 
                             s=input$session_dash, e=input$entrance_dash, 
                             select_grades=input$grade_dash) %>%
                  cld_log(), 
                file, 
                row.names=F)
    }
  )
  
  observeEvent(input$back_science, {
    rv$page <- 2
  })
  
  observeEvent(input$back_math, {
    rv$page <- 2
  })
  
  observeEvent(input$checkout_back, {
    rv$page <- 3
  })
  
  observeEvent(input$back_language, {
    rv$page <- 2
  })
  
  observeEvent(input$checkout_back_language, {
    rv$page <- 3
  })

  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
    
}

shinyApp(ui, server)

