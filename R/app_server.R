#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Polished Server-Side
  observeEvent(input$sign_out, {
    sign_out_from_shiny()
    session$reload()
  })


  output$user_panel <- renderUser({
    dashboardUser(
      name = stringr::str_replace(session$userData$user()$email, "@.*", ""),
      image = "https://www.hks.harvard.edu/sites/default/files/styles/employee_grayscale/public/bio_images/7008-1661999445.jpg",
      # image = "https://adminlte.io/themes/AdminLTE/dist/img/user2-160x160.jpg",
      title = session$userData$user()$email,
      subtitle = if (session$userData$user()$is_admin) "Admin" else "User",
      # footer = p("The footer", class = "text-center"),
      dashboardUserItem(
        width = 12,
        tagList(
          bs4Dash::actionButton(
            inputId = "sign_out",
            label = "Sign Out",
            icon = icon("sign-out-alt"),
            # width = "100px",
            size = "sm",
            status = "danger"
          ),
          if (session$userData$user()$is_admin) {
            bs4Dash::actionButton(
              inputId = NS("polished", "go_to_admin_panel"),
              label = "Admin panel",
              # icon = icon("sign-out-alt"),
              # width = "100px",
              size = "sm",
              status = "secondary"
            )
          }
        )
      )
    )
  })

  # Initialize waiter when loading course
  w <- waiter::Waiter$new(
    html = shiny::tagList(
      waiter::spin_pixel(),
      shiny::tags$br(),
      shiny::h5("Loading course...")
    ),
    color = "#343a40",
    fadeout = 500
  )

  ####### Reactive Values  #######
  selected_course <-
    shiny::reactive(shiny::req(input$filter_course))
  selected_roster <-
    shiny::reactive(shiny::req(input$filter_roster))


  # Get courses dribble
  courses <- shiny::reactive({
    req(input$path_dashboard)
    courses_temp <- get_courses(input$path_dashboard)
    arrange(courses_temp, name)
  })
  # Get spreadsheets of course's folder (googledrive library)
  course_directory <- shiny::reactive({
    get_course_directory(courses(), selected_course())
  })

  # Get sheets names of the selected roster
  roster_sheetnames <- shiny::reactive({
    get_sheetnames(course_directory(), selected_roster())
  })

  # Load Masterquiz
  # Returns masterquiz Sheet metadata
  masterquiz_md <- shiny::eventReactive(input$load_course, {
    shiny::req(input$filter_masterquiz)
    w$show()
    on.exit({
      w$hide()
    })
    get_metadata(course_directory(), input$filter_masterquiz)
  })

  shinyjs::disable("load_course")
  shiny::observeEvent(input$filter_roster_sheet,
    {
      if (input$filter_roster_sheet == "") {
        shinyjs::disable("load_course")
      } else {
        shinyjs::enable("load_course")
      }
    },
    ignoreNULL = FALSE
  )


  # Load Roster file + using roster sheet_name
  # Returns tibble() with the Roster dataframe
  roster <- shiny::eventReactive(input$load_course, {
    shiny::req(input$filter_roster, input$filter_roster_sheet)
    w$show()
    on.exit({
      w$hide()
    })
    get_roster(
      course_directory(),
      input$filter_roster,
      input$filter_roster_sheet
    )
  })
  ####### End Reactive Values  #######

  ####### Updates #######
  shiny::observeEvent(courses(), {
    shiny::updateSelectInput(
      session,
      inputId = "filter_course",
      choices = courses()$name,
      selected = courses()$name[1]
    )
  })

  # Update masterquiz and roster file names choices in filters
  # Convention: MASTER SHEET, and ROSTER strings in their filenames
  shiny::observeEvent(course_directory(), {
    shiny::updateSelectInput(
      session,
      inputId = "filter_masterquiz",
      choices = course_directory()$name,
      selected = course_directory()$name[stringr::str_detect(toupper(course_directory()$name), "MASTER SHEET")][1]
    )
    shiny::updateSelectInput(
      session,
      inputId = "filter_roster",
      choices = course_directory()$name,
      selected = course_directory()$name[stringr::str_detect(toupper(course_directory()$name), "ROSTER")][1]
    )
  })
  shiny::observeEvent(course_directory(), {
    shiny::updateSelectInput(
      session,
      inputId = "filter_roster_sheet",
      choices = roster_sheetnames(),
      selected = roster_sheetnames()[stringr::str_detect(toupper(roster_sheetnames()), "ROSTER")][1]
    )
  })

  # Update roster's sheet names options
  # Convention: roster string in the sheet name
  shiny::observeEvent(roster_sheetnames(), {
    shiny::updateSelectInput(
      session,
      inputId = "filter_roster_sheet",
      choices = roster_sheetnames(),
      selected = roster_sheetnames()[stringr::str_detect(toupper(roster_sheetnames()), "ROSTER")][1]
    )
  })

  # Hide controlbar after loading course
  shiny::observeEvent(input$load_course, {
    bs4Dash::updateControlbar(id = "controlbar", session = session)
  })
  ####### End Updates #######

  ####### Render #######
  # Render text in sidebar
  output$sidebar_intro <- shiny::renderText({
    shiny::req(roster())
    paste0("Analyzing ", selected_course())
  })

  ####### End Render #######

  ####### Call Modules  #######
  # Create global variables list to pass to modules
  main_inputs <- list(
    selected_course = selected_course,
    masterquiz_md = masterquiz_md,
    roster = roster
  )
  quiz_processed <- mod_quiz_server("quiz_1", FALSE, main_inputs = main_inputs)
  # Call the submodule with the quiz processed.
  # The quiz processed is a reactive value of the quiz module (see last line of code of mod_quiz.R)
  mod_quiz_multipleChoiceSingle_server("quiz_multipleChoiceSingle_1", FALSE, main_inputs, quiz_processed)
  mod_quiz_prediction_server("quiz_prediction_1", FALSE, main_inputs, quiz_processed)
  mod_metrics_server("metrics_1", FALSE, main_inputs = main_inputs)
  mod_roster_server("roster_1", FALSE, main_inputs = main_inputs)
}
