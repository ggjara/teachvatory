#' mod_quiz_q_aiSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
mod_quiz_aiSummary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("quizviz_analysis"),
          "Analysis",
          choices = c("3 Main ideas", "4 Main Ideas", "5 Main Ideas",
                      "3 Misconceptions", "4 Misconceptions", "Other Feature"),
          selected = "Main ideas"
        ), shiny::uiOutput(ns("otherFeatureInput")),
        shiny::selectInput(
          ns("quizviz_question"),
          "Question",
          choices = c(""),
          selected = NULL
        ),
        # Submit button
        bs4Dash::actionButton(
          inputId = ns("generate_analysis"),
          label = "Generate analysis",
          status = "primary"
        )
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          shiny::htmlOutput(
            ns("analysis_text"),
          )
        )
      )
    )
  )
}


#' mod_quiz_q_aiSummary Server Functions
#'
#' @noRd
mod_quiz_aiSummary_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
  stopifnot(is.reactive(main_inputs$roster))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    questions <- shiny::reactive({
      tryCatch({
        questions = get_questions_from_quiz(quiz_processed())
      }, error = function(e){
        NULL
      })
    })

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question",
        choices = unique(questions()),
        selected = unique(questions())[1]
      )
    })

    # Get colname of "Your Name" input. If doesn't exist, return ""
    id_colname <- shiny::reactive({
      get_idcolname(quiz_processed())
    })

#changing to names to Firstname_Last Name initial - didnt work - have to tweak a bit
   # id_colname <- shiny::reactive({
      # Get the original column with full names
   #   names_col <- get_idcolname(quiz_processed())

      # Function to convert each name
     # convert_name <- function(name) {
        # Split the name by comma and trim any whitespace
      #  parts <- trimws(strsplit(name, ",")[[1]])

        # Extract the last and first names
     #   last_name <- parts[1]
     #   first_name <- parts[2]

        # Construct the new name format: First Name + First initial of Last Name
      #  paste0(first_name, " ", substr(last_name, 1, 1))
    #  }

      # Apply the function to each name in the column
   #   sapply(names_col, convert_name)
  #  })

    ai_response <- shiny::eventReactive(input$generate_analysis, {
      shinyjs::disable("generate_analysis")
      tryCatch({
        answers_concat <- quiz_processed() %>%
          dplyr::mutate(name_answer = paste(.data[[id_colname()]], .data[[input$quizviz_question]], sep = ": ")) %>%
          dplyr::pull(name_answer) %>%
          paste(collapse = " • ")

        client <- openai::OpenAI()

        analysis_text <- if (input$quizviz_analysis == "Other Feature" && input$otherFeatureText != "") {
          # Use input from otherFeatureText if 'Other Feature' is selected and the text input is not empty
          input$otherFeatureText
        } else {
          # The standard text to be used if 'Other Feature' is not selected
          paste("Summarize the ", input$quizviz_analysis, " expressed by the students, and list up to five students who contributed to each point.")
        }

        completion <- client$chat$completions$create(
          model = "gpt-4-0125-preview",
          messages = list(
            list(
              "role" = "system",
              "content" = paste(
                "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class to identify main concepts in their answers and list the students who contributed to each concept.
            You will do the following:
            1. Summarize the ", analysis_text, " expressed by the students, and list up to five students who contributed to each point.

            I will provide the question and the students' answers. The students' answers will be provided as follow:
            [Sudent 1 Last Name, Student 1 First Name : Student 1 Answer • Sudent 2 Last Name, Student 2 First Name: Student 2 Answer • ...]

            Please be sure that you limit to 5 students.
            Format your response strictly as follows:
            <b><u>Main ideas:</b></u> <br> <b>1. Idea 1</b> (<i>Student 1 First Name, Student 1 first letter of Last Name; Student 2 First Name, Student 2 first letter of Last Name; ...; Student 5 First Name, Student 5 first letter of Last Name</i>)<br><br>
            ")
            ),
            list(
              "role" = "user",
              "content" = paste0("
            Question: ", input$quizviz_question,"

            Answers:", answers_concat)
            )
          )
        )
        shinyjs::enable("generate_analysis")
        completion$choices[[1]]$message$content

        }, error = function(e){
          "There was an error with your request. See the logs for more information."
          shinyjs::enable("generate_analysis")
        })
    })

    # Observe the selection in the dropdown
    observeEvent(input$quizviz_analysis, {
      # Check if 'Other feature' is selected
      if(input$quizviz_analysis == "Other Feature") {
        # Display the text input
        output$otherFeatureInput <- renderUI({
          shiny::textInput(ns("otherFeatureText"), "Specify your feature:")
        })
      } else {
        # Hide the text input when 'Other feature' is not selected
        output$otherFeatureInput <- renderUI({})
      }
    })

    output$analysis_text <- renderUI({
      shiny::req(ai_response())
      HTML(ai_response())
    })




  })
}
