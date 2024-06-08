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
                      "3 Misconceptions", "4 Misconceptions"),
          selected = "Main ideas"
        ),
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

    ai_response <- shiny::eventReactive(input$generate_analysis, {
      shinyjs::disable("generate_analysis")
      tryCatch({
        answers_concat <- quiz_processed() %>%
          dplyr::mutate(name_answer = paste(.data[[id_colname()]], .data[[input$quizviz_question]], sep = ": ")) %>%
          dplyr::pull(name_answer) %>%
          paste(collapse = " • ")

        client <- openai::OpenAI()
        completion <- client$chat$completions$create(
          model = "gpt-4o",
          messages = list(
            list(
              "role" = "system",
              "content" = paste(
                "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class to identify main concepts in their answers and list the students who contributed to each concept.
            You will do the following:
            1. Summarize the ", input$quizviz_analysis, " expressed by the students, and list up to five students who contributed to each point.

            I will provide the question and the students' answers. The students' answers will be provided as follow:
            [Sudent 1 Name : Student 1 Answer • Student 2 Name: Student 2 Answer • ...]

            Format your response strictly as follows:
            <b>Main ideas:</b><br>1. Idea 1 (<i>Student 1 Name; Student 2 Name; ...; Student 5 Name</i>)<br><br>
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



    output$analysis_text <- renderUI({
      shiny::req(ai_response())
      HTML(ai_response())
    })





  })
}
