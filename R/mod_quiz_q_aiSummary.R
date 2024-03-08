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
          ns("quizviz_question"),
          "Question",
          choices = c(""),
          selected = NULL
        ),
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          "asd"
        )
      )
    )
  )
}


#' mod_quiz_q_aiSummary Server Functions
#'
#' @noRd
#' @import highcharter dplyr
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
  })
}
