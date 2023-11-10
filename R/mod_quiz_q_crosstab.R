#' mod_crosstab_questionviz UI Function
#'
#' @description A shiny Module for the "Crosstab" tab with the same output as "quiz_questionviz."
#'
#' @param id, input, output, session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
mod_quiz_crosstab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("crosstab_question1"),
          "Question 1",
          choices = c(""),
          selected = NULL
        ),
        shiny::selectInput(
          ns("crosstab_question2"),
          "Question 2",
          choices = c(""),
          selected = NULL
      ),
            ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(
            outputId = ns("crosstab_table")
          )
        )
      )
    )
  )
}


#' mod_crosstab_questionviz Server Functions
#'
#' @noRd
#' @import highcharter dplyr
mod_quiz_crosstab_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
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
    #
    # #one block of code no error so far
    answers <- shiny::reactive({
      tryCatch({
        answers_temp <- quiz_processed() %>%
          mutate(!!input$crosstab_question1 := as.character(.data[[input$crosstab_question1]])) %>%
          mutate(!!input$crosstab_question1 := case_when(
            is.na(.data[[input$crosstab_question1]]) ~ "No answer",
            TRUE ~ .data[[input$crosstab_question1]],
          )) %>%
          group_by(.data[[input$crosstab_question1]]) %>%
          summarize(n = n()) %>%
          ungroup()


        answers_temp <- answers_temp |>
          pull(.data[[input$crosstab_question1]])

        c("No correct answer", answers_temp)
      }, error= function(e){
        NULL
      })
    })

    #second block of code / problem could not find function "quiz_processed"

# shiny::observeEvent(quiz_processed(), {
#   shiny::updateSelectInput(
#     session,
#     inputId = "crosstab_question1",
#     choices = questions(),
#     selected = questions()[1]
#   )
# })

    # shiny::observeEvent(quiz_processed(), {
    #   shiny::updateSelectInput(
    #     session,
    #     inputId = "crosstab_question1",
    #     choices = questions(),
    #     selected = questions()[1]
    #   )
    # })

    # shiny::observeEvent(quiz_processed(), {
    #   shiny::updateSelectInput(
    #     session,
    #     inputId = "crosstab_question2",
    #     choices = questions(),
    #     selected = questions()[1]
    #   )
    # })

    #Third block argument "name" is missing, with no default

    # output$crosstab_table <- DT::renderDataTable({
    #   shiny::req(quiz_processed())
    #   shiny::validate(
    #     shiny::need(
    #       !is.null(quiz_processed()),
    #       message = "Quiz was not uploaded correctly. Check your inputs."
    #     )
    #   )
    #   tryCatch({
    #     question_data <- answers()
    #     datatable(question_data, options = list(pageLength = 10))  # Adjust options as needed
    #   }, error = function(e){
    #     DT::datatable(data.frame())  # Return an empty table in case of an error
    #
    #
    #   }, error = function(e){
    #     NULL
    #   })
    # })
  })
}
