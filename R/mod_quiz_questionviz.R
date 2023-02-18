#' quiz_questionviz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
mod_quiz_questionviz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("quizviz_type"),
          "Type of Viz",
          choices = "",
          selected = ""
        ),
        shiny::uiOutput(ns("more_controls")),
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          highcharter::highchartOutput(
          outputId = ns("quizviz_graph")
          )
        )
      )
    )
  )
}


#' quiz_questionviz Server Functions
#'
#' @noRd
#' @import highcharter dplyr
mod_quiz_questionviz_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
  stopifnot(is.reactive(main_inputs$roster))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$more_controls <- shiny::renderUI({
      get_quizviz_dynamic_ui(input$quizviz_type, ns)
    })

    questions <- shiny::reactive({
      tryCatch({
        questions = get_questions_from_quiz(quiz_processed())
      }, error = function(e){
        NULL
      })
    })

    answers <- shiny::reactive({
      tryCatch({
        answers_temp <- quiz_processed() %>%
          mutate(!!input$quizviz_question := as.character(.data[[input$quizviz_question]])) %>%
          mutate(!!input$quizviz_question := case_when(
            is.na(.data[[input$quizviz_question]]) ~ "No answer",
            TRUE ~ .data[[input$quizviz_question]],
          )) %>%
          group_by(.data[[input$quizviz_question]]) %>%
          summarize(n = n()) %>%
          ungroup() %>%
          arrange(desc(n)) %>%
          pull(.data[[input$quizviz_question]])

        c("", answers_temp)
      }, error= function(e){
        NULL
      })

    })

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_type",
        choices = c(
          "",
          "Multiple Choice (single)",
          "Multiple Choice (multiple)",
          "Prediction"
        ),
        selected = ""
      )
    })

    shiny::observeEvent(input$quizviz_type, {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question",
        choices = questions(),
        selected = questions()[1]
      )
    })

    shiny::observeEvent(answers(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_correctanswer",
        choices = answers(),
        selected = answers()[1]
      )
    })

    output$quizviz_graph <- highcharter::renderHighchart({
      shiny::req(quiz_processed(), input$quizviz_type)
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )
      tryCatch({
      #Render chart depending on quizviz_type
      switch(input$quizviz_type,
        "Multiple Choice (single)" = chart_multiplechoise_single(
          quiz = quiz_processed(),
          question = input$quizviz_question,
          correct_answer = input$quizviz_correctanswer,
          arrange_by_frequency = input$quizviz_arrange_by_frequency
        ),
        "Multiple Choice (multiple)" = chart_multiplechoise_multiple(
          quiz_processed(),
          input$quizviz_question
        ),
        "Prediction" = chart_prediction(
          quiz = quiz_processed(),
          question = input$quizviz_question,
          correct_answer = input$quizviz_correctanswer,
          arrange_by_frequency = input$quizviz_arrange_by_frequency
        ),
      )
      }, error = function(e){
        NULL
      })
    })
  })
}
