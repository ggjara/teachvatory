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
mod_quiz_prediction_ui <- function(id) {
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
        shiny::textInput(
          ns("quizviz_correctanswer"),
          label = "Enter a reference point (if one exists)",
          value = "None"
        )
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
mod_quiz_prediction_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
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
          ungroup()
        if(input$quizviz_arrange_by_frequency){
          answers_temp <- answers_temp |>
            arrange(desc(n))
        }

        answers_temp <- answers_temp |>
          pull(.data[[input$quizviz_question]])

        c("No correct answer", answers_temp)
      }, error= function(e){
        NULL
      })

    })

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question",
        choices = questions(),
        selected = questions()[1]
      )
    })

    output$quizviz_graph <- highcharter::renderHighchart({
      shiny::req(quiz_processed())
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )
      tryCatch({
        quiz = quiz_processed()
        question = input$quizviz_question
        correct_answer = input$quizviz_correctanswer

        if (!is.na(correct_answer)) {
          quiz %>%
            pull(question) %>%
            unlist() %>%
            as.numeric() %>%
            hchart() %>%
            hc_xAxis(plotLines = list(
              list(color = "#FF5733",
                   dashStyle = "Solid",
                   width = 3,
                   value = correct_answer, zIndex = 10)))
        } else {
          quiz %>%
            pull(question) %>%
            unlist() %>%
            as.numeric() %>%
            hchart()
        }
      }, error = function(e){
        NULL
      })
    })
  })
}
