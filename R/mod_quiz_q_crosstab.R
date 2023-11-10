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
          ns("crosstab_question"),
          "Question",
          choices = c(""),
          selected = NULL
        ),
        shiny::textInput(
          ns("crosstab_correctanswer"),
          label = "Enter a reference point (if one exists)",
          value = "None"
        ),
       shinyWidgets::prettySwitch(
          inputId = ns("crosstab_axis_XRange"),
          label = "X axis: set range 0-100",
          status = "info",
          fill = TRUE,
          value = TRUE
        )
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          highcharter::highchartOutput(
            outputId = ns("crosstab_graph")
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

    answers <- shiny::reactive({
      tryCatch({
        answers_temp <- quiz_processed() %>%
          mutate(!!input$crosstab_question := as.character(.data[[input$crosstab_question]])) %>%
          mutate(!!input$crosstab_question := case_when(
            is.na(.data[[input$crosstab_question]]) ~ "No answer",
            TRUE ~ .data[[input$crosstab_question]],
          )) %>%
          group_by(.data[[input$crosstab_question]]) %>%
          summarize(n = n()) %>%
          ungroup()
        if(input$crosstab_arrange_by_frequency){
          answers_temp <- answers_temp |>
            arrange(desc(n))
        }

        answers_temp <- answers_temp |>
          pull(.data[[input$crosstab_question]])

        c("No correct answer", answers_temp)
      }, error= function(e){
        NULL
      })
    })

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "crosstab_question",
        choices = questions(),
        selected = questions()[1]
      )
    })

    output$crosstab_graph <- highcharter::renderHighchart({
      shiny::req(quiz_processed())
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )
      tryCatch({
        quiz = quiz_processed()
        question = input$crosstab_question
        correct_answer = input$crosstab_correctanswer
        axispercentage = input$crosstab_axis_percentage
        axisXRange = input$crosstab_axis_XRange
        question_title <- ifelse(nchar(question) > 200,
                                 paste0(substr(question, 1, 200), "..."),
                                 question)

        # Code for predictions without percentages

          if (!is.na(correct_answer)) {
            quiz %>%
              pull(question) %>%
              unlist() %>%
              as.numeric() %>%
              hchart() %>%
              hc_legend(enabled = FALSE) %>%
              hc_yAxis(title = list(text = "Number of respondents")) %>%
              hc_xAxis(title = list(text = "Probability")) %>%
              hc_xAxis(labels = list(style = list(fontSize = "13px")) ) %>%
              hc_yAxis(labels = list(style = list(fontSize = "13px")) ) %>%
              hc_title(text = question_title, style = list(fontSize = "15px"))  %>%
              hc_xAxis(plotLines = list(
                list(
                  color = if (correct_answer != "") "#FF5733" else "#FFFFFF",
                  dashStyle = "Solid",
                  width = if (correct_answer != "") 3 else 0,
                  value = correct_answer,
                  zIndex = 10
                )
              )) %>%
              hc_xAxis(
                min = if (axisXRange) 0 else NULL, # Set min to 0 if axisXRange is TRUE
                max = if (axisXRange) 100 else NULL
              ) %>%
              hc_exporting(
                enabled = TRUE, # always enabled
                filename = paste0("viz_", substr(question, 1, 20))
              ) %>%
              hc_chart(
                backgroundColor = "#FFFFFF"
              ) # Set the background color to white
          } else {
            quiz %>%
              pull(question) %>%
              unlist() %>%
              as.numeric() %>%
              hchart() %>%
              hc_legend(enabled = FALSE)
        }
      }, error = function(e){
        NULL
      })
    })
  })
}
