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
          ns("quizviz_question"),
          "Question 1",
          choices = c(""),
          selected = NULL
        ),
        shiny::selectInput(
          ns("quizviz_question2"),
          "Question 2",
          choices = c(""),
          selected = NULL
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("table_percentage"),
          label = "Show as percentage",
          status = "info",
          fill = TRUE,
          value = TRUE
        ),
        # shinyWidgets::prettySwitch(
        #   inputId = ns("weighted_average"),
        #   label = "Weighted Avg. for Q1",
        #   status = "info",
        #   fill = TRUE,
        #   value = FALSE
        # ),
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
#'
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
        mutate(!!input$quizviz_question := as.character(.data[[input$quizviz_question]])) %>%
        mutate(!!input$quizviz_question := case_when(
          is.na(.data[[input$quizviz_question]]) ~ "No answer",
          TRUE ~ .data[[input$quizviz_question]],
        )) %>%
        group_by(.data[[input$quizviz_question]]) %>%
        summarize(n = n()) %>%
        ungroup()

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
        choices = unique(questions()),
        selected = unique(questions())[1]
      )
    })

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question2",
        choices = unique(questions()),
        selected = unique(questions())[1]
      )
    })



    output$crosstab_table <- DT::renderDT({
      shiny::req(quiz_processed())
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )

      tryCatch({
        quiz = quiz_processed()
        question1 = input$quizviz_question
        question2 = input$quizviz_question2
        tablepercentage = input$table_percentage
        weighted_avg = input$weighted_average


          # Create a contingency table with table function
          crosstab_table <- table(quiz_processed()[[question1]], quiz_processed()[[question2]])

          # Optionally, calculate percentages
          if (tablepercentage) {
            crosstab_table <-  round(prop.table(crosstab_table),2)*100
          }

          # Optionally, calculate weighted average for Question 1


           # Add row and column totals
          crosstab_table <- addmargins(crosstab_table)

          # Convert the table to a data frame
          crosstab_df <- as.data.frame.matrix(crosstab_table)
          colnames(crosstab_df)[ncol(crosstab_df)] <- "Total"
          rownames(crosstab_df)[nrow(crosstab_df)] <- "Total"

                   # Render the datatable
          DT::datatable(crosstab_df, extensions = 'Buttons',
                        options = list(
                          pageLength = 10,
                          dom = 'tB',
                          buttons = c('copy', 'csv', 'excel', 'pdf' )
                          )
                        )  # Adjust options as needed
        }, error = function(e) {
          DT::datatable(data.frame())  # Return an empty table in case of an error
        })

    })
  })
}
