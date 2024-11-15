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
          "Question 1 (Rows)",
          choices = c(""),
          selected = NULL
        ),
        shiny::selectInput(
          ns("quizviz_question2"),
          "Question 2 (Col.)",
          choices = c(""),
          selected = NULL
        ),
          shiny::selectInput(
          ns("display_type"),
          "Display Type",
          choices = c("Show as count", "Percentage (total)", "Percentage (rows)", "Percentage (columns)", "Weighted Average for Q2"),
          selected = "Show as count"
        )
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
           DT::dataTableOutput(
           outputId = ns("crosstab_table"))
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
        display_type = input$display_type

        # Create a contingency table or calculation based on display type
        if (display_type == "Weighted Average for Q2") {
          # Ensure Question 2 can be treated as numeric
          if (!is.numeric(quiz[[question2]])) {
            quiz[[question2]] <- as.numeric(as.character(quiz[[question2]]))
          }

          # Calculate weighted average of Q2 for each category of Q1
          weighted_averages <- quiz %>%
            dplyr::group_by(.data[[question1]]) %>%
            dplyr::summarize(Average = mean(.data[[question2]], na.rm = TRUE), .groups = 'drop') %>%
            dplyr::mutate(Average = round(Average, 2))  # Round to two decimal places

          # Convert the result to a datatable, removing the default row names
          crosstab_df <- as.data.frame(weighted_averages, row.names = FALSE)


        } else {
          # Other existing calculations for contingency tables
          crosstab_table <- table(quiz[[question1]], quiz[[question2]],
                                  dnn = c(input$quizviz_question, input$quizviz_question2))

          if (display_type == "Percentage (total)") {
            crosstab_table <- round(prop.table(crosstab_table), 2) * 100
            crosstab_table <- addmargins(crosstab_table)
          } else if (display_type == "Percentage (rows)") {
            crosstab_table <- round(prop.table(crosstab_table, 1), 2) * 100
            crosstab_table <- cbind(crosstab_table, Total = 100)
          } else if (display_type == "Percentage (columns)") {
            crosstab_table <- round(prop.table(crosstab_table, 2), 2) * 100
            crosstab_table <- rbind(crosstab_table, Total = 100)
          } else {
            crosstab_table <- addmargins(crosstab_table)
          }

          # Apply rounding to one decimal place and convert to data frame
          if (display_type != "Weighted Average for Q2") {
            crosstab_table <- round(crosstab_table, 1)
          }
          crosstab_df <- as.data.frame.matrix(crosstab_table)
          if (display_type != "Percentage (columns)") {
            colnames(crosstab_df)[ncol(crosstab_df)] <- "Total"
          }
          if (display_type != "Percentage (rows)") {
            rownames(crosstab_df)[nrow(crosstab_df)] <- "Total"
          }

          if (display_type %in% c("Percentage (total)", "Percentage (rows)", "Percentage (columns)")) {
            numeric_cols <- sapply(crosstab_df, is.numeric)
            crosstab_df[, numeric_cols] <- lapply(crosstab_df[, numeric_cols], function(x) paste0(x, "%"))
          }

        }

        # Render the datatable
        if (display_type == "Weighted Average for Q2") {
          DT::datatable(crosstab_df, extensions = 'Buttons', rownames = FALSE,
                      options = list(
                        pageLength = 10,
                        dom = 'tB',
                        buttons = c('copy', 'csv', 'excel', 'pdf')
                      )
        )
          } else {
            DT::datatable(crosstab_df, extensions = 'Buttons',
                          options = list(
                            pageLength = 10,
                            dom = 'tB',
                            buttons = c('copy', 'csv', 'excel', 'pdf')
                          )
            )
          }
        # Adjust options as needed
      }, error = function(e) {
        DT::datatable(data.frame())  # Return an empty table in case of an error
      })

    })

  })
}
