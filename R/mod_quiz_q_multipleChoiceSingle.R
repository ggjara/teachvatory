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
mod_quiz_multipleChoiceSingle_ui <- function(id) {
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
        shiny::selectInput(
          ns("quizviz_correctanswer"),
          "Correct Answer",
          choices = c(""),
          selected = NULL
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_show_percentage"),
          label = "Show percentage",
          status = "info",
          fill = TRUE,
          value = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_arrange_by_frequency"),
          label = "Sort by frequency",
          status = "info",
          fill = TRUE
        ),
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
mod_quiz_multipleChoiceSingle_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
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
          summarize(n = n(), .groups = "drop") %>%
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
        choices = unique(questions()),
        selected = unique(questions())[1]
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
        arrange_by_frequency = input$quizviz_arrange_by_frequency
        show_percentage = input$quizviz_show_percentage
        char_extra = ""
        if(show_percentage){
          char_extra="%"
        }

        chart <- quiz %>%
          mutate(!!question := as.character(.data[[question]])) %>%
          mutate(!!question := case_when(
            is.na(.data[[question]]) ~ "No answer",
            TRUE ~ .data[[question]],
          ))  %>%
          group_by(.data[[question]]) %>%
          summarize(n = n(), .groups = "drop") %>%
          ungroup()

        # Check the number of categories
        if (n_distinct(chart[[question]]) > 5) {
          highchart() |>
            hc_title(text = "Question is not categorical. Plot cannot be made.")
        } else {
          if (show_percentage) {
            chart <- chart %>%
              mutate(n = round(n * 100 / sum(n, na.rm = TRUE), 2))
          }

          if (arrange_by_frequency) {
            chart <- chart %>%
              arrange(desc(n))
          }

        chart <- chart |>
          mutate(correct = .data[[question]] == correct_answer) |>
          mutate(n_correct = case_when(
            correct ~ n,
            TRUE ~ NA_integer_)
          ) |>
          mutate(n_incorrect = case_when(
            !correct ~ n,
            TRUE ~ NA_integer_
          ))

        chart <- chart %>%
          mutate(!!question := factor(.data[[question]], levels = unique(chart[[question]])))

        question_title <- ifelse(
          nchar(question)>200,
          paste0(substr(question, 1, 200), "..."),
          question
        )

        question_title <- ifelse(show_percentage, paste0(question_title, " (in %)"), paste0(question_title))

        # Fixing 0 and 1 to True and False in Responses
        x_categories <- chart[[question]]
        if (all(x_categories %in% c(TRUE, FALSE, 0, 1))) {
          x_categories <- as.character(x_categories)
          x_categories[x_categories == "0" | x_categories == "FALSE"] <- "FALSE"
          x_categories[x_categories == "1" | x_categories == "TRUE"] <- "TRUE"
        }

        highchart(type = "chart") |>
          hc_xAxis(categories = x_categories, labels = list(style = list(fontSize = '18px'))) |>
          hc_yAxis(labels = list(style = list(fontSize = '18px'))) |>
          (\(.) {
            if (correct_answer=="No correct answer")
            {
              . |>
                hc_add_series(type = 'bar', chart[['n']], color = COLOR_DEFAULT, name ="Responses")
            } else{
              . |>
                hc_add_series(type = 'bar', chart[['n_correct']], color = COLOR_GREEN, name ="Correct") |>
                hc_add_series(type = 'bar', chart[['n_incorrect']], color = COLOR_DEFAULT, name ="Incorrect")
            }
          })() |>
          hc_plotOptions(series = list(stacking="normal", dataLabels=list(enabled=T,  formatter= JS(paste0("function() { return this.y + '",char_extra,"'; }"))))) |>
          hc_legend(enabled = F) |>
          hc_title(text=question_title) |>
          hc_exporting(
            enabled = TRUE, # always enabled
            filename = paste0("viz_", substr(question, 1, 20)),
            chartOptions = list(
              chart = list(
                style = list(
                  fontFamily = 'Arial, sans-serif'
                )),
              title = list(
                style = list(
                  fontFamily = 'Arial, sans-serif'
                )))) |>
          hc_add_theme(hc_theme_smpl())
        }
      }, error = function(e){
        NULL
      })
    })
  })
}
