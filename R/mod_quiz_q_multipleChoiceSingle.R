#' quiz_questionviz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter dplyr sortable
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
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_show_title"),
          label = "Show Title",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("quizviz_show_title"), "'] == true"),
          shiny::textInput(
            inputId = ns("quizviz_custom_title"),
            label = "Custom Title (Leave empty for default)",
            placeholder = "Enter chart title..."
          )
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_use_sortable"),
          label = "Use sortable",
          status = "info",
          fill = TRUE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_flip_axis"),
          label = "Flip Axis",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_enable_comparison"),
          label = "Compare with another question?",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("quizviz_enable_comparison"), "'] == true"),
          shiny::selectInput(
            ns("quizviz_question2"),
            "Select Group Variable",
            choices = c(""),
            selected = NULL
          ),
          shinyWidgets::prettySwitch(
            inputId = ns("quizviz_customize_legend"),
            label = "Customize legend label for group variable",
            status = "info",
            fill = TRUE,
            value = FALSE
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("quizviz_customize_legend"), "'] == true"),
            shiny::textInput(
              ns("quizviz_custom_legend"),
              "Custom Legend Label",
              placeholder = "Enter custom legend label..."
            )
          )
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_lock_plot"),
          label = "Lock Plot",
          status = "warning",
          fill = TRUE,
          value = FALSE
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("quizviz_lock_plot"), "'] == true"),
          shiny::uiOutput(ns("quizviz_category_labels_ui")),
          shiny::actionButton(ns("quizviz_apply_category_changes"), "Apply Label Changes")
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("quizviz_use_sortable"), "'] == true"),
          shiny::uiOutput(ns("conditional_rank_list"))
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
    ),
    shiny::tags$script(HTML(paste0("
      Shiny.addCustomMessageHandler('controlSwitch', function(message) {
        if(message.id === 'quizviz_arrange_by_frequency' && message.active === true) {
          $('#", ns("quizviz_use_sortable"), "').bootstrapSwitch('state', false, true);
          $('#", ns("quizviz_use_sortable"), "').bootstrapSwitch('readonly', true);
        } else if (message.id === 'quizviz_arrange_by_frequency' && message.active === false) {
          $('#", ns("quizviz_use_sortable"), "').bootstrapSwitch('readonly', false);
        }
        if(message.id === 'quizviz_use_sortable' && message.active === true) {
          $('#", ns("quizviz_arrange_by_frequency"), "').bootstrapSwitch('state', false, true);
          $('#", ns("quizviz_arrange_by_frequency"), "').bootstrapSwitch('readonly', true);
        } else if (message.id === 'quizviz_use_sortable' && message.active === false) {
          $('#", ns("quizviz_arrange_by_frequency"), "').bootstrapSwitch('readonly', false);
        }
      });
    ")))
  )
}

#' quiz_questionviz Server Functions
#'
#' @noRd
#' @import highcharter dplyr sortable
mod_quiz_multipleChoiceSingle_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
  stopifnot(is.reactive(main_inputs$roster))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to hold the locked configuration when plot is locked
    locked_data <- reactiveVal(NULL)
    # Reactive value to hold the renaming mapping for the main variable
    category_labels <- reactiveVal(NULL)

    questions <- reactive({
      tryCatch({
        qs <- get_questions_from_quiz(quiz_processed())
        qs[qs != "order"]
      }, error = function(e) NULL)
    })

    answers <- reactive({
      tryCatch({
        ans <- quiz_processed() %>%
          mutate(!!input$quizviz_question := as.character(.data[[input$quizviz_question]])) %>%
          mutate(!!input$quizviz_question := case_when(
            is.na(.data[[input$quizviz_question]]) ~ "No answer",
            TRUE ~ .data[[input$quizviz_question]]
          )) %>%
          group_by(.data[[input$quizviz_question]]) %>%
          summarize(n = n(), .groups = "drop") %>%
          ungroup()
        if (input$quizviz_arrange_by_frequency) {
          ans <- ans |> arrange(desc(n))
        }
        ans <- ans |> pull(.data[[input$quizviz_question]])
        c("No correct answer", ans)
      }, error = function(e) NULL)
    })

    labels <- reactiveVal()
    observeEvent(input$quizviz_question, {
      quiz_data <- quiz_processed()
      lev <- if (is.factor(quiz_data[[input$quizviz_question]])) {
        levels(quiz_data[[input$quizviz_question]])
      } else {
        unique(quiz_data[[input$quizviz_question]])
      }
      labels(if (length(lev) <= 10) lev else NULL)
    }, ignoreNULL = FALSE)

    sorted_categories <- reactiveVal()
    observeEvent(input$sorted_categories, {
      sorted_categories(input$sorted_categories)
    })

    output$conditional_rank_list <- renderUI({
      if (is.null(labels())) return(NULL)
      sortable::rank_list(
        text = "Drag items in the list to reorder:",
        labels = labels(),
        input_id = ns("sorted_categories")
      )
    })

    observeEvent(quiz_processed(), {
      updateSelectInput(session, "quizviz_question", choices = unique(questions()), selected = unique(questions())[1])
      updateSelectInput(session, "quizviz_question2", choices = unique(questions()), selected = NULL)
    })

    observeEvent(answers(), {
      updateSelectInput(session, "quizviz_correctanswer", choices = answers(), selected = answers()[1])
    })

    # Render UI for renaming categories for main variable when plot is locked.
    output$quizviz_category_labels_ui <- renderUI({
      req(input$quizviz_lock_plot)
      # Use the ordering stored in locked_data if available; otherwise compute from quiz_processed
      cur <- if (!is.null(locked_data())) {
        locked_data()$pre_chart
      } else {
        q <- input$quizviz_question
        if (input$quizviz_use_sortable && !is.null(sorted_categories())) {
          sorted_categories()
        } else if (input$quizviz_arrange_by_frequency) {
          df <- quiz_processed()
          df %>% dplyr::count(.data[[q]]) %>% arrange(desc(n)) %>% pull(.data[[q]])
        } else {
          unique(quiz_processed()[[q]])
        }
      }
      tagList(
        lapply(cur, function(lbl) {
          textInput(ns(paste0("rename_", lbl)),
                    label = paste("Rename:", lbl),
                    value = lbl)
        })
      )
    })

    observeEvent(input$quizviz_apply_category_changes, {
      req(input$quizviz_lock_plot)
      # Use the locked ordering from locked_data
      req(locked_data())
      cur <- locked_data()$pre_chart
      new_map <- setNames(
        lapply(cur, function(lbl) input[[paste0("rename_", lbl)]]),
        cur
      )
      category_labels(new_map)
    })

    output$quizviz_graph <- highcharter::renderHighchart({
      req(quiz_processed())
      validate(need(!is.null(quiz_processed()), "Quiz was not uploaded correctly. Check your inputs."))
      tryCatch({
        # If locked, use stored locked configuration
        if (isTRUE(input$quizviz_lock_plot)) {
          if (is.null(locked_data())) {
            locked_data({
              list(
                quiz = quiz_processed(),
                question = input$quizviz_question,
                question2 = input$quizviz_question2,
                correct_answer = input$quizviz_correctanswer,
                arrange_by_frequency = input$quizviz_arrange_by_frequency,
                use_sortable = input$quizviz_use_sortable,
                show_percentage = input$quizviz_show_percentage,
                flip_axis = input$quizviz_flip_axis,
                enable_comparison = input$quizviz_enable_comparison,
                char_extra = if (input$quizviz_show_percentage) "%" else "",
                pre_chart = {
                  q <- input$quizviz_question
                  q2 <- input$quizviz_question2
                  if (!input$quizviz_enable_comparison || is.null(q2)) {
                    ch <- quiz_processed() %>%
                      mutate(!!q := as.character(.data[[q]])) %>%
                      mutate(!!q := case_when(
                        is.na(.data[[q]]) ~ "No answer",
                        TRUE ~ .data[[q]]
                      )) %>%
                      group_by(!!rlang::sym(q)) %>%
                      summarize(n = n(), .groups = "drop") %>%
                      ungroup()
                  } else {
                    ch <- quiz_processed() %>%
                      mutate(!!q := as.character(.data[[q]])) %>%
                      mutate(!!q := case_when(
                        is.na(.data[[q]]) ~ "No answer",
                        TRUE ~ .data[[q]]
                      )) %>%
                      group_by(!!rlang::sym(q2)) %>%
                      count(!!rlang::sym(q)) %>%
                      mutate(percentage = round(n / sum(n) * 100, 1)) %>%
                      ungroup()
                  }
                  if (input$quizviz_use_sortable && !is.null(sorted_categories())) {
                    x_cat <- factor(ch[[q]], levels = sorted_categories())
                    levels(x_cat)
                  } else if (input$quizviz_arrange_by_frequency) {
                    ch <- ch |> arrange(desc(n))
                    as.character(ch[[q]])
                  } else {
                    unique(ch[[q]])
                  }
                },
                title_text = if (input$quizviz_show_title) {
                  if (nzchar(input$quizviz_custom_title)) input$quizviz_custom_title else input$quizviz_question
                } else {
                  NULL
                }
              )
            })
          }
          ld <- locked_data()
          quiz <- ld$quiz
          question <- ld$question
          question2 <- ld$question2
          correct_answer <- ld$correct_answer
          arrange_by_frequency <- ld$arrange_by_frequency
          use_sortable <- ld$use_sortable
          show_percentage <- ld$show_percentage
          flip_axis <- ld$flip_axis
          enable_comparison <- ld$enable_comparison
          char_extra <- ld$char_extra
          title_text <- ld$title_text
        } else {
          quiz <- quiz_processed()
          question <- input$quizviz_question
          question2 <- input$quizviz_question2
          correct_answer <- input$quizviz_correctanswer
          arrange_by_frequency <- input$quizviz_arrange_by_frequency
          use_sortable <- input$quizviz_use_sortable
          show_percentage <- input$quizviz_show_percentage
          flip_axis <- input$quizviz_flip_axis
          enable_comparison <- input$quizviz_enable_comparison
          char_extra <- if (show_percentage) "%" else ""
          title_text <- if (input$quizviz_show_title) {
            if (nzchar(input$quizviz_custom_title)) input$quizviz_custom_title else input$quizviz_question
          } else {
            NULL
          }
        }
        # Build chart data for main question
        if (!enable_comparison || is.null(question2)) {
          chart <- quiz %>%
            mutate(!!question := as.character(.data[[question]])) %>%
            mutate(!!question := case_when(
              is.na(.data[[question]]) ~ "No answer",
              TRUE ~ .data[[question]]
            )) %>%
            group_by(!!rlang::sym(question)) %>%
            summarize(n = n(), .groups = "drop") %>%
            ungroup()
        } else {
          chart <- quiz %>%
            mutate(!!question := as.character(.data[[question]])) %>%
            mutate(!!question := case_when(
              is.na(.data[[question]]) ~ "No answer",
              TRUE ~ .data[[question]]
            )) %>%
            group_by(!!rlang::sym(question2)) %>%
            count(!!rlang::sym(question)) %>%
            mutate(percentage = round(n / sum(n) * 100, 1)) %>%
            ungroup()
        }
        if (enable_comparison && !is.null(question2)) {
          if (n_distinct(chart[[question2]], na.rm = TRUE) > 10) {
            return(
              highchart() |>
                hc_title(text = "Grouping variable has too many categories. Please select another.") |>
                hc_add_theme(hc_theme_smpl())
            )
          }
        }
        if (n_distinct(chart[[question]]) > 10) {
          return(
            highchart() |>
              hc_title(text = "Question is not categorical. Plot cannot be made.") |>
              hc_add_theme(hc_theme_smpl())
          )
        }
        if (!enable_comparison && show_percentage) {
          chart <- chart |> mutate(n = round(n * 100 / sum(n, na.rm = TRUE), 0))
        }
        if (use_sortable && !is.null(sorted_categories())) {
          chart <- chart %>%
            mutate(!!question := factor(.data[[question]], levels = sorted_categories())) %>%
            arrange(factor(.data[[question]], levels = sorted_categories()))
        } else if (arrange_by_frequency) {
          chart <- chart |> arrange(desc(n)) %>%
            mutate(!!question := factor(.data[[question]], levels = .data[[question]]))
        } else {
          chart <- chart %>% mutate(!!question := factor(.data[[question]], levels = unique(chart[[question]])))
        }
        chart <- chart %>%
          mutate(correct = .data[[question]] == correct_answer) %>%
          mutate(n_correct = case_when(correct ~ n, TRUE ~ NA_integer_)) %>%
          mutate(n_incorrect = case_when(!correct ~ n, TRUE ~ NA_integer_))
        # Determine x-axis categories from the chart data
        x_categories <- levels(chart[[question]])
        if (all(x_categories %in% c(TRUE, FALSE, 0, 1))) {
          x_categories <- as.character(x_categories)
          x_categories[x_categories %in% c("0", "FALSE")] <- "FALSE"
          x_categories[x_categories %in% c("1", "TRUE")] <- "TRUE"
        }
        # When locked, override x_categories with renamed labels if available
        if (isTRUE(input$quizviz_lock_plot)) {
          new_map <- category_labels()
          if (!is.null(new_map)) {
            x_categories <- sapply(locked_data()$pre_chart, function(lbl) {
              if (lbl %in% names(new_map)) new_map[[lbl]] else lbl
            })
          }
        }
        # Build the highchart
        if (enable_comparison && !is.null(question2)) {
          group_var <- sym(question2)
          y_value <- if (show_percentage) "percentage" else "n"
          y_axis_title <- if (show_percentage) "Proportion (%)" else "Count"
          y_format <- if (show_percentage) "{value}%" else "{value}"
          data_label_format <- if (show_percentage) "{point.y:.2f}%" else "{point.y}"
          comp_dl_opts <- if (flip_axis) {
            list(enabled = TRUE, format = data_label_format, verticalAlign = "middle", align = "left", inside = TRUE)
          } else {
            list(enabled = TRUE, format = data_label_format, verticalAlign = "top", align = "center", inside = TRUE)
          }
          base_chart <- hchart(
            chart,
            type = ifelse(flip_axis, "bar", "column"),
            hcaes(x = !!sym(question), y = !!sym(y_value), group = !!group_var)
          ) |>
            hc_xAxis(title = list(text = NULL), categories = x_categories) |>
            hc_yAxis(title = list(text = y_axis_title), labels = list(format = y_format)) |>
            hc_title(text = title_text) |>
            hc_plotOptions(series = list(dataLabels = comp_dl_opts)) |>
            hc_exporting(
              enabled = TRUE,
              filename = paste0("viz_", substr(question, 1, 20)),
              chartOptions = list(
                chart = list(style = list(fontFamily = 'Arial, sans-serif')),
                title = list(text = ifelse(is.null(title_text), "", title_text),
                             style = list(fontFamily = 'Arial, sans-serif'))
              )
            ) |>
            hc_add_theme(hc_theme_smpl())
          if (isTRUE(input$quizviz_customize_legend) && nzchar(input$quizviz_custom_legend)) {
            base_chart <- base_chart |> hc_legend(title = list(text = input$quizviz_custom_legend))
          }
          base_chart
        } else {
          highchart() |>
            hc_xAxis(categories = x_categories, labels = list(style = list(fontSize = '18px'))) |>
            hc_yAxis(labels = list(style = list(fontSize = '18px'))) |>
            (function(chart_obj) {
              if (correct_answer == "No correct answer") {
                chart_obj |> hc_add_series(type = ifelse(flip_axis, "column", "bar"),
                                           data = chart[['n']],
                                           color = COLOR_DEFAULT, name = "Responses")
              } else {
                chart_obj |> hc_add_series(type = ifelse(flip_axis, "column", "bar"),
                                           data = chart[['n_correct']],
                                           color = COLOR_GREEN, name = "Correct") |>
                  hc_add_series(type = ifelse(flip_axis, "column", "bar"),
                                data = chart[['n_incorrect']],
                                color = COLOR_DEFAULT, name = "Incorrect")
              }
            })() |>
            hc_plotOptions(series = list(
              stacking = "normal",
              dataLabels = list(
                enabled = TRUE,
                formatter = JS(paste0("function() { return this.y + '", char_extra, "'; }")),
                verticalAlign = "middle",
                align = "center",
                inside = TRUE
              )
            )) |>
            hc_legend(enabled = FALSE) |>
            hc_title(text = title_text) |>
            hc_exporting(
              enabled = TRUE,
              filename = paste0("viz_", substr(question, 1, 20)),
              chartOptions = list(
                chart = list(style = list(fontFamily = 'Arial, sans-serif')),
                title = list(text = ifelse(is.null(title_text), "", title_text),
                             style = list(fontFamily = 'Arial, sans-serif'))
              )
            ) |>
            hc_add_theme(hc_theme_smpl())
        }
      }, error = function(e) {
        message("Error in rendering chart: ", e$message)
        NULL
      })
    })

    observeEvent(input$quizviz_use_sortable, {
      if (input$quizviz_use_sortable) {
        updatePrettySwitch(session, "quizviz_arrange_by_frequency", value = FALSE)
        shinyjs::disable("quizviz_arrange_by_frequency")
      } else {
        shinyjs::enable("quizviz_arrange_by_frequency")
      }
    })

    observeEvent(input$quizviz_arrange_by_frequency, {
      if (input$quizviz_arrange_by_frequency) {
        updatePrettySwitch(session, "quizviz_use_sortable", value = FALSE)
        shinyjs::disable("quizviz_use_sortable")
      } else {
        shinyjs::enable("quizviz_use_sortable")
      }
    })

    observeEvent(input$quizviz_lock_plot, {
      if (!input$quizviz_lock_plot) {
        locked_data(NULL)
        category_labels(NULL)
      }
    })

    observe({
      if (input$quizviz_lock_plot) {
        shinyjs::disable("quizviz_question")
        shinyjs::disable("quizviz_correctanswer")
        shinyjs::disable("quizviz_show_percentage")
        shinyjs::disable("quizviz_arrange_by_frequency")
        shinyjs::disable("quizviz_show_title")
        shinyjs::disable("quizviz_custom_title")
        shinyjs::disable("quizviz_use_sortable")
        shinyjs::disable("quizviz_flip_axis")
        shinyjs::disable("quizviz_enable_comparison")
        shinyjs::disable("quizviz_question2")
        shinyjs::disable("quizviz_customize_legend")
        shinyjs::disable("quizviz_custom_legend")
      } else {
        shinyjs::enable("quizviz_question")
        shinyjs::enable("quizviz_correctanswer")
        shinyjs::enable("quizviz_show_percentage")
        shinyjs::enable("quizviz_arrange_by_frequency")
        shinyjs::enable("quizviz_show_title")
        shinyjs::enable("quizviz_custom_title")
        shinyjs::enable("quizviz_use_sortable")
        shinyjs::enable("quizviz_flip_axis")
        shinyjs::enable("quizviz_enable_comparison")
        shinyjs::enable("quizviz_question2")
        shinyjs::enable("quizviz_customize_legend")
        shinyjs::enable("quizviz_custom_legend")
      }
    })
  })
}
