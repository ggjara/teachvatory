#' quiz_q_crossBarPlot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_quiz_crossBarPlot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("quizviz_question"),
          "Primary Question",
          choices = c(""),
          selected = NULL
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_flip_axis"),
          label = "Flip Axis",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        # New: Toggle for showing/hiding title
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_show_title"),
          label = "Show Title",
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
        conditionalPanel(
          condition = paste0("input['", ns("quizviz_enable_comparison"), "'] == true"),
          shiny::selectInput(
            ns("quizviz_question2"),
            "Select Group Variable",
            choices = c(""),
            selected = NULL
          )
        ),
        # New: Custom Title Input (Only shown if title is enabled)
        conditionalPanel(
          condition = paste0("input['", ns("quizviz_show_title"), "'] == true"),
          shiny::textInput(
            ns("quizviz_custom_title"),
            label = "Custom Title (Leave empty for default)",
            placeholder = "Enter chart title...",
          )
        ),
        # New: Toggle for showing/hiding subtitle
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_show_subtitle"),
          label = "Show Subtitle",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        # New: Toggle switch to enable/disable category renaming
        shinyWidgets::prettySwitch(
          inputId = ns("quizviz_enable_category_edit"),
          label = "Modify Variable Labels?",
          status = "info",
          fill = TRUE,
          value = FALSE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("quizviz_enable_category_edit"), "'] == true"),
          shiny::uiOutput(ns("quizviz_category_labels_ui")),
          shiny::actionButton(ns("quizviz_apply_category_changes"), "Apply Changes", class = "btn btn-primary")
        )
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          highcharter::highchartOutput(
            outputId = ns("crossbar_plot"),
            height = "500px"
          )
        )
      )
    )
  )
}

#' quiz_q_crossBarPlot Server Functions
#'
#' @noRd
mod_quiz_crossBarPlot_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
  stopifnot(is.reactive(main_inputs$roster))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive list of available questions
    questions <- shiny::reactive({
      tryCatch({
        get_questions_from_quiz(quiz_processed())
      }, error = function(e) {
        NULL
      })
    })

    # Update question choices dynamically
    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(session, "quizviz_question", choices = unique(questions()), selected = unique(questions())[1])
      shiny::updateSelectInput(session, "quizviz_question2", choices = unique(questions()), selected = unique(questions())[2])
    })

    # Reactive dataset based on selected questions
    selected_data <- reactive({
      shiny::req(quiz_processed(), input$quizviz_question)
      quiz <- quiz_processed()

      question1 <- input$quizviz_question
      question2 <- input$quizviz_question2
      enable_comparison <- input$quizviz_enable_comparison

      # Preserve the original order of factors in question1
      original_levels <- unique(quiz[[question1]])

      if (!enable_comparison || is.null(question2)) {
        # Single Question Frequency Calculation
        df <- quiz %>%
          dplyr::group_by(.data[[question1]]) %>%
          dplyr::summarize(n = dplyr::n(), .groups = "drop") %>%
          dplyr::mutate(percentage = round(n / sum(n) * 100, 1))
      } else {
        # Crosstabulation - Ensure proportions within each category of question2
        df <- quiz %>%
          dplyr::group_by(.data[[question2]]) %>%
          dplyr::count(.data[[question1]]) %>%
          dplyr::mutate(
            percentage = round(n / sum(n) * 100, 1)
          )
      }

      # Preserve original factor order of question1
      df[[question1]] <- factor(df[[question1]], levels = original_levels)

      df
    })

    # Reactive value for category labels
    category_labels <- reactiveVal(list())

    # Generate UI for renaming categories (only if toggle is ON)
    output$quizviz_category_labels_ui <- renderUI({
      req(selected_data(), input$quizviz_enable_category_edit)

      df <- selected_data()
      old_labels <- unique(df[[input$quizviz_question]])

      tagList(
        lapply(old_labels, function(label) {
          textInput(ns(paste0("rename_", label)), label = paste("Rename:", label), value = label)
        })
      )
    })

    # Observe category renaming event (Only updates on button click)
    observeEvent(input$quizviz_apply_category_changes, {
      req(input$quizviz_enable_category_edit)  # Ensure toggle is ON

      df <- selected_data()
      old_labels <- unique(df[[input$quizviz_question]])

      # Store renamed labels in reactive value
      new_labels <- setNames(
        lapply(old_labels, function(label) {
          input[[paste0("rename_", label)]]  # Retrieve user-defined labels
        }),
        old_labels
      )

      category_labels(new_labels)
    })

    # Render Highchart
    # Render Highchart
    output$crossbar_plot <- highcharter::renderHighchart({
      shiny::req(selected_data())

      df <- selected_data()
      flip_axis <- input$quizviz_flip_axis
      enable_comparison <- input$quizviz_enable_comparison
      question <- input$quizviz_question

      # Check if primary question has more than 10 unique values
      if (n_distinct(df[[question]], na.rm = TRUE) > 10) {
        return(
          highchart() |>
            hc_title(text = "Question is not categorical. Plot cannot be made.") |>
            hc_add_theme(hc_theme_smpl())
        )
      }

      # Apply category label modifications (only if toggle is ON and button clicked)
      if (input$quizviz_enable_category_edit) {
        renamed_categories <- category_labels()
        if (!is.null(renamed_categories) && length(renamed_categories) > 0) {
          df[[question]] <- factor(df[[question]], levels = names(renamed_categories), labels = unname(renamed_categories))
        }
      }

      # Title checks
      title_text <- if (input$quizviz_show_title) {
        if (input$quizviz_custom_title != "") input$quizviz_custom_title else paste("Distribution of", question)
      } else {
        NULL
      }

      subtitle_text <- if (enable_comparison && input$quizviz_show_subtitle) {
        paste("Grouped by:", input$quizviz_question2)
      } else {
        NULL
      }

      # Determine chart type
      chart_type <- ifelse(flip_axis, "bar", "column")

      # Common plot options with data labels at the top
      plot_options <- list(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.2f}%",
            align = "center",        # Centered horizontally
            verticalAlign = "top",   # Labels at the top of bars/columns
            y = -5                   # Offset slightly for visibility
          )
        )
      )

      # Common export options
      export_options <- list(
        enabled = TRUE,
        filename = paste0("viz_", substr(question, 1, 20)),
        chartOptions = list(
          chart = list(
            style = list(fontFamily = 'Source Sans Pro, sans-serif'),
            backgroundColor = "#FFFFFF"  # Ensure white background when exporting
          ),
          title = list(style = list(fontFamily = 'Source Sans Pro, sans-serif'))
        )
      )

      # Base chart
      chart <- hchart(df, type = chart_type, hcaes(x = !!sym(question), y = percentage)) |>
        hc_xAxis(title = list(text = NULL)) |>
        hc_yAxis(title = list(text = "Proportion (%)"), labels = list(format = "{value}%")) |>
        hc_plotOptions(series = plot_options$series) |>
        hc_exporting(export_options)

      # Add title and subtitle dynamically
      if (enable_comparison && !is.null(input$quizviz_question2)) {
        group_var <- sym(input$quizviz_question2)
        chart <- chart |>
          hc_title(text = title_text) |>
          hc_subtitle(text = subtitle_text) |>
          hc_add_series(hcaes(group = !!group_var))
      } else {
        chart <- chart |> hc_title(text = title_text)
      }

      chart
    })

    # Highchart
    output$crossbar_plot <- highcharter::renderHighchart({
      shiny::req(selected_data())

      df <- selected_data()
      flip_axis <- input$quizviz_flip_axis
      enable_comparison <- input$quizviz_enable_comparison

      # Get the primary and secondary questions
      question1 <- input$quizviz_question
      question2 <- input$quizviz_question2

      # Preserve the original order of factors in question1
      original_levels_q1 <- unique(quiz_processed()[[question1]])
      df[[question1]] <- factor(df[[question1]], levels = original_levels_q1)

      # If comparison is enabled, check `question2`
      if (enable_comparison && !is.null(question2)) {
        # Check if question2 has more than 10 categories
        if (n_distinct(df[[question2]], na.rm = TRUE) > 10) {
          return(
            highchart() |>
              hc_title(text = "Grouping variable has too many categories. Please select another.") |>
              hc_add_theme(hc_theme_smpl())
          )
        }
      }

      # Check if primary question has more than 10 unique values
      if (n_distinct(df[[question1]], na.rm = TRUE) > 10) {
        return(
          highchart() |>
            hc_title(text = "Question is not categorical. Plot cannot be made.") |>
            hc_add_theme(hc_theme_smpl())
        )
      }

      # Apply category label modifications (only if toggle is ON and button clicked)
      if (input$quizviz_enable_category_edit) {
        renamed_categories <- category_labels()
        if (!is.null(renamed_categories) && length(renamed_categories) > 0) {
          df[[question1]] <- factor(df[[question1]], levels = names(renamed_categories), labels = unname(renamed_categories))
        }
      }

      # Title settings
      title_text <- if (input$quizviz_show_title) {
        if (input$quizviz_custom_title != "") input$quizviz_custom_title else paste("Distribution of", question1)
      } else {
        NULL
      }

      subtitle_text <- if (enable_comparison && input$quizviz_show_subtitle) {
        paste("Grouped by:", question2)
      } else {
        NULL
      }

      # Base highchart setup
      base_chart <- hchart(
        df,
        type = ifelse(flip_axis, "bar", "column"),
        hcaes(x = !!sym(question1), y = percentage)
      ) |>
        hc_xAxis(title = list(text = NULL)) |>
        hc_yAxis(title = list(text = "Proportion (%)"), labels = list(format = "{value}%")) |>
        hc_plotOptions(
          series = list(
            dataLabels = list(
              enabled = TRUE,
              format = "{point.y:.2f}%",
              verticalAlign = "top",
              inside = FALSE
            )
          )
        ) |>
        hc_exporting(
          enabled = TRUE,  # ✅ Always enabled, even without comparison
          filename = paste0("viz_", substr(question1, 1, 20)),
          buttons = list(
            contextButton = list(
              menuItems = c("downloadPNG", "downloadJPEG", "downloadPDF", "downloadSVG")
            )
          ),
          chartOptions = list(
            chart = list(style = list(fontFamily = "Source Sans Pro"), backgroundColor = "white"),
            title = list(style = list(fontFamily = "Source Sans Pro"))
          )
        ) |>
        hc_add_theme(hc_theme(chart = list(backgroundColor = "white")))

      # Add titles
      base_chart <- base_chart |> hc_title(text = title_text)

      # Comparison Chart (if enabled)
      if (enable_comparison && !is.null(question2)) {
        group_var <- sym(question2)
        base_chart <- hchart(
          df,
          type = ifelse(flip_axis, "bar", "column"),
          hcaes(x = !!sym(question1), y = percentage, group = !!group_var)
        ) |>
          hc_xAxis(title = list(text = NULL)) |>
          hc_yAxis(title = list(text = "Proportion (%)"), labels = list(format = "{value}%")) |>
          hc_title(text = title_text) |>
          hc_subtitle(text = subtitle_text) |>
          hc_plotOptions(
            series = list(
              dataLabels = list(
                enabled = TRUE,
                format = "{point.y:.2f}%",
                verticalAlign = "top",
                inside = FALSE
              )
            )
          )
      }

      base_chart
    })

  })
}

## To be copied in the UI
# mod_quiz_q_crossBarPlot_ui("quiz_q_crossBarPlot_1")

## To be copied in the server
# mod_quiz_q_crossBarPlot_server("quiz_q_crossBarPlot_1")
