#' @title quiz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bs4Dash highcharter shinyWidgets
mod_quiz_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::box(
        id = ns("box_filter"),
        width = 3,
        title = "Parameters",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        shinyWidgets::pickerInput(
          inputId = ns("filter_quiz"),
          label = "Quiz",
          choices = NULL,
          selected = NULL,
          options = list(`live-search` = TRUE,
                         `actions-box` = TRUE)
        ),
        "",
        # Submit button
        bs4Dash::actionButton(
          inputId = ns("submit_filters"),
          label = "Load Quiz",
          status = "primary"
        ),
        shiny::uiOutput(ns("crosstab_ui"))
      ),
      bs4Dash::tabBox(
        id = ns("box_performance"),
        width = 9,
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = FALSE,
        # title="A Card with tabs", # nolint
        selected = "Performance",
        status = "primary",
        type = "tabs",
        shiny::tabPanel(
          title = "Performance",
          fluidRow(
            bs4Dash::column(width = 4,
                            shinycssloaders::withSpinner(bs4Dash::bs4InfoBoxOutput(ns("infobox_1"), width=NULL), proxy.height = 80)),

            bs4Dash::column(width = 4,
                            shinycssloaders::withSpinner(bs4Dash::bs4ValueBoxOutput(ns("infobox_2"), width=NULL), proxy.height = 80)),
            bs4Dash::column(width = 4,
                            shinycssloaders::withSpinner(bs4Dash::bs4InfoBoxOutput(ns("infobox_3"), width=NULL), proxy.height = 80))
          ),
          shinycssloaders::withSpinner(uiOutput(ns("performance_box")), proxy.height = 56)
        ),
        shiny::tabPanel(title = "Responses time",
                          highcharter::highchartOutput(outputId = ns(
                          "responses_time_graph"
                        ))),
        shiny::tabPanel(
          title = "Quiz Viz",
          mod_quiz_questionviz_ui("quiz_questionviz_1")
        )
      )
    ),
    shiny::fluidRow(
      bs4Dash::box(
        id = ns("box_dataset"),
        width = 12,
        title = "Crosstab analysis",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        shiny::fluidRow(
          bs4Dash::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("filter_crosstab1"),
              label = "Question 1",
              choices = NULL,
              selected = NULL,
              options = list(`live-search` = TRUE,
                             `actions-box` = TRUE)
            ),
          ),
          bs4Dash::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = ns("filter_crosstab2"),
              label = "Question 2",
              choices = NULL,
              selected = NULL,
              options = list(`live-search` = TRUE,
                             `actions-box` = TRUE)
            )
          )
        ),
        shiny::fluidRow(column(width = 12,
                               shinycssloaders::withSpinner(DT::DTOutput(ns(
                                 "quiz_table"
                               )))))
      )
    )
  )
}

#' quiz Server Functions
#'
#' @noRd
#' @import highcharter shinyWidgets
mod_quiz_server <- function(id, stringAsFactors = FALSE, main_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ####### Reactive values  #######

    # It should be a spreadsheet so there shouldn't be any error
    # when getting the sheet name, but still I encapsulated
    # the function in a TryCatch.
    quizzes <- shiny::eventReactive(main_inputs$masterquiz_md(), {
      tryCatch({
        main_inputs$masterquiz_md()[["sheets"]]$name
      },
      error = function(e) {
        NULL
      })
    })

    # We know (by design) that masterquiz is the metadata
    # and that filter_quiz sheet name exists
    quiz <- shiny::eventReactive(input$submit_filters, {
      googlesheets4::read_sheet(main_inputs$masterquiz_md(),
                                sheet = input$filter_quiz)
    })

    id_colname <- shiny::reactive({
      get_idcolname(quiz())
    })

    # Join quiz with Roster
    quiz_processed <- shiny::reactive({
      join_quiz_roster(quiz(), main_inputs$roster(), id_colname())
    })

    # Get questions for select inputs
    questions <- shiny::reactive({
      get_questions_from_quiz(quiz_processed())
    })

    n_responses <- shiny::reactive({
      nrows <- nrow(quiz_processed())
      if (is.null(nrows)) {
        nrows <- 0
      }
      nrows
    })

    n_roster <- shiny::reactive({
      nrows <- nrow(main_inputs$roster())
      if (is.null(nrows)) {
        nrows <- 0
      }
      nrows
    })

    ####### End Reactive Values #######

    ####### Updates #######
    shiny::observeEvent(quiz_processed(), {
      bs4Dash::updateBox("box_dataset", action = "restore")
      if (input$box_dataset$collapsed) {
        bs4Dash::updateBox("box_dataset", action = "toggle")
      }
      bs4Dash::updateBox("box_performance", action = "restore")
      if (input$box_dataset$collapsed) {
        bs4Dash::updateBox("box_performance", action = "toggle")
      }
    })

    shiny::observeEvent(quizzes(), {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "filter_quiz",
        choices = quizzes(),
        selected = NULL
      )
    })

    shiny::observeEvent(questions(), {
      freezeReactiveValue(input, "filter_crosstab1")
      freezeReactiveValue(input, "filter_crosstab2")
      shinyWidgets::updatePickerInput(
        session,
        inputId = "filter_crosstab1",
        choices = questions(),
        selected = questions()[1]
      )
      shinyWidgets::updatePickerInput(
        session,
        inputId = "filter_crosstab2",
        choices = questions(),
        selected = questions()[2]
      )
    })

    ####### End Updates #######

    ####### Render #######

    output$infobox_1 <- bs4Dash::renderbs4InfoBox({
      val <- 0
      tryCatch({
        val <- n_roster()
      },
      error = function(e) {

      })
        bs4Dash::bs4InfoBox(
          title = "Students in roster",
          value = shiny::tags$h3(val),
          icon = shiny::icon("users-rectangle", lib = "font-awesome"),
          fill = TRUE,
          color = "gray"
        )
    })

    output$infobox_2 <- bs4Dash::renderbs4ValueBox({
      val <- 0
      tryCatch({
        val <- n_responses()
      },
      error = function(e) {

      })
        val<- bs4Dash::bs4ValueBox(
          subtitle = "Responses",
          value = shiny::tags$h3(val),
          icon = shiny::icon("user-check", lib = "font-awesome"),
          #fill = TRUE,
          #footer = "footes",
          footer = shiny::tagList({
            shiny::actionLink(ns("goLink"), shiny::HTML('<span style="color:#ffffff">More info</span>'))
            }),
          elevation = 2,
          color = "success"
        )

    })

    output$infobox_3 <- bs4Dash::renderbs4InfoBox({
      val <- 0
      tryCatch({
        val <- max(0, n_roster() - n_responses())
      },
      error = function(e) {

      })
        bs4Dash::bs4InfoBox(
          title = "Left to answer",
          value = shiny::tags$h3(val),
          icon = shiny::icon("user-minus", lib = "font-awesome"),
          fill = TRUE,
          color = "danger"
        )
    })



    output$performance_box <- shiny::renderUI({
      pct <- 0
      tryCatch({
        if (n_roster() != 0) {
          pct <- n_responses() / n_roster()
        }
      },
      error = function(e) {

      })
      status <- "danger"
      if (pct > 0.77) {
        status <- "success"
      } else if (pct > 0.33) {
        status <- "warning"
      }

      shiny::tagList(fluidRow(
        column(
          width = 12,
            shinyWidgets::progressBar(
              title = "Submission Rate",
              id = ns("progress_bar"),
              value = n_responses(),
              total = n_roster(),
              display_pct = TRUE,
              striped = FALSE,
              status = status
            )
        )
      ))
    })

    output$responses_time_graph <- highcharter::renderHighchart({
      shiny::req(quiz_processed())
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()) &&
            "Timestamp" %in% colnames(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )

      chart <- quiz_processed() %>%
        group_by(Date = lubridate::floor_date(Timestamp, "4 hour")) %>%
        summarize(n = n()) %>%
        ungroup() %>%
        arrange(Date) %>%
        mutate(Date = substr(Date, 1, 16))

      chart <- chart %>%
        mutate(Date = factor(Date, labels = unique(chart$Date)),
               serie = "Responses")

      highchart() %>%
        hc_xAxis(categories = chart$Date) %>%
        hc_add_series(name = "Responses",
                      data = chart$n,
                      type = "column") %>%
        hc_yAxis(title = list(text = "Responses"))  %>%
        hc_title(text = "Responses time") %>%
        hc_subtitle(text = "Grouped by 4 hours. The horizontal labels show the floor hour.") %>%
        hc_add_theme(hc_theme_smpl())
    })

    output$quiz_table <- DT::renderDT({
      shiny::req(quiz_processed())
      shiny::validate(
        shiny::need(!is.null(quiz_processed()),
                    message = "Quiz was not uploaded correctly. Check your inputs.")
      )
      col_to_match <- id_colname()
      brks <- seq(0, 1, 0.1)
      clrs <-
        colorRampPalette(c("#dc3545", "#ffc107", "#28a745"))(length(brks) + 1)

      cols_toselect <- c(col_to_match,
                         "teachly",
                         input$filter_crosstab1,
                         input$filter_crosstab2)
      cols_toshow <- c(
        col_to_match,
        "teachly",
        paste0(
          stringr::str_sub(input$filter_crosstab1, end = 200),
          "..."
        ),
        paste0(
          stringr::str_sub(input$filter_crosstab2, end = 200),
          "..."
        )
      )
      DT::datatable(
        quiz_processed() %>%
          dplyr::select(cols_toselect) %>%
          dplyr::rename(!!cols_toshow[3] := .data[[input$filter_crosstab1]],
                        !!cols_toshow[4] := .data[[input$filter_crosstab2]]),
        escape = FALSE,
        rownames = FALSE,
        style = "bootstrap4",
        filter = "top",
        selection = "none",
        options = list(
          pageLength = 100,
          autowidth = TRUE,
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel')
        )
      ) %>%
        DT::formatStyle(c("teachly"),
                        backgroundColor = DT::styleInterval(brks, clrs))
    })
    ####### End Render #######

    # output
    reactive(quiz_processed())
  })
  #mod_quiz_questionviz_server("quiz_questionviz_1", FALSE, main_inputs, quiz_processed)
}

## To be copied in the UI
# mod_quiz_ui("quiz_1")

## To be copied in the server
# mod_quiz_server("quiz_1")
