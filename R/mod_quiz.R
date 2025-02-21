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
    shinyjs::useShinyjs(),
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
        shiny::dateInput(
          inputId = ns("initial_date"),
          label = "Analysis Start Date",
          value = as.Date(INITIAL_DATE_DEFAULT)
        ),
        # Submit button
        bs4Dash::actionButton(
          inputId = ns("submit_filters"),
          label = "Load Quiz",
          status = "primary"
        )
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
                            shinycssloaders::withSpinner(bs4Dash::bs4ValueBoxOutput(ns("valuebox_1"), width=NULL), proxy.height = 142)),

            bs4Dash::column(width = 4,
                            shinycssloaders::withSpinner(bs4Dash::bs4ValueBoxOutput(ns("valuebox_2"), width=NULL), proxy.height = 142)),
            bs4Dash::column(width = 4,
                            shinycssloaders::withSpinner(bs4Dash::bs4ValueBoxOutput(ns("valuebox_3"), width=NULL), proxy.height = 142))
          ),
          shinycssloaders::withSpinner(uiOutput(ns("performance_box")), proxy.height = 56)
        ),
        shiny::tabPanel(
          title = "Multiple Choice",
          mod_quiz_multipleChoiceSingle_ui(ns("quiz_multipleChoiceSingle_1"))
        ),
        shiny::tabPanel(
          title = "Prediction",
          mod_quiz_prediction_ui(ns("quiz_prediction_1"))
        ),
        shiny::tabPanel(
          title = "AI Summary",
          mod_quiz_aiSummary_ui(ns("quiz_aiSummary_1"))
        ),
        shiny::tabPanel(
          title = "AI Quotes",
          mod_quiz_aiQuotes_ui(ns("quiz_aiQuotes_1"))
          ),
         shiny::tabPanel(
          title = "Crosstab",
          mod_quiz_crosstab_ui(ns("quiz_Crosstab_1"))
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
        shiny::fluidRow(
          column(
            width = 12,
            shinycssloaders::withSpinner(
              DT::DTOutput(
                ns(
                  "quiz_table"
                )
              )
            )
          )
          ),
          shiny::fluidRow(
            column(
              width = 12,
              shiny::actionButton(
                inputId = ns("export_selection"),
                label = "Export Selection to Excel",
                icon = shiny::icon("file-excel")
              ),
              shiny::downloadButton(
                outputId = ns("download_excel"),
                label = "",
                style = "display: none;"
              ),
              shiny::actionButton(
                inputId = ns("copy_to_clipboard"),
                label = "Copy to Clipboard",
                icon = shiny::icon("clipboard")
              ),
              shiny::actionButton(
                inputId = ns("toggle_show_selected"),
                label = "Show Selected Only",
                icon = shiny::icon("eye")
              ),
              shiny::actionButton(
                inputId = ns("return_to_og"),
                label = "Return to OG Table",
                icon = shiny::icon("undo")
              )

            )
          )
        )
    ),
    # New fluidRow for displaying all answers of the loaded quiz
    shiny::fluidRow(
      bs4Dash::box(
        id = ns("box_all_answers"),
        width = 12,
        title = "All Quiz Answers",
        status = "primary",
        solidHeader = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        shinycssloaders::withSpinner(
          DT::DTOutput(ns("all_quiz_answers"))
        )
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
      res <- googlesheets4::read_sheet(main_inputs$masterquiz_md(),
                                sheet = input$filter_quiz)
      # Correct lists
      for (col in names(res)) {
        # Check if the column is a list
        if (is.list(res[[col]])) {
          # Concatenate list elements into a single string
          res[[col]] <- sapply(res[[col]], function(x) paste(x, collapse = " "))
        }
      }

      res
    })

    # Process quiz: (1) Replace [Your Name] with [If Your Name...], (2) Delete NAs, (3) Delete repeated
    quiz_filtered <- shiny::reactive({

      shiny::req(quiz())
          filter_quiz(
            quiz = quiz(),
            col_to_match = id_colname(),
            col_alternative = id_colname_alternative(),
            initial_date = input$initial_date)
    })


    # Get colname of "Your Name" input. If doesn't exist, return ""
    id_colname <- shiny::reactive({
      get_idcolname(quiz())
    })

    # Get colname of "If your name is not listed..." input.
    # If doesn't exist, return ""
    id_colname_alternative <- shiny::reactive({
      get_idcolname_alternative(quiz())
    })

    # Join quiz with Roster
    quiz_processed <- shiny::reactive({
      join_quiz_roster(quiz_filtered(), main_inputs$roster(), id_colname())
    })

    # Get questions for select inputs
    questions <- shiny::reactive({
      get_questions_from_quiz(quiz_processed())
    })

    n_responses <- shiny::reactive({
      nrows <- nrow(quiz_processed())
      if (is.null(nrows) | id_colname()=="" | is.na(id_colname())) {
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

    modal_responses_data <- reactive({
      data_to_show <- dplyr::tibble()
      tryCatch({
        if(id_colname()=="" | is.na(id_colname())){
          data_to_show <- dplyr::tibble()
        }
        else{
          data_to_show <- quiz_processed()[id_colname()]
        }
      }, error = function(e){
        data_to_show <- dplyr::tibble()
      })
      students_list_modal(data_to_show,
                          title = "Students who have submitted the quiz")
    })

    modal_left_data <- reactive({
      data_to_show <- dplyr::tibble()
      tryCatch({
        if(id_colname()=="" | is.na(id_colname())){
          data_to_show <- main_inputs$roster()[["standardized_name"]] |>
            dplyr::select(standardized_name, teachly)
        }
        else{
        dif <- dplyr::setdiff(main_inputs$roster()[["standardized_name"]],
                              quiz_processed()[[id_colname()]])
        data_to_show <- main_inputs$roster() |>
          dplyr::filter(standardized_name %in% dif) |>
          dplyr::select(standardized_name, teachly)
        }
      }, error = function(e){
        data_to_show <- dplyr::tibble()
      })
      students_list_modal(data_to_show,
                          title = "Students who have not submitted")
    })

    modal_roster_data <- reactive({
      data_to_show <- dplyr::tibble()
      tryCatch({
        data_to_show <- main_inputs$roster()["standardized_name"]
      }, error = function(e){
        data_to_show <- dplyr::tibble()
      })
      students_list_modal(data_to_show,
                          title = "Students in roster")
    })


    ##For filter by selection in table
    selected_rows_f <- reactiveVal(c())  # Stores selected row indices
    show_selected <- reactiveVal(FALSE)  # Tracks whether to show only selected rows

    observeEvent(input$quiz_table_rows_selected, {
      selected_rows_f(input$quiz_table_rows_selected)  # Store selected rows persistently
    })

    observeEvent(input$toggle_show_selected, {
      show_selected(TRUE)  # Set to TRUE when button is pressed

      # Capture selected rows and store them persistently
      if (length(selected_rows_f()) > 0) {
        selected_table_data(quiz_processed()[selected_rows_f(), ])  # Store table once
      }
    })

    observeEvent(input$return_to_og, {
      selected_table_data(NULL)  # Reset back to the full table
    })


    ####### End Reactive Values #######

    ####### Modals #######

    shiny::observeEvent(input$open_modal_responses, {
      shiny::showModal(
        modal_responses_data()
      )
    })

    shiny::observeEvent(input$open_modal_roster, {
      shiny::showModal(
        modal_roster_data()
      )
    })

    shiny::observeEvent(input$open_modal_left, {
      shiny::showModal(
        modal_left_data()
      )
    })

    ####### End Modals #######



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

    output$valuebox_1 <- bs4Dash::renderbs4ValueBox({
      val <- 0
      tryCatch({
        val <- n_roster()
      },
      error = function(e) {

      })
      bs4Dash::bs4ValueBox(
        subtitle = "Students in roster",
        value = shiny::tags$h3(val),
        icon = shiny::icon("users", lib = "font-awesome"),
        footer = shiny::actionLink(
          ns("open_modal_roster"),
          shiny::tagList(
            shiny::HTML('<span style="color:#ffffff">Who are they?</span>'),
            shiny::icon("circle-arrow-right", lib = "font-awesome", style="color:#ffffff")
          )
        ),
        elevation = 2,
        color = "gray"
      )
    })

    output$valuebox_2 <- bs4Dash::renderbs4ValueBox({
      val <- 0
      tryCatch({
        val <- n_responses()
      },
      error = function(e) {

      })
        bs4Dash::bs4ValueBox(
          subtitle = "Responses",
          value = shiny::tags$h3(val),
          icon = shiny::icon("user-check", lib = "font-awesome"),
          #fill = TRUE,
          #footer = "footes",
          footer = shiny::actionLink(
            ns("open_modal_responses"),
            shiny::tagList(
              shiny::HTML('<span style="color:#ffffff">Who are they?</span>'),
              shiny::icon("circle-arrow-right", lib = "font-awesome", style="color:#ffffff")
            )
          ),
          elevation = 2,
          color = "success"
        )

    })

    output$valuebox_3 <- bs4Dash::renderbs4ValueBox({
      val <- 0
      tryCatch({
        val <- max(0, n_roster() - n_responses())
      },
      error = function(e) {

      })

      bs4Dash::bs4ValueBox(
        subtitle = "Left to answer",
        value = shiny::tags$h3(val),
        icon = shiny::icon("user-minus", lib = "font-awesome"),
        footer = shiny::actionLink(
          ns("open_modal_left"),
          shiny::tagList(
            shiny::HTML('<span style="color:#ffffff">Who are they?</span>'),
            shiny::icon("circle-arrow-right", lib = "font-awesome", style="color:#ffffff")
          )
        ),
        elevation = 2,
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

    # output$responses_time_graph <- highcharter::renderHighchart({
    #   shiny::req(quiz_processed())
    #   shiny::validate(
    #     shiny::need(
    #       !is.null(quiz_processed()) &&
    #         "Timestamp" %in% colnames(quiz_processed()),
    #       message = "Quiz was not uploaded correctly. Check your inputs."
    #     )
    #   )
    #
    #   chart <- quiz_processed() %>%
    #     group_by(Date = lubridate::floor_date(Timestamp, "4 hour")) %>%
    #     summarize(n = n()) %>%
    #     ungroup() %>%
    #     arrange(Date) %>%
    #     mutate(Date = substr(Date, 1, 16))
    #
    #   chart <- chart %>%
    #     mutate(Date = factor(Date, labels = unique(chart$Date)),
    #            serie = "Responses")
    #
    #   highchart() %>%
    #     hc_xAxis(categories = chart$Date) %>%
    #     hc_add_series(name = "Responses",
    #                   data = chart$n,
    #                   type = "column") %>%
    #     hc_yAxis(title = list(text = "Responses"))  %>%
    #     hc_title(text = "Responses time") %>%
    #     hc_subtitle(text = "Grouped by 4 hours. The horizontal labels show the floor hour.") %>%
    #     hc_add_theme(hc_theme_smpl())
    # })

    selected_table_data <- reactiveVal(NULL)  # Stores filtered table only once


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

      if(col_to_match!="" && !is.na(col_to_match)){
        cols_toselect <- unique(c(col_to_match,
                           "teachly",
                           input$filter_crosstab1,
                           input$filter_crosstab2))
        cols_toshow <- unique(c(
          "Name",
          "Teachly",
          stringr::str_sub(input$filter_crosstab1, end = 200),
          stringr::str_sub(input$filter_crosstab2, end = 200))
        )
      }
      else{
        cols_toselect <- unique(c(input$filter_crosstab1,
                           input$filter_crosstab2))
        cols_toshow <- unique(c(
          stringr::str_sub(input$filter_crosstab1, end = 200),
          stringr::str_sub(input$filter_crosstab2, end = 200))
        )
      }

      data_to_show <- selected_table_data()  # Use stored table after "Show Selected Only" is clicked

      if (is.null(data_to_show)) {
        data_to_show <- quiz_processed()  # Default to full table
      }



      dt <- DT::datatable(
        data_to_show %>%
          dplyr::select(cols_toselect) %>%
          data.table::setnames(
            old = cols_toselect,
            new = cols_toshow,
            skip_absent = TRUE
          ),
        escape = FALSE,
        rownames = FALSE,
        style = "bootstrap4",
        filter = "top",
        selection = "multiple",
        options = list(
          pageLength = 100,
          autowidth = TRUE,
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel')
        )
      )
      if("Teachly" %in% cols_toshow){
        dt <- dt |>
          DT::formatStyle(c("Teachly"),
                          backgroundColor = DT::styleInterval(brks, clrs))
      }

      dt

    })

    # Add observeEvent for exporting selected rows
    observeEvent(input$export_selection, {
      selected_rows <- input$quiz_table_rows_selected  # Get selected row indices

      if (length(selected_rows) > 0) {
        # Filter the data to get selected rows
        selected_data <- quiz_processed()[selected_rows, ]%>%
          dplyr::select(matches("^[Yy]our [Nn]ame$"), input$filter_crosstab1, input$filter_crosstab2)

        # Create a temporary Excel file
        temp_file <- tempfile(fileext = ".xlsx")
        writexl::write_xlsx(selected_data, temp_file)
        browseURL(temp_file)
        # Set up download handler
        output$download_excel <- downloadHandler(
          filename = function() {
            paste0("Selected_Rows_", Sys.Date(), ".xlsx")
          },
          content = function(file) {
            file.copy(temp_file, file)
          }
        )

        # Trigger download programmatically
        shinyjs::runjs("$('#download_excel')[0].click();")
      } else {
        # Notify if no rows are selected
        showNotification("No rows selected for export!", type = "error")
      }
    })

    observeEvent(input$copy_to_clipboard, {
      selected_rows <- input$quiz_table_rows_selected
      if (length(selected_rows) > 0) {
        # Select only the relevant columns: Name + selected crosstab questions
        selected_data <- quiz_processed()[selected_rows, ] %>%
          dplyr::select(matches("^[Yy]our [Nn]ame$"), input$filter_crosstab1, input$filter_crosstab2)

        # Convert to tab-separated values (TSV) for clipboard pasting
        clipboard_text <- paste(
          apply(selected_data, 1, function(row) paste(row, collapse = "\t")),
          collapse = "\n"
        )


        # JavaScript code to insert text into a hidden textarea, select it, and copy it
        js_code <- sprintf("
      var textarea = document.createElement('textarea');
      textarea.style.position = 'fixed';
      textarea.style.opacity = 0;
      textarea.value = `%s`;
      document.body.appendChild(textarea);
      textarea.select();
      document.execCommand('copy');
      document.body.removeChild(textarea);
      Shiny.setInputValue('%s', 'success');
    ", clipboard_text, session$ns("clipboard_status"))

        shinyjs::runjs(js_code)

        showNotification("Selected rows copied to clipboard!", type = "message")

      } else {
        showNotification("No rows selected for copying!", type = "error")
      }
    })





    # New output for rendering all quiz answers
    output$all_quiz_answers <- DT::renderDT({
      shiny::req(quiz_processed())
      # Adding validation
      shiny::validate(
        shiny::need(
          !is.null(quiz_processed()),
          message = "Quiz was not uploaded correctly. Check your inputs."
        )
      )

      # Prepare the quiz data
      quiz_data <- quiz_processed()

      # Conditionally remove 'timestamp', 'order', and 'Imported' if they exist
      cols_toremove <- c("Timestamp", "timestamp", "order", "Imported")
      existing_cols <- cols_toremove[cols_toremove %in% colnames(quiz_data)]
      quiz_data <- quiz_data |>
        dplyr::select(-all_of(existing_cols))

      # Rename variations of 'Your Name' and 'teachly'/'Teachly'
      quiz_data <- quiz_data %>%
        rename_with(~ "Name", .cols = matches("^[Yy]our [Nn]ame$")) %>%
        rename_with(~ "Teachly", .cols = matches("^[Tt]eachly$"))

      # 'Name' and 'Teachly' should go first as in the other crosstab
      quiz_data <- quiz_data %>%
        select(Name, Teachly, everything())

      # Matching colors based on teachly score
      brks <- seq(0, 1, 0.1)
      clrs <- colorRampPalette(c("#dc3545", "#ffc107", "#28a745"))(length(brks) + 1)

      # Create the datatable
      dt_all <- DT::datatable(
        quiz_data,
        escape = FALSE,
        rownames = FALSE,
        style = "bootstrap4",
        filter = "top",
        selection = "multiple",
        options = list(
          pageLength = 100,
          autowidth = TRUE,
          scrollX = TRUE,
          buttons = c('copy', 'csv', 'excel')
        )
      )

      # Optional: Apply style based on Teachly score if column exists
      if ("Teachly" %in% colnames(quiz_data)){
        dt_all <- dt_all |>
          DT::formatStyle(
            "Teachly",
            backgroundColor = DT::styleInterval(brks, clrs)
          )
      }

      dt_all
    })

    mod_quiz_multipleChoiceSingle_server("quiz_multipleChoiceSingle_1", FALSE, main_inputs, quiz_processed)
    mod_quiz_prediction_server("quiz_prediction_1", FALSE, main_inputs, quiz_processed)
    mod_quiz_aiSummary_server("quiz_aiSummary_1", FALSE, main_inputs, quiz_processed)
    mod_quiz_aiQuotes_server("quiz_aiQuotes_1", FALSE, main_inputs, quiz_processed)
    mod_quiz_crosstab_server("quiz_Crosstab_1", FALSE, main_inputs, quiz_processed)
    ####### End Render #######

    # output
    #reactive(quiz_processed())
  })
}

