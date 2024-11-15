#' metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_metrics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      id = ns("box_parameters1"),
      width = 12,
      title = "Options",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::fluidRow(
        bs4Dash::column(
          width = 6,
          shinyWidgets::multiInput(
            inputId = ns("filter_quiz"),
            label = "Quizzes to analyze",
            choices = c(""),
            selected = NULL,
            options = list(
              `live-search` = TRUE,
              `actions-box` = TRUE,
              `placeholder` = "Select quizzes"
            )
          ),
          shiny::dateInput(
            inputId = ns("initial_date"),
            label = "Analysis Start Date",
            value = as.Date(INITIAL_DATE_DEFAULT)
          ),
          bs4Dash::actionButton(
            inputId = ns("load_metrics"),
            label = "Load quizzes",
            status = "primary"
          )
        ),
        bs4Dash::column(
          width = 4,
          shiny::tags$strong("Visualization options"),
          shiny::tags$hr(),
          shinyWidgets::pickerInput(
            inputId = ns("teachly_columns"),
            label = "Teachly data to show",
            choices = c(
              "Email" = "email_teachly",
              "Teachly score" = "teachly",
              "Number of comments" = "teachly_comments",
              "Number of absences" = "teachly_absences"
            ),
            selected = c("email_teachly","teachly"),
            multiple = TRUE,
            options = list(`actions-box` = TRUE)
          ),
          shinyWidgets::materialSwitch(
            inputId = ns("switch_reverse"),
            label = "Reverse quiz order",
            value = TRUE,
            status = "primary"
          ),
          shiny::tags$hr(),
          shiny::tags$p("First quiz: ", shiny::textOutput(outputId = ns("first_quiz"), inline = TRUE)),
          shiny::tags$p("Last quiz: ", shiny::textOutput(outputId = ns("last_quiz"), inline = TRUE))
        )
      )

    ),
    bs4Dash::tabBox(
      id = ns("box_dataset"),
      width = 12,
      solidHeader = FALSE,
      collapsible = TRUE,
      collapsed = FALSE,
      status = "primary",
      type = "tabs",
      shiny::tabPanel(
        title = "",
        icon = shiny::icon("table", lib = "font-awesome"),
        shinycssloaders::withSpinner(DT::DTOutput(ns(
          "metrics_dataframe"
        )))
      ),
      shiny::tabPanel(
        shinyjs::useShinyjs(),
        title = "",
        icon = shiny::icon("user", lib = "font-awesome"),
        shiny::fluidRow(
          bs4Dash::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns("n_lastquizzes"),
              label = "Number of last quizzes missed",
              min = 0,
              max = 0,
              step = 1,
              value = 0
            ),
            shiny::tags$p("People who have not submitted the last ",
                          shiny::textOutput(outputId = ns("n_lastquizzes_output"),
                                            inline = TRUE),
                          " quizzes."),
            shinycssloaders::withSpinner(DT::DTOutput(ns(
              "metrics_lastquizzes"
            ))),
            shinycssloaders::withSpinner(DT::DTOutput(ns(
              "metrics_tocall"
            )))
          ),
          bs4Dash::column(
            width = 6,
            textInput(ns("subject"), "Email Subject:", "Reminder: Missing Assignments"),
            textAreaInput(
              ns("message"),
              "Email Message:",
              "Dear [Name],\n\nWe have noticed that you haven’t submitted your recent assignments. Please take a moment to submit them at your earliest convenience. If you’re facing any challenges, feel free to reach out for support.\n\nBest regards,\n\nThe Teaching Team\n\nThis email is not monitored. If you have any questions, please reply directly to Professor Levy, who is cc'ed here.\n"
            ),
            actionButton(ns("send_email"), "Send Email"),
            verbatimTextOutput(ns("email_status"))
          )
        )
      )

    )
  )
}

#' metrics Server Functions
#'
#' @noRd
mod_metrics_server <-
  function(id, stringAsFactors = FALSE, main_inputs) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns

      shinyjs::disable("load_metrics")
      shiny::observeEvent(input$filter_quiz, {
        if(is.null(input$filter_quiz)){
          shinyjs::disable("load_metrics")
        }
        else{
          shinyjs::enable("load_metrics")
        }
      }, ignoreNULL = FALSE)

      quizzes <- shiny::eventReactive(main_inputs$masterquiz_md(), {
        tryCatch({
          quizzes_temp <- main_inputs$masterquiz_md()[["sheets"]]$name

          # Ideally we should delete the next two filters
          # quizzes_temp <-
          #   quizzes_temp[!(
          #     toupper(quizzes_temp) %in% c(
          #       "ROSTER",
          #       "ANSWER KEY",
          #       "SETTINGS",
          #       "STUDENT DB",
          #       "STUDENT DB-TEST"
          #     )
          #   )]
          # quizzes_temp <- quizzes_temp[grepl("[0-9]", quizzes_temp)]
          quizzes_temp
        },
        error = function(e) {
          NULL
        })
      })

      shiny::observeEvent(quizzes(), {
        shinyWidgets::updateMultiInput(
          session,
          inputId = "filter_quiz",
          choices = quizzes(),
          selected = NULL
        )
      })

      # Send number of quizzes
      output$n_lastquizzes_output <- shiny::renderText({
        shiny::req(input$n_lastquizzes)
        input$n_lastquizzes
      })

      # Full Dataframe
      metrics_ini <- shiny::eventReactive(input$load_metrics, {
        create_metrics_dataframe(
          roster = main_inputs$roster(),
          masterquiz_md = main_inputs$masterquiz_md(),
          quizzes = input$filter_quiz,
          initial_date = input$initial_date
        )
      })

      shiny::observeEvent(metrics_ini(), {
        shiny::updateSliderInput(
          session,
          inputId = "n_lastquizzes",
          max = length(metrics_ini()$quizzes),
          value = 1
        )
      })

      # Quiz cols sorted according to switch
      quiz_cols <- shiny::reactive({
        quiz_cols <- metrics_ini()$quizzes
        if (input$switch_reverse) {
          quiz_cols <- rev(quiz_cols)
        }
        print(quiz_cols)
        quiz_cols
      })

      # First and Last quizzes
      output$first_quiz <- shiny::renderText({
        shiny::req(quiz_cols())
        quiz_cols()[length(quiz_cols())]
      })
      output$last_quiz <- shiny::renderText({
        shiny::req(quiz_cols())
        quiz_cols()[1]
      })

      # Metrics dataframe
      metrics_dataframe <- shiny::reactive({
        shiny::req(metrics_ini())
        metrics_temp <- metrics_ini()

        # Quizzes
        quizzes_cols <- quiz_cols()

        # Total
        metrics_temp$dataframe$total <-
          rowSums(metrics_temp$dataframe[, quizzes_cols])

        # Sparkline
        metrics_trend <-  metrics_temp$dataframe |>
          dplyr::select("standardized_name", rev(all_of(quizzes_cols))) |>
          tidyr::pivot_longer(!standardized_name,
                              names_to = "quiz",
                              values_to = "value") |>
          dplyr::mutate(value = as.integer(value)) |>
          dplyr::group_by(standardized_name) |>
          dplyr::mutate(value = cumsum(value)) |>
          dplyr::summarize(Trend = sparkline::spk_chr(
            value,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = max(value)
          ))

        metrics_temp$dataframe |>
          dplyr::left_join(metrics_trend)
      })

      # Metrics lastquizzes
      metrics_lastquizzes <- shiny::reactive({
        metrics_temp <- metrics_ini()

        # Quizzes
        quizzes_cols <- quiz_cols()[1:input$n_lastquizzes]

        # Total
        metrics_temp$dataframe$total <-
          rowSums(metrics_temp$dataframe[, quizzes_cols])

        # Sparkline
        metrics_trend <-  metrics_temp$dataframe |>
          dplyr::select("standardized_name", rev(all_of(quizzes_cols))) |>
          tidyr::pivot_longer(!standardized_name,
                              names_to = "quiz",
                              values_to = "value") |>
          dplyr::mutate(value = as.integer(value)) |>
          dplyr::group_by(standardized_name) |>
          dplyr::mutate(value = cumsum(value)) |>
          dplyr::summarize(Trend = sparkline::spk_chr(
            value,
            type = "line",
            chartRangeMin = 0,
            chartRangeMax = max(value)
          ))

        metrics_temp$dataframe |>
          dplyr::left_join(metrics_trend)
      })


      output$metrics_lastquizzes <- DT::renderDT({
        shiny::req(metrics_lastquizzes())
        # Teachly cols
        teachly_cols <- input$teachly_columns

        # Quiz cols
        quizzes_cols <- quiz_cols()[1:input$n_lastquizzes]

        # Teachly colors
        brks <- seq(0, 1, 0.1)
        clrs <-
          colorRampPalette(c("#dc3545", "#ffc107", "#28a745"))(length(brks) + 1)

        # DT
        DT::datatable(
          metrics_lastquizzes() |>
            dplyr::filter(total==0) |>
            dplyr::select(
              standardized_name,
              teachly_cols
            ) |>
            data.table::setnames(
              old = c(
                "standardized_name",
                "total",
                "teachly",
                "teachly_comments",
                "teachly_absences"
              ),
              new = c(
                "Name",
                "Total",
                "Teachly score",
                "Teachly comments",
                "Teachly absences"
              ),
              skip_absent = TRUE
            ),
          escape = FALSE,
          rownames = FALSE,
          extensions = "Buttons",
          #style = "bootstrap4",
          filter = "top",
          selection = "none",
          options = list(
            pageLength = 100,
            autowidth = TRUE,
            scrollX = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            dom = 'ltipB',
            fnDrawCallback = htmlwidgets::JS('
function(){
  HTMLWidgets.staticRender();
}
')
          )
        ) |>
          sparkline::spk_add_deps() |>
          (\(.) {
            if ("teachly" %in% teachly_cols)
            {
              . |>
                DT::formatStyle(c("Teachly score"),
                                backgroundColor = DT::styleInterval(brks, clrs))
            } else{
              .
            }
          })()
      })

      output$metrics_tocall <- DT::renderDT({
        DT::datatable(
          dplyr::tibble()
        )
      })

      output$metrics_dataframe <- DT::renderDT({
        shiny::req(metrics_dataframe())
        # Teachly cols
        teachly_cols <- input$teachly_columns

        # Quiz cols
        quizzes_cols <- quiz_cols()

        # Teachly colors
        brks <- seq(0, 1, 0.1)
        clrs <-
          colorRampPalette(c("#dc3545", "#ffc107", "#28a745"))(length(brks) + 1)

        # DT
        DT::datatable(
          metrics_dataframe() |>
            dplyr::select(
              standardized_name,
              teachly_cols,
              total,
              Trend,
              quizzes_cols
            ) |>
            data.table::setnames(
              old = c(
                "standardized_name",
                "total",
                "teachly",
                "teachly_comments",
                "teachly_absences"
              ),
              new = c(
                "Name",
                "Total",
                "Teachly score",
                "Teachly comments",
                "Teachly absences"
              ),
              skip_absent = TRUE
            ),
          escape = FALSE,
          rownames = FALSE,
          extensions = "Buttons",
          #style = "bootstrap4",
          selection = "none",
          filter = "top",
          options = list(
            pageLength = 100,
            autowidth = TRUE,
            scrollX = TRUE,
            buttons = c('copy', 'csv', 'excel'),
            dom = 'ltipB',
            fnDrawCallback = htmlwidgets::JS('
function(){
  HTMLWidgets.staticRender();
}
')
          )
        ) |>
          DT::formatStyle(
            seq(4 + length(teachly_cols),
                ncol(metrics_dataframe())),
            target = 'cell',
            backgroundColor = DT::styleEqual(c(1, 0), c('#28a745', '#dc3545'))
          ) |>
          DT::formatStyle(
            columns = 3 + length(teachly_cols),
            target = 'cell',
            `border-right` = DT::styleRow(seq(1, nrow(metrics_dataframe(
            ))),
            "solid 3px")
          ) |>
          sparkline::spk_add_deps() |>
          (\(.) {
            if ("teachly" %in% teachly_cols)
            {
              . |>
                DT::formatStyle(c("Teachly score"),
                                backgroundColor = DT::styleInterval(brks, clrs))
            } else{
              .
            }
          })()
      })

      ## Emails -------
      # Send email when the button is clicked
      observeEvent(input$send_email, {
        # Get data from metrics_lastquizzes and filter for students with total == 0
        data <- metrics_lastquizzes()
        missing_students <- data |>
          dplyr::filter(total == 0) |>
          dplyr::select(standardized_name, email_teachly, total)

        # Initialize success flag, error message, and list to track sent emails
        all_emails_sent    <- TRUE
        last_error_message <- NULL
        sent_students      <- list()  # List to collect names and emails of those emailed

        # Check if there are students to email
        if (nrow(missing_students) > 0) {
          for (i in 1:nrow(missing_students)) {
            # Extract student details
            student_name  <- missing_students$standardized_name[i]
            student_email <- missing_students$email_teachly[i]

            # Extract first name, using entire name if there is no comma
            name_parts <- strsplit(student_name, ",\\s*")[[1]]
            first_name <- if (length(name_parts) > 1) name_parts[2] else student_name

            # Customize the email message by replacing [Name] with the student's first name
            email_body <- gsub("\\[Name\\]", first_name, input$message)

            # Create the email
            email <- compose_email(
              body = md(email_body)
            )

            # Send the email with error handling
            tryCatch({
              smtp_send(
                email,
                from = "teachvatory.emails@gmail.com",
                to = student_email,
                cc = "dan_levy@hks.harvard.edu",
                subject = input$subject,
                credentials = creds_file("gmail_creds")
              )
              # Append to sent_students list
              sent_students <- append(sent_students, paste(student_name, "-", student_email))
              cat("Email sent to:", student_name, "-", student_email, "\n")
            }, error = function(e) {
              # Update error status and message
              all_emails_sent <<- FALSE   # Set flag to FALSE if an error occurs
              last_error_message <<- paste("Failed to send email to:", student_email, "Error:", e$message)
              cat(last_error_message, "\n")
            })
          }

          # Show confirmation modal with the list of students emailed
          sent_students_text <- paste("Emails sent to:\n", paste(sent_students, collapse = "\n"))
          showModal(modalDialog(
            title = "Emails Sent Successfully",
            tagList(pre(sent_students_text))  # Display list of students emailed with header
          ))

          # Update email status based on success flag
          if (all_emails_sent) {
            output$email_status <- renderText("Emails sent successfully!")
          } else {
            output$email_status <- renderText(last_error_message)
          }
        } else {
          output$email_status <- renderText("No students to email.")
        }

        # Clear the email status after a brief delay
        shinyjs::delay(6000, {
          output$email_status <- renderText("")
        })

      }) # end


    })
  }

## To be copied in the UI
# mod_metrics_ui("metrics_1")

## To be copied in the server
# mod_metrics_server("metrics_1")
