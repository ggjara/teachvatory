#' mod_quiz_q_aiQuote UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
mod_quiz_aiQuotes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("quizviz_analysis"),
          "Number",
          choices = c("1", "2",  "3", "4", "5"),
          selected = "3"
        ),
        shiny::selectInput(
          ns("quizviz_type"),
          "Type",
          choices = c("More Unique", "Funnier", "More Interesting", "Other"),
          selected =  "More Interesting"
        ),
        shiny::uiOutput(ns("custom_type_input")
        ), # Placeholder for the conditional input
        shiny::selectInput(
          ns("quizviz_question"),
          "Question",
          choices = c(""),
          selected = NULL
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("prioritize_teachly"),
          label = "Prioritize Low Teachly Score",
          status = "info",
          fill = TRUE ,
          value = TRUE
        ),
        # Submit button
        bs4Dash::actionButton(
          inputId = ns("generate_analysis"),
          label = "Generate analysis",
          status = "primary"
        )
      ),
      bs4Dash::column(
        width = 8,
        shinycssloaders::withSpinner(
          shiny::htmlOutput(
            ns("analysis_text"),
          )
        )
      )
    )
  )
}


#' mod_quiz_q_aiQuotes Server Functions
#'
#' @noRd
mod_quiz_aiQuotes_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
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

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question",
        choices = unique(questions()),
        selected = unique(questions())[1]
      )
    })

    shiny::observe({
      if (input$quizviz_type == "Other") {
        output$custom_type_input <- shiny::renderUI({
          shiny::textInput(ns("custom_type"), "Specify Type")
        })
      } else {
        output$custom_type_input <- shiny::renderUI({ NULL }) # Render nothing if not "Other"
      }
    })

    # Get colname of "Your Name" input. If doesn't exist, return ""
    id_colname <- shiny::reactive({
      get_idcolname(quiz_processed())
    })

    ai_response <- shiny::eventReactive(input$generate_analysis, {
      shinyjs::disable("generate_analysis")
      tryCatch({
        # Ensure we're correctly handling the 'Other' selection and treating it as a string
        type_selected <- ifelse(input$quizviz_type == "Other", input$custom_type, input$quizviz_type)

       answers_concat <- quiz_processed() %>%
          dplyr::mutate(
            formatted_answer = paste(
              .data[[id_colname()]], ": ", .data[[input$quizviz_question]],
              " |  ", .data[["teachly"]], # Assuming 'teachly' is the column name for Teachly scores
              sep = ""
            )
          ) %>%
          dplyr::pull(formatted_answer) %>%
          paste(collapse = " • ")


       prioritize_text <- if(input$prioritize_teachly) {
         " Prioritize those answers from students with low Teachly scores."
       } else {
         ""
       }


        client <- openai::OpenAI()
        completion <- client$chat$completions$create(
          model = "gpt-4o",
          messages = list(
            list(
              "role" = "system",
              "content" = paste(
                "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class.
            You will do the following:
            1. Identify the ", input$quizviz_analysis, " ", type_selected, "  answers expressed by the students. Keep the students' answers verbatim and complete. DO NOT SELECT MORE THAN ", input$quizviz_analysis, " ANSWERS. ",prioritize_text,"

            I will provide the question and the students' answers. The students' answers will be provided as follow:
            [Student 1 Name: Student 1 Answer | Student 1 Teachly score  • Student 2 Name: Student 2 Answer| Student 2 Teachly score • ...]

            Format your response strictly as follows:
            <b>Quotes:</b><br>1. Answer i <br>( <i> Student i Name </i> )<br> <i>TEACHLY SCORE: Student i Teachly Score </i> <br><br> 2. Answer j <br>( <i> Student j Name </i> )<br> <i>TEACHLY SCORE: Student j Teachly Score </i> <br><br>
            ")
            ),
            list(
              "role" = "user",
              "content" = paste0("
            Question: ", input$quizviz_question,"

            Answers:", answers_concat)
            )
          )
        )
        shinyjs::enable("generate_analysis")
        completion$choices[[1]]$message$content

        }, error = function(e){
          "There was an error with your request. See the logs for more information."
          shinyjs::enable("generate_analysis")
        })
    })




    output$analysis_text <- renderUI({
      shiny::req(ai_response())
      HTML(ai_response())
    })





  })
}
