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
#' @importFrom ellmer chat
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
          choices = c("More Unique", "Funnier", "More Interesting", "More Different", "More opposed (to each other)", "Other"),
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
          value = FALSE
        ),
        shinyWidgets::prettySwitch(
          inputId = ns("see_instruction"),
          label = "See instruction",
          status = "info",
          fill = TRUE ,
          value = FALSE
        ),
        shiny::textOutput(ns("instruction_text")
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

    # Ensure we're correctly handling the 'Other' selection and treating it as a string
    type_selected <- reactive({
      if(input$quizviz_type == "Other") {
        input$custom_type
      } else {
        input$quizviz_type
      }
    })

    shiny::observeEvent(input$quizviz_type, {
      shinyWidgets::updatePrettySwitch(
        session,
        inputId = "see_instruction",
        value = (input$quizviz_type == "Other")
      )
    })

    # Conditionally display instructional text
    shiny::observe({
  if (input$see_instruction) {
    output$instruction_text <- shiny::renderText({
      paste("Instruction: Identify the", input$quizviz_analysis, type_selected(), "answers expressed by the students")
    })
  } else {
    output$instruction_text <- shiny::renderText({ NULL })
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


        filtered_quiz <- quiz_processed()

        if (input$prioritize_teachly) {
          filtered_quiz <- filtered_quiz %>%
            dplyr::filter(.data[["teachly"]] <= 0.5 | is.na(.data[["teachly"]]))
        }

        num_answers <- length(filtered_quiz[[input$quizviz_question]])

        answers_concat <- filtered_quiz %>%
          dplyr::mutate(
            formatted_answer = paste(
              .data[[id_colname()]], ": ", .data[[input$quizviz_question]],
              " |  ", .data[["teachly"]],
              sep = ""
            )
          ) %>%
          dplyr::pull(formatted_answer) %>%
          paste(collapse = " • ")


        # Create Ellmer chat object and get completion using configured settings
        chat_obj <- create_ai_chat()
        
        # Combine system and user messages for the chat
        system_prompt <- paste(
          "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class.

            I will provide the question and the students' answers. The students' answers will be provided as follow:
            [Student 1 LastName, Student 1 FirstName: Student 1 Answer | Student 1 Teachly score  • Student 2 LastName, Student 2 FirstName: Student 2 Answer| Student 2 Teachly score • ...]



            You will do the following:
            1) Identify the ", input$quizviz_analysis, " ", type_selected, "  answers expressed by the students.
            2) DO NOT SELECT MORE THAN ", input$quizviz_analysis, " ANSWERS. Keep the students' answers VERBATIM AND COMPLETE.
            3) Format your response strictly as follows (DO NOT USE Markdown):
            <b>Quotes:</b><br> Student i's Answer <br>( <i> Student i FirstName LastName </i> )<br> <i>TEACHLY SCORE: Student i Teachly Score </i> <br><br> Student j's Answer <br>( <i> Student j FirstName LastName </i> )<br> <i>TEACHLY SCORE: Student j Teachly Score </i> <br><br>
            5) Be very careful with the names, be sure to write them as FirstName Lastname, in that order.
                           "
        )
        
        user_content <- paste0("
            Question: ", input$quizviz_question,"

            Answers:", answers_concat)
        
        full_prompt <- paste0(system_prompt, "\n\n", user_content)
        
        completion <- chat_obj$chat(full_prompt)
        result_text <- completion

        # If there are 5 or fewer answers, append the warning message
        if (num_answers <= 5) {
          result_text <- paste(result_text, "<br><br><b>5 or less students in the list</b>")
        }


        shinyjs::enable("generate_analysis")
        result_text

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
