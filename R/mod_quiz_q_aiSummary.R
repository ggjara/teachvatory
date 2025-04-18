#' mod_quiz_q_aiSummary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import highcharter
mod_quiz_aiSummary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      bs4Dash::column(
        width = 4,
        shiny::selectInput(
          ns("quizviz_number"),
          "Number",
          choices = c("1", "2",  "3", "4", "5"),
          selected = "3"
        ),
        shiny::selectInput(
          ns("quizviz_analysis"),
          "Analysis",
          choices = c("Main ideas", "Most Repeated Concepts", "Misconceptions", "Other"),
          selected = "Main ideas"
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
          inputId = ns("second_question"),
          label = "Filter by other question",
          status = "info",
          fill = TRUE ,
          value = FALSE
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("second_question"), "'] == true"),
          tagList(
            shiny::selectInput(
              ns("quizviz_question2"),
              "Question 2",
              choices = c(""),
              selected = NULL
            ),
            shiny::selectInput(
              ns("quizviz_answers2"),
              "Select Answers for Q2",
              choices = c(""), # This will be updated with actual choices in the server function
              selected = NULL
            )
          )
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


#' mod_quiz_q_aiSummary Server Functions
#'
#' @noRd
mod_quiz_aiSummary_server <- function(id, stringAsFactors = FALSE, main_inputs, quiz_processed) {
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

    shiny::observeEvent(quiz_processed(), {
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_question2",
        choices = unique(questions()),
        selected = unique(questions())[1]
      )
    })

    answers2 <- shiny::reactive({
      tryCatch({
        answers_temp <- quiz_processed() %>%
          mutate(!!input$quizviz_question2 := as.character(.data[[input$quizviz_question2]])) %>%
          mutate(!!input$quizviz_question2 := case_when(
            is.na(.data[[input$quizviz_question2]]) ~ "No answer",
            TRUE ~ .data[[input$quizviz_question2]],
          )) %>%
          group_by(.data[[input$quizviz_question2]]) %>%
          summarize(n = n()) %>%
          ungroup()

        answers_temp <- answers_temp %>%
          pull(.data[[input$quizviz_question2]]) %>%
          unique()  # Remove duplicates

        c("No correct answer", answers_temp)
      }, error = function(e) {
        NULL
      })
    })

    shiny::observeEvent(input$quizviz_question2, {
      possible_answers <- answers2()
      shiny::updateSelectInput(
        session,
        inputId = "quizviz_answers2",
        choices = possible_answers,
        selected = possible_answers[1]
      )
    })



    shiny::observe({
      if (input$quizviz_analysis == "Other") {
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

    # Ensure we're correctly handling the 'Other' selection and treating it as a string
     type_selected <- reactive({
       if(input$quizviz_analysis == "Other") {
         input$custom_type
       } else {
         input$quizviz_analysis
       }
     })

     # Conditionally display instructional text
     shiny::observe({
       if (input$see_instruction) {
         output$instruction_text <- shiny::renderText({
           paste("Instruction: Summarize the ", input$quizviz_number, " ", type_selected(), " expressed by the students.")
         })
       } else {
         output$instruction_text <- shiny::renderText({ NULL })
       }
     })

     shiny::observeEvent(input$quizviz_analysis, {
       shinyWidgets::updatePrettySwitch(
         session,
         inputId = "see_instruction",
         value = (input$quizviz_analysis == "Other")
       )
     })

    ai_response <- shiny::eventReactive(input$generate_analysis, {
      shinyjs::disable("generate_analysis")
      tryCatch({
        # Ensure we're correctly handling the 'Other' selection and treating it as a string
        type_selected <- ifelse(input$quizviz_analysis == "Other", input$custom_type, input$quizviz_analysis)

        filtered_quiz <- quiz_processed()

        if (input$second_question) {

          filtered_quiz <- filtered_quiz %>%
            dplyr::filter(.data[[input$quizviz_question2]] == input$quizviz_answers2)
        }
        num_answers <- length(filtered_quiz[[input$quizviz_question]])

        answers_concat <- filtered_quiz %>%
          dplyr::mutate(name_answer = paste(.data[[id_colname()]], .data[[input$quizviz_question]], sep = ": ")) %>%
          dplyr::pull(name_answer) %>%
          paste(collapse = " • ")

        question_text <- paste0("
        Question: ", input$quizviz_question, "\n
        Answers:", answers_concat)

        prompt_content <- paste(
          "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class to identify main concepts in their answers and list the students who contributed to each concept.
        I will provide the question and the students' answers. The students' answers will be provided as follows:
        [Student 1 LastName, Student 1 FirstName : Student 1 Answer • Student 2 LastName, Student 2 FirstName: Student 2 Answer • ...]

       You will do the following:

        1. Summarize the ", input$quizviz_number, " ", type_selected, " expressed by the students. In doing your analysis, be as thoughtful, analytical, and insightful as possible.
        2. After each point, you will list UP TO FIVE students MAX who contributed to each point.DO NOT LIST MORE THAN 5 STUDENTS.

         Format your response strictly as follows (DO NOT USE MARKDOWN and DONT USE BOLD text):
       ", type_selected, ": <br> Concept i: Answer <br> (<i>Student i FirstName LastName; Student j FirstName LastName; ...</i>)<br><br>

        Be very careful with the names, be sure to write them as FirstName Lastname, in that order.
        "
        )


        client <- openai::OpenAI()
        completion <- client$chat$completions$create(
          model = "gpt-4o",
          messages = list(
            list(
              "role" = "system",
              "content" = prompt_content
            ),
            list(
              "role" = "user",
              "content" = question_text
            )
          ),
          temperature = 0.6  # Adjust the temperature here (0.0 to 1.0)
        )
        result_text <- completion$choices[[1]]$message$content

        # If there are 5 or fewer answers, append the warning message
        if (num_answers <= 5) {
          result_text <- paste(result_text, "<br><br><b>5 or less students in the list</b>")
        }

        shinyjs::enable("generate_analysis")
        result_text

      }, error = function(e) {
        shinyjs::enable("generate_analysis")
        "There was an error with your request. See the logs for more information."
      })
    })

    output$analysis_text <- renderUI({
      shiny::req(ai_response())
      HTML(ai_response())
    })






  })
}
