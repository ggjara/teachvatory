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
          ns("quizviz_analysis"),
          "Analysis",
          choices = c("3 Main ideas", "4 Main Ideas", "5 Main Ideas",
                      "3 Misconceptions", "4 Misconceptions", "Other"),
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
          label = "Filter by other Question",
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

    ai_response <- shiny::eventReactive(input$generate_analysis, {
      shinyjs::disable("generate_analysis")
      tryCatch({
        # Ensure we're correctly handling the 'Other' selection and treating it as a string
        type_selected <- ifelse(input$quizviz_analysis == "Other", input$custom_type, input$quizviz_analysis)

        if (input$second_question) {
          answers_concat <- quiz_processed() %>%
            dplyr::mutate(name_answer = paste(.data[[id_colname()]],
                                              .data[[input$quizviz_question]],
                                              .data[[input$quizviz_question2]], sep = ": ")) %>%
            dplyr::pull(name_answer) %>%
            paste(collapse = " • ")

          question_text <- paste0("
        Question 1: ", input$quizviz_question, "\n
        Question 2: ", input$quizviz_question2, "\n
        Answer for Question 2 to consider: ", input$quizviz_answers2, "\n
        Answers:", answers_concat)

          prompt_content <- paste(
            "As an AI teaching assistant, your task is to analyse students' responses to two questions posed in class to identify main concepts in their answers and list the students who contributed to each concept.

       I will provide the questions and the students' answers. The students' answers will be provided as follows:
        [Student 1 Last Name, Student 1 First Name : Student 1 Answer Question 1 : Student 1 Answer Question 2 • Student 2 Last Name, Student 2 First Name: Student 2 Answer Question 1 : Student 2 Answer Question 2 • ...]


        You will do the following:
        1. Choosing ONLY the students that answered Question 2 as ", input$quizviz_answers2, ", summarize the ", type_selected, " expressed by those students in their answers to Question 1.
        2. After each point, you will list UP TO FIVE students MAX who contributed to each point. DO NOT LIST MORE THAN 5 STUDENTS.


        Format your response strictly as follows:
        <b>", type_selected, ":</b><br>1. Idea 1 <br> (<i>Student i FirstName LastName ; Student j FirstName LastName; ...</i>)<br><br>

        Be very careful with the names, be sure to write them as FirstName Lastname, in that order.    "
          )
        } else {
          answers_concat <- quiz_processed() %>%
            dplyr::mutate(name_answer = paste(.data[[id_colname()]], .data[[input$quizviz_question]], sep = ": ")) %>%
            dplyr::pull(name_answer) %>%
            paste(collapse = " • ")

          question_text <- paste0("
        Question: ", input$quizviz_question, "\n
        Answers:", answers_concat)

          prompt_content <- paste(
            "As an AI teaching assistant, your task is to analyse students' responses to a question posed in class to identify main concepts in their answers and list the students who contributed to each concept.
        You will do the following:
        1. Summarize the ", type_selected, " expressed by the students.
        2.After each point, you will list UP TO FIVE students MAX who contributed to each point.DO NOT LIST MORE THAN 5 STUDENTS.

        I will provide the question and the students' answers. The students' answers will be provided as follows:
        [Student 1 LastName, Student 1 FirstName : Student 1 Answer • Student 2 LastName, Student 2 FirstName: Student 2 Answer • ...]

        Format your response strictly as follows:
        <b>", type_selected, ":</b><br>1. Idea 1 <br> (<i>Student i FirstName LastName; Student j FirstName LastName; ...</i>)<br><br>

        Be very careful with the names, be sure to write them as FirstName Lastname, in that order.        "
          )
        }

        client <- openai::OpenAI()
        completion <- client$chat$completions$create(
          model = "gpt-4-0125-preview",
          messages = list(
            list(
              "role" = "system",
              "content" = prompt_content
            ),
            list(
              "role" = "user",
              "content" = question_text
            )
          )
        )
        shinyjs::enable("generate_analysis")
        completion$choices[[1]]$message$content

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
