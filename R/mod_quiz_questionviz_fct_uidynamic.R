get_quizviz_dynamic_ui <- function(quizviz_type, ns) {
  switch(
    quizviz_type,
    "Multiple Choice (single)" = shiny::tagList(
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
      shiny::checkboxInput(
        ns("quizviz_arrange_by_frequency"),
        "Order by frequency?",
        value = FALSE
      )
    ),
    "Multiple Choice (multiple)" = shiny::tagList(
      shiny::selectInput(
        ns("probando"),
        "Como estás?",
        choices = c("Bien", "Mal", "Mas o menos"),
        selected = c("Mal")
      )
    ),
    "Prediction" = shiny::tagList(
      shiny::selectInput(
        ns("quizviz_question"),
        "Question",
        choices = c(""),
        selected = NULL
      ),
      shiny::textInput(
        ns("quizviz_correctanswer"),
        label = "Enter a reference point (if one exists)"
      )
    )
  )
}
