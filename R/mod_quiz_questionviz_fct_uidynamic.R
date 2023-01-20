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
      )
    )
  )
}
