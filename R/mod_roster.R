#' roster UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_roster_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        id = ns("box_filter"),
        width = 12,
        title = "Roster",
        solidHeader = FALSE,
        collapsible = TRUE,
        DT::DTOutput(ns("roster_table"))
      )
    )
  )
}
    
#' roster Server Functions
#'
#' @noRd 
mod_roster_server <- function(id, stringAsFactors = FALSE, main_inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$roster_table <- DT::renderDT({
      shiny::validate(shiny::need(!is.null(main_inputs$roster()), message = "Check roster inputs"))
      data <- main_inputs$roster()

      brks <- seq(0, 1, 0.1)
      clrs <-
        colorRampPalette(c("red", "yellow", "green"))(length(brks) + 1)

      DT::datatable(
        data,
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        selection = "none",
        options = list(
          pageLength = 100,
          autowidth = TRUE,
          #processing = TRUE,
          #dom = "lpft",
          scrollX = TRUE
        )
      ) %>%
        DT::formatStyle(c("teachly"),
                        backgroundColor = DT::styleInterval(brks, clrs))

    })
 
  })
}
    
## To be copied in the UI
# mod_roster_ui("roster_1")
    
## To be copied in the server
# mod_roster_server("roster_1")
