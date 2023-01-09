#' conventions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_conventions_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      id = ns("conventions_box"),
      width = 12,
      title = "Conventions",
      #icon = shiny::icon("list", lib = "glyphicon"),
      solidHeader = TRUE,
      collapsible = TRUE,
      "Updated on December, 26th, 2022",
      shiny::tags$hr(),
      shiny::h4("Course folders structure"),
      shiny::tags$ol(
        shiny::tags$li("The course folder is named 'Course Items [_COURSE NAME_]'"),
        shiny::tags$li("The course folder is located inside the Dashboard main folder"),
        shiny::tags$li("(Ideally) The master quiz spreadsheet is named [_COURSE NAME_ Quizzes Master Sheet]"),
        shiny::tags$li("(Ideally) The roster spreadsheet is named [_COURSE NAME_ Roster]")
      ),
      shiny::h4("Roster"),
      shiny::tags$ol(
        shiny::tags$li("Each roster has, at least, columns [standardized_name], [teachly], and [invalid]"),
        shiny::tags$li("[standardized_name] is the same names used to populate the quizzes' [Your Name] question"),
        shiny::tags$li("(Ideally) The sheet (tab) name where the roster is called [roster]")
      ),
      shiny::h4("Quizzes"),
      shiny::tags$ol(
        shiny::tags$li("The first question of the quizzes is [Your Name]. The options for this question are the names on the roster [standardzied_name] or [FormRangerColumn] column"),
        shiny::tags$li("(Optional) The second question of the quizzes is [If your name is not listed above ...]"),
        shiny::tags$li("(Ideally) The quizzes numbering uses two digits: Q.01, Q.02a, Q.02b, Q12")
      )
    )
  )
}
    
#' conventions Server Functions
#'
#' @noRd 
mod_conventions_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_conventions_ui("conventions_1")
    
## To be copied in the server
# mod_conventions_server("conventions_1")
