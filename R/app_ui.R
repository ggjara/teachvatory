#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny bs4Dash shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Use Shinyjs
    shinyjs::useShinyjs(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      preloader = list(html = waiter::spin_pixel()),
      dark = NULL,
      # Deactivates light/darktoggle
      title = "Teachvatory",
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Teachvatory",
          color = "primary",
          image = "www/img/logosymbol_dark_background.png"
          # image = "https://www.hks.harvard.edu/sites/default/files/styles/employee_grayscale/public/bio_images/7008-1661999445.jpg" # nolint
        ),
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = shiny::icon("bars"),
        controlbarIcon = shiny::icon("th"),
        fixed = FALSE,
        leftUi = NULL,
        rightUi = bs4Dash::userOutput("user_panel")
      ),
      sidebar = bs4Dash::bs4DashSidebar(
        id = "sidebar",
        status = "primary",
        elevation = 3,
        collapsed = TRUE,
        # bs4Dash::sidebarUserPanel(name = tagList(
        #   shiny::textOutput(outputId = "sidebar_user")
        # )),
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarHeader(shiny::textOutput(outputId = "sidebar_intro")),
          bs4Dash::bs4SidebarMenuItem(
            "Quiz Analysis",
            tabName = "quiz",
            icon = icon("dashboard")
          ),
          bs4Dash::bs4SidebarMenuItem("Metrics",
            tabName = "metrics",
            icon = icon("th")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Roster",
            tabName = "roster",
            icon = icon(
              lib = "glyphicon",
              "user"
            )
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Conventions",
            tabName = "conventions",
            icon = icon(
              lib = "glyphicon",
              "list"
            )
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
        id = "controlbar",
        skin = "light",
        # pinned = FALSE,
        collapsed = FALSE,
        overlay = TRUE,
        bs4Dash::controlbarMenu(
          id = "controlbarmenu",
          bs4Dash::controlbarItem(
            title = "Course config",
            shiny::selectInput(
              inputId = "filter_course",
              label = "Course",
              choices = ""
            ),
            shiny::selectInput(
              inputId = "filter_masterquiz",
              label = "Quizzes Master Sheet",
              choices = c("")
            ),
            shiny::selectInput(
              inputId = "filter_roster",
              label = "Roster",
              choices = c("")
            ),
            shiny::selectInput(
              inputId = "filter_roster_sheet",
              label = "Roster Sheet Name",
              choices = c(""),
              selected = ""
            ),
            bs4Dash::actionButton(
              inputId = "load_course",
              label = "Load Course",

              status = "primary"
            )
          ),
          bs4Dash::controlbarItem(
            title = "Other config",
            shiny::textInput(
              inputId = "path_dashboard",
              "URL Dashboard",
              value = PATH_DASHBOARD
            )
          )
        )
      ),
      footer = bs4Dash::dashboardFooter(
        left = "Created by Dan, Marco, Tyler, & Gonzalo",
        right = "2022"
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "quiz",
            waiter::useWaiter(),
            mod_quiz_ui("quiz_1")
          ),
          bs4Dash::tabItem(
            tabName = "metrics",
            mod_metrics_ui("metrics_1")
          ),
          bs4Dash::tabItem(
            tabName = "roster",
            mod_roster_ui("roster_1")
          ),
          bs4Dash::tabItem(
            tabName = "conventions",
            mod_conventions_ui("conventions_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "teachvatory"
    )
  )
  # Add here other external resources
  # for example, you can add shinyalert::useShinyalert())
}
