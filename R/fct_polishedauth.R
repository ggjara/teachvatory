#' @import polished
#' @import shiny

# Polished configuration options
# app_name, and api_key are retrieved from the .Renv file
# through the config package (./config.yml)
polished::polished_config(
  app_name = config::get("polished_app_name"),
  api_key =  config::get("polished_api_key")
)

#' Polished custom sign in style for `polished::secure_ui`
#'
#' @description Polished Custom sign in style.
#'
#' @return A polished::sign_in_ui_default()
#'
#' @noRd
polished_custom_sign_in <- polished::sign_in_ui_default(
  color = "#FFFFFF",
  button_color = "#17153a",
  company_name = "teachvatory",
  logo_top = shiny::tags$img(
    src = "www/img/logosymbol_white_background.png",
    alt = "teachvatory Logo",
    style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"
  ),
  logo_bottom = shiny::tags$img(
    src = "www/img/logo_white_background.png",
    alt = "Teachvatory logo",
    style = "width: 200px; margin-bottom: 15px; padding-top: 15px;"
  ),
  icon_href = "favicon.ico",
)

#' Polished custom admin button for `polished::secure_ui`
#'
#' @description Polished custom admin button.
#'
#' @return A tagList containing a shiny::actionButton
#'
#' @noRd
polished_custom_admin_button <-
  shiny::tagList(
    shiny::actionButton(
      shiny::NS("polished", "go_to_admin_panel"),
      "",
      icon = shiny::icon("cog"),
      class = "btn-primary",
      style = paste0("position: fixed; ", "bottom", ": 15px; ", "left", ": 15px; z-index: 9999;")
    )
  )