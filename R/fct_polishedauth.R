#' @import polished
#' @import shiny

# Polished configuration options
# app_name, and api_key are retrieved from the .Renv file
# through the config package (./config.yml)
polished::polished_config(
  app_name = config::get("polished_app_name"),
  api_key =  config::get("polished_api_key")
)

#' Polished custom sign in
#'
#' @description Polished Custom sign in style to pass as parameter to
#' `polished::secure_ui` as `sign_in_page_ui` = `polished_custom_sign_in``
#'
#' @return A polished::sign_in_ui_default()
#'
#' @noRd
polished_custom_sign_in <- polished::sign_in_ui_default(
  color = "#FFFFFF",
  button_color = "#17153a",
  company_name = "teachvatory",
  logo_top = tagList(
    golem_add_external_resources(),
    tags$img(
      src = "https://i.ibb.co/mBbc23k/logosymbol-white-background.png",
      alt = "teachvatory Logo",
      style = "width: 125px; margin-top: 30px; margin-bottom: 30px;"
    )
  ),
  logo_bottom = tags$img(
    src = "https://i.ibb.co/nwFG9TX/logo-white-background.png",
    alt = "Teachvatory logo",
    style = "width: 200px; margin-bottom: 15px; padding-top: 15px;"
  ),
  icon_href = "favicon.ico",
)


#' Polished custom admin button
#'
#' @description Polished custom admin button to pass as parameter to
#' `polished::secure_ui`as `custom_admin_button_ui`= `polished_custom_admin_button``
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
