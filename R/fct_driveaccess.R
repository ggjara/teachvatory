# Define options for googledrive auth
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

#' Get courses from a dashabord path
#'
#' @description Filter the files that are in the dashboard folder and
#' follows the pattern "Course Items [course_name]".
#'
#' @param path_dashboard A string (url) of the path to the dashboard folder.
#' 
#' @return A filtered dribble of the files in the dashboard folder or `NULL` if fail.
#'
#' @noRd
#'@import dplyr stringr googledrive 
get_courses <- function(path_dashboard) {
  tryCatch(
    {
      files <- googledrive::as_dribble(path_dashboard)
      folder_files_dribble <- googledrive::drive_ls(files)
      folder_files_dribble %>%
        filter(stringr::str_detect(name, "Course Items ")) %>%
        mutate(
          name = stringr::str_replace(name, "Course Items \\[", ""),
          name = stringr::str_replace(name, "\\]", "")
        )
    },
    error = function(e) {
      return(NULL)
    }
  )
}

#' Get metadata of a googlesheet
#'
#' @description Filter a `directory` (dribble) by `filter`
#' 
#' @param directory A dribble of files in a directory
#' @param filter A string of the file to open
#' 
#' @return Metadata of the googlesheet or `NULL` if fail.
#'
#' @noRd
#' @import dplyr stringr googlesheets4
get_metadata <- function(directory, filter) {
  tryCatch(
    {
      filter_path <- directory %>%
        filter(name == filter)

      if (nrow(filter_path) > 0) {
        metadata <- googlesheets4::gs4_get(filter_path %>% head(1))
        return(metadata)
      }
    },
    error = function(e) {
      return(NULL)
    }
  )
}



#' Get sheetnames of file in course directory
#'
#' @description Filter a `directory` (dribble) by `filter` and returns
#' the sheetnames of the file.
#'
#' @param directory A dribble of files in a directory
#' @param filter A string of the file to open
#'
#' @return A list of sheetnames or `c("")` if fail.
#'
#' @noRd
#' @import dplyr stringr googlesheets4 googledrive
get_sheetnames <- function(directory, filter) {
  tryCatch(
    {
      filter_path <- directory %>%
        filter(name == filter)
      metadata <- googlesheets4::gs4_get(filter_path %>% head(1))
      return(metadata[["sheets"]]$name)
    },
    error = function(e) {
      return(c(""))
    }
  )
}

#' Get list of files of type `type` in `selected_course` directory
#'
#' @description Filter the `courses` (dribble) by `selected_course` and
#' returns all the files of type `type` in the course directory.
#'
#' @param courses A dribble of courses in dashboard directory
#' @param selected_course A string of the course's name
#' @param type A string of the type of file to get. Default is "spreadsheet"
#'
#' @return A dribble of `googledrive` files or a `dribble(NULL)` if fail.
#'
#' @noRd
#' @import dplyr stringr googlesheets4 googledrive
get_course_directory <- function(courses, selected_course, type = "spreadsheet") {
  tryCatch({
    course_temp <- courses %>%
      filter(name == selected_course)
    return(googledrive::drive_ls(course_temp$id[1], type = type))
  }, error = function(e) {
    return(googledrive::as_dribble(NULL))
  })
}

#' Get Roster Sheet Processed
#'
#' @description Load Roster Spreadsheet and cleans it.
#'
#' @param course_directory A dribble of files of the selected course's directory
#' @param filter_roster A string of the Roster's filename
#' @param filter_roster_sheet A string of the sheetname of the Roster spreadsheet
#'
#' @return A tibble of the Roster processed or `NULL` if fail.
#'
#' @import dplyr stringr googlesheets4 googledrive
get_roster <- function(course_directory,
                       filter_roster,
                       filter_roster_sheet) {
  tryCatch({
    roster_path <- course_directory %>%
      filter(name == filter_roster)

    # Get Roster Sheet metadata
    metadata <- googlesheets4::gs4_get(roster_path %>% head(1))
    # Get Roster dataset from input$roster sheet

    roster_temp <-
      googlesheets4::read_sheet(metadata, sheet = filter_roster_sheet)
    roster_temp <- roster_temp %>%
      filter(!is.na(name_canvas) | !is.na(email_teachly)) %>%
      filter(!Invalid) %>%
      select(
        standardized_name,
        email_teachly,
        teachly,
        teachly_comments,
        teachly_absences
      )
    return(roster_temp)
  },
  error = function(e) {
    return(NULL)
  })
}