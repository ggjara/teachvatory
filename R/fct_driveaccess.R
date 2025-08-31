# Define options for googledrive auth
options(
  gargle_oauth_email = TRUE,
  gargle_oauth_cache = ".secrets"
)

#'Get courses from a dashabord path
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

    # Filter basic conditions
    roster_temp <- roster_temp %>%
      filter(!is.na(name_canvas) | !is.na(email_teachly)) %>%
      #Filter Student Test
      filter(tolower(standardized_name) != "student, test")
    
    # Filter invalids if Invalid column exists
    if ("Invalid" %in% colnames(roster_temp)) {
      roster_temp <- roster_temp %>%
        filter(!Invalid)
    }

    # Keep basic columns and Canvas matching columns for filter sheet integration
    roster_temp <- roster_temp %>%
      select(
        standardized_name,
        email_teachly,
        teachly,
        teachly_comments,
        teachly_absences,
        # Keep Canvas columns for filter sheet matching
        any_of(c("name_canvas", "sis_canvas", "email_canvas"))
      )

    # Get only one observation per person. Keep first encounter in Roster.
    roster_temp <- roster_temp %>%
      dplyr::mutate(order = seq.int(nrow(roster_temp))) |>
      dplyr::group_by(standardized_name) |>
      dplyr::filter(order == min(order)) |>
      dplyr::ungroup()

    return(roster_temp)
  },
  error = function(e) {
    return(NULL)
  })
}

#' Get Filter Sheet Data
#'
#' @description Load filter sheet data from the same Google Sheet file as the roster.
#' Columns A, B, C are name_canvas, sis_canvas, email_canvas. Columns D onwards are filter variables.
#'
#' @param course_directory A dribble of files of the selected course's directory
#' @param filter_roster A string of the Roster's filename (same file as filter sheet)
#' @param filter_sheet A string of the sheetname of the Filter spreadsheet
#'
#' @return A tibble of the Filter sheet data or `NULL` if fail.
#'
#' @import dplyr stringr googlesheets4 googledrive
get_filter_sheet <- function(course_directory,
                            filter_roster,
                            filter_sheet) {
  tryCatch({
    roster_path <- course_directory %>%
      filter(name == filter_roster)

    # Get Sheet metadata (same file as roster)
    metadata <- googlesheets4::gs4_get(roster_path %>% head(1))
    
    # Get Filter sheet dataset
    filter_temp <- googlesheets4::read_sheet(metadata, sheet = filter_sheet)

    # Basic validation - ensure we have the expected columns A, B, C
    if (ncol(filter_temp) < 3) {
      warning("Filter sheet must have at least 3 columns (A: name_canvas, B: sis_canvas, C: email_canvas)")
      return(NULL)
    }
    
    # Rename first three columns to standard names: name_canvas, sis_canvas, email_canvas
    colnames(filter_temp)[1:3] <- c("name_canvas", "sis_canvas", "email_canvas")
    
    # Filter out rows where all key columns are NA
    filter_temp <- filter_temp %>%
      filter(!is.na(name_canvas) | !is.na(sis_canvas) | !is.na(email_canvas))
    
    # Remove any completely empty rows
    filter_temp <- filter_temp %>%
      filter(if_any(everything(), ~ !is.na(.x)))

    return(filter_temp)
  },
  error = function(e) {
    warning(paste("Error loading filter sheet:", e$message))
    return(NULL)
  })
}

#' Get Filter Variables from Filter Sheet
#'
#' @description Get list of filter variables (columns D onwards) from the filter sheet
#'
#' @param course_directory A dribble of files of the selected course's directory
#' @param filter_roster A string of the Roster's filename (same file as filter sheet)
#' @param filter_sheet A string of the sheetname of the Filter spreadsheet
#' @param return_named_list A logical indicating whether to return a named list with display names
#'
#' @return A character vector of filter variable names or named list with display names
#'
#' @import dplyr stringr googlesheets4 googledrive
get_filter_variables <- function(course_directory,
                                filter_roster,
                                filter_sheet,
                                return_named_list = FALSE) {
  tryCatch({
    filter_data <- get_filter_sheet(course_directory, filter_roster, filter_sheet)
    
    if (is.null(filter_data) || ncol(filter_data) <= 3) {
      return(character(0))
    }
    
    # Get columns D onwards (everything after name_canvas, sis_canvas, email_canvas)
    filter_columns <- colnames(filter_data)[4:ncol(filter_data)]
    
    # Remove any columns that are completely NA
    filter_columns <- filter_columns[!is.na(filter_columns)]
    
    if (return_named_list && length(filter_columns) > 0) {
      # Create prettier display names
      display_names <- filter_columns
      # Clean up common patterns in column names
      display_names <- stringr::str_replace_all(display_names, "_", " ")
      display_names <- stringr::str_to_title(display_names)
      # Capitalize common abbreviations
      display_names <- stringr::str_replace_all(display_names, "\\bId\\b", "ID")
      display_names <- stringr::str_replace_all(display_names, "\\bEmail\\b", "Email")
      display_names <- stringr::str_replace_all(display_names, "\\bCanvas\\b", "Canvas")

      # Create named vector where names are display names and values are column names
      result <- filter_columns
      names(result) <- display_names
      return(result)
    }

    return(filter_columns)
  },
  error = function(e) {
    warning(paste("Error getting filter variables:", e$message))
    return(character(0))
  })
}
