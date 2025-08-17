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
#' @param include_all_columns A logical indicating whether to include all columns or just the basic ones
#'
#' @return A tibble of the Roster processed or `NULL` if fail.
#'
#' @import dplyr stringr googlesheets4 googledrive
get_roster <- function(course_directory,
                       filter_roster,
                       filter_roster_sheet,
                       include_all_columns = FALSE) {
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

    # Select columns based on include_all_columns parameter
    if (include_all_columns) {
      # Keep all columns except specifically sensitive ones
      cols_to_exclude <- c("Invalid", "FormRanger Column")
      roster_temp <- roster_temp %>%
        select(-any_of(cols_to_exclude))
    } else {
      # Keep only basic columns as before
      roster_temp <- roster_temp %>%
        select(
          standardized_name,
          email_teachly,
          teachly,
          teachly_comments,
          teachly_absences
        )
    }

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

#' Get Available Filter Columns from Roster
#'
#' @description Get list of additional columns available for filtering from the roster
#' beyond the basic columns (standardized_name, email_teachly, teachly, teachly_comments, teachly_absences)
#'
#' @param course_directory A dribble of files of the selected course's directory
#' @param filter_roster A string of the Roster's filename
#' @param filter_roster_sheet A string of the sheetname of the Roster spreadsheet
#' @param return_named_list A logical indicating whether to return a named list with display names
#'
#' @return A character vector of additional column names or named list with display names, or empty vector if none available
#'
#' @import dplyr stringr googlesheets4 googledrive
get_roster_filter_columns <- function(
  course_directory,
  filter_roster,
  filter_roster_sheet,
  return_named_list = FALSE
) {
  tryCatch({
    roster_path <- course_directory %>%
      filter(name == filter_roster)

    # Get the raw roster data and process it the same way as get_roster
    # but we need the full column set to determine positions

    roster_path <- course_directory %>%
      filter(name == filter_roster)

    # Get Roster Sheet metadata
    metadata <- googlesheets4::gs4_get(roster_path %>% head(1))
    # Read the raw data
    roster_temp <- googlesheets4::read_sheet(metadata, sheet = filter_roster_sheet)

    # Apply the same filtering as get_roster to get the actual column structure
    roster_temp <- roster_temp %>%
      filter(!is.na(name_canvas) | !is.na(email_teachly)) %>%
      filter(tolower(standardized_name) != "student, test")

    # Filter invalids if Invalid column exists
    if ("Invalid" %in% colnames(roster_temp)) {
      roster_temp <- roster_temp %>%
        filter(!Invalid)
    }

    # Apply column exclusions (same as get_roster with include_all_columns = TRUE)
    cols_to_exclude <- c("Invalid", "FormRanger Column", "order")
    roster_temp <- roster_temp %>%
      select(-any_of(cols_to_exclude))

    # Add order column (this happens in get_roster)
    roster_temp <- roster_temp %>%
      dplyr::mutate(order = seq.int(nrow(roster_temp))) %>%
      dplyr::group_by(standardized_name) %>%
      dplyr::filter(order == min(order)) %>%
      dplyr::ungroup()

    all_columns <- colnames(roster_temp)
    num_columns <- length(all_columns)

    if (num_columns >= 25) {
      # Get columns from position 25 onwards
      potential_additional_columns <- all_columns[25:num_columns]

      # Filter out auto-named columns (like ...14, ...15, etc.)
      # Keep only columns that have actual meaningful names
      additional_columns <- potential_additional_columns[!grepl("\\.\\.\\.[0-9]+$", potential_additional_columns)]

      # Also filter out completely empty or NA column names
      additional_columns <- additional_columns[!is.na(additional_columns) & additional_columns != ""]

      # Exclude any remaining system columns
      system_columns <- c("Invalid", "FormRanger Column", "name_canvas", "order")
      additional_columns <- additional_columns[!additional_columns %in% system_columns]
    } else {
      # If there are fewer than 25 columns, no additional filter columns exist
      additional_columns <- character(0)
    }

    if (return_named_list && length(additional_columns) > 0) {
      # Create prettier display names
      display_names <- additional_columns
      # Clean up common patterns in column names
      display_names <- stringr::str_replace_all(display_names, "_", " ")
      display_names <- stringr::str_to_title(display_names)
      # Capitalize common abbreviations
      display_names <- stringr::str_replace_all(display_names, "\\bId\\b", "ID")
      display_names <- stringr::str_replace_all(display_names, "\\bEmail\\b", "Email")
      display_names <- stringr::str_replace_all(display_names, "\\bCanvas\\b", "Canvas")

      # Create named vector where names are display names and values are column names
      result <- additional_columns
      names(result) <- display_names
      result
    }

    return(additional_columns)
  },
  error = function(e) {
    if (return_named_list) {
      return(character(0))
    }
    return(character(0))
  })
}
