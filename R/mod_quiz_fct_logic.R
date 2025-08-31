#' @title Get column name of ID field
#'
#' @description Detect what is the name of the column variables that
#' is used as the student identifier in the quiz
#'
#' @param quiz A data frame.
#'
#' @return Column name as string or `""` if not found.
#' @noRd
#' @import stringr dplyr
get_idcolname <- function(quiz) {
  if (is.null(quiz) || length(colnames(quiz)) < 2) {
    return(NULL)
  }

  cols <- colnames(quiz)

  test <-
    cols[stringr::str_detect(toupper(cols), paste0("\\b", toupper(NAME_FIELD_QUIZ), "\\b"))][1]
  if (!is.na(test)) {
    col_to_match <- test
  } else if (stringr::str_detect(toupper(cols[1]), "NAME")) {
    col_to_match <- cols[1]
  } else if (stringr::str_detect(toupper(cols[2]), "NAME")) {
    col_to_match <- cols[2]
  } else {
    col_to_match <- ""
  }
  col_to_match
}


#' @title Get column name of ID field alternative when name is not listed
#'
#' @description Detect what is the name of the column variable that
#' receives student name when not provided to the id_colname
#'
#' @param quiz A data frame.
#'
#' @return Column name as string or `""` if not found.
#' @noRd
#' @import stringr dplyr
get_idcolname_alternative <- function(quiz) {
  if (is.null(quiz) || length(colnames(quiz)) < 2) {
    return(NULL)
  }

  cols <- colnames(quiz)

  test <-
    cols[stringr::str_detect(toupper(cols), toupper(NAME_ALTERNATIVE_FIELD_QUIZ))][1]

  if (!is.na(test)) {
    col_to_match <- test
  } else {
    col_to_match <- ""
  }
  col_to_match
}


#' Helper function to attempt to convert to numeric
#'
#' @param x column
#'
#' @return A column with numeric values or character if conversion fails
#' @noRd
#'
try_convert_numeric <- function(x) {
  converted <- suppressWarnings(as.numeric(x))
  if (anyNA(converted)) {
    return(as.character(x)) # return as character if conversion fails
  } else {
    return(converted)
  }
}

#' Filter raw quiz
#'
#' @param quiz A data frame.
#' @param col_to_match A string of the column name to match.
#'
#' @return A dataframe with the quiz filtered
#' @noRd
#' @import stringr dplyr
#'
filter_quiz <- function(quiz, col_to_match, col_alternative, initial_date = INITIAL_DATE_DEFAULT) {
  quiz_temp <- quiz

  if ("Timestamp" %in% colnames(quiz_temp)) {
    tryCatch(
      {
        quiz_temp <- quiz_temp |>
          dplyr::filter(Timestamp >= as.Date(initial_date))
      },
      error = function(e) {
      }
    )
  }

  if (col_to_match == "" || is.na(col_to_match)) {
    return(quiz_temp)
  }

  # Fill "Your Name" with "If your name is not listed above" when "Your Name is empty"
  if (col_alternative != "") {
    tryCatch(
      {
        quiz_temp <- quiz_temp |>
          dplyr::mutate(!!col_to_match := case_when(
            is.na(.data[[col_to_match]]) ~ .data[[col_alternative]],
            T ~ .data[[col_to_match]]
          ))
      },
      error = function(e) {
      }
    )
  }

  # Filter NAMES_TO_REMOVE_FROM_QUIZ
  # (`Student, Test`, `Dan Levy`, among others (see R/globalvars.R))
  tryCatch(
    {
      quiz_temp <- quiz_temp |>
        dplyr::filter(!(toupper(.data[[col_to_match]]) %in%
          toupper(NAMES_TO_REMOVE_FROM_QUIZ)))
    },
    error = function(e) {
    }
  )


  # Filter NAs
  tryCatch(
    {
      quiz_temp <- quiz_temp |>
        dplyr::filter(!is.na(.data[[col_to_match]]))
    },
    error = function(e) {
    }
  )


  # Keep one obseervation for student. Keep latest submission
  tryCatch(
    {
      quiz_temp <- quiz_temp |>
        dplyr::mutate(order = seq.int(nrow(quiz_temp))) |>
        dplyr::group_by(.data[[col_to_match]]) |>
        dplyr::filter(order == max(order)) |>
        dplyr::ungroup()
    },
    error = function(e) {
    }
  )

  # Use lapply to convert to numeric or to string to each column of your data frame
  quiz_temp[] <- lapply(quiz_temp, try_convert_numeric)

  quiz_temp
}


#' Join quiz responses with Roster
#'
#' @param quiz A data frame.
#' @param roster A data frame.
#' @param col_to_match A string of the column name to match.
#'
#' @return A dataframe with the quiz + the roster's teachly index.
#' If roster is empty, teachly index will be empty.
#' @noRd
#' @import stringr dplyr
join_quiz_roster <- function(quiz, roster, col_to_match) {
  if (is.null(quiz) || nrow(quiz) == 0) {
    return(NULL)
  }

  if (col_to_match == "" || is.null(col_to_match)) {
    return(quiz)
  }

  # Re format
  if (typeof(quiz %>% select(c(col_to_match)) %>% pull()) == "logical") {
    quiz <- quiz %>%
      dplyr::mutate(dplyr::across(c(col_to_match), as.character))
  }
  if (is.null(roster) || nrow(roster) == 0 || !all(c("standardized_name", "teachly") %in% colnames(roster))) {
    quiz %>%
      dplyr::left_join(
        dplyr::tibble("standardized_name" = "XXX", "teachly" = 0) %>%
          dplyr::rename(!!col_to_match := "standardized_name") %>%
          dplyr::select(c(col_to_match, "teachly")),
        by = col_to_match
      )
  } else {
    quiz %>%
      dplyr::left_join(
        roster %>%
          dplyr::rename(!!col_to_match := "standardized_name") %>%
          dplyr::select(c(col_to_match, "teachly")),
        by = col_to_match
      )
  }
}

#' Get questions from quiz
#'
#' @description Get questions from quiz by removing questions of `QUESTIONS_TO_TAKE_OUT`.
#'
#' @param quiz A data frame.
#'
#' @return A list of questions.
#' @noRd
#' @import stringr dplyr
get_questions_from_quiz <- function(quiz) {
  questions_temp <- colnames(quiz)
  questions_temp <-
    questions_temp[!str_detect(
      toupper(questions_temp),
      paste0("\\b", toupper(QUESTIONS_TO_TAKE_OUT), "\\b", collapse = "|")
    )]
  questions_temp
}

#' Modal of list of students
#'
#' @description Get list of students of `dataframe`
#'
#' @param dataframe A tibble with one column
#' @param title The title for the modal. Default is "Students"
#'
#' @return A `shiny::Modal`
#' @noRd

students_list_modal <- function(dataframe, title = "Students") {
  # Change colnames
  colnames_to_show <- c("Name", "Teachly score")

  # Teachly colors
  brks <- seq(0, 1, 0.1)
  clrs <-
    colorRampPalette(c("#dc3545", "#ffc107", "#28a745"))(length(brks) + 1)
  shiny::modalDialog(
    title = title,
    DT::datatable(
      dataframe,
      colnames = colnames_to_show[seq_len(ncol(dataframe))],
      rownames = FALSE,
      style = "bootstrap4",
      filter = "top",
      extensions = "Scroller",
      selection = "none",
      options = list(
        pageLength = 200,
        dom = "t",
        # ordering = FALSE,
        scrollY = 450
      )
    ) |>
      # If there is teachly in colnames
      (\(.) {
        if ("teachly" %in% colnames(dataframe)) {
          . |>
            DT::formatStyle(c("teachly"),
              backgroundColor = DT::styleInterval(brks, clrs)
            )
        } else {
          .
        }
      })(),
    easyClose = TRUE,
    fade = FALSE,
    footer = NULL
  )
}

#' Join Quiz with Filter Sheet Data
#'
#' @description Join quiz responses with filter sheet data based on name_canvas or id_canvas matching.
#'
#' @param quiz A data frame with quiz responses.
#' @param roster A data frame with basic roster information (for teachly data).
#' @param filter_sheet A data frame with filter sheet data.
#' @param col_to_match A string of the column name to match from quiz to roster.
#' @param filter_vars A character vector of filter variables to include from filter sheet.
#'
#' @return A dataframe with the quiz + roster's teachly index + filter variables.
#' @noRd
#' @import stringr dplyr
join_quiz_roster_with_filters <- function(quiz, roster, filter_sheet, col_to_match, filter_vars = character(0)) {
  if (is.null(quiz) || nrow(quiz) == 0) {
    return(NULL)
  }

  if (col_to_match == "" || is.null(col_to_match)) {
    return(quiz)
  }

  # Re format quiz column if needed
  if (typeof(quiz %>% select(c(col_to_match)) %>% pull()) == "logical") {
    quiz <- quiz %>%
      dplyr::mutate(dplyr::across(c(col_to_match), as.character))
  }

  # Step 1: Join quiz with basic roster for teachly data
  quiz_with_roster <- join_quiz_roster(quiz, roster, col_to_match)
  
  if (is.null(quiz_with_roster)) {
    return(NULL)
  }

  # Step 2: Join with filter sheet data if available
  if (is.null(filter_sheet) || nrow(filter_sheet) == 0 || length(filter_vars) == 0) {
    # Return quiz with roster only if no filter sheet or variables
    return(quiz_with_roster)
  }

  # The roster is the source of truth. Use it to bridge quiz data to filter sheet data
  # We need to match roster records to filter sheet records using Canvas information
  
  # Prepare filter data with selected variables
  available_filter_vars <- filter_vars[filter_vars %in% colnames(filter_sheet)]
  
  if (length(available_filter_vars) == 0) {
    return(quiz_with_roster)
  }
  
  # Create a mapping from roster to filter sheet using Canvas columns
  # This will map standardized_name to the filter variables
  
  # Start with empty filter data for each student
  filter_mapping <- data.frame(
    standardized_name = unique(quiz_with_roster[[col_to_match]]),
    stringsAsFactors = FALSE
  )
  
  # Add filter variables as empty columns
  for (var in available_filter_vars) {
    filter_mapping[[var]] <- NA
  }
  
  # For each student in the quiz, try to find their filter data
  for (i in seq_len(nrow(filter_mapping))) {
    student_name <- filter_mapping$standardized_name[i]
    
    # Get the roster record for this student
    roster_record <- roster %>%
      filter(standardized_name == student_name) %>%
      slice(1)  # Take first if duplicates
    
    if (nrow(roster_record) == 0) next
    
    matched_filter_row <- NULL
    
    # Priority 1: Match by sis_canvas (Harvard ID) if available
    if ("sis_canvas" %in% colnames(roster_record) && 
        !is.na(roster_record$sis_canvas) && 
        roster_record$sis_canvas != "") {
      matched_filter_row <- filter_sheet %>%
        filter(!is.na(sis_canvas) & sis_canvas == roster_record$sis_canvas) %>%
        slice(1)
    }
    
    # Priority 2: Match by email_canvas if no sis_canvas match
    if ((is.null(matched_filter_row) || nrow(matched_filter_row) == 0) &&
        "email_canvas" %in% colnames(roster_record) && 
        !is.na(roster_record$email_canvas) && 
        roster_record$email_canvas != "") {
      matched_filter_row <- filter_sheet %>%
        filter(!is.na(email_canvas) & email_canvas == roster_record$email_canvas) %>%
        slice(1)
    }
    
    # Priority 3: Match by name_canvas with string similarity if no email match
    if ((is.null(matched_filter_row) || nrow(matched_filter_row) == 0) &&
        "name_canvas" %in% colnames(roster_record) && 
        !is.na(roster_record$name_canvas) && 
        roster_record$name_canvas != "") {
      
      # First try exact match
      matched_filter_row <- filter_sheet %>%
        filter(!is.na(name_canvas) & name_canvas == roster_record$name_canvas) %>%
        slice(1)
      
      # If no exact match, try string similarity
      if (nrow(matched_filter_row) == 0) {
        filter_names <- filter_sheet %>%
          filter(!is.na(name_canvas) & name_canvas != "") %>%
          pull(name_canvas)
        
        if (length(filter_names) > 0) {
          # Calculate string distances
          distances <- stringr::str_distance(roster_record$name_canvas, filter_names, method = "jw")
          min_distance <- min(distances, na.rm = TRUE)
          
          # Use a threshold for similarity (0.2 means 80% similarity)
          if (min_distance <= 0.2) {
            best_match_name <- filter_names[which.min(distances)]
            matched_filter_row <- filter_sheet %>%
              filter(name_canvas == best_match_name) %>%
              slice(1)
          }
        }
      }
    }
    
    # If we found a match, copy the filter values
    if (!is.null(matched_filter_row) && nrow(matched_filter_row) > 0) {
      for (var in available_filter_vars) {
        if (var %in% colnames(matched_filter_row)) {
          filter_mapping[i, var] <- matched_filter_row[[var]]
        }
      }
    }
  }
  
  # Join the quiz_with_roster data with the filter mapping
  result <- quiz_with_roster %>%
    left_join(filter_mapping, by = setNames("standardized_name", col_to_match))
  
  return(result)
}
