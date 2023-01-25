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
  if (is.null(quiz) || length(colnames(quiz)) < 2)
    return(NULL)

  cols <- colnames(quiz)

  test <-
    cols[stringr::str_detect(toupper(cols), paste0("\\b", toupper(NAME_FIELD_QUIZ), "\\b"))][1]
  if (!is.na(test)) {
    col_to_match <- test
  } else if (stringr::str_detect(toupper(cols[1]), "NAME")) {
    col_to_match <- cols[1]
  } else if (stringr::str_detect(toupper(cols[2]), "NAME")) {
    col_to_match <- cols[2]
  } else if (length(cols) >= 3) {
    col_to_match <- cols[3]
  } else {
    col_to_match <- ""
  }
  col_to_match
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
  if (is.null(quiz)) {
    return(NULL)
  }

  #Re format
  if (typeof(quiz %>% select(c(col_to_match)) %>% pull()) == "logical") {
    quiz <- quiz %>%
      dplyr::mutate(dplyr::across(c(col_to_match), as.character))
  }
  if (is.null(roster) || nrow(roster) == 0 ||
        !all(c("standardized_name", "teachly") %in% colnames(roster))) {
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
    questions_temp[!str_detect(toupper(questions_temp),
                               paste0("\\b", toupper(QUESTIONS_TO_TAKE_OUT), "\\b", collapse = "|"))]
  return(questions_temp)
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

students_list_modal <- function(dataframe, title = "Students"){
  shiny::modalDialog(
    title = title,
    DT::datatable(
      dataframe,
      colnames = rep("", ncol(dataframe)),
      rownames = FALSE,
      style = "bootstrap4",
      filter = "top",
      extensions = "Scroller",
      selection = "none",
      options = list(
        pageLength = 200,
        dom = "t",
        ordering = FALSE,
        scrollY = 450
      )
    ),
    easyClose = TRUE,
    fade = FALSE,
    footer = NULL
  )
}
