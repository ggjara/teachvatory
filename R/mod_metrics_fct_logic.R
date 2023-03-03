create_metrics_dataframe <-
  function(roster, masterquiz_md, quizzes, initial_date = INITIAL_DATE_DEFAULT) {
    print(quizzes)
    final_quizzes <- c()
    for (quiz in quizzes) {
      temp <- googlesheets4::read_sheet(masterquiz_md, sheet = quiz)
      print(initial_date)
      # Filter Quiz - Function at mod_quiz_fct_logic.R
      temp <- filter_quiz(quiz = temp,
                          col_to_match = get_idcolname(temp),
                          col_alternative = get_idcolname_alternative(temp),
                          initial_date = initial_date)

      if (nrow(temp) > 0) {
        name_col <- get_idcolname(temp)
        if (name_col != "" & !is.na(name_col)) {
          class(name_col) <- "character"
          temp <- temp |>
            dplyr::group_by(.data[[name_col]]) |>
            dplyr::summarise(n = TRUE) |>
            dplyr::ungroup() |>
            dplyr::rename(!!quiz := n)

          roster <- roster |>
            dplyr::left_join(temp, by = c("standardized_name" = name_col))

          roster[is.na(roster[[quiz]]), quiz] <- FALSE
          final_quizzes <- append(final_quizzes, quiz)
        }
      }
    }
    return(list("dataframe" = roster,
                "quizzes" = final_quizzes))
  }
