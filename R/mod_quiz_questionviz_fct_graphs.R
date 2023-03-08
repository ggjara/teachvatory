#' Barplot for multiple choice question - single answer
chart_multiplechoise_single <- function(quiz, question,
                                        correct_answer = "",
                                        arrange_by_frequency = TRUE) {
  chart <- quiz %>%
    mutate(!!question := as.character(.data[[question]])) %>%
    mutate(!!question := case_when(
      is.na(.data[[question]]) ~ "No answer",
      TRUE ~ .data[[question]],
    ))  %>%
    group_by(.data[[question]]) %>%
    summarize(n = n()) %>%
    ungroup() |>
    mutate(correct = .data[[question]] == correct_answer) |>
    mutate(n_correct = case_when(
      correct ~ n,
      TRUE ~ NA_integer_)) |>
    mutate(n_incorrect = case_when(
      !correct ~ n,
      TRUE ~ NA_integer_
    ))

  if(arrange_by_frequency){
    chart <- chart %>%
      arrange(desc(n))
  }


  chart <- chart %>%
    mutate(!!question := factor(.data[[question]], levels = unique(chart[[question]])))

  question_title <- ifelse(nchar(question)>200,
                           paste0(substr(question, 1, 200), "..."),
                           question)
  highchart(type = "chart") |>
    hc_xAxis(categories = chart[[question]]) |>
    (\(.) {
      if (correct_answer=="No correct answer")
      {
        . |>
          hc_add_series(type = 'bar', chart[['n']], color = COLOR_DEFAULT, name ="Responses")
      } else{
        . |>
          hc_add_series(type = 'bar', chart[['n_correct']], color = COLOR_GREEN, name ="Correct") |>
          hc_add_series(type = 'bar', chart[['n_incorrect']], color = COLOR_DEFAULT, name ="Incorrect")
      }
    })() |>
    hc_plotOptions(series = list(stacking="normal", dataLabels=list(enabled=T))) |>
    hc_legend(enabled = T) |>
    hc_title(text=question_title) |>
    hc_exporting(
      enabled = TRUE, # always enabled
      filename = paste0("viz_", substr(question, 1, 20))
    )
}

#' Barplot for multiple choice question
chart_multiplechoise_multiple <- function(quiz, question) {
  chart <- quiz %>%
    group_by(Date = lubridate::floor_date(Timestamp, "12 hour")) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    arrange(Date) %>%
    mutate(Date = substr(Date, 1, 16))

  chart <- chart %>%
    mutate(
      Date = factor(Date, labels = unique(chart$Date)),
      serie = "Responses"
    )

  highchart() %>%
    hc_xAxis(categories = chart$Date) %>%
    hc_add_series(
      name = "Responses", data = chart$n,
      type = "column"
    ) %>%
    hc_yAxis(title = list(text = "Responses"))  %>%
    hc_title(text = "Responses time") %>%
    hc_subtitle(text = "Grouped by 12 hours. The horizontal labels show the floor hour.") %>%
    hc_add_theme(hc_theme_smpl())
}

#' Histogram for prediction questions
chart_prediction <- function(quiz, question,
                                        correct_answer = NA,
                                        arrange_by_frequency = TRUE) {

  if (!is.na(correct_answer)) {
    quiz %>%
      pull(question) %>%
      unlist() %>%
      as.numeric() %>%
      hchart() %>%
      hc_xAxis(plotLines = list(
                 list(color = "#FF5733",
                      dashStyle = "Solid",
                      width = 3,
                      value = correct_answer, zIndex = 10)))
  } else {
    quiz %>%
      pull(question) %>%
      unlist() %>%
      as.numeric() %>%
      hchart()
  }

}
