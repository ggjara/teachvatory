#' Barplot for multiple choice question - single answer
chart_multiplechoise_single <- function(quiz, question, correct_answer = "") {
  chart <- quiz %>%
    mutate(!!question := as.character(.data[[question]])) %>%
    mutate(!!question := case_when(
      is.na(.data[[question]]) ~ "No answer",
      TRUE ~ .data[[question]],
    ))  %>%
    group_by(.data[[question]]) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(correct = case_when(
      .data[[question]] == correct_answer ~ "Correct",
      TRUE ~ "Incorrect"))  %>%
    arrange(desc(n))

  chart <- chart %>%
    mutate(!!question := factor(.data[[question]], levels = unique(chart[[question]]))) %>%
    mutate(correct = factor(correct, levels = c("Incorrect", "Correct")))

  highchart() %>%
    hc_xAxis(categories = chart[[question]]) %>%
    hc_add_series(chart, "bar", name = "Responses", hcaes(
      x = .data[[question]], y = n,
      color = correct,
    )) %>%
    hc_legend(enabled = FALSE)
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