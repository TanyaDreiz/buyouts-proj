
results <- expand.grid(trigger = triggers, threshold = thresholds,
                       stringsAsFactors = FALSE) |>
  purrr::pmap_dfr(function(trigger, threshold) {
    dedup_buyouts |>
      mutate(
        trigger   = trigger,
        threshold = threshold,
        year_t = case_when(
          near_line <= threshold                                 ~ 2024,
          .data[[paste0("near_", trigger, slrs[[1]])]] <= threshold ~ 2030,
          .data[[paste0("near_", trigger, slrs[[2]])]] <= threshold ~ 2050,
          .data[[paste0("near_", trigger, slrs[[3]])]] <= threshold ~ 2075,
          .data[[paste0("near_", trigger, slrs[[4]])]] <= threshold ~ 2100,
          .default = NA_real_
        )
      )
  })

results |>
  group_by(trigger, threshold) |>
  summarise(n_triggered = sum(!is.na(year_t)),
            share = mean(!is.na(year_t)),
            .groups = "drop")
