land_long <- ce_buyouts |>
  select(tmk, starts_with("land_value_ce")) |>
  pivot_longer(-tmk,
               names_to = "level",
               names_prefix = "land_value_ce",
               values_to = "land_value_remaining")

results <- results |>
  mutate(level = level_key$level[match(year_t, level_key$year)]) |>
  left_join(land_long, by = c("tmk", "level"),
            relationship = "many-to-one")

land_long <- land_long |>
  mutate(tmk = as.character(tmk))

#Filter NA values
results <- results %>%
  filter(!(is.na(year_tce)))

buyouts_cost_filtered <- ce_buyouts %>%
  mutate(
    sa_used = case_when(
      year_tce <= 2030 ~ coalesce(na_if(sa_ce05, 0), na_if(sa_ce11, 0),
                                  na_if(sa_ce20, 0), na_if(sa_ce32, 0)),
      year_tce == 2050 ~ coalesce(na_if(sa_ce11, 0), na_if(sa_ce20, 0),
                                  na_if(sa_ce32, 0)),
      year_tce == 2075 ~ coalesce(na_if(sa_ce20, 0), na_if(sa_ce32, 0)),
      year_tce == 2100 ~ na_if(sa_ce32, 0)
    ) %>% replace_na(0),
    pct_lost       = pmin(sa_used / area, 1),
    landvalue_disc = land_value * (1 - pct_lost),
    public_costce  = building_value + landvalue_disc
  )