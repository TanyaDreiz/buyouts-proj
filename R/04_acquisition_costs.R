#Calculate land value - land lost to hazard
level_key <- tibble::tribble(
  ~year, ~level,
  2024,  "05",
  2030,  "05",
  2050,  "11",
  2075,  "20",
  2100,  "32"
)
hazard <- "ce"
levels <- unique(level_key$level)  # "05" "11" "20" "32"

#Loop costs 
for (level in levels) {
  for (hazard in hazard_types) {
    
    #Create column names 
    percent_hazard <- paste0("percenthazard_", hazard, level) 
    shape_column_title <- paste0("sa_", hazard, level) 
    land_applied_col <- paste0("land_value_", hazard, level) 
    
    # Create costs calculations
    ce_buyouts[[percent_hazard]] <- ce_buyouts[[shape_column_title]] / ce_buyouts$area
    ce_buyouts[[land_applied_col]] <- (1 - round(ce_buyouts[[percent_hazard]], digits = 6)) * ce_buyouts$land_value
  }
}

#Move year columns closer to front
buyouts_cost_filtered <- buyouts_cost_filtered %>%
  select(1, year_tce, everything())

#Rename landvalue cols 
buyouts_cost_filtered <- buyouts_cost_filtered %>%
  rename_with(~ gsub("land_value_", "landvalue_", .x), starts_with("land_value_"))

#Calculate public costs based on time and hazard
buyouts_cost_filtered <- buyouts_cost_filtered %>%
  mutate(
    public_costce = case_when(
      year_tce == 2024 ~ building_value + landvalue_ce05,
      year_tce == 2030 ~ building_value + landvalue_ce05,
      year_tce == 2050 ~ building_value + landvalue_ce11,
      year_tce == 2075 ~ building_value + landvalue_ce20,
      year_tce == 2100 ~ building_value + landvalue_ce32,
      TRUE ~ NA_real_
    ))

#add discount rate
buyouts_cost_filtered <- buyouts_cost_filtered %>%
  mutate(public_costce_discounted = public_costce / (1.03 ^ (year_tce - 2024)))

#clean dataframe
county <- buyouts_cost_filtered[, c("tmk", "year_tce", "total_value", "public_costce_discounted")]

#Apply cap and discount rate
discount_rate <- 0.03
base_year <- 2024
cap_2024 <- 779700

county_capped_alldata <- county_capped_alldata %>%
  mutate(
    years_out = year_tce - 2024,
    public_costce_disc = public_costce / (1.03 ^ years_out),
    cap_disc = 779700 / (1.03 ^ years_out),
    capped_costce = pmin(public_costce_disc, cap_disc)
  )


#Download dataframes
write_csv(buyouts_cost_filtered, here("data", "processed", "county_alldata.csv"))
write_csv(county, here("data", "processed", "county_clean.csv"))
write_csv(buyouts_cost_filtered, here("data", "processed", "county__capped_alldata.csv"))
