#Upload buildings count dataset
buildings_count <- buildings <- read_csv(here("data", "raw", "buildings_count.csv"))

#Clean up dataset
clean_buildings <- buildings_count[, c("tmk", "Point_Count")]
colnames(clean_buildings) <- c("tmk", "point_count")

#Merge by TMK 
buyouts_cost <- merge(dedup_buyouts, clean_buildings, by="tmk")

#Calculate land value - land lost to hazard
years <- c("2024", "2030","2050","2075","2100")  
hazard_types <- c("ce")
levels <- c("05", "11", "20", "32")

#Loop costs 
for (level in levels) {
  for (hazard in hazard_types) {
    
    # Create column names 
    percent_hazard <- paste0("percenthazard_", hazard, level) 
    shape_column_title <- paste0("sa_", hazard, level) 
    land_applied_col <- paste0("land_value_", hazard, level) 
    
    # Create costs calculations
    buyouts_cost[[percent_hazard]] <- buyouts_cost[[shape_column_title]] / buyouts_cost$area
    buyouts_cost[[land_applied_col]] <- (1 - round(buyouts_cost[[percent_hazard]], digits = 6)) * buyouts_cost$land_value
  }
}

#Filter those with more than 4 buildings per parcel 
buyouts_cost <- buyouts_cost %>%
  filter(point_count <= 4)

#Filter NA values
buyouts_cost_filtered <- buyouts_cost %>%
  filter(!(is.na(year_tce)))

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

#Apply the capped amount 
county_capped <- county %>%
  mutate(public_costce_capped = pmin(public_costce_discounted, 779700))

buyouts_cost_filtered <- buyouts_cost_filtered %>%
  mutate(discount_countywide = public_costce / (1.03 ^ (year_tce - 2024)),
         capped_discounted = pmin(discount_countywide, 779700))

#Download dataframes
write_csv(buyouts_cost_filtered, here("data", "processed", "county_alldata.csv"))
write_csv(county, here("data", "processed", "county_clean.csv"))
write_csv(buyouts_cost_filtered, here("data", "processed", "county__capped_alldata.csv"))
