census_api_key("add your census API", install = TRUE)  

#Get census datasets imported directly
year_acs <- 2023
geo_level <- "block group"      
state_fp <- "HI"
county_fp <- "Honolulu"   

tables_needed <- c(
  hh_type   = "B11001",
  hh_size   = "B25010",
  hh_income = "B19001"
)
# Pull variable metadata once so labels are available for cleanup later
vars_2023 <- load_variables(year_acs, "acs5", cache = TRUE)

# Fetch all three tables in one pass, tagging each result by table name
acs_raw <- map(tables_needed, function(tbl) {
  get_acs(
    geography = geo_level,
    table     = tbl,
    state     = state_fp,
    county    = county_fp,
    year      = year_acs,
    survey    = "acs5",
    cache_table = TRUE
  )
}) %>%
  bind_rows(.id = "source_table")

#Attach human-readable variable labels to the raw pull
acs_labeled <- acs_raw %>%
  left_join(
    vars_2023 %>% select(variable = name, label, concept),
    by = "variable"
  ) %>%
  select(source_table, GEOID, NAME, variable, label, concept, estimate, moe)

#Split NAME into block group and tract
acs_labeled <- acs_labeled %>%
  mutate(
    block_group = str_extract(NAME, "(?<=Block Group )\\d+"),
    tract       = str_extract(NAME, "(?<=Census Tract )[\\d.]+")
  )

#Filter out unnecessary rows 
acs_labeled <- acs_labeled %>%
  filter(label != "Estimate!!Total:")

acs_labeled <- acs_labeled %>%
  filter(label != "Estimate!!Average household size --!!Total:")

# Keep only Family/Nonfamily subtotal rows for hh_type
acs_labeled <- acs_labeled %>%
  filter(
    source_table != "hh_type" |
      label %in% c("Estimate!!Total:!!Family households:",
                   "Estimate!!Total:!!Nonfamily households:")
  )

#Clean up text
acs_labeled <- acs_labeled %>%
  mutate(
    label = str_remove(label, "^Estimate!!Total:!!"),
    label = str_remove(label, "^Estimate!!Average household size --!!Total:!!"),
    label = str_remove(label, "^Estimate!!Total:")
  )

#Split back into three clean, table-specific data frames 
hh_tenure_df   <- acs_labeled %>% filter(source_table == "hh_size")
hh_income_df <- acs_labeled %>% filter(source_table == "hh_income")

#Pivot wide
hh_income_wide <- hh_income_df %>%
  select(GEOID, label, estimate, block_group, tract) %>%
  pivot_wider(names_from = label, values_from = estimate)

hh_tenure_wide <- hh_tenure_df %>%
  select(GEOID, label, estimate, block_group, tract) %>%
  pivot_wider(names_from = label, values_from = estimate)

#Save census data
write_csv(hh_composition, here("data", "processed", "census_data", "hh_type.csv"))
write_csv(hh_income_wide, here("data", "processed", "census_data", "hh_income.csv"))
write_csv(hh_tenure_wide, here("data", "processed", "census_data", "hh_tenure.csv"))


