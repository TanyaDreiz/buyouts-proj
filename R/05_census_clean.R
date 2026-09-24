census_api_key("add your census API", install = TRUE)  

#Get census datasets imported directly
year_acs <- 2023
geo_level <- "block group"      
state_fp <- "HI"
county_fp <- "Honolulu"   

tables_needed <- c(
  hh_type   = "B11001",
  hh_size   = "B25010",
  hh_income = "B19001",
)
# Pull variable metadata once so labels are available for cleanup later
vars_2023 <- load_variables(year_acs, "acs5", cache = TRUE)

#Fetch all three tables in one pass, tagging each result by table name
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

#Clean up labels hh_size
acs_labeled <- acs_labeled %>%
  mutate(label = str_replace(label, "^Estimate!!Average household size --!!Total:$", "Average household size"))

acs_labeled <- acs_labeled %>%
  mutate(label = str_replace(label, "Estimate!!Average household size --!!Total:!!Owner occupied", "Owner occupied"))

acs_labeled <- acs_labeled %>%
  mutate(label = str_replace(label, "Estimate!!Average household size --!!Total:!!Renter occupied", "Renter occupied"))

#Clean up labels all
acs_labeled <- acs_labeled %>%
  mutate(
    label = str_remove(label, "^Estimate!!Total:!!"),
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

#Clean hh_type df
hh_type <- hh_type %>%
  select(-c(total_hh_moe, multi_person_moe, multi_person_share, living_alone_moe, living_alone_share, NAME))

#Clean hh_size df
hh_size <- hh_size %>%
  select(-c(`Owner occupied`,`Renter occupied`))

colnames(hh_size) <- c("GEOID", "block_group", "tract", "avg_hh")

#Clean hh_income
colnames(hh_income) <- c("GEOID", "block_group", "tract","less_10000",
                         "10000_14999", "15000_19999", "20000_24999", "25000_29999",
                         "30000_34999", "35000_39999", "40000_44999", "45000_49999", "50000_59999", "60000_74999", "75000_99999",
                         "100000_124999", "125000_149999", "150000_199999", "200000_inf")


#Merge by GEOID
acs_merged <- list(hh_type, hh_income, hh_size, hh_tenure) %>%
  reduce(left_join, by = "GEOID")

#Clean hh_type df
acs_merged <- acs_merged %>%
  select(-c(total))

#Round renter, avg hh and owner occupied to whole number
acs_clean <- acs_merged %>%
  mutate(
    avg_hh = round(avg_hh),
  )

#Clean up columns
acs_clean <- acs_clean %>%
  rename(
    block = block_group.x,
    tract = tract.x
  )

acs_clean <- acs_clean %>%
  rename(
    multi = multi_person_est,
    single = living_alone_est
  )

acs_clean <- acs_clean %>%
  select(-c(total))                         

#Reorganize columns 
acs_clean <- acs_clean %>%
  relocate(tract, .after = geoid)

acs_clean <- acs_clean %>%
  relocate(block, .after = tract)

acs_clean <- acs_clean %>%
  relocate(owner, .after = single)

acs_clean <- acs_clean %>%
  relocate(renter, .after = owner)

acs_clean <- acs_clean %>%
  relocate(avg_hh, .after = renter)

#Save census data
write_csv(hh_composition, here("data", "processed", "census_data", "hh_type.csv"))
write_csv(hh_income_wide, here("data", "processed", "census_data", "hh_income.csv"))
write_csv(hh_tenure_wide, here("data", "processed", "census_data", "hh_tenure.csv"))
write.csv(hh_tenure_new, here ("data", "raw", "hh_tenure_raw.csv"))
write.csv(acs_raw, here ("data", "raw", "acs_rawdata.csv"))
write.csv(hh_type, here ("data", "raw", "hh_type_raw.csv"))
write.csv(tenure_clean, here ("data", "processed", "census_data", "hh_tenure.csv"))
write.csv(acs_clean, here ("data", "processed", "census_data", "acs_clean_all.csv"))
