#Pull Tenure df 
hh_tenure_new <- get_acs(
  geography = "block group",
  variables = c(
    total_tenure = "B25003_001",
    owner_occ    = "B25003_002",
    renter_occ   = "B25003_003"
  ),
  state = "HI",
  county = "Honolulu",
  year = 2023,   # match whatever ACS vintage you've been using for the others
  survey = "acs5",
  output = "wide"
)

#Split NAME into block group and tract
hh_tenure_new <- hh_tenure_new %>%
  mutate(
    block_group = str_extract(NAME, "(?<=Block Group )\\d+"),
    tract       = str_extract(NAME, "(?<=Census Tract )[\\d.]+")
  )

#Keep only columns we need
tenure_clean <- hh_tenure_new %>%
  select(-c(NAME, total_tenureM, owner_occM, renter_occM))

#Rename columns
colnames(tenure_clean) <- c("GEOID", "total", "owner", "renter", "block_group", "tract") 
