#Clean up census work 
clean_hhtype <- hh_type_raw[, c("GEOID", "total_hh", "multi_person_share",
                                "living_alone_share", "block_group", "tract")]
clean_hhsize <- hh_size[, c("GEOID", "block_group", "tract", "Average household size")]

#Rename columns 
colnames(clean_hhsize) <- c("GEOID", "block_group", "tract", "avg_hh_size")

#Round average household size column using standard rounding convention
round_half_up <- function(x, digits = 0) {
  scale <- 10^digits
  sign(x) * floor(abs(x) * scale + 0.5) / scale
}

clean_hhsize$avg_hh_size <- round_half_up(clean_hhsize$avg_hh_size, digits = 0)

#Merge hhsize and hhtype together
census_size_type <- clean_hhtype %>%
  left_join(clean_hhsize %>% select(GEOID, avg_hh_size), by = "GEOID")

#Reorganize columns 
census_size_type <- census_size_type %>%
  relocate(tract, .after = GEOID)

census_size_type <- census_size_type %>%
  relocate(block_group, .after = tract)

#Add income dataset
colnames(hh_income) <- c("GEOID", "block_group", "tract","less_10000",
                         "10000_14999", "15000_19999", "20000_24999", "25000_29999",
                         "30000_34999", "35000_39999", "40000_44999", "45000_49999", "50000_59999", "60000_74999", "75000_99999",
                         "100000_124999", "125000_149999", "150000_199999", "200000_inf")
#Organize columns 
hh_income <- hh_income %>%
  relocate(tract, .after = GEOID)

hh_income <- hh_income %>%
  relocate(block_group, .after = tract)

#Merge to census type 
census_size_type <- census_size_type %>%
  left_join(hh_income, by = "GEOID")

#Clean extra block group cols
census_size_type <- census_size_type %>%
  select(-c(block_group.y, tract.y))
census_size_type <- census_size_type %>%
  rename(block_group = block_group.x, tract = tract.x)

#Rename share to pct
income_census <- income_census %>%
  rename(multi_pct = multi_person_share, single_pct = living_alone_share)

#Calculate percent of households in each income bracket
income_brackets <- c(
  "less_10000", "10000_14999", "15000_19999", "20000_24999", "25000_29999",
  "30000_34999", "35000_39999", "40000_44999", "45000_49999", "50000_59999",
  "60000_74999", "75000_99999", "100000_124999", "125000_149999",
  "150000_199999", "200000_inf"
)

income_census <- census_size_type %>%
  mutate(across(
    all_of(income_brackets),
    ~ round((. / total_hh) * 100, 1),
    .names = "{.col}_pct"
  ))

#Calculate percentage of owners per block group
hh_tenure$pct_owner <- hh_tenure$owner / hh_tenure$total

#Merge to income dataframe
income_census <- income_census %>%
  left_join(hh_tenure %>% select(GEOID, pct_owner), by = "GEOID")
income_census <- income_census %>%
  relocate(pct_owner, .after = single_pct)

#Add total living alone and total multi family households
income_census <- income_census %>%
  left_join(hh_type %>% select(GEOID, multi_person_est, living_alone_est), by = "GEOID")
income_census <- income_census %>%
  relocate(multi_person_est, .before = multi_pct)
income_census <- income_census %>%
  relocate(living_alone_est, .before = single_pct)

#Update ALICE estimates using both single and multi-family thresholds
#Ordered from lowest to highest income bin
income_bins <- c(
  "less_10000", "10000_14999", "15000_19999", "20000_24999",
  "25000_29999", "30000_34999", "35000_39999", "40000_44999",
  "45000_49999", "50000_59999", "60000_74999", "75000_99999",
  "100000_124999"
)

#Cumulative sum across bins, row by row
cum_matrix <- t(apply(income_census[, income_bins], 1, cumsum))

#Add cutoff bins
income_census$alice1 <- cum_matrix[, 7]
income_census$alice2 <- cum_matrix[, 10]
income_census$alice3 <- cum_matrix[, 11]
income_census$alice4 <- cum_matrix[, 12]
income_census$alice5 <- cum_matrix[, 13]

income_census <- income_census %>%
  mutate(
    total_hh = as.numeric(total_hh),
    
    multi_threshold = case_when(
      multi_person_est == 0                    ~ 0,
      avg_hh_size < 2.5                         ~ alice2,
      avg_hh_size >= 2.5 & avg_hh_size < 3.5    ~ alice3,
      avg_hh_size >= 3.5 & avg_hh_size < 4.5    ~ alice4,
      avg_hh_size >= 4.5                        ~ alice5,
      TRUE ~ NA_real_
    ),
    
    alice_single_count = (living_alone_est / total_hh) * alice1,
    alice_multi_count = (multi_person_est / total_hh) * multi_threshold,
    alice_count = alice_single_count + alice_multi_count,
    alice_pct = alice_count / total_hh
  )


#Clean up df a bit
clean_alice <- income_census[, c("GEOID", "tract", "block_group", "total_hh", "single_pct", "multi_pct",
                                 "pct_owner", "alice_pct", "alice_count", "parcel_count")]
#Clean up alice and parcel count join dataframe
clean_alice <- alice_pct_allcensus[, c("geoid", "tract", "block", "total_hh",
                                       "owner", "alice_pct", "parcel_count")]
#double checking easier code
alice_buyouts <- alice_buyouts %>%
  mutate(
    total_ce_owner = round(parcel_count * pct_owner, 0),
    total_ce_owner_alice = round(total_ce_owner * alice_pct, 0),
    pct_alice_hazard_owner = ifelse(
      total_ce_owner == 0,
      NA,
      round((total_ce_owner_alice / parcel_count) * 100, 1)
    )
  )

alice_buyouts <- alice_buyouts %>%
  mutate(
    pct_alice_hazard_owner = if_else(
      round(parcel_count * pct_owner, 0) == 0,
      NA_real_,
      round(
        round(round(parcel_count * pct_owner, 0) * alice_pct, 0) / parcel_count * 100,
        1
      )
    )
  )

write.csv(alice_buyout, here ("data", "processed", "alice", "alice_buyouts.csv"))

