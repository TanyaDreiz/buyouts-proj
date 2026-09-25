#Ordered from lowest to highest income bin
income_bins <- c(
  "less_10000", "10000_14999", "15000_19999", "20000_24999",
  "25000_29999", "30000_34999", "35000_39999", "40000_44999",
  "45000_49999", "50000_59999", "60000_74999", "75000_99999",
  "100000_124999"
)

#Cumulative sum across bins, row by row
cum_matrix <- t(apply(acs_clean[, income_bins], 1, cumsum))

#Add cutoff bins
acs_clean$alice1 <- cum_matrix[, 7]
acs_clean$alice2 <- cum_matrix[, 10]
acs_clean$alice3 <- cum_matrix[, 11]
acs_clean$alice4 <- cum_matrix[, 12]
acs_clean$alice5 <- cum_matrix[, 13]

#Get ALICE percentages
acs_clean <- acs_clean %>%
  mutate(
    total_hh = as.numeric(total_hh),
    alice_pct = case_when(
      avg_hh < 1.5                      ~ alice1 / total_hh,
      avg_hh >= 1.5 & avg_hh < 2.5  ~ alice2 / total_hh,
      avg_hh >= 2.5 & avg_hh < 3.5  ~ alice3 / total_hh,
      avg_hh >= 3.5 & avg_hh < 4.5  ~ alice4 / total_hh,
      avg_hh >= 4.5                     ~ alice5 / total_hh,
      TRUE ~ NA_real_
    )
  )

#Clean up alice and parcel count join dataframe
clean_alice <- alice_pct_allcensus[, c("geoid", "tract", "block", "total_hh",
                                       "owner", "alice_pct", "parcel_count")]

#Create new column for percent of owners per CBG
clean_alice$pct_owner <- clean_alice$owner / clean_alice$total_hh

#Calculate count of homeowners that are in a CE-exposed parcel
clean_alice$count_ce_owner <- clean_alice$parcel_count * clean_alice$pct_owner

#Calculate count of homeowners exposed to CE that are ALICE
clean_alice$count_ce_owner_alice <- clean_alice$count_ce_owner * clean_alice$alice_pct

#Calculate percentage of ALICE homeowners per CBG
clean_alice$pct_alice_hazard_owner <- (clean_alice$count_ce_owner_alice / clean_alice$count_ce_owner) * 100

#double checking easier code
alice_code_check <- alice_pct_allcensus %>%
  mutate(
    pct_owner = owner / total_hh,
    total_ce_owner = parcel_count * pct_owner,
    total_ce_owner_alice = total_ce_owner * alice_pct,
    pct_alice_hazard_owner = ifelse(
      total_ce_owner == 0,
      NA,
      round((total_ce_owner_alice / total_ce_owner) * 100, 1)
    ),
    pct_owner_display = round(pct_owner * 100, 1)
  )

write.csv(acs_clean, here ("data", "processed", "census_data", "alice_pct_allcensus.csv"))
