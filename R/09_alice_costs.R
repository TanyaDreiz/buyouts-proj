#Calculate ALICE costs using mean total value per cbg 
#Clean up dataset
clean_tmkcbg <- tmk_cbg[, c("tmk", "geoid20", "total_value")]

#Add mean housing value per block group 
clean_tmkcbg <- clean_tmkcbg %>%
  group_by(geoid20) %>%
  mutate(mean_housing_value = mean(total_value, na.rm = TRUE)) %>%
  ungroup()

#Keep one distinct geoid20 
mean_costs <- clean_tmkcbg %>%
  distinct(geoid20, mean_housing_value)

#Join to ALICE buyouts to calculate costs 
alice_buyout_costs <- alice_buyout %>%
  left_join(mean_costs, by = c("geoid" = "geoid20"))

#Multiply mean value by number of owners who are ALICE in CE hazard
alice_buyout_costs$alice_cost <- alice_buyout_costs$total_ce_owner_alice * alice_buyout_costs$mean_housing_value

write.csv(alice_costs_unique, here ("data", "processed", "buyout_programs", "alice_buyout_cost.csv"))
