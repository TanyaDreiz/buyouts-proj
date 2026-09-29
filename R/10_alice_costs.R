#Calculate ALICE costs using mean total value per cbg 
#Clean up dataset
cbg_rpa <- cbg_rpa[, c("tmk", "geoid20", "district")]

#add RPA 
alice_buyouts <- alice_buyouts %>%
  left_join(
    cbg_rpa %>% select(geoid20, district),
    by = c("GEOID" = "geoid20")
  )

#Rename geoid column 
alice_buyouts <- alice_buyouts %>% rename(geoid20 = GEOID)

#Calculate mean housing value per block group 
allcounty_capped_geography <- allcounty_capped_geography %>%
  group_by(geoid20) %>%
  mutate(mean_buyout_cost = mean(public_costce_disc, na.rm = TRUE)) %>%
  ungroup()

#Keep one distinct geoid20 alice_buyouts <- alice_buyouts %>% rename(geoid20 = GEOID)
allcounty_dedup <- allcounty_capped_geography %>%
  distinct(geoid20, .keep_all = TRUE)

#Add to alice dataframe
alice_buyouts <- alice_buyouts %>%
  left_join(
    allcounty_dedup %>% select(geoid20, mean_buyout_cost),
    by = "geoid20"
  )


#Multiply mean value by number of owners who are ALICE in CE hazard
alice_buyouts$alice_cost <- alice_buyouts$total_ce_owner_alice * alice_buyouts$mean_buyout_cost

write.csv(alice_buyouts, here ("data", "processed", "buyout_programs", "alice_buyout_cost.csv"))
