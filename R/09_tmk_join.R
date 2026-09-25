#Clean tmk_cbg dataframe (spatial join block groups and parcels in ArcGIS)
clean_tmkcbg <- tmk_cbg[, c("tmk", "geoid20")]

#Get count of parcels per CBG
parcel_counts <- clean_tmkcbg %>%
  count(geoid20, name = "parcel_count")

#Join to ALICE 
alice_pct_allcensus <- alice_pct_allcensus %>%
  left_join(parcel_counts, by = c("geoid" = "geoid20"))

alice_pct_allcensus <- alice_pct_allcensus %>%
  filter(!is.na(parcel_count))
