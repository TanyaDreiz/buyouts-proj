# Keep an untouched copy of the base data so each iteration starts clean
base_buyouts <- dedup_buyouts

for (trigger in triggers) {
  for (threshold in thresholds) {
    
    # Fresh copy for this combination only
    working_df <- base_buyouts
    
    object <- paste0("dedup_buyouts", trigger, "_thresh", threshold)
    col_year <- paste0("year_t", trigger)
    
    col_near2030 <- paste0("near_", trigger, slrs[[1]])
    col_near2050 <- paste0("near_", trigger, slrs[[2]])
    col_near2075 <- paste0("near_", trigger, slrs[[3]])
    col_near2100 <- paste0("near_", trigger, slrs[[4]])
    
    working_df[[col_year]] <- ifelse(working_df[[col_near2100]] <= threshold, 2100, NA)
    working_df[[col_year]] <- ifelse(working_df[[col_near2075]] <= threshold, 2075, working_df[[col_year]])
    working_df[[col_year]] <- ifelse(working_df[[col_near2050]] <= threshold, 2050, working_df[[col_year]])
    working_df[[col_year]] <- ifelse(working_df[[col_near2030]] <= threshold, 2030, working_df[[col_year]])
    working_df[[col_year]] <- ifelse(working_df[["near_line"]] <= threshold, 2024, working_df[[col_year]])
    
    assign(object, working_df)
  }
}


# ---- Save processed data ------------------------------------------
write_csv(dedup_buyoutsce_thresh0, here("data", "processed", "buyouts_thresh0.csv"))
write_csv(dedup_buyoutsce_thresh1.5, here("data", "processed", "buyouts_thresh1.5.csv"))
write_csv(dedup_buyoutsce_thresh3, here("data", "processed", "buyouts_thresh3.csv"))
write_csv(dedup_buyoutsce_thresh4.6, here("data", "processed", "buyouts_thresh4.6.csv"))
write_csv(dedup_buyoutsce_thresh6.1, here("data", "processed", "buyouts_thresh6.1.csv"))
