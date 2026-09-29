#Assign values
triggers <- c("ce")
slrs <- c("05","11","20","32") #ft
years <- c(2030,2050,2075,2100)
thresholds <- c(0,1.5,3,4.6,6.1) #m

#Loop time, thresholds, triggers to create new dataframes with timestamps  
for(trigger in triggers) {
  for(threshold in thresholds) {
    object <- paste0("dedup_buyouts", trigger, "_thresh", threshold)
    
    col_year <- paste0("year_t", trigger)
    col_near2030 <- paste0("near_", trigger, slrs[[1]])
    col_near2050 <- paste0("near_", trigger, slrs[[2]])
    col_near2075 <- paste0("near_", trigger, slrs[[3]])
    col_near2100 <- paste0("near_", trigger, slrs[[4]])
    
    
    dedup_buyouts[[col_year]] <- ifelse(dedup_buyouts[[col_near2100]] <= threshold, 2100, NA)
    dedup_buyouts[[col_year]] <- ifelse(dedup_buyouts[[col_near2075]] <= threshold, 2075, dedup_buyouts[[col_year]])
    dedup_buyouts[[col_year]] <- ifelse(dedup_buyouts[[col_near2050]] <= threshold, 2050, dedup_buyouts[[col_year]])
    dedup_buyouts[[col_year]] <- ifelse(dedup_buyouts[[col_near2030]] <= threshold, 2030, dedup_buyouts[[col_year]])
    dedup_buyouts[[col_year]] <- ifelse(dedup_buyouts[["near_line"]] <= threshold, 2024, dedup_buyouts[[col_year]])
    
    assign(object, dedup_buyouts)
  }
}

#Filter NA values
ce_buyouts <- dedup_buyoutsce_thresh6.1 %>%
  filter(!(is.na(year_tce)))

# ---- Save processed data ------------------------------------------
write_csv(dedup_buyoutsce_thresh0, here("data", "processed", "buyouts_thresh0.csv"))
write_csv(dedup_buyoutsce_thresh1.5, here("data", "processed", "buyouts_thresh1.5.csv"))
write_csv(dedup_buyoutsce_thresh3, here("data", "processed", "buyouts_thresh3.csv"))
write_csv(dedup_buyoutsce_thresh4.6, here("data", "processed", "buyouts_thresh4.6.csv"))
write_csv(dedup_buyoutsce_thresh6.1, here("data", "processed", "buyouts_thresh6.1.csv"))
write_csv(ce_buyouts, here("data", "processed", "ce_buyouts_hazardloop.csv"))
