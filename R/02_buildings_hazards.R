#Keep only cols we need
clean_buildings <- buildings[, c("tmk", "LATITUDE", "LONGITUDE", "Shape__Are", "Shape__Len", "NEAR_line", "NEAR_CE05",
                                 "NEAR_CE11", "NEAR_CE20", "NEAR_CE32", "NEAR_PF05", "NEAR_PF11", "NEAR_PF20",
                                 "NEAR_PF32")]
clean_hazards <- hazards[, c("tmk8num", "area_og", "SA_CE05", "SA_CE11", "SA_CE20", "SA_CE32", "SA_PF05", "SA_PF11",
                             "SA_PF20", "SA_PF32")]
#Rename columns 
colnames(clean_buildings) <- c("tmk", "lat", "long", "shape_area", "shape_length", "near_line", "near_ce05",
                               "near_ce11", "near_ce20", "near_ce32", "near_pf05", "near_pf11", "near_pf20", "near_pf32")
colnames(clean_hazards) <- c("tmk", "area", "sa_ce05", "sa_ce11", "sa_ce20", "sa_ce32", "sa_pf05", "sa_pf11", "sa_pf20",
                             "sa_pf32")

#Turn TMK into character
clean_buildings$tmk <- as.character(clean_buildings$tmk)
clean_hazards$tmk <- as.character(clean_hazards$tmk)

#Merge buildings and hazards together by TMK 
buildings_hazards <- merge(clean_buildings, clean_hazards, by = "tmk")

#Merge building_hazards to assessors
assessors_hazards <- merge(buildings_hazards, res_noncpr, by = "tmk")

##Check for high value properties that may not have been ID'd in CPR count 
#Group by tmk
high_value_grp <- assessors_hazards %>%
  group_by(tmk) 

#Filter for high value property
high_values <- high_value_grp %>%
  filter(total_value >= 20000000)

#Remove tmks after checking qpublic 
assessors_hazards <- assessors_hazards %>% 
  filter(!tmk %in% c("23022062", "23034027", "23036005", "26011025", "26012001", "26012065", "26012066", "27006022",
                     "27020001", "31032001", "31032003", "31032004", "31032006", "31032007", "31032027", "31032030",
                     "31032031", "31033001", "31041005", "91001034", "97012002", "98019003"))

#Remove duplicate TMKs
dedup_buyouts <- assessors_hazards %>%
  distinct(tmk, .keep_all = TRUE)

# ---- Save processed data ------------------------------------------
write_csv(dedup_buyouts, here("data", "processed", "final_buildings_hazards_join.csv"))
