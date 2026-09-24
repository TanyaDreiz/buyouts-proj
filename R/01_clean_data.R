#Load libraries 
library(data.table)
library(foreign)
library(sf)
library(dplyr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(here)
library(purrr)
library(tidycensus)

# Disable scientific notation
options(scipen = 999)

#Load raw data
assessors <- read_csv(here("data", "raw", "assessors2024.csv"))
buildings_count <- read_csv(here("data", "raw", "buildings_count.csv"))
buildings <- read_csv(here("data", "raw", "building_join_copy.csv"))
hazards <- read.csv(here("data", "raw", "TMK_Hazards_Calculated.csv"))

#Clean assessors data
#Keep only columns we need
clean_assessors <- assessors[, c("TMK", "Current_Building_Applied_Value", "Current_Land_Applied_Value",
                                 "Current_Total_Value", "Land_Class")]
#Rename columns 
colnames(clean_assessors) <- c("tmk", "building_value", "land_value", "total_value", "land_class")

#Turn TMK cols into character
clean_assessors$tmk <- as.character(clean_assessors$tmk)

#Filter assessor's data for residential only
residential <- clean_assessors[clean_assessors$land_class == "RESIDENTIAL" |
                                 clean_assessors$land_class == "RESIDENTIAL A", ]

#Add new column for TMK8
residential$tmk8 <- as.numeric(substr(residential$tmk, 1,8))

#Remove tmk12 column
residential <- residential %>%
  select(-tmk)

#Rename tmk8 to tmk, move to front of df 
residential <- residential %>%
  rename(tmk = tmk8) %>%
  select(tmk, everything())

##ID and Remove CPR units 
#Add new column for CPR unit counts
cpr_count <- residential %>%
  count(tmk) %>%
  filter(n >= 1)

#Group by tmk
grp_res <- residential %>%
  group_by(tmk)

#Merge two data frames by tmk
cpr_res <- merge(grp_res, cpr_count, by="tmk")

#Get rid of CPR units greater than 4
res_noncpr <- cpr_res %>%
  filter(n <= 4)

# ---- Save processed data ------------------------------------------
write_csv(res_noncpr, here("data", "processed", "residential_noncpr_tmks.csv"))
