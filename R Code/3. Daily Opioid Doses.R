opioid <- read_excel("3. Daily Opioids.xlsx")
library(janitor)
library(tidyverse)
library(lubridate)
library(zoo)

#Clean Column Names
opioid <- opioid %>% 
  clean_names()

# Rename variables naming opioid infusions and dosage time intervals
opioid_var <- c(morphinf = "medication_5", fentinf = "medication_14", hydroinf = "medication_23",
                morph0_3 = "x00_03_6", morph3_6 = "x03_06_7", morph6_9 = "x06_09_8", morph9_12 = "x09_12_9", morph12_15 = "x12_15_10", morph15_18 = "x15_18_11", morph18_21 = "x18_21_12", morph21_24 = "x21_00_13",
                fent0_3 = "d00_03", fent3_6 = "d03_06", fent6_9 = "d06_09", fent9_12 = "d09_12", fent12_15 = "d12_15", fent15_18 = "d15_18", fent18_21 = "d18_21", fent21_24 = "d21_00",
                hydro0_3 = "x00_03_24", hydro3_6 = "x03_06_25", hydro6_9 = "x06_09_26", hydro9_12 = "x09_12_27", hydro12_15 = "x12_15_28", hydro15_18 = "x15_18_29", hydro18_21 = "x18_21_30", hydro21_24 = "x21_00_31")

opioid <- opioid %>% 
  rename(all_of(opioid_var))

# Change character to numeric
opioid <- opioid %>% 
  mutate(across(c(`morph0_3`:`morph21_24`, `fent0_3`:`fent21_24`, `hydro0_3`:`hydro21_24`), as.numeric))

# Change NA to 0
opioid <- opioid %>% 
  replace(is.na(.), 0)

# Create Daily Opioid Dose (OME)
opioid <- opioid %>% 
  rowwise() %>% 
  mutate(
    morphinfome = (sum(c_across(morph0_3:morph21_24), na.rm = F)*3),
    fentinfome = (sum(c_across(fent0_3:fent21_24), na.rm = F)/5),
    hydroinfome = (sum(c_across(hydro0_3:hydro21_24), na.rm = F)*3)
  ) %>% 
  ungroup()

# Create Total Daily OME
opioid <- opioid %>% 
  mutate(
    total_ome = (morphinfome + fentinfome + hydroinfome)
  )