pain <- read_excel("2. Daily Pain Scores.xlsx")
library(janitor)
library(tidyverse)
library(lubridate)

# Convert Time integer into Datetime
library(openxlsx)
pain <- pain %>% mutate(Time = convertToDateTime(pain$Time, origin = "1900-01-01"))

#Clean Column Names
pain <- pain %>% 
  clean_names()

# Clean Up Data Frame
pain <- pain %>% 
  filter(ventday != "NULL", assessment_tool !="NPS_static", assessment_tool != "NPS_dyn")

# Create Time Difference Variable Between Pain Scores
pain <- pain %>% 
  complete(time = seq.POSIXt(min(ceiling_date(time , 'day')), 
                             max(floor_date(time , 'day')), by = 'day')) %>% 
  arrange(time) %>%
  fill(c(patient_id, assessment_tool, score)) %>% fill(ventday, .direction = "up") %>%
  mutate(time_diff_hours = as.numeric(lead(time) - time, units = 'hours'))

# Create pain categories
pain <- pain %>% 
  mutate(pain_cat = case_when(
    # CPOT Assessment Tool
    (assessment_tool == "CPOT") & (score == 0) ~ "None",
    (assessment_tool == "CPOT") & (score >= 1) & (score <=2) ~ "Mild",
    (assessment_tool == "CPOT") & (score >= 3) & (score <=5) ~ "Moderate",
    (assessment_tool == "CPOT") & (score >= 6) & (score <=8) ~ "Severe",
    # NPS_static Assessment Tool
    (assessment_tool == "NPS_static") & (score == 0) ~ "None",
    (assessment_tool == "NPS_static") & (score >= 1) & (score <=3) ~ "Mild",
    (assessment_tool == "NPS_static") & (score >= 4) & (score <=7) ~ "Moderate",
    (assessment_tool == "NPS_static") & (score >= 8) & (score <=10) ~ "Severe",
    # NPS_dyn Assessment Tool
    (assessment_tool == "NPS_dyn") & (score == 0) ~ "None",
    (assessment_tool == "NPS_dyn") & (score >= 1) & (score <=3) ~ "Mild",
    (assessment_tool == "NPS_dyn") & (score >= 4) & (score <=7) ~ "Moderate",
    (assessment_tool == "NPS_dyn") & (score >= 8) & (score <=10) ~ "Severe",
    # BPS Assessment Tool
    (assessment_tool == "BPS") & (score <= 3) ~ "None",
    (assessment_tool == "BPS") & (score >= 4) & (score <=6) ~ "Mild",
    (assessment_tool == "BPS") & (score >= 7) & (score <=9) ~ "Moderate",
    (assessment_tool == "BPS") & (score >= 10) & (score <=12) ~ "Severe"
  ))

# Create Time in No Pain
pain <- pain %>% 
  mutate(pain_none = ifelse(pain_cat == "None", time_diff_hours, 0))

# Create Time in Mild Pain
pain <- pain %>% 
  mutate(pain_mild = ifelse(pain_cat == "Mild", time_diff_hours, 0))

# Create Time in Moderate Pain
# Create Time in No Pain
pain <- pain %>% 
  mutate(pain_mod = ifelse(pain_cat == "Moderate", time_diff_hours, 0))

# Create Time in Severe Pain
# Create Time in No Pain
pain <- pain %>% 
  mutate(pain_sev = ifelse(pain_cat == "Severe", time_diff_hours, 0))

# Create Time in Moderate to Severe Pain
# Create Time in No Pain
pain <- pain %>% 
  mutate(pain_mod_sev = ifelse(pain_cat == "Moderate", time_diff_hours, ifelse(pain_cat == "Severe", time_diff_hours, 0)))


# Create Variable of % Time in Each Pain Category Per Day (Excluding Ventday = NULL and NPS scores as they usually are done with CPOT in ventilated patients)
pain <- pain %>% 
  arrange(patient_id, ventday) %>% 
  group_by(patient_id, ventday) %>% 
  summarise(across(c(pain_none, pain_mild, pain_mod, pain_sev, pain_mod_sev), list(sum))) %>% 
  mutate(across(where(is.numeric), round, 1))


# Remove numbers at end of variable names
new_var_name <- c(pain_none = "pain_none_1", pain_mild = "pain_mild_1", pain_mod = "pain_mod_1", pain_sev = "pain_sev_1", pain_mod_sev = "pain_mod_sev_1")
pain <- pain %>% 
  rename(all_of(new_var_name))


# Convert to wide format
pain <- pain %>% 
  pivot_wider(
    id_cols = patient_id,
    names_from = c(ventday),
    names_glue = "vent{ventday}_{.value}",
    values_from = c(pain_none, pain_mild, pain_mod, pain_sev, pain_mod_sev),
    values_fill = 0)

