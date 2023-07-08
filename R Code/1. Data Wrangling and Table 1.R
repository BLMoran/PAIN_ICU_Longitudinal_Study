## Data Wrangling and Generation of Table 1

# Import Excel file #
library(readxl)
demogr <- read_excel("1. Demographics.xlsx")

library(tidyverse)
library(gtsummary)

library(tidyverse)
library(gtsummary)

# Create Age Variable from DOB and ICU Admission Date
demogr <- demogr %>%
  mutate(age = lubridate::as.duration(`Admission Date` - PatientDOB) %>% as.numeric('years'))

# Create ICU LOS Variable
demogr <- demogr %>%
  mutate(icu_los = lubridate::as.duration(`ICU Discharge Date/Time` - `Admission Date`) %>% as.numeric('days'))

# Create Hospital LOS Variable
demogr <- demogr %>%
  mutate(hosp_los = lubridate::as.duration(`Hospital Discharge Date/Time` - HospAD) %>% as.numeric('days'))

# Create Duration of Ventilation Variable
demogr <- demogr %>% 
  mutate(vent_dur = lubridate::as.duration(`Vent End time` - `Vent start time`) %>% as.numeric('days'))

# Create Variables for admission diagnoses
demogr <- demogr %>%
  mutate(AP3Diag = case_when(
    (AP3Diagnosis...15>=1301) & (AP3Diagnosis...15<=1304) ~ "Post-Operative",
    (AP3Diagnosis...15>=1401) & (AP3Diagnosis...15<=1413) ~ "Post-Operative",
    (AP3Diagnosis...15>=1602) & (AP3Diagnosis...15<=1605) ~ "Post-Operative",
    (AP3Diagnosis...15>=1701) & (AP3Diagnosis...15<=1705) ~ "Post-Operative",
    (AP3Diagnosis...15>=1801) & (AP3Diagnosis...15<=1803) ~ "Post-Operative",
    (AP3Diagnosis...15>=1902) & (AP3Diagnosis...15<=1904) ~ "Post-Operative",
    (AP3Diagnosis...15==2101) ~ "Post-Operative",
    (AP3Diagnosis...15==2201) ~ "Post-Operative",
    (AP3Diagnosis...15>=101) & (AP3Diagnosis...15<=111) ~ "Medical",
    (AP3Diagnosis...15>=201) & (AP3Diagnosis...15<=213) ~ "Medical",
    (AP3Diagnosis...15>=301) & (AP3Diagnosis...15<=313) ~ "Medical",
    (AP3Diagnosis...15>=406) & (AP3Diagnosis...15<407) ~ "Medical",
    (AP3Diagnosis...15>=501) & (AP3Diagnosis...15<=504) ~ "Sepsis",
    (AP3Diagnosis...15>601) & (AP3Diagnosis...15<=605) ~ "Trauma",
    (AP3Diagnosis...15>=701) & (AP3Diagnosis...15<=704) ~ "Medical",
    (AP3Diagnosis...15>=801) & (AP3Diagnosis...15<=802.11) ~ "Medical",
    (AP3Diagnosis...15>=901) & (AP3Diagnosis...15<=903) ~ "Medical",
    (AP3Diagnosis...15>=1101) & (AP3Diagnosis...15<=1102) ~ "Medical",
    (AP3Diagnosis...15 == 212.03 | AP3Diagnosis...15 == 213.02) ~ "COVID"))

# Reorder Apache3 Diagnosis
demogr <- demogr %>% mutate(AP3Diag = factor(AP3Diag, levels = c("Medical", "Sepsis", "Trauma", "Post-Operative", "COVID")))

# Reorder Admission Source
demogr <- demogr %>% mutate(`source of admission` = factor(`source of admission`, levels = c("Emergency department", "OT/Recovery", "Ward", "Other hospital ICU/HDU")))

# Create COVID Binary Variable
demogr <- demogr %>%
  mutate(COVID = as.character(AP3Diagnosis...15 %in% c(212.03, 213.02))) 

# Convert Clinical Frailty Score (CFS) to Categorical
demogr <- demogr %>%
  mutate(CFS = case_when(
    Fraility %in% c("Very Fit", "Well", "Managing Well") ~ "Fit",
    Fraility %in% "Vulnerable" ~ "Vulnerable",
    Fraility %in% c("Mildly Frail", "Moderately Frail", "Severely Frail", "Very Severely Frail", "Terminally Ill") ~ "Frail")) %>% 
  # Reorder Apache3
  mutate(CFS = factor(CFS, levels = c("Fit", "Vulnerable", "Frail")))

# Generate Table 1
demogr %>% 
  select(age, PatientSex, APACHE3, AP3Diag, 'source of admission', vent_dur, CFS, icu_los, `ICU Mortality`, hosp_los, `Hospital Mortality`) %>% 
  tbl_summary(missing = "no",
              label = list(age="Age in years, median (IQR)",
                           PatientSex="Female, number (%)",
                           APACHE3="APACHE III score, median (IQR)",
                           AP3Diag="Admission diagnosis, number (%)",
                           'source of admission' ="Admission source, number (%)",
                           CFS = "Frailty, number (%)",
                           icu_los = "ICU length of stay, median days (IQR)",
                           hosp_los = "Hospital length of stay, median days (IQR)",
                           vent_dur = "Duration of ventilation, median days (IQR)"),
              value = list(PatientSex ~ "Female",
                           'ICU Mortality' ~ "Yes",
                           'Hospital Mortality' ~ "Yes" ),
              statistic = list(all_continuous() ~ "{median} ({p25}-{p75})",
                               all_categorical() ~ "{n} ({p}%)")) %>%  
  bold_labels() %>% 
  italicize_levels() %>% 
  modify_caption("**Table 1. Patient Demographics**") %>% 
  
  modify_table_body(
    ~.x %>% 
      
      # add demographic grouping variable
      rbind(
        tibble(
          variable="Demographics",
          var_type=NA,
          var_label = "Demographics",
          row_type="label",
          label="Demographics",
          stat_0= NA)) %>% 
      
      # add admission grouping variable
      rbind(
        tibble(
          variable="Admission Characteristics",
          var_type=NA,
          var_label = "Admission Characteristics",
          row_type="label",
          label="Admission Characteristics",
          stat_0= NA)) %>%  
      
      # add outcome grouping variable
      rbind(
        tibble(
          variable="Outcome Characteristics",
          var_type=NA,
          var_label = "Outcome Characteristics",
          row_type="label",
          label="Outcome Characteristics",
          stat_0= NA)) %>%  
      
      # specify the position you want these in
      
      arrange(factor(variable, levels=c("Demographics",
                                        "age",
                                        "PatientSex",
                                        "Admission Characteristics",
                                        "APACHE3",
                                        "AP3Diag",
                                        "source of admission",
                                        "CFS",
                                        "Outcome Characteristics",
                                        "vent_dur",
                                        "icu_los",
                                        "ICU Mortality",
                                        "hosp_los",
                                        "Hospital Mortality"))))%>%
  
  # and you can then indent the actual variables
  modify_column_indent(columns=label, rows=variable %in% c("age",
                                                           "PatientSex",
                                                           "APACHE3",
                                                           "AP3Diag",
                                                           "source of admission",
                                                           "CFS",
                                                           "vent_dur",
                                                           "icu_los",
                                                           "ICU Mortality",
                                                           "hosp_los",
                                                           "Hospital Mortality"))%>%
  
  # and double indent their levels
  modify_column_indent(columns=label, rows= (variable %in% c("AP3Diag",
                                                             "source of admission",
                                                             "CFS") 
                                             & row_type=="level"),
                       double_indent=T)
