# Version date: 2024-02-02

# Load packages
library(plyr)
library(dplyr)
library(lubridate)
`%!in%` = Negate(`%in%`)

# Read in REDCap data for Kenya and Tanzania (saved in Box)
df_og1 <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data-en_2024-02-02.csv") #update file path to local machine
df1 <- df_og1 %>% select(-c(grep("_complete", names(df_og1)))) 
df1 <- df1 %>% select(-c(251:ncol(df1)), "visit_reason___5")
colnames(df1)[colnames(df1) == "malnutrition"] = "malnutrition_dx"

# Read in REDCap data for Senegal (saved in Box, from different REDCap project)
df_og2 <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data-fr_2024-02-02.csv") #update file path to local machine
df2 <- df_og2 %>% select(-c(grep("_complete", names(df_og2)))) 
df2 <- df2 %>% select(-c(204:ncol(df2)))
colnames(df2)[colnames(df2) == "visit_reason"] = "visit_reason___1"

# Cross check column names and add to respective df's
new_columns1 <- setdiff(names(df2), names(df1)) # Identify columns in df2 that are not in df1
# Add missing columns to df1 with NA values
for (column in new_columns1) {
  df1[[column]] <- NA
}
new_columns2 <- setdiff(names(df1), names(df2)) # Identify columns in df1 that are not in df2
# Add missing columns to df2 with NA values
for (column in new_columns2) {
  df2[[column]] <- NA
}

# Additional cleaning of Senegal df
df2$redcap_data_access_group <- ifelse(df2$facility_name=="SEN02", "senegal_mboro", ifelse(df2$facility_name=="SEN01", "senegal_ngeniene", df2$redcap_data_access_group))

# Join df's together
df <- rbind(df1, df2)

# Basic cleaning (exclude ineligible records)
df <- subset(df, is.na(exclude_data_rsn) | exclude_data_rsn!=2) # remove duplicates, N = 25
df <- subset(df, cg_consent_yn==1) # remove non-consented, N = 1
df <- subset(df, is.na(exclude_data_rsn) | exclude_data_rsn!=3) # remove records w/insufficient data, N = 42

# Replace all 999 with NA
df$cg_edu_mother <- ifelse(df$cg_edu_mother>=99, NA, df$cg_edu_mother)
df$care_referral <- ifelse(df$care_referral>=99, NA, df$care_referral)
df$nearest_facility_study <- ifelse(df$nearest_facility_study>=99, NA, df$nearest_facility_study)
df$household_head <- ifelse(df$household_head>=99, NA, df$household_head)
df$household_toilet <- ifelse(df$household_toilet>=99, NA, df$household_toilet)
df$shared_toilet_yn <- ifelse(df$shared_toilet_yn>=99, NA, df$shared_toilet_yn)
df$household_fuel <- ifelse(df$household_fuel>=99, NA, df$household_fuel)
df$household_water_time <- ifelse(df$household_water_time>=99, NA, df$household_water_time)
df$household_floor <- ifelse(df$household_floor>=99, NA, df$household_floor)
df$travel_time <- ifelse(df$travel_time>=99, NA, df$travel_time)
df$travel_cost <- ifelse(df$travel_cost>=99, NA, df$travel_cost)

df$spo2 <- ifelse(df$spo2>=999, NA, df$spo2)
df$pr <- ifelse(df$pr>=999, NA, df$pr)
df$rr <- ifelse(df$rr>=999, NA, df$rr)
df$temp_po <- ifelse(df$temp_po>=999, NA, df$temp_po)
df$weight <- ifelse(df$weight>=999, NA, df$weight)
df$height <- ifelse(df$height>=999, NA, df$height)
df$muac <- ifelse(df$muac>=999, NA, df$muac)
df$hb <- ifelse(df$hb>=999, NA, df$hb)
df$refferal <- ifelse(df$refferal>=99, NA, df$refferal)

# Calculate age
df$bdate <- paste(df$birth_year, df$birth_month, "15", sep = "-")
df$month_diff <- interval(as.Date(df$bdate), as.Date(df$visit_date)) %/% days(1) / (365/12)
df$age_months <- round(df$month_diff, 2)
df$age_cat <- case_when(df$age_months<2 ~ "0-1 month",
                        df$age_months>=2 & df$age_months<12 ~ "2-11 months",
                         df$age_months>=12 ~ "12-59 months")
df$age_months <- ifelse(df$age_months<0, 0.5, df$age_months)
df <- df %>% select(-c("bdate", "month_diff"))

# Add cg relationship for codes
df$cg_relationship <- case_when(df$cg_relationship==1 ~ "Mother and father",
                                df$cg_relationship==2 ~ "Mother and grandmother",
                                df$cg_relationship==3 ~ "Mother only",
                                df$cg_relationship==4 ~ "Father only",
                                df$cg_relationship==5 ~ "Grandmother only",
                                df$cg_relationship==6 ~ "Grandfather only",
                                df$cg_relationship==7 ~ "Sibling",
                                df$cg_relationship==8 ~ "Other family member",
                                df$cg_relationship==9 ~ "Community member",
                                df$cg_relationship==10 ~ "Other")

# Rename visit reason and symptoms variables
colnames(df)[colnames(df) == "visit_reason___1"] = "visit_rsn_illness"
colnames(df)[colnames(df) == "visit_reason___2"] = "visit_rsn_trauma"
colnames(df)[colnames(df) == "visit_reason___3"] = "visit_rsn_immunize"
colnames(df)[colnames(df) == "visit_reason___4"] = "visit_rsn_routine"

colnames(df)[colnames(df) == "reported_sxs___1"] = "cg_report_cough"
colnames(df)[colnames(df) == "reported_sxs___2"] = "cg_report_rapidbreathing"
colnames(df)[colnames(df) == "reported_sxs___3"] = "cg_report_fever"
colnames(df)[colnames(df) == "reported_sxs___4"] = "cg_report_diarrhea"
colnames(df)[colnames(df) == "reported_sxs___5"] = "cg_report_vomit"
colnames(df)[colnames(df) == "reported_sxs___6"] = "cg_report_other"

# Create duration variables
df$po_start <- paste(df$po_start, ":00", sep = "")
df$po_start <- strptime(df$po_start, format = '%H:%M:%S')
df$po_stop <- paste(df$po_stop, ":00", sep = "")
df$po_stop <- strptime(df$po_stop, format = '%H:%M:%S')
df$po_duration <- difftime(df$po_stop, df$po_start, units = "secs")

df$consult_start <- paste(df$consult_start, ":00", sep = "")
df$consult_start <- strptime(df$consult_start, format = '%H:%M:%S')
df$consult_stop <- paste(df$consult_stop, ":00", sep = "")
df$consult_stop <- strptime(df$consult_stop, format = '%H:%M:%S')
df$consult_duration <- difftime(df$consult_stop, df$consult_start, units = "secs")

# Create other variables
df$country <- case_when(df$facility_name=="TZN01"|df$facility_name=="TZN02" ~ "Tanzania",
                        df$facility_name=="SEN01"|df$facility_name=="SEN02" ~ "Senegal",
                        df$facility_name=="KYA01"|df$facility_name=="KYA02" ~ "Kenya")
df$bmi <- (df$weight/(df$height^2))*10000
df$danger_assessed_yn <- ifelse(!is.na(df$drink_yn) & !is.na(df$vomit_yn) & !is.na(df$convulsions_yn), 1, 0) # all danger signs assessed by provider
df$mainsxs_assessed_yn <- ifelse(!is.na(df$cough_yn) & !is.na(df$dyspnea_days) & !is.na(df$diarrhea_yn) & !is.na(df$fever_yn) & !is.na(df$earproblem_yn) & !is.na(df$anemia_yn), 1, 0) # all main sxs assessed by provider
df$clinical_measures_complete <- ifelse(!is.na(df$spo2 & df$pr & df$rr & df$temp_po), 1, 0)

# Calculate z-scores for nutrition metrics; use to create indicator variables -- not possible, sex of child not collected during study

# Create anemia status variable (based on WHO guidelines: WHO/NMH/NHD/MNM/11.1)
df$anemia_status <- case_when(df$hb<7.0 ~ "severe",
                              df$hb<10 & df$hb>=7.0 ~ "moderate",
                              df$hb<11 & df$hb>=10.0 ~ "mild",
                              df$hb>=11.0 ~ "none")
df$anemia_status <- ifelse(df$age_months<6, NA, df$anemia_status)

# Remove caregiver interview variables
df <- df %>% select(-c("cg_interview_yn", "cg_sex", "cg_overall_comfort", "cg_like_most", "cg_like_least", "cg_prov_challenges_yn", "cg_prov_challenges", "cg_overall_satisfied", "cg_confident_use", "cg_confident_performance", "cg_adequate_assess_yn", "cg_adequate_assess_rsn", "cg_advantage", "cg_concerns", "cg_compare_assess", "cg_compare_assess_rsn", "cg_useful", "cg_useful_rsn", "cg_rec_device", "cg_rec_facility", "cg_rec_facility_rsn", "cg_overall_impression", "cg_time_change_yn", "cg_time_change", "cg_understand_purpose", "cg_dx_confidence", "cg_discomfort", "cg_discomfort_des", "cg_recommend", "cg_change_desire", "cg_othe_comments", "caregiver_interview_complete"))

# Write file as .csv to shared Box folder
write.csv(df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data_clean_2024-02-23.csv")
