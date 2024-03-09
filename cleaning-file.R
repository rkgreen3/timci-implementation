# Version date: 2024-03-08

# Load packages
library(plyr)
library(dplyr)
library(lubridate)
`%!in%` = Negate(`%in%`)

# Read in REDCap data for Kenya and Tanzania (saved in Box)
df_og1 <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data-en_2024-03-08.csv") #update file path to local machine
df1 <- df_og1 %>% dplyr:::select(-c(grep("_complete", names(df_og1)))) 
df1 <- df1 %>% dplyr:::select(-c(251:ncol(df1)), "visit_reason___5")
colnames(df1)[colnames(df1) == "malnutrition"] = "malnutrition_dx"
df1$travel_cost <- case_when(df1$travel_cost ==1 ~ "1-20 KSH",
                             df1$travel_cost ==2 ~ "21-40 KSH",
                             df1$travel_cost ==3 ~ "41-60 KSH",
                             df1$travel_cost ==4 ~ "61-80 KSH",
                             df1$travel_cost ==5 ~ "81-100 KSH",
                             df1$travel_cost ==6 ~ ">100 KSH",
                             df1$travel_cost ==7 ~ "1-1000 TSH",
                             df1$travel_cost ==8 ~ "1001-5000 TSH",
                             df1$travel_cost ==9 ~ "5001-10000 TSH",
                             df1$travel_cost ==10 ~ ">10000 TSH",
                             df1$travel_cost ==0 ~ "0")

# Read in REDCap data for Senegal (saved in Box, from different REDCap project)
df_og2 <- read.csv("C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data-fr_2024-03-08.csv") #update file path to local machine
df2 <- df_og2 %>% dplyr:::select(-c(grep("_complete", names(df_og2)))) 
df2 <- df2 %>% dplyr:::select(-c(204:ncol(df2)))
colnames(df2)[colnames(df2) == "visit_reason"] = "visit_reason___1"
df2$travel_cost <- case_when(df2$travel_cost ==1 ~ "<500 CFA",
                             df2$travel_cost ==2 ~ "500-999 CFA",
                             df2$travel_cost ==3 ~ "1000-3000 CFA",
                             df2$travel_cost ==4 ~ ">3000 CFA")

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

# Correct misspelled variable names
colnames(df)[colnames(df) == "houshold_children"] = "household_children"
colnames(df)[colnames(df) == "refferal"] = "referral"

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
df$referral <- ifelse(df$referral>=99, NA, df$referral)

# Calculate age
df$bdate <- paste(df$birth_year, df$birth_month, "15", sep = "-")
df$month_diff <- interval(as.Date(df$bdate), as.Date(df$visit_date)) %/% days(1) / (365/12)
df$age_months <- round(df$month_diff, 2)
df$age_cat <- case_when(df$age_months<2 ~ "0-1 month",
                        df$age_months>=2 & df$age_months<12 ~ "2-11 months",
                         df$age_months>=12 ~ "12-59 months")
df$age_months <- ifelse(df$age_months<0, 0.5, df$age_months)
df <- subset(df, age_months<60) # remove participants >59 months, N = 17
df <- df %>% dplyr:::select(-c("bdate", "month_diff"))

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

# Rename visit reason and symptoms and travel mode variables
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

colnames(df)[colnames(df) == "travel_mode___1"] = "travel_mode_walk"
colnames(df)[colnames(df) == "travel_mode___2"] = "travel_mode_bicycle"
colnames(df)[colnames(df) == "travel_mode___3"] = "travel_mode_motorcycle"
colnames(df)[colnames(df) == "travel_mode___4"] = "travel_mode_bus"
colnames(df)[colnames(df) == "travel_mode___5"] = "travel_mode_rickshaw"
colnames(df)[colnames(df) == "travel_mode___6"] = "travel_mode_publictaxi"
colnames(df)[colnames(df) == "travel_mode___7"] = "travel_mode_privatetaxi"
colnames(df)[colnames(df) == "travel_mode___8"] = "travel_mode_privatecar"
colnames(df)[colnames(df) == "travel_mode___9"] = "travel_mode_sharedcar"
colnames(df)[colnames(df) == "travel_mode___10"] = "travel_mode_carriage"
colnames(df)[colnames(df) == "travel_mode___11"] = "travel_mode_other"

# Create duration variables
df$po_start <- paste(df$po_start, ":00", sep = "")
df$po_start <- strptime(df$po_start, format = '%H:%M:%S')
df$po_stop <- paste(df$po_stop, ":00", sep = "")
df$po_stop <- strptime(df$po_stop, format = '%H:%M:%S')
df$po_duration <- difftime(df$po_stop, df$po_start, units = "secs")
df$po_duration <- as.integer(df$po_duration, units = "secs")
df$po_duration_min <- df$po_duration/60

df$consult_start <- paste(df$consult_start, ":00", sep = "")
df$consult_start <- strptime(df$consult_start, format = '%H:%M:%S')
df$consult_stop <- paste(df$consult_stop, ":00", sep = "")
df$consult_stop <- strptime(df$consult_stop, format = '%H:%M:%S')
df$consult_duration <- difftime(df$consult_stop, df$consult_start, units = "secs")
df$consult_duration <- as.integer(df$consult_duration, units = "secs")
df$consult_duration_min <- df$consult_duration/60

# Create other variables
df$country <- case_when(df$facility_name=="TZN01"|df$facility_name=="TZN02" ~ "Tanzania",
                        df$facility_name=="SEN01"|df$facility_name=="SEN02" ~ "Senegal",
                        df$facility_name=="KYA01"|df$facility_name=="KYA02" ~ "Kenya")
df$bmi <- (df$weight/(df$height^2))*10000
df$danger_assessed_yn <- ifelse(!is.na(df$drink_yn) & !is.na(df$vomit_yn) & !is.na(df$convulsions_yn), 1, 0) # all danger signs assessed by provider
df$mainsxs_assessed_yn <- ifelse(!is.na(df$cough_yn) & !is.na(df$dyspnea_yn) & !is.na(df$diarrhea_yn) & !is.na(df$fever_yn) & !is.na(df$earproblem_yn) & !is.na(df$anemia_yn), 1, 0) # all main sxs assessed by provider
df$cough_dx_yn <- ifelse(is.na(df$cough_dx), 0, 1)
df$diarrhea_dx_yn <- ifelse(is.na(df$diarrhea_dx), 0, 1)
df$fever_dx_yn <- ifelse(is.na(df$fever_dx), 0, 1)
df$ear_dx_yn <- ifelse(is.na(df$ear_dx), 0, 1)
df$malnutrition_dx_yn <- ifelse(is.na(df$malnutrition_dx), 0, 1)
df$anemia_dx_yn <- ifelse(is.na(df$anemia_dx), 0, 1)
df$hiv_dx_yn <- ifelse(is.na(df$hiv_dx), 0, 1)
df$clinical_measures_complete <- ifelse(!is.na(df$spo2 & df$pr & df$rr & df$temp_po), 1, 0)
df$travel_cost_usd <- case_when(df$travel_cost == "0" ~ "$0",
                                df$travel_cost == "1-20 KSH" ~ "$0.01-0.14",
                                df$travel_cost == "21-40 KSH" ~ "$0.15-0.28",
                                df$travel_cost == "41-60 KSH" ~ "$0.29-0.42",
                                df$travel_cost == "61-80 KSH" ~ "$0.43-0.56",
                                df$travel_cost == "81-100 KSH" ~ "$0.57-0.70",
                                df$travel_cost == ">100 KSH" ~ ">$0.70",
                                df$travel_cost == "1-1000 TSH" ~ "<$0.40",
                                df$travel_cost == "1001-5000 TSH" ~ "$0.40-2.00",
                                df$travel_cost == "5001-10000 TSH" ~ "$2-3.92",
                                df$travel_cost == ">10000 TSH" ~ ">$3.92",
                                df$travel_cost == "< 500 CFA" ~ "<$0.83",
                                df$travel_cost == "500-999 CFA" ~ "$0.83-1.66",
                                df$travel_cost == "1000-3000 CFA" ~ "$1.66-4.98",
                                df$travel_cost == ">3000 CFA" ~ ">$5")
df$travel_mode <- case_when(df$travel_mode_bicycle==1 ~ "Bicycle",
                            df$travel_mode_walk==1 ~ "Walk",
                            df$travel_mode_motorcycle==1 ~ "Motorcycle",
                            df$travel_mode_bus==1 ~ "Public mini-van/bus",
                            df$travel_mode_rickshaw==1 ~ "Rickshaw",
                            df$travel_mode_publictaxi==1 ~ "Public taxi",
                            df$travel_mode_privatetaxi==1 ~ "Private taxi",
                            df$travel_mode_privatecar==1 ~ "Own private car",
                            df$travel_mode_sharedcar==1 ~ "Shared car",
                            df$travel_mode_carriage==1 ~ "Pack animals/carriage",
                            df$travel_mode_other==1 ~ "Other")

# Calculate z-scores for nutrition metrics; use to create indicator variables -- not possible, sex of child not collected during study

# Create anemia status variable (based on WHO guidelines: WHO/NMH/NHD/MNM/11.1)
df$anemia_status <- case_when(df$hb<7.0 ~ "severe",
                              df$hb<10 & df$hb>=7.0 ~ "moderate",
                              df$hb<11 & df$hb>=10.0 ~ "mild",
                              df$hb>=11.0 ~ "none")
df$anemia_status <- ifelse(df$age_months<6, NA, df$anemia_status)

# Create other clinical outcome variables
df$hypoxia_severe <- ifelse(df$spo2<=90, 1, 0)
df$hypoxia_moderate <- ifelse(df$spo2<=92, 1, 0)
df$fever <- ifelse(df$temp_po>=38, 1, 0)
df$fast_breathing <- case_when(df$age_months<12 & df$rr>50 ~ 1,
                               df$age_months>11 & df$age_months<36 & df$rr>40 ~ 1,
                               df$age_months>35 & df$rr>30 ~ 1)
df$fast_breathing <- ifelse(is.na(df$fast_breathing), 0, df$fast_breathing)
df$tachycardia <- case_when(df$age_months<12 & df$pr>180 ~ 1,
                            df$age_months>11 & df$pr>140 ~ 1)
df$tachycardia <- ifelse(is.na(df$tachycardia), 0, df$tachycardia)

#tdf <- df %>% dplyr::select(screening_id, country, age_months, spo2_yn, spo2, hypoxia_severe, hypoxia_moderate, temp_po_yn, temp_po, fever, rr_yn, rr, fast_breathing, pr_yn, pr, tachycardia)
#tdf2 <- subset(tdf, temp_po_yn==1 & is.na(temp_po) | rr_yn==1 & is.na(rr) | pr_yn==1 & is.na(pr))

# Remove caregiver interview variables
#df <- df %>% dplyr:::select(-c("cg_interview_yn", "cg_sex", "cg_overall_comfort", "cg_like_most", "cg_like_least", "cg_prov_challenges_yn", "cg_prov_challenges", "cg_overall_satisfied", "cg_confident_use", "cg_confident_performance", "cg_adequate_assess_yn", "cg_adequate_assess_rsn", "cg_advantage", "cg_concerns", "cg_compare_assess", "cg_compare_assess_rsn", "cg_useful", "cg_useful_rsn", "cg_rec_device", "cg_rec_facility", "cg_rec_facility_rsn", "cg_overall_impression", "cg_time_change_yn", "cg_time_change", "cg_understand_purpose", "cg_dx_confidence", "cg_discomfort", "cg_discomfort_des", "cg_recommend", "cg_change_desire", "cg_othe_comments", "caregiver_interview_complete"))

# Write file as .csv to shared Box folder
write.csv(df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data_clean_2024-03-08.csv")
