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

# Calculate age
df$bdate <- paste(df$birth_year, df$birth_month, "15", sep = "-")
df$month_diff <- interval(as.Date(df$bdate), as.Date(df$visit_date)) %/% days(1) / (365/12)
df$age_months <- round(df$month_diff, 2)
df$age_cat <- case_when(df$age_months<2 ~ "0-1 month",
                         df$age_months>=2 ~ "2-59 months")
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
df$po_duration <- difftime(df$po_start, df$po_stop, units = "secs")

# Basic cleaning (exclude ineligible records)
df <- subset(df, exclude_data_rsn!=2) # remove duplicates, N = 25
df <- subset(df, cg_consent_yn==1) # remove non-consented, N = 0
df <- subset(df, exclude_data_rsn!=3) # remove records w/insufficient data, N = 43

# Write file as .csv to shared Box folder
write.csv(df, "C:/Users/rgreen/Box/3_Output 3/Hybrid study/Implementation Study Analysis/implementation-data_clean_2024-02-02.csv")
