# Version date:

# Load packages
library(plyr)
library(dplyr)

# Read in REDCap data for Kenya and Tanzania (saved in Box)
df_og1 <- read.csv("") #update file path to local machine
# Read in REDCap data for Senegal (saved in Box, from different REDCap project)
df_og2 <- read.csv("") #update file path to local machine

# Join df's together

# Basic cleaning (exclude ineligible records)