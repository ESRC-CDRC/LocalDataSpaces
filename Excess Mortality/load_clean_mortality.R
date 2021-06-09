########################################
### Load and clean mortality records ###
########################################

# Purpose: To load all monthly mortality records for 2020, tidy the datasets and combine into a single analytical file. 

# Note: ICD-10 code for COVID-19 is U07.1 (virus identified) and U07.2 (not identified but suspected or probable). In the data there will be no full stop (so U071)

# Re: which cause of death measures to use - typically we would just use underlying cause of death, however we may want to create two measures - one for that and one for any mention on the death certificate. They do give different numbers, for example 351 cases U071 in ICD10U vs 523 cases of U071 across ICD1001 to ICD10015. ONS typically release both, but focus on the former.

# Libraries
library(data.table)
library(foreign)

# Load multiple files at once into a single file
temp <- list.files(path = "//vmlfps01v/DataStore$/Monthly Deaths/SPSS/", pattern = "*.sav", full.names = TRUE) # Identify list of all files
mortality <- rbindlist(lapply(temp, read.spss)) # Load into R as single data frame
rm(temp) # Tidy

# Subset only 2020 deaths
mortality_2020 <- mortality[mortality$DODYR == 2020]
rm(mortality)

# Keep only variables we will need (those described above)
mortality_small <- mortality_2020[,c("PCDR", "SEX", "AGEINYRS", "AGEGROUP", "MARSTAT", "DODDY", "DODMT", "DODYR", "DOR", "ICD10U", "ICD10001", "ICD10002", "ICD10003", "ICD10004", "ICD10005", "ICD10006", "ICD10007", "ICD10008", "ICD10009", "ICD10010", "ICD10011", "ICD10012", "ICD10013", "ICD10014", "ICD10015")] # Subset variables
rm(mortality_2020) # Tidy

# Identify COVID-19 related records:
# 1. By underlying cause
mortality_small$covid_underlying <- 0 # Create blank variable for no for all
mortality_small$covid_underlying[mortality_small$ICD10U == "U071" | mortality_small$ICD10U == "U072"] <- 1 # Recode as 1 if COVID was underlying cause of death

# 2. By COVID-19 mentioned anywhere
mortality_small$covid_mentioned <- 0 # Create blank variable for no for all
mortality_small$covid_mentioned[mortality_small$ICD10001 == "U071" | mortality_small$ICD10002 == "U071" | mortality_small$ICD10003 == "U071" | mortality_small$ICD10004 == "U071" | mortality_small$ICD10005 == "U071" | mortality_small$ICD10006 == "U071" | mortality_small$ICD10007 == "U071" | mortality_small$ICD10008 == "U071" | mortality_small$ICD10009 == "U071" | mortality_small$ICD10010 == "U071" | mortality_small$ICD10011 == "U071" | mortality_small$ICD10012 == "U071" | mortality_small$ICD10012 == "U071" | mortality_small$ICD10013 == "U071" | mortality_small$ICD10014 == "U071" | mortality_small$ICD10015 == "U071" | mortality_small$ICD10001 == "U072" | mortality_small$ICD10002 == "U072" | mortality_small$ICD10003 == "U072" | mortality_small$ICD10004 == "U072" | mortality_small$ICD10005 == "U072" | mortality_small$ICD10006 == "U072" | mortality_small$ICD10007 == "U072" | mortality_small$ICD10008 == "U072" | mortality_small$ICD10009 == "U072" | mortality_small$ICD10010 == "U072" | mortality_small$ICD10011 == "U072" | mortality_small$ICD10012 == "U072" | mortality_small$ICD10012 == "U072" | mortality_small$ICD10013 == "U072" | mortality_small$ICD10014 == "U072" | mortality_small$ICD10015 == "U072"] <- 1 # Recode as 1 if COVID was mentioned at any point on the death certificate

# Join on geographical codes
pc_lkup <- read.csv("R:/Geography/Postcode directories and lookups/2020 AUGUST Postcode lookup/NSPL_AUG_2020_UK/Data/NSPL_AUG_2020_UK.csv") # Load national statistics postcode lookup file
lkup <- pc_lkup[c("pcd", "oa11", "lsoa11", "msoa11", "laua", "ctry", "ru11ind", "imd")] # Keep only variables intend to attach
mortality_geo <- merge(mortality_small, lkup, by.x = "PCDR", by.y = "pcd", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
rm(pc_lkup, mortality_small, lkup)

# Calculate IMD deciles (country specific and not comparable - apologies for not efficient code here)

# 1. Define English deciles
mortality_geo$imd_decile_e <- NA
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1 & mortality_geo$imd <= 3284] <- 1 # Most deprived decile
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 3285 & mortality_geo$imd <= 6569] <- 2
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 6570 & mortality_geo$imd <= 9853] <- 3
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 9854 & mortality_geo$imd <= 13138] <- 4
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 13139 & mortality_geo$imd <= 16422] <- 5
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 16423 & mortality_geo$imd <= 19706] <- 6
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 19707 & mortality_geo$imd <= 22991] <- 7
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 22992 & mortality_geo$imd <= 26275] <- 8
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 26276 & mortality_geo$imd <= 29560] <- 9
mortality_geo$imd_decile_e[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 29561 & mortality_geo$imd <= 32844] <- 10 # Least deprived decile

# 2. Define Welsh deciles
mortality_geo$imd_decile_w <- NA
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1 & mortality_geo$imd <= 191] <- 1 # Most deprived decile
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 192 & mortality_geo$imd <= 382] <- 2
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 383 & mortality_geo$imd <= 573] <- 3
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 574 & mortality_geo$imd <= 764] <- 4
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 765 & mortality_geo$imd <= 955] <- 5
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 956 & mortality_geo$imd <= 1145] <- 6
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1146 & mortality_geo$imd <= 1336] <- 7
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1337 & mortality_geo$imd <= 1527] <- 8
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1528 & mortality_geo$imd <= 1718] <- 9
mortality_geo$imd_decile_w[mortality_geo$ctry == "E92000001" & mortality_geo$imd >= 1719 & mortality_geo$imd <= 1909] <- 10 # Least deprived decile

# Save file
write.csv(mortality_geo, "./Data/mortality_2020.csv")
rm(mortality_geo)
gc()
