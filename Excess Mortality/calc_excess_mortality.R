#########################################
### Calcuate excess mortality for LAs ###
#########################################

# Purpose: To load all mortality records for 2015-2019, tidy the datasets, combine into a single analytical file and calculate the average number of deaths per month per deprivation quintile for each Local Authority. 

# Libraries
library(data.table)
library(foreign)

# Load all required data for 2015-2019 #

# 2019

# Mortality data
m2019 <- read.spss(file = "//vmlfps01v/DataStore$/Mortality/2010s/Mortality_2019.sav", to.data.frame = TRUE) # Load
m2019 <- m2019[c("SEX", "AGEINYRS", "ICD10U", "PCDR", "DODMT", "DODDY")] # Subset required variables
m2019$PCDR <- as.character(m2019$PCDR) # Change class to character for joining purposes
m2019$PCDR <- gsub(" ", "", m2019$PCDR, fixed=F) # Exclude whitespace for matching

# Load postcode lookup file and join on geographical zones to records
pc_lkup <- read.csv("R:/Geography/Postcode directories and lookups/2019_Nov NSPL_Postcode lookup/Data/NSPL_NOV_2019_UK.csv") # Load national statistics postcode lookup file
lkup <- pc_lkup[c("pcd", "oa11", "lsoa11", "msoa11", "laua")] # Keep only variables intend to attach
lkup$pcd <- as.character(lkup$pcd) # Change class to character for joining purposes
lkup$pcd <- gsub(" ", "", lkup$pcd, fixed=F)  # Exclude whitespace for matching
test <- merge(m2019, lkup, by.x = "PCDR", by.y = "pcd", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
rm(pc_lkup, lkup)


# 2018

# Mortality data
m2018 <- read.spss(file = "//vmlfps01v/DataStore$/Mortality/2010s/Mortality_2018.sav", to.data.frame = TRUE) # Load
m2018 <- m2018[c("SEX", "AGEINYRS", "ICD10U", "PCDR", "DODMT", "DODDY")] # Subset required variables
m2018$PCDR <- as.character(m2018$PCDR) # Change class to character for joining purposes
m2018$PCDR <- gsub(" ", "", m2018$PCDR, fixed=F) # Exclude whitespace for matching

# Load postcode lookup file and join on geographical zones to records
pc_lkup <- read.csv("P:/Working/Occupational inequalities/Data/2018 Postcode Lookup/NSPCL_NOV18_UK_LU/NSPCL_NOV18_UK_LU.csv") # Load national statistics postcode lookup file
lkup <- pc_lkup[c("pcd7", "oa11cd", "lsoa11cd", "msoa11cd", "ladcd")] # Keep only variables intend to attach
lkup$pcd7 <- as.character(lkup$pcd7) # Change class to character for joining purposes
lkup$pcd7 <- gsub(" ", "", lkup$pcd7, fixed=F)  # Exclude whitespace for matching
m2018 <- merge(m2018, lkup, by.x = "PCDR", by.y = "pcd7", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
#rm(pc_lkup, lkup)


# 2017

# Mortality data
m2017 <- read.spss(file = "//vmlfps01v/DataStore$/Mortality/2010s/Mortality_2017.sav", to.data.frame = TRUE) # Load
m2017 <- m2017[c("SEX", "AGEINYRS", "ICD10U", "PCDR", "DODMT", "DODDY")] # Subset required variables
m2017$PCDR <- as.character(m2017$PCDR) # Change class to character for joining purposes
m2017$PCDR <- gsub(" ", "", m2017$PCDR, fixed=F) # Exclude whitespace for matching

# Load postcode lookup file and join on geographical zones to records (join onto 2018 as has LAD code and same performance)
#pc_lkup <- read.csv("R:/Geography/Postcode directories and lookups/2017 NSPL Postcode lookup centroids/ONS_Postcode_Directory_Latest_Centroids/ONS_Postcode_Directory_Latest_Centroids.csv") # Load national statistics postcode lookup file
#lkup <- pc_lkup[c("pcd", "oa11", "lsoa11", "msoa11", "ladcd")] # Keep only variables intend to attach
m2017 <- merge(m2017, lkup, by.x = "PCDR", by.y = "pcd7", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
rm(pc_lkup, lkup)


# 2016

# Mortality data
m2016 <- read.spss(file = "//vmlfps01v/DataStore$/Mortality/2010s/Mortality_2016_corrected.sav", to.data.frame = TRUE) # Load
m2016 <- m2016[c("SEX", "AGEC", "AGECUNIT", "ICD10U", "PCDR", "DODMT", "DODDY")] # Subset required variables
m2016$AGEINYRS <- NA # Calculate age in years
m2016$AGEINYRS[m2016$AGECUNIT == 1] <- m2016$AGEC[m2016$AGECUNIT == 1] # If Age calculated in years, then take the number as it is
m2016$AGEINYRS[m2016$AGECUNIT > 1] <- 0 # If Age calculated in days, weeks or month, then is 0
m2016 <- m2016[c("SEX", "AGEINYRS", "ICD10U", "PCDR", "DODMT", "DODDY")] # Keep only variables required
m2016$PCDR <- as.character(m2016$PCDR) # Change class to character for joining purposes
m2016$PCDR <- gsub(" ", "", m2016$PCDR, fixed=F) # Exclude whitespace for matching

# Load postcode lookup file and join on geographical zones to records
# Note 2016 postcodes do not join here
pc_lkup <- read.csv("R:/Geography/Postcode directories and lookups/2016 NSPL Postcode lookup/National_Statistics_Postcode_Lookup_UK.csv") # Load national statistics postcode lookup file
lkup <- pc_lkup[c("Postcode.1", "Lower.Super.Output.Area.Code", "Middle.Super.Output.Area.Code", "Local.Authority.Code")] # Keep only variables intend to attach
lkup$Postcode.1 <- as.character(lkup$Postcode.1) # Change class to character for joining purposes
lkup$Postcode.1 <- gsub(" ", "", lkup$Postcode.1, fixed=F)  # Exclude whitespace for matching
m2016 <- merge(m2016, lkup, by.x = "PCDR", by.y = "Postcode.1", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
rm(pc_lkup, lkup)


# 2015

# Mortality data
m2015 <- read.spss(file = "//vmlfps01v/DataStore$/Mortality/2010s/mortality_2015.sav", to.data.frame = TRUE) # Load
m2015 <- m2015[c("SEX", "AGEC", "AGECUNIT", "ICD10U", "PCDR", "DODMT", "DODDY")] # Subset required variables
m2015$AGEINYRS <- NA # Calculate age in years
m2015$AGEINYRS[m2015$AGECUNIT == 1] <- m2015$AGEC[m2015$AGECUNIT == 1] # If Age calculated in years, then take the number as it is
m2015$AGEINYRS[m2015$AGECUNIT > 1] <- 0 # If Age calculated in days, weeks or month, then is 0
m2015 <- m2015[c("SEX", "AGEINYRS", "ICD10U", "PCDR", "DODMT", "DODDY")] # Keep only variables required
m2015$PCDR <- as.character(m2015$PCDR) # Change class to character for joining purposes
m2015$PCDR <- gsub(" ", "", m2015$PCDR, fixed=F) # Exclude whitespace for matching

# Load postcode lookup file and join on geographical zones to records
# Note 2015 postcodes do not join here
pc_lkup <- read.csv("P:/Working/Occupational inequalities/Data/2015 Postcode Lookup/2015_NSPL_NOV_csv/Data/NSPL_NOV_2015_UK.csv") # Load national statistics postcode lookup file
lkup <- pc_lkup[c("pcd", "lsoa11", "msoa11", "laua")] # Keep only variables intend to attach
lkup$pcd <- as.character(lkup$pcd) # Change class to character for joining purposes
lkup$pcd <- gsub(" ", "", lkup$pcd, fixed=F)  # Exclude whitespace for matching
m2015 <- merge(m2015, lkup, by.x = "PCDR", by.y = "pcd", all.x = TRUE) # Join adminsitrative boundaries onto mortality data by postcode
rm(pc_lkup, lkup)


# Join files together 

# Harmonize variable names to "lsoa11", "msoa11", "laua"
names(m2018)[names(m2018) == "lsoa11cd"] <- "lsoa11"
names(m2018)[names(m2018) == "msoa11cd"] <- "msoa11"
names(m2018)[names(m2018) == "ladcd"] <- "laua"
names(m2017)[names(m2017) == "lsoa11cd"] <- "lsoa11"
names(m2017)[names(m2017) == "msoa11cd"] <- "msoa11"
names(m2017)[names(m2017) == "ladcd"] <- "laua"
names(m2016)[names(m2016) == "Lower.Super.Output.Area.Code"] <- "lsoa11"
names(m2016)[names(m2016) == "Middle.Super.Output.Area.Code"] <- "msoa11"
names(m2016)[names(m2016) == "Local.Authority.Code"] <- "laua"
m2019$oa11 <- NULL # drop
m2018$oa11cd <- NULL # drop
m2017$oa11cd <- NULL # drop

# Join
mortality <- rbind(m2019, m2018, m2017, m2016, m2015)
rm(m2019, m2018, m2017, m2016, m2015) # Tidy

# Save file
write.csv(mortality, "./Data/mortality_2015_2019.csv")

# Join on geographical data
imd <- read.csv("./Data/imd2019.csv") # Load IMD
imd <- imd[c("lsoa11", "imd_rank", "imd_decile")] # Subset
imd$lsoa11 <- as.character(imd$lsoa11) # Set same class
mortality$lsoa11 <- as.character(mortality$lsoa11)
mortality_geo <- merge(mortality, imd, by = "lsoa11", all.x=T) # Join
rm(imd, mortality)

# Save file
write.csv(mortality_geo, "./Data/mortality_2015_2019.csv")

# Create quintiles for England
mortality_geo <- fread("./Data/mortality_2015_2019.csv") # Load
#mortality_geo <- data.table(mortality_geo) # If have not done above line
mortality_geo$imd_quintile_e <- NA # Calculate quintiles
mortality_geo$imd_quintile_e[mortality_geo$imd_decile == 1 | mortality_geo$imd_decile == 2] <- 1
mortality_geo$imd_quintile_e[mortality_geo$imd_decile == 3 | mortality_geo$imd_decile == 4] <- 2
mortality_geo$imd_quintile_e[mortality_geo$imd_decile == 5 | mortality_geo$imd_decile == 6] <- 3
mortality_geo$imd_quintile_e[mortality_geo$imd_decile == 7 | mortality_geo$imd_decile == 8] <- 4
mortality_geo$imd_quintile_e[mortality_geo$imd_decile == 9 | mortality_geo$imd_decile == 10] <- 5
mortality_geo$SEX <- as.integer(mortality_geo$SEX) # Convert to match main dataset (as messy)

# Calculate excess mortality denominators
month_count_LA <- mortality_geo[, list(total_deaths = .N), by = c("DODMT", "SEX", "imd_quintile_e", "laua")] # Aggregate counts to LA by IMD quintile by sex per month
month_count_LA$mean_deaths <- month_count_LA$total_deaths / 5 # Calculate average numbers of deaths per year
write.csv(month_count_LA, "./Data/total_deaths_15_19_LA_sex_month_imd.csv") # save

# Tidy
rm(mortality_geo, month_count_LA)
gc()
