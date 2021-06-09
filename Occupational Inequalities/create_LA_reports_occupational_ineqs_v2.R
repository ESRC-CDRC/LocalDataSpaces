###################################################
### Create LA Occupational Inequalities Reports ###
###################################################

# Purpose: Script automatically creates a report include data and tables for all Local Authorities

# Set libraries folder
lib_base="P:\\Working\\Libraries"
assign(".lib.loc", lib_base, envir = environment(.libPaths))
rm(lib_base)

# Libraries
# To analyse data
library(readstata13)
library(data.table)
library(ggplot2)
library(tidyr)

# Load data
full_data <- data.table(read.dta13("//vmlfps04v/DataStore2$/COVID-19 Infection Survey, Monthly/CIS_Extract_2021_02_03/Extract_20210203.dta"))

# Drop all April and Feb data for stability of estimates (as both only have a few days)
full_data <- full_data[full_data$visit_date >= "2020-05-01" & full_data$visit_date < "2021-02-01"]

# Calculate time variables for aggregating data
full_data$week <- week(full_data$visit_date) # Identify week from date (numeric)
full_data$month <- month(full_data$visit_date) # Identify month from date (numeric)

# Calculate all data for exporting

# Plot 1
days_count <- full_data[, list(count = .N), by = c("month", "geography_name", "geography_code")] # Aggregate counts per day
days_count <- days_count[days_count$count >= 10] # Supress low counts
write.csv(days_count, "./Outputs/CIS Occupational LA Reports/Data/plot1.csv")

# Plot 2
# Calculate summary stats by LA
days_count <- full_data[, list(count = .N), by = c("result_mk", "geography_name", "geography_code")] # Aggregate counts per day
days_count_wide <- spread(days_count, result_mk, count) # Convert to wide format
days_count_wide$Positive[is.na(days_count_wide$Positive)] <- 0 # Fill in 0s
days_count_wide$Negative[is.na(days_count_wide$Negative)] <- 0 # Fill in 0s
days_count_wide$total <- days_count_wide$Negative + days_count_wide$Positive # Calculate total
days_count_wide <- days_count_wide[days_count_wide$Positive >= 10] # Supress low counts
days_count_wide$pc_positive <- (days_count_wide$Positive / days_count_wide$total) * 100 # Calculate percentage
days_count_wide <- days_count_wide[,c("geography_name", "geography_code", "total", "Positive", "pc_positive")] # Keep vars
write.csv(days_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot2.csv")
# Get national average for all areas
table(full_data$result_mk)

# Plot 3
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_status", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_status),] # Drop missing data
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_status", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot3.csv")
# Get national average for all areas
table(full_data$work_status, full_data$result_mk)


# Plot 4
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_status_v1", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_status_v1),] # Drop missing data
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_status_v1", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot4.csv")
# Get national average for all areas
table(full_data$work_status_v1, full_data$result_mk)


# Plot 5
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_sector", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_sector),] # Drop missing data
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[month_count_wide$work_sector != "NA(Not currently working)",] # Drop not working
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_sector", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot5.csv")
# Get national average for all areas
table(full_data$work_sector, full_data$result_mk)

# Plot 6
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_location", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_location),] # Drop missing data
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[month_count_wide$work_location != "<=15y",] # Drop categories not needed
month_count_wide <- month_count_wide[month_count_wide$work_location != ">=75y",] # Drop categories not needed
month_count_wide <- month_count_wide[month_count_wide$work_location != "Not applicable, not currently working",] # Drop categories not needed
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_location", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot6.csv")
# Get national average for all areas
table(full_data$work_location, full_data$result_mk)

# Plot 7
full_data$work_social_distancing[full_data$work_social_distancing == "Relatively easy to maintain 2m"] <- "Easy to maintain 2m" # recode
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_social_distancing", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_social_distancing),] # Drop missing data
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[month_count_wide$work_social_distancing != "N/A (not working/in education etc)",] # Drop those not working
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_social_distancing", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot7.csv")
# Get national average for all areas
table(full_data$work_social_distancing, full_data$result_mk)


# Plot 9
month_count <- full_data[, list(count = .N), by = c("result_mk", "work_direct_contact_patients_etc", "geography_name", "geography_code")] # Aggregate
month_count_wide <- spread(month_count, result_mk, count) # Reshape
month_count_wide$Positive[is.na(month_count_wide$Positive)] <- 0 # Fill in 0s
month_count_wide$Negative[is.na(month_count_wide$Negative)] <- 0 # Fill in 0s
month_count_wide$total <- month_count_wide$Negative + month_count_wide$Positive # Calculate total
month_count_wide <- month_count_wide[month_count_wide$Positive >= 10] # Supress low counts
month_count_wide$pc_positive <- (month_count_wide$Positive / month_count_wide$total) * 100 # Calculate percentage
month_count_wide <- month_count_wide[!is.na(month_count_wide$work_direct_contact_patients_etc),] # Drop missing data
month_count_wide$pc_positive[is.na(month_count_wide$pc_positive)] <- 0 # For those not a number
month_count_wide <- month_count_wide[,c("geography_name", "geography_code", "work_direct_contact_patients_etc", "total", "Positive", "pc_positive")] # Keep vars
write.csv(month_count_wide, "./Outputs/CIS Occupational LA Reports/Data/plot8.csv")
# Get national average for all areas
table(full_data$work_direct_contact_patients_etc, full_data$result_mk)


