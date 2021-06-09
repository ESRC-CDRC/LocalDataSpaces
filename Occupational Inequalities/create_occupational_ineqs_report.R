###############################################
### Create Occupational Inequalities Report ###
###############################################

# Purpose: Code will create all occupational inequalities reports.

# Libraries
library(rmarkdown)
library(ggplot2)
library(viridis)
library(readxl)

# Load CIS data for plots and tidy where needed 
plot1 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot1.csv") # Load in file

plot2 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot2.csv")
plot2$se <- sqrt(((plot2$pc_positive * (100 - plot2$pc_positive)) / plot2$total)) # Create 95% Confidence Intervals
plot2$lwr <- plot2$pc_positive - (plot2$se * 1.96) # Lower bound
plot2$upr <- plot2$pc_positive + (plot2$se * 1.96) # Upper bound

plot3 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot3.csv")
plot3$se <- sqrt(((plot3$pc_positive * (100 - plot3$pc_positive)) / plot3$total))
plot3$lwr <- plot3$pc_positive - (plot3$se * 1.96)
plot3$upr <- plot3$pc_positive + (plot3$se * 1.96)
levels(plot3$work_status)[levels(plot3$work_status) == "Not working (unemployed, retired, long-term sick etc.)"] <- "Not working" # Rename to make plotting clearer
levels(plot3$work_status)[levels(plot3$work_status) == "Furloughed (temporarily not working)"] <- "Furloughed"

plot4 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot4.csv")
plot4$se <- sqrt(((plot4$pc_positive * (100 - plot4$pc_positive)) / plot4$total))
plot4$lwr <- plot4$pc_positive - (plot4$se * 1.96)
plot4$upr <- plot4$pc_positive + (plot4$se * 1.96)
levels(plot4$work_status_v1)[levels(plot4$work_status_v1) == "5y and older in full-time education"] <- "5y and older in full time education" # Rename to make plotting clearer

plot5 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot5.csv")
plot5$se <- sqrt(((plot5$pc_positive * (100 - plot5$pc_positive)) / plot5$total))
plot5$lwr <- plot5$pc_positive - (plot5$se * 1.96)
plot5$upr <- plot5$pc_positive + (plot5$se * 1.96)
levels(plot5$work_sector)[levels(plot5$work_sector) == "Arts,Entertainment or Recreation"] <- "Arts" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Civil service or Local Government"] <- "Civil service"
levels(plot5$work_sector)[levels(plot5$work_sector) == "Financial services incl. insurance"] <- "Financial services" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Food production, agriculture, farming"] <- "Food production" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Hospitality (e.g. hotel, restaurant)"] <- "Hospitality" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Information technology and communication"] <- "ICT" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Manufacturing or construction"] <- "Manufacturing" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Other occupation sector"] <- "Other"
levels(plot5$work_sector)[levels(plot5$work_sector) == "Personal services (e.g. hairdressers)"] <- "Personal services" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Retail sector (incl. wholesale)"] <- "Retail sector" 
levels(plot5$work_sector)[levels(plot5$work_sector) == "Transport (incl. storage, logistic)"] <- "Transport" 

plot6 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot6.csv")
plot6$se <- sqrt(((plot6$pc_positive * (100 - plot6$pc_positive)) / plot6$total))
plot6$lwr <- plot6$pc_positive - (plot6$se * 1.96)
plot6$upr <- plot6$pc_positive + (plot6$se * 1.96)
levels(plot6$work_location)[levels(plot6$work_location) == "Working somewhere else (not your home)"] <- "Working somewhere else"
levels(plot6$work_location)[levels(plot6$work_location) == "Both (from home and somewhere else)"] <- "Both"

plot7 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot7.csv")
plot7$se <- sqrt(((plot7$pc_positive * (100 - plot7$pc_positive)) / plot7$total))
plot7$lwr <- plot7$pc_positive - (plot7$se * 1.96)
plot7$upr <- plot7$pc_positive + (plot7$se * 1.96)
levels(plot7$work_social_distancing)[levels(plot7$work_social_distancing) == "Very difficult to be more than 1m away"] <- "Difficult to be more than 1m away" 

plot8 <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/Data/plot8.csv")
plot8$se <- sqrt(((plot8$pc_positive * (100 - plot8$pc_positive)) / plot8$total))
plot8$lwr <- plot8$pc_positive - (plot8$se * 1.96)
plot8$upr <- plot8$pc_positive + (plot8$se * 1.96)

# Load in England averages for benchmarks
plot3_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot3")
plot3_eng$se <- sqrt(((plot3_eng$pc_positive * (100 - plot3_eng$pc_positive)) / plot3_eng$total))
plot3_eng$lwr <- plot3_eng$pc_positive - (plot3_eng$se * 1.96)
plot3_eng$upr <- plot3_eng$pc_positive + (plot3_eng$se * 1.96)

plot4_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot4")
plot4_eng$se <- sqrt(((plot4_eng$pc_positive * (100 - plot4_eng$pc_positive)) / plot4_eng$total))
plot4_eng$lwr <- plot4_eng$pc_positive - (plot4_eng$se * 1.96)
plot4_eng$upr <- plot4_eng$pc_positive + (plot4_eng$se * 1.96)

plot5_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot5")
plot5_eng$se <- sqrt(((plot5_eng$pc_positive * (100 - plot5_eng$pc_positive)) / plot5_eng$total))
plot5_eng$lwr <- plot5_eng$pc_positive - (plot5_eng$se * 1.96)
plot5_eng$upr <- plot5_eng$pc_positive + (plot5_eng$se * 1.96)

plot6_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot6")
plot6_eng$se <- sqrt(((plot6_eng$pc_positive * (100 - plot6_eng$pc_positive)) / plot6_eng$total))
plot6_eng$lwr <- plot6_eng$pc_positive - (plot6_eng$se * 1.96)
plot6_eng$upr <- plot6_eng$pc_positive + (plot6_eng$se * 1.96)
levels(plot6_eng$work_location)[levels(plot6_eng$work_location) == "Work from home"] <- "Working from home"

plot7_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot7")
plot7_eng$se <- sqrt(((plot7_eng$pc_positive * (100 - plot7_eng$pc_positive)) / plot7_eng$total))
plot7_eng$lwr <- plot7_eng$pc_positive - (plot7_eng$se * 1.96)
plot7_eng$upr <- plot7_eng$pc_positive + (plot7_eng$se * 1.96)
plot7_eng$work_social_distancing <- as.factor(plot7_eng$work_social_distancing)
levels(plot7_eng$work_social_distancing)[levels(plot7_eng$work_social_distancing) == "Difficult to maintain 2m, but can 1m"] <- "Difficult to maintain 2m, but can be 1m" 

plot8_eng <- read_excel("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Occupational Inequalities/england_averages.xlsx", sheet = "plot8")
plot8_eng$se <- sqrt(((plot8_eng$pc_positive * (100 - plot8_eng$pc_positive)) / plot8_eng$total))
plot8_eng$lwr <- plot8_eng$pc_positive - (plot8_eng$se * 1.96)
plot8_eng$upr <- plot8_eng$pc_positive + (plot8_eng$se * 1.96)


## For each Local Authority, create a report ##

# Get structure for saving files
lkup <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Boundaries/Local_Authority_District__May_2020__to_Covid_Infection_Survey__October_2020__Lookup_in_England.csv") # Load LAD to CIS lookup
gor_lkup <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Boundaries/Local_Authority_District_to_Region__December_2020__Lookup_in_England.csv") # Load LAD to GOR lookup
gor_lkup$LAD20NM <- NULL # Drop as duplicated variable
gor_lkup$FID <- NULL
lkup <- merge(lkup, gor_lkup, by = "LAD20CD", all.x = T) # Join together
rm(gor_lkup)

# Fudge to print logos in report
wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])),
             "/OneDrive - The University of Liverpool/Local Profiles")
# wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])), 
#   "/Dropbox/Research/ADRUK Covid/Local Profiles")
wd.data <- paste0(wd, "/Data")

# Run code and save on OneDrive folder
for (j in unique(lkup$LAD20CD)) {
  
  # Boring bits
  print(j) # To track progress
  i <- as.character(lkup$CIS20CD[lkup$LAD20CD == j]) # Define matching CIS geography
  name <- unique(as.character(plot1$geography_name[plot1$geography_code == i])) # Store name
  region <- lkup$RGN20NM[lkup$LAD20CD == j] # Store region
  
  # Create following markdown file
  rmarkdown::render("~/OneDrive - The University of Liverpool/Local Profiles/Code/Occupational Inequalities/cis_occupational_la_template.Rmd", # Render this file
                    output_file = paste(j, "_occupational_inequalities.html", sep = ""), # Define filename
                    output_dir = paste0("~/OneDrive - The University of Liverpool/Local Profiles/Outputs/", gsub(" ", "_", region), "/LAD/", j, "/")) # Save to
  
}

# Tidy
rm(list=ls())
gc()