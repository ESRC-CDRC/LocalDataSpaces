###################################################
### Create Excess Mortality Inequalities Report ###
###################################################

# Purpose: Code will create all excess mortality reports.

# Libraries
library(data.table)
library(rmarkdown)
library(ggplot2)
library(viridis)


## Load data and tidy where needed ##

# Load data
eng_plots <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Excess Mortality/england_plots.csv")
la_plots <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Covid/Excess Mortality/la_plots.csv")

# Join on GOR region
gor_lkup <- read.csv("~/OneDrive - The University of Liverpool/Local Profiles/Data/Boundaries/Local_Authority_District_to_Region__December_2020__Lookup_in_England.csv") # Load
gor_lkup$LAD20NM <- NULL # Drop as duplicated variable
gor_lkup$FID <- NULL
la_plots <- merge(la_plots, gor_lkup, by.x = "laua", by.y = "LAD20CD", all.x = T) # Join together
rm(gor_lkup)

# Calculate ratios
eng_plots$ratio_eng <- eng_plots$total_deaths_2020 / (eng_plots$total_deaths_15_19 / 5)
la_plots$ratio_local <- la_plots$total_deaths_2020 / (la_plots$total_deaths_15_19 / 5)

# Convert to percentage
eng_plots$pc_excess <- eng_plots$pc_excess * 100
la_plots$pc_excess <- la_plots$pc_excess * 100

# Tidy values
la_plots$excess_vs_eng <- as.numeric(as.character(la_plots$excess_vs_eng))

# Calculate local difference to national benchmark
lkup <- eng_plots[c("SEX", "DODMT", "imd_quintile_e", "ratio_eng")] # Subset what we need
la_plots <- merge(la_plots, lkup, by = c("SEX", "DODMT", "imd_quintile_e"), all.x = T)
la_plots$ratio_ratio <- la_plots$ratio_local / la_plots$ratio_eng

# Tidy deprivation quintiles
la_plots$imd_quintile_e[la_plots$imd_quintile_e == 1] <- "Most Deprived"
la_plots$imd_quintile_e[la_plots$imd_quintile_e == 2] <- "Quintile 2"
la_plots$imd_quintile_e[la_plots$imd_quintile_e == 3] <- "Quintile 3"
la_plots$imd_quintile_e[la_plots$imd_quintile_e == 4] <- "Quintile 4"
la_plots$imd_quintile_e[la_plots$imd_quintile_e == 5] <- "Least Deprived"
la_plots$imd_quintile_e <- factor(la_plots$imd_quintile_e, levels = c("Most Deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Least Deprived")) # reorder as factor

## Create national level plots #

# Define sex for labels
sex.labs <- c("Males", "Females")
names(sex.labs) <- c(1, 2)

# Figure 1
fig1 <- ggplot(eng_plots, aes(x = DODMT, y = excess_deaths, group = factor(imd_quintile_e), color = factor(imd_quintile_e))) + # Plot excess deaths per month
  geom_line() + # Plot values as line plot
  facet_wrap(~SEX, labeller = labeller(SEX = sex.labs)) + # Stratify plot by sex
  xlab("Month") + # Edit labels
  ylab("Number of excess deaths") +
  ggtitle("Figure 1: Excess deaths in 2020 by deprivation quintile and sex") +
  ylim(min(eng_plots$excess_deaths), max(eng_plots$excess_deaths)) + # Set y axis bounds
  scale_color_viridis_d(name = "Deprivation Quintile", labels = c("Most Deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Least Deprived")) + # Edit legend labels
  scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c("Jan", "Mar", "May", "July", "Sept", "Nov")) # Revise month from number to label

# Figure 2
fig2 <- ggplot(eng_plots, aes(x = DODMT, y = ratio_eng, group = factor(imd_quintile_e), color = factor(imd_quintile_e))) + # Plot excess deaths per month
  geom_line() + # Plot values as line plot
  facet_wrap(~SEX, labeller = labeller(SEX = sex.labs)) + # Stratify plot by sex
  xlab("Month") + # Edit labels
  ylab("Ratio of deaths in 2020 vs mean deaths 2015-2019") +
  ggtitle("Figure 2: Relative change in deaths in 2020 (vs expected)") +
  scale_color_viridis_d(name = "Deprivation Quintile", labels = c("Most Deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Least Deprived")) + # Edit legend labels
  scale_x_continuous(breaks = c(1,3,5,7,9,11), labels = c("Jan", "Mar", "May", "July", "Sept", "Nov")) + # Revise month from number to label
  scale_y_continuous(breaks = c(0.8,1,1.2,1.4,1.6,1.8,2.0,2.2)) # Set y axis bounds

# Fudge to print logos in report
wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])),
             "/OneDrive - The University of Liverpool/Local Profiles")
# wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])), 
#   "/Dropbox/Research/ADRUK Covid/Local Profiles")
wd.data <- paste0(wd, "/Data")

## For each Local Authority, create a report ##

# Run code and save on OneDrive folder
for (i in unique(la_plots$laua)) {
  
  # Boring bits
  print(i) # To track progress
  names <- la_plots$LAD20NM[la_plots$laua == i] # To define area name
  for_plotting <- la_plots[la_plots$laua == i,] # Subset Local Authority
  region <- for_plotting$RGN20NM[1] # Store region
  
  # Create following markdown file
  rmarkdown::render("~/OneDrive - The University of Liverpool/Local Profiles/Code/Excess Mortality/excess_mortality_reports.Rmd", # Render this file
                    output_file = paste(i, "_excess_mortality.html", sep = ""), # Define filename
                    output_dir = paste0("~/OneDrive - The University of Liverpool/Local Profiles/Outputs/", gsub(" ", "_", region), "/LAD/", i, "/")) # Save to
  
}

# Tidy
rm(list=ls())
gc()

