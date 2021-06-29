###################################################
### Create LA Demographic Inequalities Reports ###
###################################################

# Purpose: Script automatically creates a report including visualisations, for all Local Authorities on Demographic Inequalities in COVID 19

###################################################
# LOADING PACKAGE LIBRARIES

# Set libraries folder
#lib_base="P:\\Working\\Libraries\\"
#assign(".lib.loc", lib_base, envir = environment(.libPaths))
#rm(lib_base)
# Libraries
library(data.table)
library(rmarkdown)
library(markdown)
library(ggplot2)
library(knitr)
library(tidyr)
library(readxl)
library(foreign)
library(sf)
library(broom)
library(sp)
library(rgeos)
library(maptools)
library(scales)
library(viridis)
library(plotly)
library(dplyr)
library(extrafont)
#font_import()
#fonts()
loadfonts(device="win") # run each time R session starts
###################################################
### GENERATE DEMOGRAPHICS REPORTS
# Read in the non-disclosive datasets exported from ONS SRS, file path will be altered externally
plot1_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\plot1_x.csv")
plot2_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\plot2_x.csv")
plot3_4_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\plot3_4_x.csv")
plot5_6_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\plot5_6_x.csv")
plot7_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\plot7_x.csv")
pillar2_info <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\pillar2_info.csv")

# Format all date variables correctly
plot2_x$appointmentdate<-as.Date(plot2_x$appointmentdate, "%d/%m/%Y")
plot3_4_x$appointmentdate<-as.Date(plot3_4_x$appointmentdate, "%d/%m/%Y")
plot5_6_x$appointmentdate<-as.Date(plot5_6_x$appointmentdate, "%d/%m/%Y")
plot7_x$appointmentdate<-as.Date(plot7_x$appointmentdate, "%d/%m/%Y")
pillar2_info$min_date <-as.Date(pillar2_info$min_date, "%d/%m/%Y")
pillar2_info$max_date <-as.Date(pillar2_info$max_date,"%d/%m/%Y")

# Join on Region Name to Data
region <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Lookups\\Geographic_Lookup_X.csv")
region <- region[, c(6,8,9)]
# missing 6 LA Codes, so fill them in 
df <- data.frame(LAD_CD=c("E06000048", "E07000097", "E07000100", "E07000101", "E07000104", "E08000020"), RGN_NM=c("North East", "South East", "South East", "South East", "South East","North East"), RGN_CD=c("E12000001", "E12000008", "E12000008","E12000008","E12000008","E12000001"))
region<- rbind(region, df)
region <- unique(region)

# Join Region Names to all the non-disclosive data
plot1_x <- merge(plot1_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot2_x <- merge(plot2_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot3_4_x <- merge(plot3_4_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot5_6_x <- merge(plot5_6_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot7_x <- merge(plot7_x, region, by.x="LA_CODE", by.y="LAD_CD")
pillar2_info <- merge(pillar2_info, region, by.x="LA_CODE", by.y="LAD_CD")

# Add lookup from 2011 LA to 2019 LA to ensure Population Data can be added 

# National and Regional Averages 
pos_age_averages <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\pos_age_agg.csv")
pos_gender_averages <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\pos_gender_agg.csv")
pos_gender_age_averages <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\pos_gender_age_agg.csv")
pos_averages <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Demographic Inequalities\\pos_agg.csv")

# Set column names to match data
colnames(pos_gender_averages) <- c("appointmentdate", "Gender", "positive", "total", "rolling_pc_positive", "RGN11NM")
colnames(pos_age_averages) <- c("appointmentdate", "Age", "positive", "total", "rolling_pc_positive", "RGN11NM")
colnames(pos_gender_age_averages) <- c("appointmentdate", "Gender", "Age", "positive", "total", "rolling_pc_positive", "RGN11NM")
# Remove NA values 
pos_gender_averages <- pos_gender_averages[pos_gender_averages$Gender=="M" | pos_gender_averages$Gender=="F",]
pos_gender_age_averages <- pos_gender_age_averages[pos_gender_age_averages$Gender=="M" | pos_gender_age_averages$Gender=="F",]

# Read in Population Data for Population Pyramid 
pop_pyramid <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\Ethnic Group Population Pyramids.csv")

###############################################################
# Add week column and aggregate testing totals up to week for LA-level data
plot2_x$week <- cut(plot2_x$appointmentdate, "week")
plot3_4_x$week <- cut(plot3_4_x$appointmentdate, "week")
plot5_6_x$week <- cut(plot5_6_x$appointmentdate, "week")
plot7_x$week <- cut(plot7_x$appointmentdate, "week")

tests <- plot2_x %>% group_by(week, LA_CODE, RGN_NM, RGN_CD) %>% summarise("tests"=sum(total)) %>% ungroup()
tests_sex <- plot3_4_x %>% group_by(week, LA_CODE, RGN_NM, RGN_CD, Gender) %>% summarise("tests"=sum(total)) %>% ungroup()
tests_age <- plot5_6_x %>% group_by(week, LA_CODE, RGN_NM, RGN_CD, Age) %>% summarise("tests"=sum(total)) %>% ungroup()
tests_sex_age <- plot7_x %>% group_by(week, LA_CODE, RGN_NM, RGN_CD, Age, Gender) %>% summarise("tests"=sum(total)) %>% ungroup()

# Add week column and aggregate testing totals up to week for Regional and National Aggregates
pos_averages$week <- cut(pos_averages$appointmentdate, "week") 
pos_age_averages$week <- cut(pos_age_averages$appointmentdate, "week") 
pos_gender_averages$week <- cut(pos_gender_averages$appointmentdate, "week") 
pos_gender_age_averages$week <- cut(pos_gender_age_averages$appointmentdate, "week") 

tests_averages <- pos_averages %>% group_by(week,RGN11NM) %>% summarise("tests"= sum(total)) %>% ungroup() 
tests_sex_averages <- pos_gender_averages %>% group_by(week, RGN11NM, Gender) %>% summarise("tests"=sum(total)) %>% ungroup() 
tests_age_averages <- pos_age_averages %>% group_by(week, RGN11NM, Age) %>% summarise("tests"=sum(total)) %>% ungroup()
tests_sex_age_averages <- pos_gender_age_averages %>% group_by(week, RGN11NM, Age, Gender) %>% summarise("tests"=sum(total)) %>% ungroup()

###############################################################
# Add Population Data to calculate normalised testing uptake 

# Read in Male Population Data, Format into Age Bands at LSOA Level
population_male <- read_xlsx("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted2.xlsx", sheet=5, skip=4)
y <- colnames(population_male[,8:98]) # selecting all age columns 
population_male$Gender <- "M" # adding gender column

population_male_melt <- melt(population_male, id.vars=c("LSOA Code", "LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "Gender"), measure=c(y)) # melt data into many rows of Ages
population_male_melt$variable <- as.numeric(population_male_melt$variable)

labs<- c("0 to 4", "5 to 14", "15 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 to 74", "75 to 84", "85+")

population_male_melt$Age <- cut(population_male_melt$variable, breaks = c(0,4,14,24,34,44,54,64,74,84, Inf), labels=labs)
population_male_melt <- subset(population_male_melt, select=c("LSOA Code", "LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "Gender", "value", "Age"))

# Read in Female Population Data, Format into Age Bands at LSOA Level
population_female <- read_xlsx("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted2.xlsx", sheet=6, skip=4)
y <- colnames(population_female[,8:98]) # selecting all age columns 
population_female$Gender <- "F" # adding gender column

population_female_melt <- melt(population_female, id.vars=c("LSOA Code", "LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "Gender"), measure=c(y)) # melt data into many rows of Ages
population_female_melt$variable <- as.numeric(population_female_melt$variable)

population_female_melt$Age <- cut(population_female_melt$variable, breaks = c(0,4,14,24,34,44,54,64,74,84, Inf), labels=labs)
population_female_melt <- subset(population_female_melt, select=c("LSOA Code", "LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "Gender", "value", "Age"))

# Bind the two datasets together into cleaned population dataset 
population_all <- rbind(population_male_melt, population_female_melt)
# Add Region to the data 
population_all <- merge(population_all, region, by.x="LA Code (2019 boundaries)", by.y="LAD_CD")

# Group data into LA Populations 
population_all_LA <- population_all %>% group_by(`LA Code (2019 boundaries)`, `LA name (2019 boundaries)`) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_LA_age <- population_all %>% group_by(`LA Code (2019 boundaries)`, `LA name (2019 boundaries)`, `Age`) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_LA_gender <- population_all %>% group_by(`LA Code (2019 boundaries)`, `LA name (2019 boundaries)`, `Gender`) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_LA_gender_age <- population_all %>% group_by(`LA Code (2019 boundaries)`, `LA name (2019 boundaries)`, `Age`, `Gender`) %>% summarise("Population"=sum(value)) %>% ungroup()

# Group data into National and Regional Populations 
population_all_Reg <- population_all %>% group_by(RGN_CD, RGN_NM) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_Reg <- population_all_Reg %>% add_row("RGN_CD"="000", "RGN_NM"="National", "Population"=sum(population_all_Reg$Population))
 
population_all_Reg_age <- population_all %>% group_by(RGN_CD, RGN_NM, `Age`) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_Reg_gender <- population_all %>% group_by(RGN_CD, RGN_NM, `Gender`) %>% summarise("Population"=sum(value)) %>% ungroup()
population_all_Reg_gender_age <- population_all %>% group_by(RGN_CD, RGN_NM, `Age`, `Gender`) %>% summarise("Population"=sum(value)) %>% ungroup()
###########################################################
# Calculate Data Required for Testing Uptake 
# Join to National and regional averages and calculate per 100k testing uptake
tests_averages <- merge(tests_averages, population_all_Reg, by.x="RGN11NM", by.y="RGN_NM", all.x=TRUE)
tests_averages$total_100k <- tests_averages$tests/tests_averages$Population * 100000

# Calculate test uptake per 100k by Gender
tests_sex_averages <- merge(tests_sex_averages, population_all_Reg_gender, by.x=c("RGN11NM","Gender"), by.y=c("RGN_NM", "Gender"), all.x=TRUE)
# Create National level data by Sex
national_tests_sex <- tests_sex_averages %>% group_by(Gender) %>% summarise("Population"=sum(unique(Population), na.rm=TRUE)) %>% ungroup()
national_tests_sex$RGN11NM <- "National"
tests_sex_averages <- merge(tests_sex_averages, national_tests_sex, by.x=c("Gender", "RGN11NM"), by.y=c("Gender", "RGN11NM"), all.x=TRUE)
# If else to ensure all data within same column 
tests_sex_averages$Population.x <- ifelse(!is.na(tests_sex_averages$Population.x), tests_sex_averages$Population.x, tests_sex_averages$Population.y)
# Set column names and remove spare column 
tests_sex_averages <- tests_sex_averages[,1:6] # CHECK
colnames(tests_sex_averages) <- c("Gender", "RGN11NM", "week", "tests", "RGN_CD", "Population")
# Calculate per 100k values
tests_sex_averages$total_100k <- tests_sex_averages$tests/tests_sex_averages$Population * 100000

# Calculate test uptake per 100k by Age
tests_age_averages <- merge(tests_age_averages, population_all_Reg_age, by.x=c("RGN11NM","Age"), by.y=c("RGN_NM", "Age"), all.x=TRUE)
# Join Population by Age Group Nationally
national_tests_age <- tests_age_averages %>% group_by(Age) %>% summarise("Population"=sum(unique(Population), na.rm=TRUE)) %>% ungroup()
national_tests_age$RGN11NM<-"National"
tests_age_averages <- merge(tests_age_averages, national_tests_age, by.x=c("Age", "RGN11NM"), by.y=c("Age", "RGN11NM"), all.x=TRUE)
# If else to ensure all data within same column 
tests_age_averages$Population.x <- ifelse(!is.na(tests_age_averages$Population.x), tests_age_averages$Population.x, tests_age_averages$Population.y)
# Set column names and remove spare column 
tests_age_averages <- tests_age_averages[,1:6]
colnames(tests_age_averages) <- c("Age", "RGN11NM", "week", "tests", "RGN_CD", "Population")
# Calculate per 100k values
tests_age_averages$total_100k <- tests_age_averages$tests/tests_age_averages$Population * 100000


# Calculate test uptake per 100k by Sex and Age
tests_sex_age_averages <- merge(tests_sex_age_averages, population_all_Reg_gender_age, by.x=c("RGN11NM","Age", "Gender"), by.y=c("RGN_NM", "Age", "Gender"), all.x=TRUE)
national_tests_sex_age <- tests_sex_age_averages %>% group_by(Gender, Age) %>% summarise("Population"=sum(unique(Population), na.rm=TRUE)) %>% ungroup()
national_tests_sex_age$RGN11NM<-"National"
tests_sex_age_averages <- merge(tests_sex_age_averages, national_tests_sex_age, by.x=c("Age", "Gender", "RGN11NM"), by.y=c("Age","Gender", "RGN11NM"), all.x=TRUE)
# If else to ensure all data within same column 
tests_sex_age_averages$Population.x <- ifelse(!is.na(tests_sex_age_averages$Population.x), tests_sex_age_averages$Population.x, tests_sex_age_averages$Population.y)
# Set column names and remove spare column 
tests_sex_age_averages <- tests_sex_age_averages[,1:7]
colnames(tests_sex_age_averages) <- c("Age", "Gender","RGN11NM", "week", "tests", "RGN_CD", "Population")
# Calculate per 100k values
tests_sex_age_averages$total_100k <- tests_sex_age_averages$tests/tests_sex_age_averages$Population * 100000


# Join population data by LA and calculate per 100k values
tests <- merge(tests, population_all_LA, by.x="LA_CODE", by.y="LA Code (2019 boundaries)")
tests$total_100k <- tests$tests/tests$Population * 100000

tests_sex <- merge(tests_sex, population_all_LA_gender, by.x=c("LA_CODE","Gender"), by.y=c("LA Code (2019 boundaries)", "Gender"))
tests_sex$total_100k <- tests_sex$tests/tests_sex$Population * 100000

tests_age <- merge(tests_age, population_all_LA_age, by.x=c("LA_CODE", "Age"), by.y=c("LA Code (2019 boundaries)", "Age"))
tests_age$total_100k <- tests_age$tests/tests_age$Population * 100000

tests_sex_age <- merge(tests_sex_age, population_all_LA_gender_age, by.x=c("LA_CODE","Gender", "Age"), by.y=c("LA Code (2019 boundaries)","Gender", "Age"))
tests_sex_age$total_100k <- tests_sex_age$tests/tests_sex_age$Population * 100000

############################################################
# Positive Cases per 100k 
plot1_x<- merge(plot1_x, population_all_LA, by.x="LA_CODE", by.y="LA Code (2019 boundaries)")
plot1_x$Freq_100k <- plot1_x$Freq/plot1_x$Population * 100000
tests_average <- plot1_x %>% group_by("appointmentdate"=appointmentdate) %>% summarise("Tests"= mean(Freq, na.rm=TRUE)) %>% ungroup()

pos_cases_100k <- plot1_x %>% group_by("appointmentdate"=appointmentdate) %>% summarise("Freq_100k"=sum(Freq, na.rm=TRUE) / population_all_Reg$Population[10] * 100000) %>% ungroup()


pos_cases_reg_100k <- plot1_x %>% group_by("appointmentdate"=appointmentdate, "Region"=RGN_NM) %>% summarise("Tests"= sum(Freq, na.rm=TRUE)) %>% ungroup()
pos_cases_reg_100k <- merge(pos_cases_reg_100k, population_all_Reg, by.x="Region", by.y="RGN_NM")
pos_cases_reg_100k$Freq_100k <- pos_cases_reg_100k$Tests/pos_cases_reg_100k$Population * 100000

# Remove extra variables 
rm(df, population_all, population_all_LA, population_all_LA_age, population_all_LA_gender, population_all_LA_gender_age, population_all_Reg, population_all_Reg_age, population_all_Reg_gender, population_all_Reg_gender_age, population_female, population_female_melt, population_male, population_male_melt, y, labs, region)

# E0600028 and E06000029 and E06000048 and E07000048 and E07000097 and E07000100 and E07000101 and E07000104 and E07000190 and E07000191 and E07000201  and E07000204 and E07000205 and E07000206 and E08000020 fail to run? 

#which(pillar2_info$LA_CODE==i)
#pillar2_info <- pillar2_info[pillar2_info$LA_CODE!="E06000028" & pillar2_info$LA_CODE!="E06000029" & pillar2_info$LA_CODE!="E06000048" & pillar2_info$LA_CODE!="E07000048" & pillar2_info$LA_CODE!="E07000097" & pillar2_info$LA_CODE!="E07000100" & pillar2_info$LA_CODE!="E07000101"  & pillar2_info$LA_CODE!="E07000104" & pillar2_info$LA_CODE!="E07000190" & pillar2_info$LA_CODE!="E07000191"  & pillar2_info$LA_CODE!="E07000201" & pillar2_info$LA_CODE!="E07000204" & pillar2_info$LA_CODE!="E07000205" & pillar2_info$LA_CODE!="E07000206" & pillar2_info$LA_CODE!="E08000020",]
#pillar2_info <- pillar2_info[26:nrow(pillar2_info),]

# Read in missing reports file and re-run just these 
missing <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\missing_reports.csv")
# Subset to only ones with demographic as false (not yet available) 
missing <- missing[missing$Demographic==FALSE,]
pillar2_info <- pillar2_info[pillar2_info$LA_CODE %in% missing$LA_CODE]

############################################################
for (i in unique(pillar2_info$LA_CODE)) { # run for every LA CODE in the dataset
  print(i)
  pillar2_info_sub <- pillar2_info[pillar2_info$LA_CODE==i,] #subset context info
  # Some have been failing on data_1 creation so added this code to check 
  data_1 <- plot1_x[plot1_x$LA_CODE==i,] # Subset aggregated plotting data
  if (nrow(data_1)!=0) {
  data_2 <- plot2_x[plot2_x$LA_CODE==i,] # Subset aggregated plotting data
  data_3 <- plot3_4_x[plot3_4_x$LA_CODE==i,] # Subset aggregated plotting data
  data_4 <- plot5_6_x[plot5_6_x$LA_CODE==i,] # Subset aggregated plotting data
  data_5 <- plot7_x[plot7_x$LA_CODE==i,] # Subset aggregated plotting data
  tests_sub <- tests[tests$LA_CODE==i, ] # Subset aggregated plotting data
  tests_age_sub <- tests_age[tests_age$LA_CODE==i, ] # Subset aggregated plotting data
  tests_sex_sub <- tests_sex[tests_sex$LA_CODE==i, ] # Subset aggregated plotting data
  tests_sex_age_sub <- tests_sex_age[tests_sex_age$LA_CODE==i, ] # Subset aggregated plotting data
  pop_pyramid_sub <- pop_pyramid[pop_pyramid$LA_CODE==i,] # Subset aggregated plotting data
  rmarkdown::render("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\Demographic Inequalities\\Demographic Inequalities\\Demographic_Template.Rmd", html_document(toc=TRUE), 
                    output_file = paste0(i,"_","Demographic_Inequalities_COVID_19",".html"), # Save the report as this filename
                    output_dir = paste0("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Outputs\\", gsub(" ", "_", pillar2_info_sub$RGN_NM), "\\LAD\\", i, "\\"))
  } 
}
###################################################
