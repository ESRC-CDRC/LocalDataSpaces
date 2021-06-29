###################################################
### Create LA Demographic Inequalities Reports ###
###################################################

# Purpose: Script automatically creates a report including visualisations, maps and tables for Norfolk using 7 LAD data within Norfolk Area

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

plot1_x <- merge(plot1_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot2_x <- merge(plot2_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot3_4_x <- merge(plot3_4_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot5_6_x <- merge(plot5_6_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot7_x <- merge(plot7_x, region, by.x="LA_CODE", by.y="LAD_CD")
pillar2_info <- merge(pillar2_info, region, by.x="LA_CODE", by.y="LAD_CD")

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

tests_sex_averages <- merge(tests_sex_averages, population_all_Reg_gender, by.x=c("RGN11NM","Gender"), by.y=c("RGN_NM", "Gender"), all.x=TRUE)
tests_sex_averages$Population[tests_sex_averages$RGN11NM=="National"]<-sum(unique(tests_sex_averages$Population), na.rm=TRUE)
tests_sex_averages$total_100k <- tests_sex_averages$tests/tests_sex_averages$Population * 100000

tests_age_averages <- merge(tests_age_averages, population_all_Reg_age, by.x=c("RGN11NM","Age"), by.y=c("RGN_NM", "Age"), all.x=TRUE)
tests_age_averages$Population[tests_age_averages$RGN11NM=="National"]<-sum(unique(tests_age_averages$Population), na.rm=TRUE)
tests_age_averages$total_100k <- tests_age_averages$tests/tests_age_averages$Population * 100000

tests_sex_age_averages <- merge(tests_sex_age_averages, population_all_Reg_gender_age, by.x=c("RGN11NM","Age", "Gender"), by.y=c("RGN_NM", "Age", "Gender"), all.x=TRUE)
tests_sex_age_averages$Population[tests_sex_age_averages$RGN11NM=="National"]<-sum(unique(tests_sex_age_averages$Population), na.rm=TRUE)
tests_sex_age_averages$total_100k <- tests_sex_age_averages$tests/tests_sex_age_averages$Population * 100000


# Join population data by LA and calculate per 100k testing uptake
tests <- merge(tests, population_all_LA, by.x="LA_CODE", by.y="LA Code (2019 boundaries)")
tests$total_100k <- tests$tests/tests$Population * 100000

tests_sex <- merge(tests_sex, population_all_LA_gender, by.x=c("LA_CODE","Gender"), by.y=c("LA Code (2019 boundaries)", "Gender"))
tests_sex$total_100k <- tests_sex$tests/tests_sex$Population * 100000

tests_age <- merge(tests_age, population_all_LA_age, by.x=c("LA_CODE", "Age"), by.y=c("LA Code (2019 boundaries)", "Age"))
tests_age$total_100k <- tests_age$tests/tests_age$Population * 100000

tests_sex_age <- merge(tests_sex_age, population_all_LA_gender_age, by.x=c("LA_CODE","Gender", "Age"), by.y=c("LA Code (2019 boundaries)","Gender", "Age"))
tests_sex_age$total_100k <- tests_sex_age$tests/tests_sex_age$Population * 100000

# Remove extra variables 
rm(df, population_all, population_all_LA, population_all_LA_age, population_all_LA_gender, population_all_LA_gender_age, population_all_Reg, population_all_Reg_age, population_all_Reg_gender, population_all_Reg_gender_age, population_female, population_female_melt, population_male, population_male_melt, y, labs, region)

# 
# # Calculate Data Required for Testing Uptake 
# 
# # Join to National and regional averages and calculate per 100k testing uptake
# pos_averages <- merge(pos_averages, population_all_Reg, by.x="RGN11NM", by.y="RGN_NM", all.x=TRUE)
# pos_averages$total_100k <- pos_averages$total/pos_averages$Population * 100000
# 
# pos_gender_averages <- merge(pos_gender_averages, population_all_Reg_gender, by.x=c("RGN11NM","Gender"), by.y=c("RGN_NM", "Gender"), all.x=TRUE)
# pos_gender_averages$Population[pos_gender_averages$RGN11NM=="National"]<-sum(unique(pos_gender_averages$Population), na.rm=TRUE)
# pos_gender_averages$total_100k <- pos_gender_averages$total/pos_gender_averages$Population * 100000
# 
# pos_age_averages <- merge(pos_age_averages, population_all_Reg_age, by.x=c("RGN11NM","Age"), by.y=c("RGN_NM", "Age"), all.x=TRUE)
# pos_age_averages$Population[pos_age_averages$RGN11NM=="National"]<-sum(unique(pos_age_averages$Population), na.rm=TRUE)
# pos_age_averages$total_100k <- pos_age_averages$total/pos_age_averages$Population * 100000
# 
# pos_gender_age_averages <- merge(pos_gender_age_averages, population_all_Reg_gender_age, by.x=c("RGN11NM","Age", "Gender"), by.y=c("RGN_NM", "Age", "Gender"), all.x=TRUE)
# pos_gender_age_averages$Population[pos_gender_age_averages$RGN11NM=="National"]<-sum(unique(pos_gender_age_averages$Population), na.rm=TRUE)
# pos_gender_age_averages$total_100k <- pos_gender_age_averages$total/pos_gender_age_averages$Population * 100000
# 
# 
# # Join population data by LA and calculate per 100k testing uptake
# plot2_x <- merge(plot2_x, population_all_LA, by.x="LA_CODE", by.y="LA Code (2019 boundaries)")
# plot2_x$total_100k <- plot2_x$total/plot2_x$Population * 100000
# 
# plot3_4_x <- merge(plot3_4_x, population_all_LA_gender, by.x=c("LA_CODE","Gender"), by.y=c("LA Code (2019 boundaries)", "Gender"))
# plot3_4_x$total_100k <- plot3_4_x$total/plot3_4_x$Population * 100000
# 
# plot5_6_x <- merge(plot5_6_x, population_all_LA_age, by.x=c("LA_CODE", "Age"), by.y=c("LA Code (2019 boundaries)", "Age"))
# plot5_6_x$total_100k <- plot5_6_x$total/plot5_6_x$Population * 100000
# 
# plot7_x <- merge(plot7_x, population_all_LA_gender_age, by.x=c("LA_CODE","Gender", "Age"), by.y=c("LA Code (2019 boundaries)","Gender", "Age"))
# plot7_x$total_100k <- plot7_x$total/plot7_x$Population * 100000
# 
# # Remove extra variables 
# rm(df, population_all, population_all_LA, population_all_LA_age, population_all_LA_gender, population_all_LA_gender_age, population_all_Reg, population_all_Reg_age, population_all_Reg_gender, population_all_Reg_gender_age, population_female, population_female_melt, population_male, population_male_melt, y, labs, region)

############################################################
# Positive Cases Averages
tests_average <- plot1_x %>% group_by("appointmentdate"=appointmentdate) %>% summarise("Tests"= mean(Freq, na.rm=TRUE)) %>% ungroup()
tests_reg_average <- plot1_x %>% group_by("appointmentdate"=appointmentdate, "Region"=RGN_NM) %>% summarise("Tests"= mean(Freq, na.rm=TRUE)) %>% ungroup()

############################################################
##### Bespoke Analysis #####
# Aggregate data to Norfolk level for Equality Impact Assessment 

# To create reports at higher level- ie County, change the line below to the Local Authorities within the Area of Study
pillar2_info_sub <- pillar2_info[pillar2_info$LA_NAME=="Breckland" | pillar2_info$LA_NAME=="South Norfolk" | pillar2_info$LA_NAME=="Norwich" | pillar2_info$LA_NAME=="Great Yarmouth" | pillar2_info$LA_NAME=="Broadland" | pillar2_info$LA_NAME=="North Norfolk" | pillar2_info$LA_NAME=="King's Lynn and West Norfolk", ]


data_1 <- plot1_x[plot1_x$LA_CODE %in% pillar2_info$LA_CODE,]
data_2 <- plot2_x[plot2_x$LA_CODE %in% pillar2_info$LA_CODE,]
data_3 <- plot3_4_x[plot3_4_x$LA_CODE %in% pillar2_info$LA_CODE,]
data_4 <- plot5_6_x[plot5_6_x$LA_CODE %in% pillar2_info$LA_CODE,]
data_5 <- plot7_x[plot7_x$LA_CODE %in% pillar2_info$LA_CODE, ]
tests_sub <- tests[tests$LA_CODE %in% tests$LA_CODE, ]
tests_age_sub <- tests_age[tests_age$LA_CODE %in% pillar2_info$LA_CODE,]
tests_sex_sub <- tests_sex[tests_sex$LA_CODE %in% pillar2_info$LA_CODE,]
tests_sex_age_sub <- tests_sex_age[tests_sex_age$LA_CODE %in% pillar2_info$LA_CODE, ]

# Set LA_CODE and LA_NAME to all codes with semi-colon between 
pillar2_info_sub$LA_CODE <- paste(unique(pillar2_info_sub$LA_CODE), collapse=";")
pillar2_info_sub$LA_NAME <- paste(unique(pillar2_info_sub$LA_NAME), collapse=";")
i <- pillar2_info_sub$LA_CODE

# Aggregate data, and create mean values
data_1 <- data_1 %>% group_by(appointmentdate) %>% summarise("Freq"=mean(Freq,na.rm=TRUE)) %>% ungroup()
data_2 <- data_2 %>% group_by(appointmentdate) %>% summarise("rolling_pc_positive"=mean(rolling_pc_positive, na.rm=TRUE), "total"=sum(total)) %>% ungroup()
data_3 <- data_3 %>% group_by(appointmentdate, Gender) %>% summarise("rolling_pc_positive"=mean(rolling_pc_positive,na.rm=TRUE), "total"=sum(total)) %>% ungroup()
data_4 <- data_4 %>% group_by(appointmentdate, Age) %>% summarise("rolling_pc_positive"=mean(rolling_pc_positive,na.rm=TRUE), "total"=sum(total)) %>% ungroup()
data_5 <- data_5 %>% group_by(appointmentdate, Gender, Age) %>% summarise("rolling_pc_positive"=mean(rolling_pc_positive,na.rm=TRUE),"total"=sum(total)) %>% ungroup()
tests_sub <- tests_sub %>% group_by(week) %>% summarise("total_100k"=mean(total_100k, na.rm=TRUE))
tests_sex_sub <- tests_sex_sub %>% group_by(week,Gender) %>% summarise("total_100k"=mean(total_100k, na.rm=TRUE)) 
tests_age_sub<- tests_age_sub %>% group_by(week, Age) %>% summarise("total_100k"=mean(total_100k, na.rm=TRUE)) 
tests_sex_age_sub <- tests_sex_age_sub %>% group_by(week, Age, Gender) %>% summarise("total_100k"=mean(total_100k, na.rm=TRUE))

############################################################
  rmarkdown::render("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\Demographic Inequalities\\Demographic Inequalities\\Demographic_Template_Norfolk.Rmd", html_document(toc=TRUE), 
                    #output_file = paste0(i,"_","Demographic_Inequalities_COVID_19",".html"), # Save the report as this filename
                    output_file = paste0("Norfolk_Demographic_Inequalities_COVID_19",".html"), # Save the report as this filename
                    output_dir = paste0("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Outputs\\"))
  #output_dir = paste0("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Outputs\\", gsub(" ", "_", pillar2_info_sub$RGN_NM), "\\LAD\\", i, "\\"))
###################################################
