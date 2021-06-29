###################################################
### Create Ethnicity LA Reports ###
###################################################

# Purpose: Script automatically creates a report including visualisations, maps and tables for all Local Authorities on Ethnic Inequalities in COVID 19

###################################################
# LOADING PACKAGE LIBRARIES
# Set working directory 
setwd("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\")
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
library(dplyr)
#install.packages("viridis")
library(viridis)
library(stringi)
#font_import()
#fonts()
loadfonts(device="win") # run each time R session starts
###################################################
# LOAD IN TIER RESTRICTIONS AND CENSUS ETHNICITY DATA
# Read in labels for vertical lines 
tiers <- data.frame("Label"=c("National Lockdown","Rule of Six Outdoors","Tiers Introduced", "Hospitality Reopened","         Rule of Six Outdoors and Indoors","National Lockdown","Tiers Introduced", "Tier 4 Introduced", "National Lockdown", "Rule of Six Outdoors", "Outdoor Hospitality and Retail Reopened"),
                    "Date"=c("2020/03/23","2020/06/01","2020/07/04","2020/09/14","2020/10/14","2020/11/01","2020/12/02","2020/12/19", "2021/01/06", "2021/03/29", "2021/04/12"))
tiers$Date <- as.Date(tiers$Date)

###################################################
# ETHNICITY UPTAKE OF TESTING
# Using 2019 Ethnicity Estimates by LA- https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/008781populationdenominatorsbybroadethnicgroupandforwhitebritishlocalauthoritiesinenglandandwales2011to2017

# Read in population Census Data at LA level
pop_2019 <- read_xlsx("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\2019 Ethnicity Breakdowns by LA.xlsx")
# Set column names
colnames(pop_2019) <- c("LA_CODE", "LA_NAME", "Age", "Sex", "White", "of_which_White_British", "Mixed/Multiple Ethnic Group", "Asian/Asian British", "Black/African/Caribbean/Black British", "Other Ethnic Group")

# Remove of_which_White_British
pop_2019 <- subset(pop_2019, select=-c(of_which_White_British))
pop_2019 <- as.data.table(pop_2019)

# melt table
pop_2019_melt <- melt(pop_2019, id.vars=c("LA_CODE","LA_NAME","Age", "Sex"), measure=c("White", "Asian/Asian British", "Black/African/Caribbean/Black British", "Mixed/Multiple Ethnic Group", "Other Ethnic Group"))
# Set column names 
colnames(pop_2019_melt) <- c("LA_CODE", "LA_NAME", "Age", "Sex", "Ethnicity", "Population")
# Group data by Local Authority 
pop_2019 <- pop_2019_melt %>% group_by(LA_CODE, Ethnicity) %>% summarise("Population"=sum(Population)) %>% ungroup()
# Calculate Percentage of Population 
pop_2019_melt <- pop_2019_melt %>% group_by(LA_CODE, Sex) %>% mutate("Total_Pop"=sum(Population)) %>% mutate("perc_Pop"=Population/Total_Pop * 100) %>% ungroup()

# Save pop_2019_melt to file 
fwrite(pop_2019_melt,"C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\Ethnic Group Population Pyramids.csv")

################################################
# BRING IN REGION DATA
# Join on Region Name to Data
region <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Lookups\\Geographic_Lookup_X.csv")
region <- region[, c(6,8,9)]
# missing 6 LA Codes, so fill them in 
df <- data.frame(LAD_CD=c("E06000048", "E07000097", "E07000100", "E07000101", "E07000104", "E08000020"), RGN_NM=c("North East", "South East", "South East", "South East", "South East","North East"), RGN_CD=c("E12000001", "E12000008", "E12000008","E12000008","E12000008","E12000001"))
region<- rbind(region, df)
region <- unique(region)

# CALCULATING NATIONAL AND  REGIONAL POPULATIONS BY ETHNICITY
pop_2019_region<- merge(pop_2019, region, by.x="LA_CODE", by.y="LAD_CD")

national_pop_2019 <- pop_2019_region %>% group_by(Ethnicity) %>% summarise("Pop"=sum(Population)) %>% ungroup()
national_pop_2019$RGN11NM <- "National"
pop_2019_region<- pop_2019_region %>% group_by(RGN_NM, Ethnicity) %>% mutate("Pop"=sum(Population)) %>% ungroup()
pop_2019_region<-subset(pop_2019_region, select=c("Ethnicity", "RGN_NM", "Pop"))
region_pop_2019<- pop_2019_region %>% group_by(RGN_NM, Ethnicity) %>% summarise("Pop"=Pop) %>% ungroup()
region_pop_2019 <- region_pop_2019[!(duplicated(region_pop_2019)),]

national_pop_2019$Ethnicity <- as.character(national_pop_2019$Ethnicity)
region_pop_2019$Ethnicity <- as.character(region_pop_2019$Ethnicity)
region_pop_2019$Ethnicity[region_pop_2019$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
region_pop_2019$Ethnicity[region_pop_2019$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"
national_pop_2019$Ethnicity[national_pop_2019$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
national_pop_2019$Ethnicity[national_pop_2019$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

###############################################
# # In this section, we will use 2011 Census Ethnicity Data- grouped to same Ethnicities as Test and Trace Data to calculate the % uptake. 

# # Read in population Census Data at LA Level
# census <- read_xls("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Population\\Census_LA.xls", sheet=2, skip=14, col_names = c("Area_Code", "Area_Name", "UA", "LA","All categories: Ethnic Group", "White: English/Welsh/Scottish/Northern Irish/British", "White: Irish", "White: Gypsy or Irish Traveller", "White: Other White", "Mixed/multiple ethnic group: White and Black Caribbean", "Mixed/multiple ethnic group: White and Black African", "Mixed/multiple ethnic group: White and Asian", "Mixed/multiple ethnic group: Other Mixed", "Asian/Asian British: Indian", "Asian/Asian British: Pakistani", "Asian/Asian British: Bangladeshi", "Asian/Asian British: Chinese", "Asian/Asian British: Other Asian", "Black/African/Caribbean/Black British: African","Black/African/Caribbean/Black British: Caribbean", "Black/African/Caribbean/Black British: Other Black", "Other ethnic group: Arab", "Other ethnic group: Any other ethnic group"))
# # set column names
# #colnames(census) <- c("Geography", "All Pop",
#                    #   "White: English/Welsh/Scottish/Northern Irish/British", "White: Irish","White: Gypsy or Irish Traveller","White: Other White","Multiple/Mixed Ethnic Group: White and Black Caribbean","Multiple/Mixed Ethnic Group: White and Black African","Multiple/Mixed Ethnic Group: White and Asian","Multiple/Mixed Ethnic Group: Other Mixed","Asian/Asian British: Indian","Asian/Asian British: Pakistani","Asian/Asian British: Bangladeshi","Asian/Asian British: Chinese","Asian/Asian British: Other","Black/African/Caribbean/ Black British: African","Black/African/Caribbean/ Black British: Caribbean","Black/African/Caribbean/ Black British: Other Black","Other Ethnic Group: Arab", "Other Ethnic Group: Any Other Ethnic Group")
# 
# # remove Welsh data
# census<- census[1:(which(census$Area_Name=="WALES")-1),]
# # remove columns with no LA or UA (retains 361 as some regions are retained)
# census <- census[!is.na(census$LA) | !is.na(census$UA),]
# # remove data in UA column that does not have UA as last two letters
# census$UA_Check <- stri_sub(census$UA, -2, -1)
# census$UA_LA <- NA
# census$UA_LA[census$UA_Check=="UA"]<- 1
# census <- census[census$UA_Check=="UA" | !is.na(census$LA), ] # keep only UA and LAs in the dataset
# 
# # Group Census 2011 Data into ethnic groups that match NHS Test and Trace
# census$White_pop <- (census$`White: English/Welsh/Scottish/Northern Irish/British` + census$`White: Irish` + census$`White: Gypsy or Irish Traveller` + census$`White: Other White`) # Calculate population in this group
# 
# census$AsianAsianBritish_pop <- (census$`Asian/Asian British: Indian`+ census$`Asian/Asian British: Pakistani` + census$`Asian/Asian British: Bangladeshi`+ census$`Asian/Asian British: Chinese`+ census$`Asian/Asian British: Other Asian`)
# 
# census$BlackAfricanCaribbeanBlackBritish_pop <- (census$`Black/African/Caribbean/Black British: African` + census$`Black/African/Caribbean/Black British: Caribbean` + census$`Black/African/Caribbean/Black British: Other Black`)
# 
# census$MixedMultiple_pop <- (census$`Mixed/multiple ethnic group: Other Mixed` + census$`Mixed/multiple ethnic group: White and Asian` + census$`Mixed/multiple ethnic group: White and Black African` + census$`Mixed/multiple ethnic group: White and Black Caribbean`)
# census$OtherEthnic_pop <- (census$`Other ethnic group: Arab` + census$`Other ethnic group: Any other ethnic group`)
# 
# # Subset census to keep only useful variables
# census <- subset(census, select= c(Area_Code, LA, White_pop, AsianAsianBritish_pop, BlackAfricanCaribbeanBlackBritish_pop, MixedMultiple_pop, OtherEthnic_pop))
# 
# # Set to data.table
# census <- as.data.table(census)
# colnames(census) <- c("Area_Code", "LA", "White", "Asian/Asian British", "Black/African/Caribbean/Black British","Mixed/Multiple Ethnic Group", "Other Ethnic Group")
# 
# # Spread the census to enable merging
# census <- melt(census, id.vars="Area_Code", measure=c("White", "Asian/Asian British", "Black/African/Caribbean/Black British", "Mixed/Multiple Ethnic Group", "Other Ethnic Group"))
# colnames(census) <- c("LA_CODE", "Ethnicity", "Population")
###################################################
### GENERATE ETHNICITY REPORTS

# LOAD IN NON-DISCLOSIVE DATASETS FOR PLOTTING
plot1_2_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\Plot1_2_x.csv")
plot3_4_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\Plot3_4_x.csv")
plot5_6_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\Plot5_6_x.csv")
plot7_8_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\Plot7_8_x.csv")
plot9_10_x <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\Plot9_10_x.csv")
pillar2_info <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\pillar2_info.csv")

# Join MY 2019 Populations to data via LA Code
plot1_2_x <- merge(plot1_2_x, pop_2019, by.x=c("LA_CODE", "Ethnicity"), by.y=c("LA_CODE", "Ethnicity"), all.x=TRUE) # join on Census ethnicity data
plot3_4_x <- merge(plot3_4_x, pop_2019, by.x=c("LA_CODE", "Ethnicity"), by.y=c("LA_CODE", "Ethnicity"), all.x=TRUE) # join on Census ethnicity data
plot5_6_x <- merge(plot5_6_x, pop_2019, by.x=c("LA_CODE", "Ethnicity"), by.y=c("LA_CODE", "Ethnicity"), all.x=TRUE) # join on Census ethnicity data

# Rename the Ethnicity Variables to enable wrapping of title 
plot1_2_x$Ethnicity[plot1_2_x$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
plot1_2_x$Ethnicity[plot1_2_x$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

plot3_4_x$Ethnicity[plot3_4_x$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
plot3_4_x$Ethnicity[plot3_4_x$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

plot5_6_x$Ethnicity[plot5_6_x$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
plot5_6_x$Ethnicity[plot5_6_x$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

plot7_8_x$Ethnicity[plot7_8_x$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
plot7_8_x$Ethnicity[plot7_8_x$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

plot9_10_x$Ethnicity[plot9_10_x$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
plot9_10_x$Ethnicity[plot9_10_x$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

# Read in National and Regional Aggregate Files 
pos_ethnicity_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\pos_ethnicity_agg.csv")
pos_ethnicity_age_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\pos_ethnicity_age_agg.csv")
pos_ethnicity_gender_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\pos_ethnicity_gender_agg.csv")
repeat_uptake_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\repeat_uptake_agg.csv")
single_uptake_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Ethnic Inequalities\\single_uptake_agg.csv")

# Set column names 
colnames(pos_ethnicity_agg) <- c("appointmentdate", "Ethnicity", "positive", "total", "rolling_pc_positive", "RGN11NM")
colnames(pos_ethnicity_gender_agg) <- c("appointmentdate", "Ethnicity", "Gender", "positive", "total", "rolling_pc_positive", "RGN11NM")
colnames(pos_ethnicity_age_agg) <- c("appointmentdate", "Ethnicity", "Age", "positive", "total", "rolling_pc_positive", "RGN11NM")
colnames(single_uptake_agg) <- c("week", "Ethnicity", "tests", "RGN11NM")
colnames(repeat_uptake_agg) <- c("week", "Ethnicity", "tests", "RGN11NM")
# Remove NA values 
pos_ethnicity_age_agg <- pos_ethnicity_age_agg[pos_ethnicity_age_agg$Ethnicity!="",]
pos_ethnicity_gender_agg <- pos_ethnicity_gender_agg[pos_ethnicity_gender_agg$Ethnicity!="",]
pos_ethnicity_gender_agg <- pos_ethnicity_gender_agg[pos_ethnicity_gender_agg$Gender!="",]
pos_ethnicity_agg <- pos_ethnicity_agg[pos_ethnicity_agg$Ethnicity!="",]
single_uptake_agg<- single_uptake_agg[single_uptake_agg$Ethnicity!="",]

# Rename Ethnicity Groups to match data 
pos_ethnicity_agg$Ethnicity[pos_ethnicity_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
pos_ethnicity_agg$Ethnicity[pos_ethnicity_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

pos_ethnicity_gender_agg$Ethnicity[pos_ethnicity_gender_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
pos_ethnicity_gender_agg$Ethnicity[pos_ethnicity_gender_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

pos_ethnicity_age_agg$Ethnicity[pos_ethnicity_age_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
pos_ethnicity_age_agg$Ethnicity[pos_ethnicity_age_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

single_uptake_agg$Ethnicity[single_uptake_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
single_uptake_agg$Ethnicity[single_uptake_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

repeat_uptake_agg$Ethnicity[repeat_uptake_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
repeat_uptake_agg$Ethnicity[repeat_uptake_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"
######################################################
# Proportional area graph of single vs repeat testing uptake 
uptake_graphs <- merge(plot1_2_x, plot3_4_x, by=c("LA_CODE", "Ethnicity", "week"), all.y=TRUE)

uptake_graphs <- pivot_longer(uptake_graphs, cols=c("Tests.x", "Tests.y"))
uptake_graphs$name[uptake_graphs$name=="Tests.x"]<- "Single Test Uptake"
uptake_graphs$name[uptake_graphs$name=="Tests.y"]<- "Repeated Test Uptake"
colnames(uptake_graphs) <- c("LA_CODE", "Ethnicity", "week", "Population.x", "Population.y", "Uptake", "value")

# Proportional area graph of single vs repeat testing uptake 
uptake_graphs_agg <- merge(single_uptake_agg, repeat_uptake_agg, by=c("RGN11NM", "Ethnicity", "week"), all.y=TRUE)
uptake_graphs_agg <- pivot_longer(uptake_graphs_agg, cols=c("tests.x", "tests.y"))
uptake_graphs_agg$name[uptake_graphs_agg$name=="tests.x"]<- "Single Test Uptake"
uptake_graphs_agg$name[uptake_graphs_agg$name=="tests.y"]<- "Repeated Test Uptake"
colnames(uptake_graphs_agg) <- c("RGN11NM", "Ethnicity", "week", "Uptake", "value")
uptake_graphs_agg <- merge(uptake_graphs_agg, region_pop_2019, by.x=c("RGN11NM", "Ethnicity"), by.y=c("RGN_NM", "Ethnicity"), all.x=TRUE)
uptake_graphs_agg <- merge(uptake_graphs_agg, national_pop_2019, by.x=c("RGN11NM", "Ethnicity"), by.y=c("RGN11NM", "Ethnicity"),all.x=TRUE)
uptake_graphs_agg$Pop <- ifelse(!is.na(uptake_graphs_agg$Pop.x), uptake_graphs_agg$Pop.x, uptake_graphs_agg$Pop.y)
uptake_graphs_agg$prop <- uptake_graphs_agg$value/ uptake_graphs_agg$Pop * 100000
uptake_graphs_agg <- uptake_graphs_agg[uptake_graphs_agg$Ethnicity!="" & uptake_graphs_agg$Ethnicity!="Prefer not to say", ]
uptake_graphs_agg <- uptake_graphs_agg %>% group_by(RGN11NM, week, Uptake) %>% mutate(prop_weight= prop/sum(prop)) %>% ungroup()
#####################################################
# Calculate % Uptake by Ethnicity
plot1_2_x$pc_tests <- plot1_2_x$Tests/plot1_2_x$Population * 100
plot3_4_x$pc_tests <- plot3_4_x$Tests/plot3_4_x$Population * 100
plot1_2_x$pc_tests[plot1_2_x$pc_tests>100] <- 100
plot3_4_x$pc_tests[plot3_4_x$pc_tests>100] <- 100

# Cumulative % Uptake by Ethnicity (not Recommended to plot- misleading)
plot1_2_x <- plot1_2_x %>% group_by(Ethnicity, LA_CODE) %>% mutate("cum_tests"=cumsum(Tests)) %>% ungroup() 
plot1_2_x$cum_pc_tests <- plot1_2_x$cum_tests/ plot1_2_x$Population * 100
plot3_4_x <- plot3_4_x %>% group_by(Ethnicity, LA_CODE) %>% mutate("cum_tests"=cumsum(Tests)) %>% ungroup() 
plot3_4_x$cum_pc_tests <- plot3_4_x$cum_tests/ plot3_4_x$Population * 100
plot1_2_x$cum_pc_tests[plot1_2_x$cum_pc_tests>100] <- 100
plot3_4_x$cum_pc_tests[plot3_4_x$cum_pc_tests>100] <- 100

# Join regions to the data
plot1_2_x <- merge(plot1_2_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot3_4_x <- merge(plot3_4_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot5_6_x <- merge(plot5_6_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot7_8_x <- merge(plot7_8_x, region, by.x="LA_CODE", by.y="LAD_CD")
plot9_10_x <- merge(plot9_10_x, region, by.x="LA_CODE", by.y="LAD_CD")
pillar2_info <- merge(pillar2_info, region, by.x="LA_CODE", by.y="LAD_CD")

# Sort to date format 
plot5_6_x$appointmentdate<-as.Date(plot5_6_x$appointmentdate, "%d/%m/%Y")
plot7_8_x$appointmentdate<-as.Date(plot7_8_x$appointmentdate, "%d/%m/%Y")
plot9_10_x$appointmentdate<-as.Date(plot9_10_x$appointmentdate, "%d/%m/%Y")

# National Uptake

national_uptake<- single_uptake_agg %>% group_by(week, Ethnicity) %>% summarise("Tests"=sum(tests, na.rm=TRUE)) %>% ungroup()
national_uptake$RGN11NM <- "National"

national_uptake <- merge(national_uptake, national_pop_2019)

# Regional Uptake
regional_uptake <- single_uptake_agg %>% group_by(week, Ethnicity, RGN11NM) %>% summarise("Tests"=sum(tests,na.rm=TRUE)) %>% ungroup()
regional_uptake <- merge(regional_uptake, pop_2019_region, by.x=c("Ethnicity", "RGN11NM"), by.y=c("Ethnicity", "RGN_NM"))
regional_uptake <- regional_uptake[!(duplicated(regional_uptake)),]

# Join National and Regional Uptake 
uptake_agg <- rbind(national_uptake, regional_uptake)
uptake_agg$pc_tests <- uptake_agg$Tests/uptake_agg$Pop * 100

uptake_agg$Ethnicity[uptake_agg$Ethnicity=="Asian/Asian British"] <- "Asian /Asian British"
uptake_agg$Ethnicity[uptake_agg$Ethnicity=="Black/African/Caribbean/Black British"] <- "Black/African /Caribbean /Black British"

#######################################
# Total tests 
plot5_6_x$week <- cut(plot5_6_x$appointmentdate, "week") # add week to data (7-day rolling averages, so daily atm)
tests_data <- plot5_6_x %>% group_by(week, Ethnicity, LA_CODE) %>% summarise("Population"=Population, "Tests"= sum(total, na.rm=TRUE)) %>% ungroup() # summarise data to week, Ethnicity and LA
tests_data <- tests_data[!duplicated(tests_data),] # remove duplicated rows?
tests_data <- tests_data[tests_data$Ethnicity!="Prefer not to say",] # remove Ethnicity that does not have a match in MY2019 estimates 

# National Tests 
tests_data_national <- plot5_6_x %>% group_by(week, Ethnicity) %>% summarise("Tests"= sum(total, na.rm=TRUE)) %>% ungroup() # summarise data to week, Ethnicity and LA
tests_data_national <- tests_data_national[tests_data_national$Ethnicity!="Prefer not to say",] # remove Ethnicity that does not have a match in MY2019 estimates 
# Join together
tests_data_national <- merge(tests_data_national, national_pop_2019, by="Ethnicity")

# Regional Tests
tests_data_regional <- plot5_6_x %>% group_by(week, Ethnicity, RGN_NM) %>% summarise("Tests"= sum(total, na.rm=TRUE)) %>% ungroup() # summarise data to week, Ethnicity and LA
tests_data_regional <- tests_data_regional[tests_data_regional$Ethnicity!="Prefer not to say",] # remove Ethnicity that does not have a match in MY2019 estimates 
# Join together
tests_data_regional <- merge(tests_data_regional, region_pop_2019, by=c("Ethnicity", "RGN_NM"))

# E0600028 and E06000029 and E06000048 and E07000048 and E07000097 and E07000100 and E07000101 and E07000104 and E07000190 and E07000191 and E07000201  and E07000204 and E07000205 and E07000206 and E08000020 fail to run? 
#which(pillar2_info$LA_CODE==i)
#pillar2_info <- pillar2_info[pillar2_info$LA_CODE!="E06000028" & pillar2_info$LA_CODE!="E06000029" & pillar2_info$LA_CODE!="E06000048" & pillar2_info$LA_CODE!="E07000048" & pillar2_info$LA_CODE!="E07000097" & pillar2_info$LA_CODE!="E07000100" & pillar2_info$LA_CODE!="E07000101"  & pillar2_info$LA_CODE!="E07000104" & pillar2_info$LA_CODE!="E07000190" & pillar2_info$LA_CODE!="E07000191"  & pillar2_info$LA_CODE!="E07000201" & pillar2_info$LA_CODE!="E07000204" & pillar2_info$LA_CODE!="E07000205" & pillar2_info$LA_CODE!="E07000206" & pillar2_info$LA_CODE!="E08000020",]
#pillar2_info <- pillar2_info[17:nrow(pillar2_info),]


# Read in missing reports file and re-run just these 
missing <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\missing_reports.csv")
# Subset to only ones with demographic as false (not yet available) 
missing <- missing[missing$Ethnic==FALSE & missing$Economic==TRUE,]
pillar2_info <- pillar2_info[pillar2_info$LA_CODE %in% missing$LA_CODE]

############################################################
for (i in unique(pillar2_info$LA_CODE)) { # subset the pillar 2 Test and Trace Dataset
  pillar2_info_sub <- pillar2_info[pillar2_info$LA_CODE == i,] # Subset information on Test and Trace Dataset for Local Authority
  data_1 <- plot1_2_x[plot1_2_x$LA_CODE==i,] # Subset aggregated plotting data
  data_2 <- plot3_4_x[plot3_4_x$LA_CODE==i,] # Subset aggregated plotting data
  uptake_sub <- uptake_graphs[uptake_graphs$LA_CODE==i, ] #Subset aggregated plotting data
  data_3 <- plot5_6_x[plot5_6_x$LA_CODE==i,] # Subset aggregated plotting data
  data_4 <- plot7_8_x[plot7_8_x$LA_CODE==i,] # Subset aggregated plotting data
  data_5 <- plot9_10_x[plot9_10_x$LA_CODE==i,] # Subset aggregated plotting data
  pop_2019_sub<- as.data.table(pop_2019_melt[pop_2019_melt$LA_CODE==i,])# Subset aggregated plotting data
  test_sub <- tests_data[tests_data$LA_CODE==i,] # Subset aggregated plotting data
  rmarkdown::render("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\Demographic Inequalities\\Ethnic Inequalities\\Ethnicity_Template.Rmd", html_document(toc=TRUE), 
                    output_file = paste0(i,"_","Ethnic_Inequalities_COVID 19",".html"), # Save the report as this filename
                    output_dir = paste0("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Outputs\\", gsub(" ", "_", pillar2_info_sub$RGN_NM), "\\LAD\\", i, "\\"))
}
###################################################
