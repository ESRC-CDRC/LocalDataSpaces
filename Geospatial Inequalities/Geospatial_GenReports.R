###################################################
### Create Geospatial LA Reports ###
###################################################

# Purpose: Script automatically creates a report including visualisations, maps and tables for all Local Authorities

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
library(sf)
library(biscale)
library(dplyr)
library(cowplot)
library(RColorBrewer)
###################################################
# GENERATE GEOSPATIAL REPORTS
# Read in LSOA Shapefile for geospatial plotting
shapefile <- read_sf(dsn="C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Boundaries", layer="LSOA_2011_EW_BFC") # Read in LSOA shapefile

plotfirstwave <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Geospatial Inequalities\\plotfirstwave_x.csv")
plotsecondwave <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Geospatial Inequalities\\plotsecondwave_x.csv")
plottileplot <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Geospatial Inequalities\\plottile_x.csv")
pillar2_info <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Geospatial Inequalities\\pillar2_info_x.csv")
# Set as data frame
pillar2_info <- as.data.frame(pillar2_info)

plotfirstwave$IMD_correct <- 11-plotfirstwave$IMD # needed as IMD in the datasets is actually reversed to enable bivariate mapping
plotsecondwave$IMD_correct <- 11-plotsecondwave$IMD # needed as IMD in the datasets is actually reversed to enable bivariate mapping

# Set Date to Week
plottileplot$week<-as.Date(plottileplot$week)

# Join on Region Name to Data
region <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Lookups\\Geographic_Lookup_X.csv")
region <- region[, c(6,8,9)]
# missing 6 LA Codes, so fill them in 
df <- data.frame(LAD_CD=c("E06000048", "E07000097", "E07000100", "E07000101", "E07000104", "E08000020"), RGN_NM=c("North East", "South East", "South East", "South East", "South East","North East"), RGN_CD=c("E12000001", "E12000008", "E12000008","E12000008","E12000008","E12000001"))
region<- rbind(region, df)
region <- unique(region)

plotfirstwave <- merge(plotfirstwave, region, by.x="LA_CODE", by.y="LAD_CD")
plotsecondwave <- merge(plotsecondwave, region, by.x="LA_CODE", by.y="LAD_CD")
plottileplot <- merge(plottileplot, region, by.x="LA_CODE", by.y="LAD_CD")
pillar2_info <- merge(pillar2_info, region, by.x="LA_CODE", by.y="LAD_CD")

# Add IUC Labels to plotfirstwave and plotsecondwave, rather than the number alone 
IUC <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Lookups\\IUC_Lookup.csv")
plotfirstwave <- merge(plotfirstwave, IUC, by.x="IUC", by.y="Group Code")
plotsecondwave <- merge(plotsecondwave, IUC, by/x="IUC", by.y="Group Code")

# Read in National and Regional Aggregates 
geospatial_agg <- fread("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Data\\Covid\\Geospatial Inequalities\\geospatial_agg.csv")

# Ensure NA if total less than 10 (not 0) - as initially built for tileplot and NA set to 0 so didn't have loads of holes
plottileplot$pc_positive[plottileplot$total<10 | is.na(plottileplot$total)]<-NA
plottileplot$pc_positive.1[plottileplot$total.1<10 | is.na(plottileplot$total.1)]<-NA
plottileplot$pc_positive.2[plottileplot$total.2<10 | is.na(plottileplot$total.2)]<-NA
plottileplot$pc_positive.3[plottileplot$total.3<10 | is.na(plottileplot$total.3)]<-NA
plottileplot$pc_positive.4[plottileplot$total.4<10 | is.na(plottileplot$total.4)]<-NA

# Join on geometry to data for plotting
plotfirstwave <- merge(shapefile, plotfirstwave, by.x="LSOA11CD", by.y="LSOA11CD") 
plotsecondwave <- merge(shapefile, plotsecondwave, by.x="LSOA11CD", by.y="LSOA11CD") 

# Producing all the plots required and storing them into a list for use in report generation
plot_list <- list()


# Produce bivariate legends (one for each Plot to aid understanding) 
legend_IMD <- bi_legend(pal="DkBlue", dim=3, xlab="% Positivity", ylab="IMD Deprivation", size=7)
legend_pop <- bi_legend(pal="DkBlue", dim=3, xlab="% Positivity", ylab="Population Density", size=7)
legend_h <- bi_legend(pal="DkBlue", dim=3, xlab="% Positivity", ylab="Healthcare Deprivation", size=7)
legend_over <- bi_legend(pal="DkBlue", dim=3, xlab="% Positivity", ylab="% Overcrowding", size=7)
legend_IUC <- bi_legend(pal="DkBlue", dim=3, xlab="% Positivity", ylab="Digital Exclusion", size=7)

# Set colour palette for IUC map as not continuous 
mycolors <- colorRampPalette(brewer.pal(8, "Pastel1"))(10)

#i="E08000035" # Testing Report Generation
i="E06000048" # one of 6 changes in dataset based upon our lookup
for (i in unique(pillar2_info$LA_CODE)) { 
  print(i) # progress checking
  pillar2_info_sub <- pillar2_info[pillar2_info$LA_CODE==i,]
  # Subset pillar2 to keep only week of data 
  pillar2_tiers <- subset(plottileplot, select=c(week))
  pillar2_tiers<- pillar2_tiers[!(duplicated(pillar2_tiers$week)),] # subset to keep only the non-duplicated weeks (ie all weeks in the dataset)
  pillar2_tiers <- as.data.frame(pillar2_tiers)
  colnames(pillar2_tiers) <- c("week")
  pillar2_tiers$week <- sort(pillar2_tiers$week, decreasing=FALSE)
  pillar2_tiers$week_num_normalised <- 1:nrow(pillar2_tiers) # count from 1 upwards to give a week number
  pillar2_tiers$week_num_strf <- as.numeric(strftime(as.Date(pillar2_tiers$week), format="%V")) # return week number in the year for that Date
  
  tiers <- data.frame("Label"=c("National Lockdown","Rule of Six Outdoors","Tiers Introduced", "Hospitality Reopened","         Rule of Six Outdoors and Indoors","National Lockdown","Tiers Introduced", "Tier 4 Introduced", "National Lockdown", "Rule of Six Outdoors", "Outdoor Hospitality and Retail Reopened"),
                      "Date"=c("2020/03/23","2020/06/01","2020/07/04","2020/09/14","2020/10/14","2020/11/01","2020/12/02","2020/12/19", "2021/01/06", "2021/03/29", "2021/04/12"))
  tiers$Date <- as.Date(tiers$Date)
  tiers$week_num_strf <- as.numeric(strftime(as.Date(tiers$Date), format="%V")) # return week number in the year for that Date
  
  # Join onto tiers dataset 
  tiers <- merge(tiers, pillar2_tiers, by.x="week_num_strf", by.y="week_num_strf")
  
  # Subset plotfirstwave 
  plotfirstwave_sub <- plotfirstwave[plotfirstwave$LA_CODE==i,]
  # Subset plotsecondwave 
  plotsecondwave_sub <- plotsecondwave[plotsecondwave$LA_CODE==i,]
  # subset plottileplot
  plottileplot_sub <- plottileplot[plottileplot$LA_CODE==i,]
  
  
  ################################
  # INDEX OF MULTIPLE DEPRIVATION
  # create map 
  IMD_first <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = bi_class), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and IMD",fill="Positivity Rate - IMD", subtitle="First Wave- January 01 2020 to August 31 2020", caption="Source: NHS Test and Trace Pillar 2;\n 2019 Index of Multiple Deprivation.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
    # create Map
  IMD_sec <- ggplot() +
    geom_sf(data=plotsecondwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotsecondwave_sub, aes(fill = bi_class), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and IMD",fill="Positivity Rate - IMD",subtitle=paste0("Second Wave- September 01 2020 to ", format(pillar2_info_sub$max_date[1],  "%B %d %Y")), caption="Source: NHS Test and Trace Pillar 2;\n 2019 Index of Multiple Deprivation.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
     
    # Decile Map
 IMD_map <- ggplot() +
   geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
   geom_sf(data = plotfirstwave_sub, aes(fill = as.factor(IMD_correct)), color=NA, lwd = 0) +
   labs(
     title = "Index of Multiple Deprivation Decile",fill="Index of Multiple Deprivation Decile", caption="Source: 2019 Index of Multiple Deprivation.") +
   scale_x_continuous(expand = c(0,0)) +
   coord_sf() +
   theme_classic() +
   theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
         plot.subtitle = element_text(face = "italic"),
         panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="right") + 
   guides(fill=guide_legend(ncol=2, nrow=5)) + scale_fill_viridis_d(begin=0, end=0.9)
 
    # Join plots together
    IMD_together <- plot_grid(IMD_first, legend_IMD, IMD_sec, ncol=3, rel_widths=c(1, 0.3, 1))
    IMD_together <- plot_grid(IMD_map, IMD_together, nrow=2, rel_heights=c(0.8,1))
    
    # Rename factors for clarity when labelling
    plottileplot_sub$IMD[plottileplot_sub$IMD==1]<-"1-Most Deprived"
    plottileplot_sub$IMD[plottileplot_sub$IMD==5]<-"5-Least Deprived"
    geospatial_agg$IMD[geospatial_agg$IMD==1]<- "1-Most Deprived"
    geospatial_agg$IMD[geospatial_agg$IMD==5] <- "5-Least Deprived"
   
     # Positivity Rate by IMD Quintile
  IMD_plot <- ggplot(plottileplot_sub, aes(x=week, y=pc_positive)) + 
    geom_line(aes(color=as.factor(IMD))) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM=="National",], aes(x=week, y=pc_positive),color="white", size=1.1, alpha=0.5) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM==pillar2_info$RGN_NM,], aes(x=week, y=pc_positive), color="black", alpha=0.3) +
    scale_color_viridis_d(begin=0, end=0.9) +
    labs(title="COVID-19 Positivity Rate by Week and IMD Quintile",fill="Positivity Rate - Pop per Sq Km",subtitle="Comparison with England (Black) and Region (White).") + 
    xlab("Week") + ylab("Weekly COVID-19 Positivity") + facet_wrap(~IMD) +
    theme(legend.position="none", axis.text.x=element_text(angle=90),
    panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption=element_text(face="italic"),plot.subtitle = element_text(face = "italic")) + theme(panel.grid = element_blank())
  
  ################################
  # PEOPLE PER SQ KM

  # create map 
  pop_first <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = bi_class.1), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Population Density",fill="Positivity Rate - IMD",subtitle="First Wave- January 01 2020 to August 31 2020", caption="Source: NHS Test and Trace Pillar 2;\n 2019 Mid-Year Population Estimates.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  # create Map
  pop_sec <- ggplot() +
    geom_sf(data=plotsecondwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotsecondwave_sub, aes(fill = bi_class.1), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Population Density",fill="Positivity Rate - IMD",subtitle=paste0("Second Wave- September 01 2020 to ", format(pillar2_info_sub$max_date[1],  "%B %d %Y")), caption="Source: NHS Test and Trace Pillar 2;\n 2019 Mid-Year Population Estimates.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  
  # Decile Map
  pop_map <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = pop_sqkm), color=NA, lwd = 0) +
    labs(
      title = "Population Density",fill="Population Density (Population per Sq Km)", caption="Source: 2019 Mid-Year Population Estimates.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="right") + 
    scale_fill_viridis_c(begin=0, end=0.9, direction=-1)
  
  # Join plots together
  pop_together <- plot_grid(pop_first, legend_pop, pop_sec, ncol=3, rel_widths=c(1, 0.3, 1))
  pop_together <- plot_grid(pop_map, pop_together, nrow=2, rel_heights=c(0.8,1))
  
  # Rename factors for clarity when labelling
  plottileplot_sub$Pop <- as.character(plottileplot_sub$Pop)
  plottileplot_sub$Pop[plottileplot_sub$Pop==1]<-"1-Least Densely Populated"
  plottileplot_sub$Pop[plottileplot_sub$Pop==5]<-"5-Most Densely Populated"
  geospatial_agg$Pop[geospatial_agg$Pop==1]<- "1-Least Densely Populated"
  geospatial_agg$Pop[geospatial_agg$Pop==5] <-"5-Most Densely Populated"
  
  # Positivity Rate by Pop Density Quintile
  pop_plot <- ggplot(plottileplot_sub, aes(x=week, y=pc_positive.1)) + 
    geom_line(aes(color=as.factor(Pop))) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM=="National",], aes(x=week, y=pc_positive.1),color="white", size=1.1, alpha=0.5) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM==pillar2_info$RGN_NM,], aes(x=week, y=pc_positive.1), color="black", alpha=0.3) +
    scale_color_viridis_d(begin=0, end=0.9) +
    labs(title="COVID-19 Positivity Rate by Week and IMD Quintile",fill="Positivity Rate - Pop per Sq Km", color="IMD Quintile \n(1 is most deprived)",subtitle="Comparison with England (Black) and Region (White).") + 
    xlab("Week") + ylab("Weekly COVID-19 Positivity") + facet_wrap(~Pop) +
    theme(legend.position="none", axis.text.x=element_text(angle=90),
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption=element_text(face="italic"),plot.subtitle = element_text(face = "italic")) + theme(panel.grid = element_blank())
  
  ################################
  # ACCESS TO HEALTHCARE
  
  # create map 
  h_first <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = bi_class.2), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Access to Healthcare", subtitle="First Wave- January 01 2020 to August 31 2020", caption="Source: NHS Test and Trace Pillar 2;\n 2020 Access to Healthy Assets & Hazards (AHAH).") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  # create Map
  h_sec <- ggplot() +
    geom_sf(data=plotsecondwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotsecondwave_sub, aes(fill = bi_class.2), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Access to Healthcare",subtitle=paste0("Second Wave- September 01 2020 to ", format(pillar2_info_sub$max_date[1],  "%B %d %Y")),caption="Source: NHS Test and Trace Pillar 2;\n 2020 Access to Healthy Assets & Hazards (AHAH).") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  
  # Decile Map
  h_map <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = as.factor(h_dec)), color=NA, lwd = 0) +
    labs(
      title = "Access to Healthcare Decile",fill="Access to Healthcare Decile", caption="Source: 2020 Access to Healthy Assets & Hazards (AHAH).") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="right") + 
    guides(fill=guide_legend(ncol=2, nrow=5)) + scale_fill_viridis_d(begin=0, end=0.9)
  
  # Join plots together
  h_together <- plot_grid(h_first, legend_h, h_sec, ncol=3, rel_widths=c(1, 0.3, 1))
  h_together <- plot_grid(h_map, h_together, nrow=2, rel_heights=c(0.8,1))
  
  # Rename factors for clarity when labelling
  plottileplot_sub$Healthcare<- as.character(plottileplot_sub$Healthcare)
  plottileplot_sub$Healthcare[plottileplot_sub$Healthcare==1]<-"1-Best Healthcare Access"
  plottileplot_sub$Healthcare[plottileplot_sub$Healthcare==5]<-"5-Worst Healthcare Access"
  geospatial_agg$Healthcare[geospatial_agg$Healthcare==1]<- "1-Best Healthcare Access"
  geospatial_agg$Healthcare[geospatial_agg$Healthcare==5] <-"5-Worst Healthcare Access"
  
  # Positivity Rate by Pop Density Quintile
  h_plot <- ggplot(plottileplot_sub, aes(x=week, y=pc_positive.2)) + 
    geom_line(aes(color=as.factor(Healthcare))) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM=="National",], aes(x=week, y=pc_positive.2),color="white", size=1.1, alpha=0.5) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM==pillar2_info$RGN_NM,], aes(x=week, y=pc_positive.2), color="black", alpha=0.3) +
    scale_color_viridis_d(begin=0, end=0.9) +
    labs(title="COVID-19 Positivity Rate by Week and Access to Healthcare Quintile", caption="Source: NHS Test and Trace Pillar 2; 2020 Access to Healthy Assets & Hazards (AHAH).",subtitle="Comparison with England (Black) and Region (White).") + 
    xlab("Week") + ylab("Weekly COVID-19 Positivity") + facet_wrap(~Healthcare) +
    theme(legend.position="none", axis.text.x=element_text(angle=90),
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption=element_text(face="italic"),plot.subtitle = element_text(face = "italic")) + theme(panel.grid = element_blank())
  
  ################################
  # OVERCROWDING
  
  # create map 
  overcrowded_first <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = bi_class.3), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Overcrowding",subtitle="First Wave- January 01 2020 to August 31 2020", caption="Source: NHS Test and Trace Pillar 2;\n 2011 Census Occupancy Rating.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  # create Map
  overcrowded_sec <- ggplot() +
    geom_sf(data=plotsecondwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotsecondwave_sub, aes(fill = bi_class.3), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Overcrowding",subtitle=paste0("Second Wave- September 01 2020 to ", format(pillar2_info_sub$max_date[1],  "%B %d %Y")),caption="Source: NHS Test and Trace Pillar 2;\n 2011 Census Occupancy Rating.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  
  # Decile Map
  overcrowded_map <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = overcrowded), color=NA, lwd = 0) +
    labs(
      title = "Overcrowding",fill="Overcrowding (%)", caption="Source: 2011 Census Occupancy Rating.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="right") + 
    scale_fill_viridis_c(begin=0, end=0.9, direction=-1)
  
  # Join plots together
  overcrowded_together <- plot_grid(overcrowded_first, legend_over, overcrowded_sec, ncol=3, rel_widths=c(1, 0.3, 1))
  overcrowded_together <- plot_grid(overcrowded_map, overcrowded_together, nrow=2, rel_heights=c(0.8, 1))
  
  # Rename factors for clarity when labelling
  plottileplot_sub$Overcrowded<-as.character(plottileplot_sub$Overcrowded)
  plottileplot_sub$Overcrowded[plottileplot_sub$Overcrowded==1]<-"1-Least Overcrowded"
  plottileplot_sub$Overcrowded[plottileplot_sub$Overcrowded==5]<-"5-Most Overcrowded"
  geospatial_agg$Overcrowded[geospatial_agg$Overcrowded==1]<- "1-Least Overcrowded"
  geospatial_agg$Overcrowded[geospatial_agg$Overcrowded==5] <-"5-Most Overcrowded"
  
  # Positivity Rate by Pop Density Quintile
  overcrowded_plot <- ggplot(plottileplot_sub, aes(x=week, y=pc_positive.3)) + 
    geom_line(aes(color=as.factor(Overcrowded))) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM=="National",], aes(x=week, y=pc_positive.3),color="white", size=1.1, alpha=0.5) +
    geom_line(data=geospatial_agg[geospatial_agg$RGN11NM==pillar2_info$RGN_NM,], aes(x=week, y=pc_positive.3), color="black", alpha=0.3) +
    scale_color_viridis_d(begin=0, end=0.9) +
    labs(title="COVID-19 Positivity Rate by Week and Overcrowding Quintile", caption="Source: NHS Test and Trace Pillar 2; 2011 Census Occupancy Rating.",subtitle="Comparison with England (Black) and Region (White).") + 
    xlab("Week") + ylab("Weekly COVID-19 Positivity") + facet_wrap(~Overcrowded) +
    theme(legend.position="none", axis.text.x=element_text(angle=90),
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption=element_text(face="italic"),plot.subtitle = element_text(face = "italic")) + theme(panel.grid = element_blank())
  
  ################################
  #INTERNET USER CLASSIFICATION
  
  # create map 
  IUC_first <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = bi_class.4), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Internet User Classification",subtitle="First Wave- January 01 2020 to August 31 2020", caption="Source: NHS Test and Trace Pillar 2;\n 2018 Internet User Classification.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  # create Map
  IUC_sec <- ggplot() +
    geom_sf(data=plotsecondwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotsecondwave_sub, aes(fill = bi_class.4), color=NA, lwd = 0) +
    labs(
      title = "COVID-19 Positivity Rate and Internet User Classification",subtitle=paste0("Second Wave- September 01 2020 to ", format(pillar2_info_sub$max_date[1],  "%B %d %Y")),caption="Source: NHS Test and Trace Pillar 2;\n 2018 Internet User Classification.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="none") + 
    guides(fill=guide_legend(ncol=3, nrow=3))  + scale_fill_manual(values=c("3-3" = "#3b4994", "2-3" = "#8c62aa","1-3" = "#be64ac", "3-2" = "#5698b9","2-2" = "#a5add3", "1-2" = "#dfb0d6",
                                                                            "3-1" = "#5ac8c8", "2-1" = "#ace4e4","1-1" = "#e8e8e8"), drop=FALSE)
  
  # Decile Map
  # Reorder IUC Factor into correct order 
  plotfirstwave_sub$`Group Name`<- factor(plotfirstwave_sub$`Group Name`, levels=c("e-Cultural Creators", "e-Professionals", "e-Veterans", "Youthful Urban Fringe", "e-Rational Utilitarians", "e-Mainstream", "Passive and Uncommitted Users", "Digital Seniors", "Settled Offline Communities", "e-Withdrawn"))
  
  IUC_map <- ggplot() +
    geom_sf(data=plotfirstwave_sub%>%group_by(LA_CODE) %>% summarise(), color="grey", size=0.9)+
    geom_sf(data = plotfirstwave_sub, aes(fill = as.factor(`Group Name`)), color=NA, lwd = 0) +
    labs(
      title = "Internet User Classification",fill="Internet User Classification", caption="Source: 2018 Internet User Classification.") +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic")) + theme(axis.ticks.y = element_blank(), axis.ticks.x=element_blank(), axis.text.x= element_blank(), axis.text.y=element_blank(), panel.grid.major = element_blank(), plot.title =element_text(hjust=0.5), legend.position="right") + 
    guides(fill=guide_legend(ncol=2, nrow=5)) + scale_fill_manual(values=mycolors)

  # Join plots together
  IUC_together <- plot_grid(IUC_first, legend_IUC, IUC_sec, ncol=3, rel_widths=c(1, 0.3, 1))
  IUC_together <- plot_grid(IUC_map, IUC_together, nrow=2, rel_heights=c(0.8, 1))
  
  ################################
  # Store to list under LA_CODE (i) 
  plot_list[[i]]<- list(IMD_together, IMD_plot, pop_together, pop_plot, h_together, h_plot, overcrowded_together, overcrowded_plot, IUC_together)
  print("Created and stored plots to list")
}
# Remove everything except pillar2_info, plot_list and aggregated averages in geospatial_agg 
rm(IMD_first, IMD_sec, IMD_together, IMD_plot, pop_first, pop_sec, pop_together, pop_plot, h_first, h_sec, h_together, h_plot, overcrowded_first, overcrowded_sec, overcrowded_together, overcrowded_plot, IUC_first, IUC_sec, IUC_together, h_map, IMD_map, IUC_map, df, legend_h, legend_IMD, legend_over, legend_pop, overcrowded_map, shapefile, region, pop_map, plottileplot, plottileplot_sub, plotfirstwave, plotfirstwave_sub, pillar2_tiers,tiers, legend_IUC, plotsecondwave, plotsecondwave_sub, i, pillar2_info_sub, IUC)
###################################################
# OUTPUT LOOP 

# E0600028 and E06000029 and E06000048 and E07000048 and E07000097 and E07000100 and E07000101 and E07000104 and E07000190 and E07000191 and E07000201  and E07000204 and E07000205 and E07000206 and E08000020 fail to run? 
#which(pillar2_info$LA_CODE==i)
#pillar2_info <- pillar2_info[pillar2_info$LA_CODE!="E06000028" & pillar2_info$LA_CODE!="E06000029" & pillar2_info$LA_CODE!="E06000048" & pillar2_info$LA_CODE!="E07000048" & pillar2_info$LA_CODE!="E07000097" & pillar2_info$LA_CODE!="E07000100" & pillar2_info$LA_CODE!="E07000101"  & pillar2_info$LA_CODE!="E07000104" & pillar2_info$LA_CODE!="E07000190" & pillar2_info$LA_CODE!="E07000191"  & pillar2_info$LA_CODE!="E07000201" & pillar2_info$LA_CODE!="E07000204" & pillar2_info$LA_CODE!="E07000205" & pillar2_info$LA_CODE!="E07000206" & pillar2_info$LA_CODE!="E08000020",]
#pillar2_info <- pillar2_info[2:nrow(pillar2_info),]

# For each Local Authority in the dataset, create a report via this loop
for (i in unique(pillar2_info$LA_CODE)){ # For each Local Authority
  pillar2_info_sub <- pillar2_info[pillar2_info$LA_CODE==i,] # subset context info
  print(i) # Print current code being processed for monitoring
  rmarkdown::render("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Code\\Geospatial Inequalities\\Geospatial_Template.Rmd", html_document(toc=TRUE), 
                    output_file = paste0(i,"_","Geospatial_Inequalities_COVID_19",".html"), # Save the report as this filename
                    output_dir = paste0("C:\\Users\\medsleea\\University of Leeds\\MacDonald, Jacob - Local Profiles\\Outputs\\", gsub(" ", "_", pillar2_info_sub$RGN_NM), "\\LAD\\", i, "\\"))
}
###################################################