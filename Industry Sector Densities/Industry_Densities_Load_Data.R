#############################################################################
#############################################################################
###########  
#########    LOCAL INDUSTRY SECTOR DENSITIES PROFILING
#######       
#####    - Script to load the necessary final data files and generate series
###        of visual descriptives.
#

#############################################################################
##### SECTION 1: PROJECT SET UP AND IMPORT LOOKUPS
###

# PROJECT & LIBRARY SET UP
#----------------------------------------------------------------------------
# Prior to running the template for this one-pager, run the following script
#   to load and clean the necessary data files.
# Library should be set beforehand to the following path and read in packages.
#
# To install package, look for source file in folder structure:
#   > install.packages("R:\\Software Add-Ons\\R-library\\NEW 3.6 R-Library\\sp_1.4-1.zip", repos = NULL)
# 
# lib_base="P:\\Working\\Libraries"
# assign(".lib.loc", lib_base, envir = environment(.libPaths))

library(data.table)
library(sf)
library(rmarkdown)
library(markdown)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)
library(randomcoloR)
library(knitr)
library(stringr)
library(numDeriv)
library(dplyr)
library(tidyr)
library(tidygraph)
library(kableExtra)
library(scales)
library(cowplot)
library(readxl)

wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])),
  "/OneDrive - The University of Liverpool/Local Profiles")
# wd <- paste0(ifelse(Sys.info()[[1]]=="Linux", paste0("/home/", Sys.info()[[7]], "/Documents"), paste0("/Users/", Sys.info()[[7]])), 
#   "/Dropbox/Research/ADRUK Covid/Local Profiles")
wd.data <- paste0(wd, "/Data")
#----------------------------------------------------------------------------

# SET PALETTES OF COLOURS
#----------------------------------------------------------------------------
# https://www.schemecolor.com/superlative.php

palette <- list()
palette[[1]] <- data.table(rbind(cbind(Colour="Blue", Col="#205D8A", Col_L="#E9EFF3", Col_H="#1A4A6E"),
  cbind(Colour="Green", Col="#33A989", Col_L="#EBF6F3", Col_H="#29876E"),
  cbind(Colour="Yellow", Col="#E3BA58", Col_L="#FCF8EE", Col_H="#B69546"),
  cbind(Colour="Orange", Col="#C77946", Col_L="#F9F2ED", Col_H="#B36D3F"),
  cbind(Colour="Red", Col="#AF4242", Col_L="#F7ECEC", Col_H="#9E3B3B"),
  cbind(Colour="Purple", Col="#802956", Col_L="#F2EAEE", Col_H="#662145")))
palette[[2]] <- c("#E6194B", "#3CB44B", "#FFE119", "#4363D8", "#F58231", "#911EB4",
  "#42D4F4", "#F032E6", "#BFEf45", "#FABED4", "#469990", "#DCBEFF", "#9A6324",
  "#FFFAC8", "#800000", "#AAFFC3", "#808000", "#FFD8B1", "#000075", "#A9A9A9",
  "#000000", "#FFFFFF")
palette[[3]] <- data.table(rbind(
  cbind(Retail_Class="Regional Centre", Colour="#DF1C44"),
  cbind(Retail_Class="Major Town Centre", Colour="#FD8F03"),
  cbind(Retail_Class="Town Centre", Colour="#F0C126"),
  cbind(Retail_Class="Market Town Centre", Colour="#5CD6B7"),
  cbind(Retail_Class="District Centre", Colour="#0099DE"),
  cbind(Retail_Class="Local Centre", Colour="#BABFC3"),
  cbind(Retail_Class="Small Local Centre", Colour="#D9DADC"),
  cbind(Retail_Class="Retail Park", Colour="#E7D8ED"),
  cbind(Retail_Class="Out of Town Shopping Centres", Colour="#B4BADD")))

# show_col(palette[[1]]$Col)
# show_col(palette[[1]]$Col_L)
# show_col(palette[[1]]$Col_H)
# show_col(palette[[2]])
# show_col(palette[[3]]$Colour)





#----------------------------------------------------------------------------

# SOURCE LOOKUPS AND CODES
#----------------------------------------------------------------------------
geographic.lookup <- fread(file=paste0(wd.data, "/Lookups/Geographic_Lookup_X.csv"))
LAD_CIS <- fread(file=paste0(wd.data, "/Boundaries/Local_Authority_District__May_2020__to_Covid_Infection_Survey__October_2020__Lookup_in_England.csv"))[,c("LAD20CD", "CIS20CD")]
names(LAD_CIS) <- c("LAD_CD", "CIS_CD")
LAD_CA <- fread(file=paste0(wd.data, "/Boundaries/Local_Authority_District_to_Combined_Authority__December_2020__Lookup_in_England.csv"))[,c("LAD20CD", "CAUTH20CD", "CAUTH20NM")]
names(LAD_CA) <- c("LAD_CD", "CA_CD", "CA_NM")
LSOA_LEP <- fread(file=paste0(wd.data, "/Boundaries/Lower_Layer_Super_Output_Area__2011__to_Local_Enterprise_Partnership__April_2020__Lookup_in_England.csv"), na.strings = c(""))[,c("LSOA11CD", "LEP20CD1", "LEP20NM1", "LEP20CD2", "LEP20NM2")]
names(LSOA_LEP) <- c("LSOA_CD", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2")
geographic.lookup <- merge(geographic.lookup, LAD_CIS, all.x=T, sort=F, by="LAD_CD")
geographic.lookup <- merge(geographic.lookup, LAD_CA, all.x=T, sort=F, by="LAD_CD")
geographic.lookup <- merge(geographic.lookup, LSOA_LEP, all.x=T, sort=F, by="LSOA_CD")
CIS.naming <- fread(file=paste0(wd.data, "/Lookups/CIS_Naming.csv"))
geographic.lookup <- merge(geographic.lookup, CIS.naming, all.x=T, sort=F, by="CIS_CD")
rm(LAD_CIS, LAD_CA, LSOA_LEP, CIS.naming); gc()

SIC.lookup <- fread(file=paste0(wd.data, "/Lookups/SIC_Lookup_X.csv"))
SIC.lookup$Division_CD <- as.character(ifelse(nchar(SIC.lookup$Division_CD)==4, paste0("0", SIC.lookup$Division_CD), paste0(SIC.lookup$Division_CD)))

LDS.lookup <- data.table(rbind(cbind("Southend", c("E06000033"), c("East of England")),
  cbind("Cambridgeshire and Peterborough", c("E07000008", "E07000009", "E07000010", "E07000011", "E07000012", "E06000031"), c("East of England")),
  cbind("Norfolk County Council", c("E07000143", "E07000144", "E07000146", "E07000145", "E07000147", "E07000148", "E07000149"), c("East of England")),
  cbind("Hertfordshire County Council", c("E07000095", "E07000096", "E07000097", "E07000098", "E07000099", "E07000100", "E07000101", "E07000102", "E07000103", "E07000104"), c("East of England")),
  cbind("Portsmouth, Hampshire and Isle of Wight", 
    c("E07000084", "E07000085", "E07000086", "E07000087", "E07000088", "E07000089", "E07000090", "E07000091", "E07000092", "E07000093", "E07000094", "E06000044", "E06000045", "E06000046"), 
    c("South East Public Health")),
  cbind("Surrey; West Sussex", 
    c("E07000207", "E07000208", "E07000209", "E07000210", "E07000211", "E07000212", "E07000213", "E07000214", 
      "E07000215", "E07000216", "E07000217", "E07000061", "E07000062", "E07000063", "E07000064", "E07000065"), c("South East Public Health")),
  cbind("Westminster", c("E09000033", "E09000020"), NA),
  cbind("Harborough District Council", c("E07000131"), NA),
  cbind("Leicester", c("E06000016"), NA),
  cbind("Birmingham and Solihull", c("E08000025", "E08000029"), NA),
  cbind("Nottingham City and Nottinghamshire County Council", c("E06000018", "E07000170", "E07000171", "E07000172", "E07000173", "E07000174", "E07000175", "E07000176"), NA),
  cbind("South Yorkshire Group", c("E08000016", "E08000017", "E08000018", "E08000019"), NA),
  cbind("Bury Council and CCG", c("E08000002"), NA),
  cbind("Liverpool Combined City Region", c("E08000012", "E08000013", "E08000014", "E08000015", "E08000011", "E06000006"), NA),
  cbind("Lincolnshire County Council", c("E07000136", "E07000137", "E07000138", "E07000139", "E07000140", "E07000141", "E07000142"), NA),
  cbind("Hackney", c("E09000012"), NA),
    cbind("City of York", c("E06000014"), NA)))
names(LDS.lookup) <- c("LDS_Name", "LAD_CD", "LDS_Group")

# housing.lookup <- data.table(cbind(type=c("Total", "Detached", "Semi Detached", "Terraced", "Flat", "Other"),
#   order=c(1, 2, 3, 4, 5, 6), colour=c("#000000", palette[[1]][palette[[1]]$Colour %in% c("Blue", "Red", "Yellow", "Green", "Purple")]$Col)))

# SOC.lookup <- fread(file=paste0(wd.data, "/Lookup/SOC_Lookup_X.csv"))
#----------------------------------------------------------------------------

# READ IN SPATIAL BOUNDARIES 
#----------------------------------------------------------------------------
WZ.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Workplace_Zones_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp"),
  stringsAsFactors = F, quiet = T), crs=27700)[,c("wz11cd", "lad11cd")]
names(WZ.sp)[names(WZ.sp)=="wz11cd"] <- "WZ_CD"
names(WZ.sp)[names(WZ.sp)=="lad11cd"] <- "LAD_CD"
WZ.sp <- merge(WZ.sp, unique(geographic.lookup[,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]),
  all.x=T, sort=F, by="LAD_CD")

# LSOA.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/LSOA_2011_EW_BFC.shp"), 
#   stringsAsFactors = F, quiet = T), crs=27700)[,c("LSOA11CD")]
# names(LSOA.sp)[names(LSOA.sp)=="LSOA11CD"] <- "LSOA_CD"
# LSOA.sp <- merge(LSOA.sp, unique(geographic.lookup[,c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]), 
#   all.x=T, sort=F, by="LSOA_CD")
# LSOA.sp$area_ha <- as.numeric(st_area(LSOA.sp))*0.0001

LAD.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Local_Authority_Districts_(December_2020)_UK_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("LAD20CD")]
names(LAD.sp)[names(LAD.sp)=="LAD20CD"] <- "LAD_CD"
LAD.sp <- merge(LAD.sp, unique(geographic.lookup[,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]), 
  all.x=T, sort=F, by="LAD_CD")

LEP.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Local_Enterprise_Partnerships_(April_2020)_Boundaries_EN_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("lep20cd", "lep20nm")]
names(LEP.sp)[names(LEP.sp)=="lep20cd"] <- "LEP_CD1"
names(LEP.sp)[names(LEP.sp)=="lep20nm"] <- "LEP_NM1"
t <- unique(geographic.lookup[,c("RGN_CD", "RGN_NM", "LEP_CD1", "LEP_NM1")])
t <- t[-c(17, 18, 21, 41, 42),]
LEP.sp <- merge(LEP.sp, t, all.x=T, sort=F, by="LEP_CD1"); rm(t)

CA.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Combined_Authorities_(December_2019)_Boundaries_EN_BFC.shp"),
  stringsAsFactors = F, quiet = T), crs=27700)[,c("cauth19cd", "cauth19nm")]
names(CA.sp)[names(CA.sp)=="cauth19cd"] <- "CA_CD"
names(CA.sp)[names(CA.sp)=="cauth19nm"] <- "CA_NA"
CA.sp <- merge(CA.sp, unique(geographic.lookup[,c("RGN_CD", "RGN_NM", "CA_CD", "CA_NM")]),
  all.x=T, sort=F, by="CA_CD")

RGN.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Regions_(December_2019)_Boundaries_EN_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("rgn19cd", "rgn19nm")]
names(RGN.sp)[names(RGN.sp)=="rgn19cd"] <- "RGN_CD"
names(RGN.sp)[names(RGN.sp)=="rgn19nm"] <- "RGN_NM"

# Retail.sp <- st_transform(st_read(gsub("ADRUK Covid/Local Profiles", "Retail/Exports/Pre Release/Retail_Centres_UK_v2.gpkg", wd), 
#   stringsAsFactors = F, quiet = T), crs=27700)
# names(Retail.sp)[names(Retail.sp)=="geom"] <- c("geometry")
# st_geometry(Retail.sp) <- "geometry"
# t <- fread(gsub("ADRUK Covid/Local Profiles", "Retail/Exports/Pre Release/Retail_Centres_Statistics.csv", wd))
# Retail.sp <- merge(Retail.sp, t[,c("RC_ID")], all.x=T, sort=F, by="RC_ID")
# 
# CIS.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Covid_Infection_Survey__October_2020__EN_BFC.shp"), 
#   stringsAsFactors = F, quiet = T), crs=27700)[,c("CIS20CD")]
#----------------------------------------------------------------------------

# SET UP SUBSET OF LADs FOR BATCH GENERATING
#----------------------------------------------------------------------------
# TO EVENTUALLY RUN ON THE FULL SET OF LADs (OR LDS SPECIFIC)
#
# FIGURE 3 DOES NOT RUN WITH THESE LADs - THEY ARE PREVIOUS CODES AND NOT UPDATED
# c("E06000053", "E06000029", "E07000051", "E07000049", "E07000205", "E07000190", 
#   "E06000028", "E07000201", "E07000206", "E07000204", "E07000053", "E07000052", 
#   "E07000191", "E07000191", "E07000048", "E07000050")
#
# set.seed(8675309)
# local.profiles <- unique(geographic.lookup[geographic.lookup$LAD_CD %in% local.profiles,c("LAD_CD", "RGN_NM")])[,.SD[sample(.N, min(2,.N))],by = RGN_NM]$LAD_CD
# local.profiles <- unique(geographic.lookup$LAD_CD)[!is.na(unique(geographic.lookup$LAD_CD))]
# local.profiles <- unique(LDS.lookup$LAD_CD)

local.profiles <- c(unique(LDS.lookup$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="Yorkshire and The Humber",]$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="East of England",]$LAD_CD))
local.profiles <- local.profiles[which(local.profiles %in% unique(geographic.lookup$LAD_CD))]

# local.profiles <- local.profiles[!(local.profiles %in% 
#   c("E06000053", "E06000029", "E07000051", "E07000049", "E07000205", "E07000190",
#   "E06000028", "E07000201", "E07000206", "E07000204", "E07000053", "E07000052",
#   "E07000191", "E07000191", "E07000048", "E07000050"))]
#----------------------------------------------------------------------------

#############################################################################
##### SECTION 2: LOAD IN AND FORMAT CLEANED (NON-DISCLOSIVE) DATASETS
###

# NON-DISCLOSIVE AGGREGATE BRES, BSD and BC
#----------------------------------------------------------------------------
BRES.Counts <- list()
BRES.LAD <- fread(file=paste0(wd.data, "/Economic/BRES_LAD_15_19_X.csv"))
BRES.Counts[[1]] <- BRES.LAD
BRES.LAD <- merge(BRES.LAD, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD")]), all.x=T, sort=F, by="LAD_CD")
BRES.LEP <- BRES.LAD[,.(business=sum(business, na.rm=T), N.small=sum(N.small, na.rm=T), weighted_ftempee=sum(weighted_ftempee, na.rm=T), 
  weighted_ptempee=sum(weighted_ptempee, na.rm=T), weighted_totempee=sum(weighted_totempee, na.rm=T), NE.small=sum(NE.small, na.rm=T), 
  private=sum(private, na.rm=T), NE.private=sum(NE.private, na.rm=T)),by=.(BIG_CD, snapshot, LEP_CD1)]
BRES.LEP <- merge(BRES.LEP, unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])[!duplicated(unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])$LEP_CD1)], all.x=T, sort=F, by="LEP_CD1")
BRES.Counts[[2]] <- BRES.LEP
BRES.CA <- BRES.LAD[,.(business=sum(business, na.rm=T), N.small=sum(N.small, na.rm=T), weighted_ftempee=sum(weighted_ftempee, na.rm=T), 
  weighted_ptempee=sum(weighted_ptempee, na.rm=T), weighted_totempee=sum(weighted_totempee, na.rm=T), NE.small=sum(NE.small, na.rm=T), 
  private=sum(private, na.rm=T), NE.private=sum(NE.private, na.rm=T)),by=.(BIG_CD, snapshot, CA_CD)]
BRES.CA <- merge(BRES.CA, unique(geographic.lookup[,c("CA_CD", "RGN_CD")])[!is.na(unique(geographic.lookup[,c("CA_CD", "RGN_CD")])$CA_CD)], all.x=T, sort=F, by="CA_CD")
BRES.Counts[[3]] <- BRES.CA; rm(BRES.LAD, BRES.LEP, BRES.CA)
names(BRES.Counts) <- c("LAD", "LEP", "CA")

BSD.BIG <- list()
BSD.LAD <- fread(file=paste0(wd.data, "/Economic/BSD_LAD_14_20_X1.csv"))
BSD.BIG[[1]] <- BSD.LAD
BSD.LAD <- merge(BSD.LAD, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD")]), all.x=T, sort=F, by="LAD_CD")
BSD.LEP <- BSD.LAD[,.(business=sum(business, na.rm=T), employment=sum(employment, na.rm=T)),by=.(BIG_CD, snapshot, LEP_CD1)]
BSD.LEP <- merge(BSD.LEP, unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])[!duplicated(unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])$LEP_CD1)], all.x=T, sort=F, by="LEP_CD1")
BSD.BIG[[2]] <- BSD.LEP
BSD.CA <- BSD.LAD[,.(business=sum(business, na.rm=T), employment=sum(employment, na.rm=T)),by=.(BIG_CD, snapshot, CA_CD)]
BSD.CA <- merge(BSD.CA, unique(geographic.lookup[,c("CA_CD", "RGN_CD")])[!is.na(unique(geographic.lookup[,c("CA_CD", "RGN_CD")])$CA_CD)], all.x=T, sort=F, by="CA_CD")
BSD.BIG[[3]] <- BSD.CA; rm(BSD.LAD, BSD.LEP, BSD.CA)
names(BSD.BIG) <- c("LAD", "LEP", "CA")

BSD.sic2 <- list()
BSD.LAD <- fread(file=paste0(wd.data, "/Economic/BSD_LAD_14_20_X2.csv"))
BSD.LAD$sic.division <- as.character(ifelse(nchar(BSD.LAD$sic.division)==1, paste0("0", BSD.LAD$sic.division), paste0(BSD.LAD$sic.division)))
BSD.sic2[[1]] <- BSD.LAD
BSD.LAD <- merge(BSD.LAD, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD")]), all.x=T, sort=F, by="LAD_CD")
BSD.LEP <- BSD.LAD[,.(business=sum(business, na.rm=T), employment=sum(employment, na.rm=T)),by=.(sic.division, snapshot, LEP_CD1)]
BSD.LEP <- merge(BSD.LEP, unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])[!duplicated(unique(geographic.lookup[,c("LEP_CD1", "RGN_CD")])$LEP_CD1)], all.x=T, sort=F, by="LEP_CD1")
BSD.sic2[[2]] <- BSD.LEP
BSD.CA <- BSD.LAD[,.(business=sum(business, na.rm=T), employment=sum(employment, na.rm=T)),by=.(sic.division, snapshot, CA_CD)]
BSD.CA <- merge(BSD.CA, unique(geographic.lookup[,c("CA_CD", "RGN_CD")])[!is.na(unique(geographic.lookup[,c("CA_CD", "RGN_CD")])$CA_CD)], all.x=T, sort=F, by="CA_CD")
BSD.sic2[[3]] <- BSD.CA; rm(BSD.LAD, BSD.LEP, BSD.CA)
names(BSD.sic2) <- c("LAD", "LEP", "CA")

BSD.Dens <- list()
BSD.Dens[[1]] <- fread(file=paste0(wd.data, "/Economic/BSD_LAD_Densities_X.csv"))
BSD.Dens[[2]] <- fread(file=paste0(wd.data, "/Economic/BSD_LEP_Densities_X.csv"))
BSD.Dens[[3]] <- fread(file=paste0(wd.data, "/Economic/BSD_CA_Densities_X.csv"))
names(BSD.Dens) <- c("LAD", "LEP", "CA")

BSD.DensSP <- list()
BSD.DensSP[[1]] <- fread(file=paste0(wd.data, "/Economic/BSD_LAD_DensitiesSP_X.csv"))
BSD.DensSP[[2]] <- fread(file=paste0(wd.data, "/Economic/BSD_LEP_DensitiesSP_X.csv"))
BSD.DensSP[[3]] <- fread(file=paste0(wd.data, "/Economic/BSD_CA_DensitiesSP_X.csv"))
names(BSD.DensSP) <- c("LAD", "LEP", "CA")

BC.2020 <- fread(file=paste0(gsub("OneDrive - The University of Liverpool", "Dropbox/Research/ADRUK Covid", wd.data), "/Economic/business_census2020.csv"))
BC.2020 <- BC.2020[BC.2020$companystatus=="Active", c("companystatus", "siccode", "postcode")]
BC.2020$siccode <- as.character(ifelse(nchar(BC.2020$siccode)==4, paste0("0", BC.2020$siccode), paste0(BC.2020$siccode)))
BC.2020$postcode[BC.2020$postcode==""] <- NA
BC.2020 <- BC.2020[!is.na(BC.2020$postcode),]
BC.2020$pc.match <- as.character(gsub(" ", "", BC.2020$postcode))
NSPL <- fread(gsub("OneDrive - The University of Liverpool/Local Profiles/Data", "Dropbox/Liverpool/GDSL/Lookups/NSPL_FEB_2020_UK.csv", wd.data))
NSPL$pc.match <- as.character(gsub(" ", "", NSPL$pcd))
BC.2020 <- merge(BC.2020, NSPL[,c("pc.match", "lsoa11", "wz11")], all.x=T, sort=F, by="pc.match")
BC.2020$companystatus <- NULL
BC.2020$postcode <- NULL
BC.2020$pc.match <- NULL
names(BC.2020) <- c("SIC", "LSOA_CD", "WZ_CD")
BC.2020 <- BC.2020[!is.na(BC.2020$LSOA_CD),]
rm(NSPL)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 0: SIC SECTOR SIC CODES
###

# SECTOR COLOUR LOOKUP PLOT
#----------------------------------------------------------------------------
t <- data.table(rbind(cbind(c("Agriculture, forestry & fishing"), c("Section A"), c("BIG 1")),
  cbind(c("Mining, quarrying & utilities"), c("Section B", "Section D", "Section E"), c("BIG 2")),
  cbind(c("Manufacturing"), c("Section C"), c("BIG 3")),
  cbind(c("Construction"), c("Section F"), c("BIG 4")),
  cbind(c("Motor trades"), c("SIC_45"), c("BIG 5")),
  cbind(c("Wholesale"), c("SIC_46"), c("BIG 6")),
  cbind(c("Retail"), c("SIC_47"), c("BIG 7")),
  cbind(c("Transport & storage (inc postal)"), c("Section H"), c("BIG 8")),
  cbind(c("Accommodation & food services"), c("Section I"), c("BIG 9")),
  cbind(c("Information & communication"), c("Section J"), c("BIG 10")),
  cbind(c("Finance & insurance"), c("Section K"), c("BIG 11")),
  cbind(c("Property"), c("Section L"), c("BIG 12")),
  cbind(c("Professional, scientific & technical"), c("Section M"), c("BIG 13")),
  cbind(c("Business administration and support services"), c("Section N"), c("BIG 14")),
  cbind(c("Public administration & defence"), c("Section O"), c("BIG 15")),
  cbind(c("Education"), c("Section P"), c("BIG 16")),
  cbind(c("Health"), c("Section Q"), c("BIG 17")),
  cbind(c("Arts, entertainment, recreation and other services"), c("Section R", "Section S", "Section T", "Section U"), c("BIG 18"))))
names(t) <- c("BIG_NM", "Section", "BIG_CD")

palette.lookup <- data.table(rbind(cbind(paste0("SIC_0", 1:4), rep("Section A", length(paste0("SIC_0", 1:4)))),
  cbind(paste0("SIC_0", 5:9), rep("Section B", length(paste0("SIC_0", 5:9)))),
  cbind(paste0("SIC_", 10:33), rep("Section C", length(paste0("SIC_", 10:33)))),
  cbind(paste0("SIC_", 35), rep("Section D", length(paste0("SIC_", 35)))),
  cbind(paste0("SIC_", 36:39), rep("Section E", length(paste0("SIC_", 36:39)))),
  cbind(paste0("SIC_", 41:43), rep("Section F", length(paste0("SIC_", 41:43)))),
  cbind(paste0("SIC_", 45:47), rep("Section G", length(paste0("SIC_", 45:47)))),
  cbind(paste0("SIC_", 49:53), rep("Section H", length(paste0("SIC_", 49:53)))),
  cbind(paste0("SIC_", 55:56), rep("Section I", length(paste0("SIC_", 55:56)))),
  cbind(paste0("SIC_", 58:63), rep("Section J", length(paste0("SIC_", 58:63)))),
  cbind(paste0("SIC_", 64:66), rep("Section K", length(paste0("SIC_", 64:66)))),
  cbind(paste0("SIC_", 68), rep("Section L", length(paste0("SIC_", 68)))),
  cbind(paste0("SIC_", 69:75), rep("Section M", length(paste0("SIC_", 69:75)))),
  cbind(paste0("SIC_", 77:82), rep("Section N", length(paste0("SIC_", 77:82)))),
  cbind(paste0("SIC_", 84), rep("Section O", length(paste0("SIC_", 84)))),
  cbind(paste0("SIC_", 85), rep("Section P", length(paste0("SIC_", 85)))),
  cbind(paste0("SIC_", 86:88), rep("Section Q", length(paste0("SIC_", 86:88)))),
  cbind(paste0("SIC_", 90:93), rep("Section R", length(paste0("SIC_", 90:93)))),
  cbind(paste0("SIC_", 94:96), rep("Section S", length(paste0("SIC_", 94:96)))),
  cbind(paste0("SIC_", 97:98), rep("Section T", length(paste0("SIC_", 97:98)))),
  cbind(paste0("SIC_", 99), rep("Section U", length(paste0("SIC_", 99))))))
names(palette.lookup) <- c("SIC", "Section")

palette.lookup <- merge(palette.lookup, t, all.x=T, sort=F, by="Section")
palette.lookup <- merge(palette.lookup, t, all.x=T, sort=F, by.x="SIC", by.y="Section")
palette.lookup$BIG_NM.x[is.na(palette.lookup$BIG_NM.x)] <- palette.lookup$BIG_NM.y[is.na(palette.lookup$BIG_NM.x)]; palette.lookup$BIG_NM.y <- NULL
palette.lookup$BIG_CD.x[is.na(palette.lookup$BIG_CD.x)] <- palette.lookup$BIG_CD.y[is.na(palette.lookup$BIG_CD.x)]; palette.lookup$BIG_CD.y <- NULL
names(palette.lookup)[names(palette.lookup)=="BIG_NM.x"] <- "BIG_NM"
names(palette.lookup)[names(palette.lookup)=="BIG_CD.x"] <- "BIG_CD"

palette.lookup <- merge(palette.lookup, unique(SIC.lookup[,c("BIG_CD", "Colour")]), all.x=T, sort=F, by="BIG_CD")
names(palette.lookup) <- c("BIG_CD", "SIC", "Section", "BIG_NM", "colour"); rm(t)

t <- data.frame(do.call(rbind, lapply(split(palette.lookup, palette.lookup$BIG_CD), function(x) paste0(unique(x$Section), collapse="; "))))
t$BIG_CD <- rownames(t)
names(t) <- c("Sections", "BIG_CD")
t$Sections <- as.character(t$Sections)
t$BIG_CD <- as.character(t$BIG_CD)
t$Sections[t$BIG_CD=="BIG 5"] <- "SIC 45"
t$Sections[t$BIG_CD=="BIG 6"] <- "SIC 46"
t$Sections[t$BIG_CD=="BIG 7"] <- "SIC 47"

palette.lookup <- merge(unique(SIC.lookup[,c("BIG_CD", "BIG_NM", "Colour")]), t, all.x=T, sort=F); rm(t)
palette.lookup$Sections[palette.lookup$Sections=="Section B; Section D; Section E"] <- "Section B; D; E"
palette.lookup$Sections[palette.lookup$Sections=="Section R; Section S; Section T; Section U"] <- "Section R; S; T; U"
palette.lookup$Bar <- 5
palette.lookup$Colour <- as.character(palette.lookup$Colour)
palette.lookup$BIG_CD <- factor(palette.lookup$BIG_CD, levels=rev(paste0("BIG ", 1:18)))
palette.lookup$label <- paste0(as.character(palette.lookup$BIG_NM), " (", as.character(palette.lookup$Sections), ")  (", as.character(palette.lookup$Colour), ")")

palette.lookup <- ggplot(palette.lookup, aes(x=BIG_CD, y=Bar, fill=Colour), colour="#EDEDEB") +
  geom_bar(stat = "identity", fill = palette.lookup$Colour, alpha=0.6) +
  scale_fill_manual(values=c(palette.lookup$Colour)) +
  geom_text(data=palette.lookup, mapping=aes(x=BIG_CD, y=0, label=label), fontface="italic", colour="black", hjust=0, alpha=0.8) +
  labs(title = "SIC Broad Industry Group", subtitle = "Colour matching guide", x = "", y = "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1)),
    color = guide_legend(override.aes = list(size = 1))) +
  theme_classic() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    legend.position = "none", legend.title = element_blank(), panel.background=element_rect(fill="#EDEDEB"))
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 0: NATIONAL INCORPORATIONS AND DISSOLUTIONS
###

# (COMPANIES HOUSE) WEEK ON WEEK INC. & DISSOLUTIONS
#----------------------------------------------------------------------------
business.change <- fread(file=paste0(wd.data, "/Retail & Fast Indicators/Business_Change.csv"))
business.change$Date.scaled <- as.Date(paste0(gsub("Week ", "2020-", business.change$Date), -1), format = "%Y-%W-%u")
t1 <- business.change[,c("Date", "Inc_2019", "Dis_2019", "Date.scaled")]
names(t1) <- c("Date", "Incorporation", "Dissolution", "Date.scaled")
t1$Year <- 2019
t2 <- business.change[,c("Date", "Inc_2020", "Dis_2020", "Date.scaled")]
names(t2) <- c("Date", "Incorporation", "Dissolution", "Date.scaled")
t2$Year <- 2020
t3 <- business.change[,c("Date", "Inc_2021", "Dis_2021", "Date.scaled")]
names(t3) <- c("Date", "Incorporation", "Dissolution", "Date.scaled")
t3$Year <- 2021
business.change <- rbind(t1, t2)
business.change <- rbind(business.change, t3); rm(t1, t2, t3)
business.change1 <- business.change[,c("Incorporation", "Date.scaled", "Year")]
names(business.change1) <- c("N", "Date.scaled", "Year")
business.change1$Type <- "Incorporations"
business.change2 <- business.change[,c("Dissolution", "Date.scaled", "Year")]
names(business.change2) <- c("N", "Date.scaled", "Year")
business.change2$Type <- "Dissolutions"
business.change <- rbind(business.change1, business.change2); rm(business.change1, business.change2)
business.change$Year <- factor(as.character(business.change$Year), levels=c("2019", "2020", "2021"))

fig.businessD <- ggplot(data=business.change, aes(Date.scaled, N, group=Year, colour=Year)) +
  geom_line(lwd=1) +
  geom_point() +
  facet_wrap(~Type, scales = "free_y", nrow=4) +
  labs(title = "Week-on-Week Dissolutions and Incorporations", subtitle = "National Business Level Changes", y="Counts",
    caption = paste0("Source: ONS Faster Indicators - Companies House Registry")) +    
  scale_y_continuous(expand = c(0,0), labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_colour_manual(values=rev(c(scales::viridis_pal()(3)))) +
  scale_x_date(expand = c(0,0), date_breaks="1 month", labels=date_format("%B"), limits = c(as.Date("2020-01-01"), NA)) +
  theme_classic() +
  theme(legend.position="bottom", legend.title = element_blank(), plot.caption = element_text(face = "italic"),
    plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
    axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_rect(fill="#EDEDEB"))
rm(business.change)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 1: DISTRIBUTION AND DIVISION OF INDUSTRY SECTORS
###

# SET UP CORRECT SCALES AND LIMITS
#----------------------------------------------------------------------------
fig.1 <- BSD.BIG[["LAD"]][,c("BIG_CD", "LAD_CD", "RGN_CD", "business", "employment", "snapshot")]
fig.1 <- fig.1[fig.1$snapshot==2020,]
nation1 <- fig.1[,list(business=sum(business), employment=sum(employment)), by=BIG_CD]
region1 <- lapply(split(fig.1, fig.1$RGN_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=BIG_CD] })
local1 <- lapply(split(fig.1, fig.1$LAD_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=BIG_CD] })

fig.1 <- BSD.sic2[["LAD"]][,c("sic.division", "LAD_CD", "RGN_CD", "business", "employment", "snapshot")]
fig.1 <- fig.1[fig.1$snapshot==2020,]
nation2 <- fig.1[,list(business=sum(business), employment=sum(employment)), by=sic.division]
region2 <- lapply(split(fig.1, fig.1$RGN_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=sic.division] })
local2 <- lapply(split(fig.1, fig.1$LAD_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=sic.division] })

scales <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  
  totals <- merge(local1[[names.lookup$LAD_CD]], region1[[names.lookup$RGN_CD]][,c("BIG_CD", "business", "employment")], all.x=T, sort=F, by="BIG_CD")
  names(totals) <- c("BIG_CD", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN")
  totals <- merge(totals, nation1[,c("BIG_CD", "business", "employment")], all.x=T, sort=F, by="BIG_CD")
  names(totals) <- c("BIG_CD", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN", "business_ENG", "employment_ENG")
  totals$business <- totals$business_LCL
  totals$business_LCL <- totals$business_LCL/sum(totals$business_LCL)
  totals$business_RGN <- totals$business_RGN/sum(totals$business_RGN)
  totals$business_ENG <- totals$business_ENG/sum(totals$business_ENG)
  totals$employment <- totals$employment_LCL
  totals$employment_LCL <- totals$employment_LCL/sum(totals$employment_LCL)
  totals$employment_RGN <- totals$employment_RGN/sum(totals$employment_RGN)
  totals$employment_ENG <- totals$employment_ENG/sum(totals$employment_ENG)
  
  t1 <- max(c(max(totals$business_LCL, na.rm=T), max(totals$business_RGN, na.rm=T), max(totals$business_ENG, na.rm=T)))
  t2 <- max(c(max(totals$employment_LCL, na.rm=T), max(totals$employment_RGN, na.rm=T), max(totals$employment_ENG, na.rm=T)))
  
  totals <- merge(local2[[names.lookup$LAD_CD]], region2[[names.lookup$RGN_CD]][,c("sic.division", "business", "employment")], all.x=T, sort=F, by="sic.division")
  names(totals) <- c("sic.division", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN")
  totals <- merge(totals, nation2[,c("sic.division", "business", "employment")], all.x=T, sort=F, by="sic.division")
  names(totals) <- c("SIC", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN", "business_ENG", "employment_ENG")
  totals$business <- totals$business_LCL
  totals$business_LCL <- totals$business_LCL/sum(totals$business_LCL)
  totals$business_RGN <- totals$business_RGN/sum(totals$business_RGN)
  totals$business_ENG <- totals$business_ENG/sum(totals$business_ENG)
  totals$employment <- totals$employment_LCL
  totals$employment_LCL <- totals$employment_LCL/sum(totals$employment_LCL)
  totals$employment_RGN <- totals$employment_RGN/sum(totals$employment_RGN)
  totals$employment_ENG <- totals$employment_ENG/sum(totals$employment_ENG)
  
  t3 <- max(c(max(totals$business_LCL, na.rm=T), max(totals$business_RGN, na.rm=T), max(totals$business_ENG, na.rm=T)))
  t4 <- max(c(max(totals$employment_LCL, na.rm=T), max(totals$employment_RGN, na.rm=T), max(totals$employment_ENG, na.rm=T)))
  
  return(cbind(LAD_CD=G, B.max=max(t1, t3), E.max=max(t2, t4)))  })
scales <- data.table(do.call(rbind, scales))
scales$B.max <- as.numeric(as.character(scales$B.max))
scales$E.max <- as.numeric(as.character(scales$E.max))
scales <- unique(scales)

rm(fig.1, nation1, nation2, region1, region2, local1, local2)
#----------------------------------------------------------------------------

# (BSD LAD-2020) BUSINESS / EMPLOYMENT BY BROAD SIC GROUPS 
#----------------------------------------------------------------------------
fig.1 <- BSD.BIG[["LAD"]][,c("BIG_CD", "LAD_CD", "RGN_CD", "business", "employment", "snapshot")]
fig.1 <- fig.1[fig.1$snapshot==2020,]

nation <- fig.1[,list(business=sum(business), employment=sum(employment)), by=BIG_CD]
region <- lapply(split(fig.1, fig.1$RGN_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=BIG_CD] })
local <- lapply(split(fig.1, fig.1$LAD_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=BIG_CD] })

fig.1 <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  
  totals <- merge(local[[names.lookup$LAD_CD]], region[[names.lookup$RGN_CD]][,c("BIG_CD", "business", "employment")], all.x=T, sort=F, by="BIG_CD")
  names(totals) <- c("BIG_CD", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN")
  totals <- merge(totals, nation[,c("BIG_CD", "business", "employment")], all.x=T, sort=F, by="BIG_CD")
  names(totals) <- c("BIG_CD", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN", "business_ENG", "employment_ENG")
  totals$business <- totals$business_LCL
  totals$business_LCL <- totals$business_LCL/sum(totals$business_LCL)
  totals$business_RGN <- totals$business_RGN/sum(totals$business_RGN)
  totals$business_ENG <- totals$business_ENG/sum(totals$business_ENG)
  totals$employment <- totals$employment_LCL
  totals$employment_LCL <- totals$employment_LCL/sum(totals$employment_LCL)
  totals$employment_RGN <- totals$employment_RGN/sum(totals$employment_RGN)
  totals$employment_ENG <- totals$employment_ENG/sum(totals$employment_ENG)
  
  totals.B <- totals[order(-totals$business), c("BIG_CD", "business", "business_LCL", "business_RGN", "business_ENG")]
  totals.B$x.coord <- rev(1:length(totals.B$BIG_CD))
  totals.B$y.coord <- 0
  totals.B$label <- paste0("N = ", format(totals.B$business, big.mark=","))
  totals.B <- merge(totals.B, unique(SIC.lookup[,c("BIG_CD", "BIG_NM", "Colour")]), all.x=T, sort=F)
  totals.B <- totals.B[totals.B$business >= 10,]
  totals.B <- totals.B[!is.na(totals.B$BIG_NM) & !is.na(totals.B$BIG_CD),]
  totals.B <- totals.B[totals.B$BIG_NM!="NA" & totals.B$BIG_CD!="NA",]
  totals.B$BIG_CD <- factor(totals.B$BIG_CD, levels=totals.B$BIG_CD)
  
  fig.1A <- ggplot() +
    geom_col(data=totals.B, aes(x=totals.B$BIG_CD, y=totals.B$business/sum(totals.B$business), fill=totals.B$BIG_CD), alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_ENG, pch=1, colour="black", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_ENG, pch="|", colour="black", stroke=1.5, size=3, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_RGN, pch=1, colour="white", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_RGN, pch="|", colour="white", stroke=1.5, size=3, alpha=0.5) +
    coord_flip() +
    scale_x_discrete(expand = c(0,0), labels=rev(totals.B$BIG_NM), limits=rev(levels(totals.B$BIG_CD))) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0, ceiling(scales[scales$LAD_CD==G,]$B.max*100)/100)) +    
    labs(title=paste0("Registered Businesses"),
         subtitle=paste0("Division by SIC Broad Industry Group (", "Total: ", format(sum(totals.B$business), big.mark=","), ")")) +
    geom_text(data=totals.B, x=totals.B$x.coord, y=totals.B$y.coord, aes(label=totals.B$label), alpha=0.4, fontface="italic", hjust=0) +
    scale_fill_manual(values=totals.B$Colour, aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none",
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB")) 
  
  totals.E <- totals[order(-totals$employment), c("BIG_CD", "employment", "employment_LCL", "employment_RGN", "employment_ENG")]
  totals.E$x.coord <- rev(1:length(totals.E$BIG_CD))
  totals.E$y.coord <- 0
  totals.E$label <- paste0("N = ", format(totals.E$employment, big.mark=","))
  totals.E <- merge(totals.E, unique(SIC.lookup[,c("BIG_CD", "BIG_NM", "Colour")]), all.x=T, sort=F)
  totals.E <- totals.E[totals.E$employment >= 10, ]
  totals.E <- totals.E[!is.na(totals.E$BIG_NM) & !is.na(totals.E$BIG_CD),]
  totals.E <- totals.E[totals.E$BIG_NM!="NA" & totals.E$BIG_CD!="NA",]
  totals.E$BIG_CD <- factor(totals.E$BIG_CD, levels=totals.E$BIG_CD)
  
  fig.1B <- ggplot() +
    geom_col(data=totals.E, aes(x=totals.E$BIG_CD, y=totals.E$employment/sum(totals.E$employment), fill=totals.E$BIG_CD), alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_ENG, pch=1, colour="black", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_ENG, pch="|", colour="black", stroke=1.5, size=3, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_RGN, pch=1, colour="white", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_RGN, pch="|", colour="white", stroke=1.5, size=3, alpha=0.5) +
    coord_flip() +
    scale_x_discrete(expand = c(0,0), labels=rev(totals.E$BIG_NM), limits=rev(levels(totals.E$BIG_CD))) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0, ceiling(scales[scales$LAD_CD==G,]$E.max*100)/100)) +    
    labs(title=paste0("Registered Employment"),
         subtitle=paste0("Division SIC Broad Industry Group (", "Total: ", format(sum(totals.E$employment), big.mark=","), ")")) +
    geom_text(data=totals.E, x=totals.E$x.coord, y=totals.E$y.coord, aes(label=totals.E$label), alpha=0.4, fontface="italic", hjust=0) +
    scale_fill_manual(values=totals.E$Colour, aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none", 
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB")) 
  return(list(fig.1A, fig.1B))
})
fig.1A_BUS <- lapply(fig.1, function(x) x[[1]])
names(fig.1A_BUS) <- local.profiles

fig.1A_EMP <- lapply(fig.1, function(x) x[[2]])
names(fig.1A_EMP) <- local.profiles
#----------------------------------------------------------------------------

# (BSD LAD-2020) TOP 20 2-DIGIT SIC DIVISION OF BUSINESS / EMPLOYMENT
#----------------------------------------------------------------------------
# - Uses the most recent (2020) BSD for business and employment counts.
# - This figure compares the breakdown of employment and business within an LAD
#    by broad SIC code. Compareable proporitions against regional and national 
#    breakdowns are also provided.
# - This figure should give some indication on the relative prevalence of workforce
#    typology LAD by LAD.

fig.1 <- BSD.sic2[["LAD"]][,c("sic.division", "LAD_CD", "RGN_CD", "business", "employment", "snapshot")]
fig.1 <- fig.1[fig.1$snapshot==2020,]

nation <- fig.1[,list(business=sum(business), employment=sum(employment)), by=sic.division]
region <- lapply(split(fig.1, fig.1$RGN_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=sic.division] })
local <- lapply(split(fig.1, fig.1$LAD_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=sic.division] })

fig.1 <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  
  totals <- merge(local[[names.lookup$LAD_CD]], region[[names.lookup$RGN_CD]][,c("sic.division", "business", "employment")], all.x=T, sort=F, by="sic.division")
  names(totals) <- c("sic.division", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN")
  totals <- merge(totals, nation[,c("sic.division", "business", "employment")], all.x=T, sort=F, by="sic.division")
  names(totals) <- c("SIC", "business_LCL", "employment_LCL", "business_RGN", "employment_RGN", "business_ENG", "employment_ENG")
  totals$business <- totals$business_LCL
  totals$business_LCL <- totals$business_LCL/sum(totals$business_LCL)
  totals$business_RGN <- totals$business_RGN/sum(totals$business_RGN)
  totals$business_ENG <- totals$business_ENG/sum(totals$business_ENG)
  totals$employment <- totals$employment_LCL
  totals$employment_LCL <- totals$employment_LCL/sum(totals$employment_LCL)
  totals$employment_RGN <- totals$employment_RGN/sum(totals$employment_RGN)
  totals$employment_ENG <- totals$employment_ENG/sum(totals$employment_ENG)
  
  totals.B <- totals[order(-totals$business), c("SIC", "business", "business_LCL", "business_RGN", "business_ENG")]
  totals.B <- merge(totals.B, unique(SIC.lookup[,c("Division_CD", "BIG_CD", "BIG_NM", "Division_NM", "Colour")]), all.x=T, sort=F, by.x="SIC", by.y="Division_CD")
  totals.B <- totals.B[!is.na(totals.B$BIG_NM) & !is.na(totals.B$BIG_CD),]
  totals.B <- totals.B[totals.B$BIG_NM!="NA" & totals.B$BIG_CD!="NA",]
  totals.B <- totals.B[1:20,]
  totals.B <- totals.B[totals.B$business >= 10, ]
  totals.B$x.coord <- rev(1:length(totals.B$SIC))
  totals.B$y.coord <- 0
  totals.B$label <- paste0(totals.B$BIG_NM, " (N = ", format(totals.B$business, big.mark=","), ")")
  totals.B$SIC <- factor(totals.B$SIC, levels=totals.B$SIC)
  totals.B$BIG_NM <- factor(totals.B$BIG_NM, levels=unique(totals.B$BIG_NM))
  totals.B$label <- factor(totals.B$label, levels=unique(totals.B$label))

  fig.1A <- ggplot() +
    geom_col(data=totals.B, aes(x=totals.B$SIC, y=totals.B$business_LCL, fill=totals.B$SIC), alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_ENG, pch=1, colour="black", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_ENG, pch="|", colour="black", stroke=1.5, size=3, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_RGN, pch=1, colour="white", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.B, x=totals.B$x.coord, y=totals.B$business_RGN, pch="|", colour="white", stroke=1.5, size=3, alpha=0.5) +
    coord_flip() +
    scale_x_discrete(expand = c(0,0), labels=rev(totals.B$Division_NM), limits=rev(levels(totals.B$SIC))) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0, ceiling(scales[scales$LAD_CD==G,]$B.max*100)/100)) +    
    labs(subtitle=paste0("Top 20 SIC Division (2-Digit) Industry Sectors (", "Total: ", format(sum(totals.B$business), big.mark=","), ")"), 
         y=paste0("Proportion Distribution Comparison w. England (Black) and ", names.lookup$RGN_NM, " Region (White)"),
         caption=paste0("Source: ONS SRS - Business Structure Dataset 2020")) +
    geom_text(data=totals.B, x=totals.B$x.coord, y=totals.B$y.coord, aes(label=totals.B$label), alpha=0.3, fontface="italic", hjust=0) +
    scale_fill_manual(values=totals.B$Colour, aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), legend.position="none", plot.caption=element_text(face="italic"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"), plot.title = element_blank())
  
  
  totals.E <- totals[order(-totals$employment), c("SIC", "employment", "employment_LCL", "employment_RGN", "employment_ENG")]
  totals.E <- merge(totals.E, unique(SIC.lookup[,c("Division_CD", "BIG_CD", "BIG_NM", "Division_NM", "Colour")]), all.x=T, sort=F, by.x="SIC", by.y="Division_CD")
  totals.E <- totals.E[!is.na(totals.E$BIG_NM) & !is.na(totals.E$BIG_CD),]
  totals.E <- totals.E[totals.E$BIG_NM!="NA" & totals.E$BIG_CD!="NA",]
  totals.E <- totals.E[1:20,]
  totals.E <- totals.E[totals.E$employment >= 10, ]
  totals.E$x.coord <- rev(1:length(totals.E$SIC))
  totals.E$y.coord <- 0
  totals.E$label <- paste0(totals.E$BIG_NM, " (N = ", format(totals.E$employment, big.mark=","), ")")
  totals.E$SIC <- factor(totals.E$SIC, levels=totals.E$SIC)
  totals.E$BIG_NM <- factor(totals.E$BIG_NM, levels=unique(totals.E$BIG_NM))
  totals.E$label <- factor(totals.E$label, levels=unique(totals.E$label))

  fig.1B <- ggplot() +
    geom_col(data=totals.E, aes(x=totals.E$SIC, y=totals.E$employment_LCL, fill=totals.E$SIC), alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_ENG, pch=1, colour="black", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_ENG, pch="|", colour="black", stroke=1.5, size=3, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_RGN, pch=1, colour="white", stroke=1, size=2, alpha=0.5) +
    geom_point(data=totals.E, x=totals.E$x.coord, y=totals.E$employment_RGN, pch="|", colour="white", stroke=1.5, size=3, alpha=0.5) +
    coord_flip() +
    scale_x_discrete(expand = c(0,0), labels=rev(totals.E$Division_NM), limits=rev(levels(totals.E$SIC))) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent, limits=c(0, ceiling(scales[scales$LAD_CD==G,]$E.max*100)/100)) +    
    labs(subtitle=paste0("Top 20 SIC Division (2-Digit) Industry Sectors (", "Total: ", format(sum(totals.E$employment), big.mark=","), ")"), 
         y=paste0("Proportion Distribution Comparison w. England (Black) and ", names.lookup$RGN_NM, " Region (White)"),
         caption=paste0("Source: ONS SRS - Business Structure Dataset 2020")) +
    geom_text(data=totals.E, x=totals.E$x.coord, y=totals.E$y.coord, aes(label=totals.E$label), alpha=0.3, fontface="italic", hjust=0) +
    scale_fill_manual(values=totals.E$Colour, aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), legend.position="none", plot.caption=element_text(face="italic"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"), plot.title = element_blank()) 
  return(list(fig.1A, fig.1B))
})
fig.1B_BUS <- lapply(fig.1, function(x) x[[1]])
names(fig.1B_BUS) <- local.profiles

fig.1B_EMP <- lapply(fig.1, function(x) x[[2]])
names(fig.1B_EMP) <- local.profiles
rm(fig.1, local, nation, region)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 2: COMPARABLE PROPORTIONS AMONG INDUSTRY SECTORS
###

# (BRES LAD-2019)  COMPARISON OF PROPORTIONS
#----------------------------------------------------------------------------
# SIC comparison of binary proportions: full time; part time; status; SME...
# BSD LAD-2014:2018) INDUSTRY 1, 5, 10 YEAR SURVIVAL RATES

fig.2 <- BRES.Counts[["LAD"]][,c("BIG_CD", "LAD_CD", "RGN_CD", "business", "N.small", "weighted_ftempee", 
  "weighted_ptempee", "weighted_totempee", "NE.small", "private", "NE.private", "snapshot")]
fig.2 <- fig.2[fig.2$snapshot==2019,]

local <- lapply(split(fig.2, fig.2$LAD_CD), function(x){
  data.table(x)[, .(business = sum(business), N.small = sum(N.small), weighted_ftempee = sum(weighted_ftempee), 
    weighted_ptempee = sum(weighted_ptempee), weighted_totempee = sum(weighted_totempee), 
    NE.small = sum(NE.small), private = sum(private), NE.private = sum(NE.private)), by = BIG_CD]})
local <- lapply(local, function(x){
  x[,.(business = sum(business), N.small = N.small/business*100, private = private/business*100, 
    weighted_ftempee = weighted_ftempee/weighted_totempee*100, weighted_ptempee = weighted_ptempee/weighted_totempee*100, weighted_totempee = sum(weighted_totempee),
    NE.small = NE.small/weighted_totempee*100, NE.private = NE.private/weighted_totempee*100), by = BIG_CD]})

fig.2 <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])

  total.LCL <- local[[names.lookup$LAD_CD]]
  total.LCL$B.label <- format(total.LCL$business, big.mark=",")
  total.LCL$E.label <- format(round(total.LCL$weighted_totempee, digits=0), big.mark=",")
  total.LCL$business <- total.LCL$business/max(total.LCL$business)*100
  total.LCL$weighted_totempee <- total.LCL$weighted_totempee/max(total.LCL$weighted_totempee)*100
  
  t <- total.LCL[,c("BIG_CD", "business", "weighted_totempee", "B.label", "E.label")]
  t1 <- t[,c("BIG_CD", "business", "B.label")]
  names(t1) <- c("BIG_CD", "value", "label2")
  t1$variable <- "business"
  t2 <- t[,c("BIG_CD", "weighted_totempee", "E.label")]
  names(t2) <- c("BIG_CD", "value", "label2")
  t2$variable <- "weighted_totempee"
  t <- rbind(t1, t2); rm(t1, t2)
  
  total.LCL <- melt(total.LCL[,-c("B.label", "E.label")], id.vars = "BIG_CD")
  total.LCL$label <- as.character(paste0(round(total.LCL$value, digits=2), "%"))
  total.LCL <- merge(total.LCL, unique(SIC.lookup[,c("BIG_CD", "BIG_NM", "Colour")]), all.x=T, sort=F, by="BIG_CD")
  
  total.LCL <- merge(total.LCL, t, all.x=T, sort=F, by=c("BIG_CD", "value", "variable"))
  total.LCL$label[!is.na(total.LCL$label2)] <- total.LCL$label2[!is.na(total.LCL$label2)]
  total.LCL$label2 <- NULL
  total.LCL$label[total.LCL$label=="NA%"] <- "-"
  
  total.LCL$BIG_CD <- factor(total.LCL$BIG_CD, levels=total.LCL[total.LCL$variable=="business", ][order(total.LCL[total.LCL$variable=="business",]$value),]$BIG_CD)
  total.LCL$variable <- factor(total.LCL$variable, levels=c("business", "N.small", "private", "weighted_totempee", "weighted_ftempee", "weighted_ptempee", "NE.small", "NE.private"))
  levels(total.LCL$variable)[levels(total.LCL$variable)=="business"] <- "Business Count"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="N.small"] <- "% Small Business (< 50)"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="private"] <- "% Private Business"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="weighted_totempee"] <- "Total Employment"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="weighted_ftempee"] <- "% Full-time Employment"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="weighted_ptempee"] <- "% Part-time Employment"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="NE.small"] <- "% Small Business Employment"
  levels(total.LCL$variable)[levels(total.LCL$variable)=="NE.private"] <- "% Private Business Employment"
  
  fig.2 <- ggplot(total.LCL, aes(x=variable, y=BIG_CD, colour=BIG_CD, fill=BIG_CD, size=value)) +
    geom_point(alpha=0.5) +
    geom_text(aes(label=label), colour="black", size=3) +
    scale_y_discrete(labels=unique(total.LCL[,c("BIG_CD", "BIG_NM")])[order(unique(total.LCL[,c("BIG_CD", "BIG_NM")])$BIG_CD),]$BIG_NM) +
    scale_size_continuous(range=c(5, 15)) +
    labs(title=paste0("Business & Workforce Structure"),
         subtitle=paste0("Breakdowns by SIC Broad Industry Group"), caption=paste0("Source: ONS SRS - Business Registry and Employment Survey 2019")) +
    scale_colour_manual(values=unique(total.LCL[,c("BIG_CD", "Colour")])[order(unique(total.LCL[,c("BIG_CD", "Colour")])$BIG_CD),]$Colour) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank(), legend.position="none",
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"), plot.caption=element_text(face="italic"))
  return(fig.2)
})
names(fig.2) <- local.profiles
rm(local)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 3: BUSINESS & EMPLOYMENT GROWTH OVER TIME 
###

# (BSD LAD-2014:2020) 
#----------------------------------------------------------------------------
# - Evolution of totals by SIC over time

fig.3 <- BSD.BIG[["LAD"]][,c("BIG_CD", "LAD_CD", "RGN_CD", "business", "employment", "snapshot")]

nation <- fig.3[,list(business=sum(business), employment=sum(employment)), by=.(snapshot)]
region <- lapply(split(fig.3, fig.3$RGN_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=.(snapshot)]})
local <- lapply(split(fig.3, fig.3$LAD_CD), function(x) { x[,list(business=sum(business), employment=sum(employment)), by=.(BIG_CD, snapshot)]})

fig.3 <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  
  LAD.plot <- local[[names.lookup$LAD_CD]]
  LAD.plot <- LAD.plot[,c("BIG_CD", "snapshot", "business")]
  LAD.plot <- merge(LAD.plot, unique(SIC.lookup[,c("BIG_CD", "Colour")]), all.x=T, sort=F, by="BIG_CD")
  LAD.plot$BIG_CD <- factor(LAD.plot$BIG_CD, levels=LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "business")][order(LAD.plot[LAD.plot$snapshot==2020, ]$business),]$BIG_CD)
  
  LAD.line <- LAD.plot[,list(business=sum(business)), by=.(snapshot)]
  RGN.avg <- region[[names.lookup$RGN_CD]]
  RGN.avg <- data.frame(snapshot=c(2014:2020), business=(RGN.avg$business/RGN.avg$business[1])*LAD.line$business[1])
  ENG.avg <- data.frame(snapshot=c(2014:2020), business=(nation$business/nation$business[1])*LAD.line$business[1])
  
  fig.3A <- ggplot() +
    geom_bar(data=LAD.plot, mapping=aes(x=snapshot, y=business, fill=BIG_CD), width=0.3, stat="identity", alpha=0.4) +
    geom_line(data=RGN.avg, mapping=aes(x=as.numeric(snapshot), y=business), colour="white", alpha=1) +
    geom_point(data=RGN.avg, mapping=aes(x=as.numeric(snapshot), y=business), colour="white", alpha=1) +
    geom_line(data=ENG.avg, mapping=aes(x=as.numeric(snapshot), y=business), colour="black", alpha=1) +
    geom_point(data=ENG.avg, mapping=aes(x=as.numeric(snapshot), y=business), colour="black", alpha=1) +
    geom_line(data=LAD.line, mapping=aes(x=as.numeric(snapshot), y=business), colour="black", alpha=0.3, linetype="dashed") +
    geom_point(data=LAD.line, mapping=aes(x=as.numeric(snapshot), y=business), colour="black", alpha=0.3) +
    scale_x_continuous(expand = c(0,0), breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020), labels=c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
    scale_y_continuous(expand = c(0,0), limits = c(0, ceiling(max(max(LAD.line$business, na.rm=T), max(RGN.avg$business, na.rm=T), max(ENG.avg$business, na.rm=T))*1.05)), labels=function(x)x/1000) +    
    scale_fill_manual(values=LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "business", "Colour")][order(LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "business", "Colour")]$business)]$Colour, 
                      aesthetics=c("fill")) +
    labs(title=paste0("Stock Growth 2014-2020"),
         subtitle=paste0("Comparison w. England (Black) and ", names.lookup$RGN_NM, " Region (white)"), y="Counts (1,000)",
         caption="", x="Registered Business") +
    theme_classic() +
    theme(legend.position="none", plot.caption=element_text(face="italic"), 
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"))
  
  LAD.plot <- local[[names.lookup$LAD_CD]]
  LAD.plot <- LAD.plot[,c("BIG_CD", "snapshot", "employment")]
  LAD.plot <- merge(LAD.plot, unique(SIC.lookup[,c("BIG_CD", "Colour")]), all.x=T, sort=F, by="BIG_CD")
  LAD.plot$BIG_CD <- factor(LAD.plot$BIG_CD, levels=LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "employment")][order(LAD.plot[LAD.plot$snapshot==2020, ]$employment),]$BIG_CD)
  
  LAD.line <- LAD.plot[,list(employment=sum(employment)), by=.(snapshot)]
  RGN.avg <- region[[names.lookup$RGN_CD]]
  RGN.avg <- data.frame(snapshot=c(2014:2020), employment=(RGN.avg$employment/RGN.avg$employment[1])*LAD.line$employment[1])
  ENG.avg <- data.frame(snapshot=c(2014:2020), employment=(nation$employment/nation$employment[1])*LAD.line$employment[1])
  
  fig.3B <- ggplot() +
    geom_bar(data=LAD.plot, mapping=aes(x=snapshot, y=employment, fill=BIG_CD), width=0.3, stat="identity", alpha=0.4) +
    geom_line(data=RGN.avg, mapping=aes(x=as.numeric(snapshot), y=employment), colour="white", alpha=1) +
    geom_point(data=RGN.avg, mapping=aes(x=as.numeric(snapshot), y=employment), colour="white", alpha=1) +
    geom_line(data=ENG.avg, mapping=aes(x=as.numeric(snapshot), y=employment), colour="black", alpha=1) +
    geom_point(data=ENG.avg, mapping=aes(x=as.numeric(snapshot), y=employment), colour="black", alpha=1) +
    geom_line(data=LAD.line, mapping=aes(x=as.numeric(snapshot), y=employment), colour="black", alpha=0.3, linetype="dashed") +
    geom_point(data=LAD.line, mapping=aes(x=as.numeric(snapshot), y=employment), colour="black", alpha=0.3) +
    scale_x_continuous(expand = c(0,0), breaks = c(2014, 2015, 2016, 2017, 2018, 2019, 2020), labels=c("2014", "2015", "2016", "2017", "2018", "2019", "2020")) +
    scale_y_continuous(expand = c(0,0), limits = c(0, ceiling(max(max(LAD.line$employment, na.rm=T), max(RGN.avg$employment, na.rm=T), max(ENG.avg$employment, na.rm=T))*1.05)), labels=function(x)x/1000) +    
    scale_fill_manual(values=LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "employment", "Colour")][order(LAD.plot[LAD.plot$snapshot==2020, c("BIG_CD", "employment", "Colour")]$employment)]$Colour, 
                      aesthetics=c("fill")) +
    labs(title="",
         subtitle="", y="Counts (1,000)", x="Registered Employment",
         caption=paste0("Source: ONS SRS - Business Structure Dataset 2014-2020")) +
    theme_classic() +
    theme(legend.position="none", plot.caption=element_text(face="italic"),
          panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"))
  
  return(list(fig.3A, fig.3B))
})
fig.3A <- lapply(fig.3, function(x) x[[1]])
names(fig.3A) <- local.profiles

fig.3B <- lapply(fig.3, function(x) x[[2]])
names(fig.3B) <- local.profiles
rm(fig.3, local, nation, region)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 4: SECTOR INFECTION RATES - WEEKLY (7-DAY) HEATMAP {DAILY (14-DAY)}
###

# (CIS 2021) 
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 5: EMPLOYMENT DENSITIES AND RANKINGS
###

# (BSD Densities) GET PEAK DENSITY AND EMPLOYMENT RANKINGS
#----------------------------------------------------------------------------
peak.x <- split(BSD.Dens[["LAD"]], paste0(BSD.Dens[["LAD"]]$LAD_CD, "-", BSD.Dens[["LAD"]]$variable))
peak.x <- lapply(peak.x, function(d) d[which.max(d$y.val),]$x.val)
peak.x <- as.data.frame(do.call(rbind, peak.x))
peak.x$LAD_CD <- as.character(do.call(rbind, lapply(strsplit(rownames(peak.x), "-"), function(x) x[1])))
peak.x$variable <- as.character(do.call(rbind, lapply(strsplit(rownames(peak.x), "-"), function(x) x[2])))
names(peak.x) <- c("peak.x", "LAD_CD", "variable")
rownames(peak.x) <- NULL

peak.x <- peak.x[peak.x$variable=="Employment", c("peak.x", "LAD_CD")]

Emp.sum <- BSD.BIG[["LAD"]][BSD.BIG[["LAD"]]$snapshot==2020, c("LAD_CD", "employment", "snapshot")]
Emp.sum <- Emp.sum[,list(employment=sum(employment)),by=LAD_CD]
Emp.rank <- merge(peak.x, Emp.sum, all.x=T, sort=F, by="LAD_CD")
Emp.rank$peak.x[Emp.rank$peak.x<0] <- 0

DENS.xtr <- peak.x[c(which.max(peak.x$peak.x), which.min(peak.x$peak.x), which.min(abs((peak.x$peak.x-mean(peak.x$peak.x)))),
  which.min(abs((peak.x$peak.x-median(peak.x$peak.x))))),]$LAD_CD
TOTL.xtr <- Emp.rank[c(which.max(Emp.rank$employment), which.min(Emp.rank$employment), which.min(abs((Emp.rank$employment-mean(Emp.rank$employment)))),
  which.min(abs((Emp.rank$employment-median(Emp.rank$employment))))),]$LAD_CD
#----------------------------------------------------------------------------

# (BSD LAD / Densities) DENSITY RANKING PLOTS
#----------------------------------------------------------------------------
fig.5 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_NM", "RGN_CD")])
  
  d1 <- ggplot(Emp.rank, aes(x=peak.x, y=reorder(LAD_CD, peak.x))) +
    geom_segment(aes(x=0, xend=peak.x, y=reorder(LAD_CD, peak.x), yend=reorder(LAD_CD, peak.x)), colour="white", alpha=0.3) +
    geom_point(colour="white", size=2, alpha=0.3) +
    geom_segment(data=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,], aes(x=rep(0,4), xend=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$peak.x,
      y=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$LAD_CD, yend=Emp.rank[which(Emp.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colours=palette[[1]][palette[[1]]$Colour=="Red",]$Col, alpha=0.4) +
    geom_point(data=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,], aes(x=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$peak.x, 
      y=Emp.rank[which(Emp.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colour=palette[[1]][palette[[1]]$Colour=="Red",]$Col, size=2, alpha=0.4) +
    geom_segment(aes(x=0, xend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$peak.x, y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6) +
    geom_segment(aes(x=0, xend=peak.x, y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6, linetype="dashed") +
    geom_point(data=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,], aes(x=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$peak.x, 
      y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", size=2, alpha=0.6) +    
    scale_y_discrete(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    labs(title="", caption="\n", subtitle="LAD Density Rank", x="(log) Density") +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      legend.position="none", plot.caption=element_text(face="italic"), panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"))
    
  d2 <- ggplot(Emp.rank, aes(x=employment, y=reorder(LAD_CD, employment))) +
    geom_segment(aes(x=0, xend=employment, y=reorder(LAD_CD, employment), yend=reorder(LAD_CD, employment)), colour="white", alpha=0.3) +
    geom_point(colour="white", size=2, alpha=0.3) +
    geom_segment(data=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,], aes(x=rep(0,4), xend=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$employment,
      y=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$LAD_CD, yend=Emp.rank[which(Emp.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colours=palette[[1]][palette[[1]]$Colour=="Red",]$Col, alpha=0.4) +
    geom_point(data=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,], aes(x=Emp.rank[Emp.rank$LAD_CD %in% DENS.xtr,]$employment, 
      y=Emp.rank[which(Emp.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colour=palette[[1]][palette[[1]]$Colour=="Red",]$Col, size=2, alpha=0.4) +
    geom_segment(aes(x=0, xend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$employment, y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6) +
    geom_segment(aes(x=0, xend=employment, y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6, linetype="dashed") +
    geom_point(data=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,], aes(x=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$employment, 
      y=Emp.rank[Emp.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", size=2, alpha=0.6) +    
    scale_y_discrete(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0), labels=function(x)x/1000) +
    labs(title="", caption="OA Job Density Distributions\nSource: ONS SRS - Business Structure Dataset 2020", subtitle="LAD Jobs Rank", x="Jobs (1,000)") +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      legend.position="none", plot.caption=element_text(face="italic"), panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"))
  return(list(d1, d2)) })
fig.5A <- lapply(fig.5, function(x) x[[1]])
names(fig.5A) <- local.profiles

fig.5B <- lapply(fig.5, function(x) x[[2]])
names(fig.5B) <- local.profiles
#----------------------------------------------------------------------------

# (BSD Densities) DENSITY DISTRIBUTION PLOTS
#----------------------------------------------------------------------------
fig.5 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_NM", "RGN_CD")])
  t1 <- BSD.Dens[["LAD"]][BSD.Dens[["LAD"]]$LAD_CD %in% names.lookup$LAD_CD & BSD.Dens[["LAD"]]$variable=="Employment",]
  t1$Zone <- "Local"
  t2 <- BSD.Dens[["LAD"]][BSD.Dens[["LAD"]]$LAD_CD %in% unique(geographic.lookup[geographic.lookup$RGN_CD==names.lookup$RGN_CD,]$LAD_CD) & BSD.Dens[["LAD"]]$variable=="Employment",]
  t2$Zone <- "Regional"
  t3 <- BSD.Dens[["LAD"]][BSD.Dens[["LAD"]]$LAD_CD %in% DENS.xtr & BSD.Dens[["LAD"]]$variable=="Employment",]
  t2$Zone <- "National"
  
  fig.5 <- ggplot() +
    geom_line(t2, mapping=aes(x=x.val, y=y.val, group=LAD_CD), colour="white", alpha=0.7) +
    geom_line(t3, mapping=aes(x=x.val, y=y.val, group=LAD_CD), colour=palette[[1]][palette[[1]]$Colour=="Red",]$Col, alpha=0.7) +
    geom_area(t1, mapping=aes(x=x.val, y=y.val), colour="black", alpha=0.6) +
    geom_line(t1, mapping=aes(x=x.val, y=y.val), colour="black", alpha=0.6) +
    geom_vline(xintercept = Emp.rank[Emp.rank$LAD_CD==G,]$peak.x, linetype="dotted", alpha=0.3) +
    scale_x_continuous(expand=c(0,0), limits=c(0, 12)) +
    scale_y_continuous(expand=c(0,0), limits=c(0, round(max(t2$y.val, na.rm=T)*1.1, digits=2))) +
    labs(title=paste0("Employment Density Distributions"),
      subtitle=paste0("Comparison with ", names.lookup$RGN_NM, " Region LADs (White) and national extremes (red)"), x="(log) Jobs per Hectare", y="Frequency", caption="\n") +
    annotate('text', y=0.98*(max(t2$y.val, na.rm=T)*1.1), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("~ Peak OA Job Density Distribution: ", round(exp(Emp.rank[Emp.rank$LAD_CD==G,]$peak.x), digits=2), " jobs per ha.")) +
    annotate('text', y=0.94*(max(t2$y.val, na.rm=T)*1.1), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Avg. Peak OA Job Density across LADs: ", round(exp(mean(Emp.rank$peak.x)), digits=2), " jobs per ha.")) +
    annotate('text', y=0.90*(max(t2$y.val, na.rm=T)*1.1), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Med. Peak OA Job Density across LADs: ", round(exp(median(Emp.rank$peak.x)), digits=2), " jobs per ha.")) +
    annotate('text', y=0.86*(max(t2$y.val, na.rm=T)*1.1), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Total Employment: ", format(Emp.rank[Emp.rank$LAD_CD==G,]$employment, big.mark=","))) +
    theme_classic() +
    theme(legend.position="none", panel.background = element_rect(fill="#EDEDEB", colour="#EDEDEB"), legend.title=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      plot.caption=element_text(face="italic"))
    return(fig.5)
})
names(fig.5) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 6: SMALL AREA BUSINESS DENSITY
###

# (Business Census 2020) 
#----------------------------------------------------------------------------
fig.6 <- merge(WZ.sp, BC.2020[,.(N=.N), by=c("WZ_CD")], all.x=T, sort=F)
fig.6$area_ha <- as.numeric(st_area(fig.6))*0.0001
fig.6$N[is.na(fig.6$N)] <- 0
fig.6$density <- fig.6$N/fig.6$area_ha

density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])
  q <- unique(fig.6[fig.6$LEP_CD1==names.lookup$LEP_CD1 | fig.6$LEP_CD2==names.lookup$LEP_CD1 ,])
  q$B.decile <- cut(q$density, breaks = unique(quantile(q$density, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$density, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  
  r <- ggplot() +
    geom_sf(data = q, aes(fill = B.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("#AF4242", 0.6), lwd = 1, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(palette[[1]][palette[[1]]$Colour=="Red",]$Col_L, palette[[1]][palette[[1]]$Colour=="Red",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title="Rank"),
      color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Business Density Deciles 2020"),
      subtitle=paste0("Within the broader ", names.lookup$LEP_NM1, " LEP"), x="Registered Business per (Workplace Zone) Hectare", caption="Source: CDRC Business Census 2020 (Companies House)") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background=element_rect(fill="#EDEDEB"), plot.caption = element_text(face = "italic"))
    return(r) })
names(density.plots) <- local.profiles
fig.6plot <- density.plots; rm(density.plots)
#----------------------------------------------------------------------------





#############################################################################
##### GENERATE INDUSTRY DENSITIES LOCAL PROFILE
###

# COMPILING LOCAL AUTHORITY DISTRICTS
#----------------------------------------------------------------------------
for(S in local.profiles){
  locality <- unique(geographic.lookup[geographic.lookup$LAD_CD==S, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CIS_CD", "CIS_NM")])
  
  rmarkdown::render(paste0(wd, "/Code/Industry Sector Densities/Industry_Densities_Template.Rmd"),
    output_file=paste0(S, "_IndustryDensities.html"), 
    output_dir=paste0(wd, "/Outputs/", gsub(" ", "_", locality$RGN_NM), "/LAD/", S, "/"))
}
#----------------------------------------------------------------------------

# COMPILING LOCAL ENTERPRISE PARTNERSHIPS
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------

# COMPILING COMBINED AUTHORITIES
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------