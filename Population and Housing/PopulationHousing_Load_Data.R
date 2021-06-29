#############################################################################
#############################################################################
###########  
#########    POPULATION AND HOUSING
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

# show_col(palette[[1]]$Col)
# show_col(palette[[1]]$Col_L)
# show_col(palette[[1]]$Col_H)
# show_col(palette[[2]])
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

# SIC.lookup <- fread(file=paste0(wd.data, "/Lookups/SIC_Lookup_X.csv"))
# SIC.lookup$Division_CD <- as.character(ifelse(nchar(SIC.lookup$Division_CD)==4, paste0("0", SIC.lookup$Division_CD), paste0(SIC.lookup$Division_CD)))

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

housing.lookup <- data.table(cbind(type=c("Total", "Detached", "Semi Detached", "Terraced", "Flat", "Other"),
  order=c(1, 2, 3, 4, 5, 6), colour=c("#000000", palette[[1]][palette[[1]]$Colour %in% c("Blue", "Red", "Yellow", "Green", "Purple")]$Col)))

# SOC.lookup <- fread(file=paste0(wd.data, "/Lookup/SOC_Lookup_X.csv"))
#----------------------------------------------------------------------------

# READ IN SPATIAL BOUNDARIES 
#----------------------------------------------------------------------------
# WZ.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Workplace_Zones_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp"), 
#   stringsAsFactors = F, quiet = T), crs=27700)[,c("wz11cd", "lad11cd")]
# names(WZ.sp)[names(WZ.sp)=="wz11cd"] <- "WZ_CD"
# names(WZ.sp)[names(WZ.sp)=="lad11cd"] <- "LAD_CD"
# WZ.sp <- merge(WZ.sp, unique(geographic.lookup[,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]), 
#   all.x=T, sort=F, by="LAD_CD")

LSOA.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/LSOA_2011_EW_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("LSOA11CD")]
names(LSOA.sp)[names(LSOA.sp)=="LSOA11CD"] <- "LSOA_CD"
LSOA.sp <- merge(LSOA.sp, unique(geographic.lookup[,c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]), 
  all.x=T, sort=F, by="LSOA_CD")
LSOA.sp$area_ha <- as.numeric(st_area(LSOA.sp))*0.0001

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
# CIS.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Covid_Infection_Survey__October_2020__EN_BFC.shp"), 
#   stringsAsFactors = F, quiet = T), crs=27700)[,c("CIS20CD")]
# CIS.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries/Covid_Infection_Survey__October_2020__EN_BFC.shp"), 
#   stringsAsFactors = F, quiet = T), crs=27700)[,c("CIS20CD")]
#----------------------------------------------------------------------------

# SET UP SUBSET OF LADs FOR BATCH GENERATING
#----------------------------------------------------------------------------
# TO EVENTUALLY RUN ON THE FULL SET OF LADs (OR LDS SPECIFIC)
#
# FIGURE 3 DOES NOT RUN WITH THESE LADs - THEY ARE PREVIOUS CODES AND NOT UPDATED

# set.seed(8675309)
# local.profiles <- unique(geographic.lookup[geographic.lookup$LAD_CD %in% local.profiles,c("LAD_CD", "RGN_NM")])[,.SD[sample(.N, min(2,.N))],by = RGN_NM]$LAD_CD
# local.profiles <- unique(geographic.lookup$LAD_CD)[!is.na(unique(geographic.lookup$LAD_CD))]
# local.profiles <- unique(LDS.lookup$LAD_CD)

local.profiles <- c(unique(LDS.lookup$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="Yorkshire and The Humber",]$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="East of England",]$LAD_CD))

local.profiles <- local.profiles[which(local.profiles %in% unique(geographic.lookup$LAD_CD))]

local.profiles <- local.profiles[!(local.profiles %in% 
  c("E06000053", "E06000029", "E07000051", "E07000049", "E07000205", "E07000190",
  "E06000028", "E07000201", "E07000206", "E07000204", "E07000053", "E07000052",
  "E07000191", "E07000191", "E07000048", "E07000050", "E07000008"))]
#----------------------------------------------------------------------------

#############################################################################
##### SECTION 2: LOAD IN AND FORMAT CLEANED (NON-DISCLOSIVE) DATASETS
###

# MID-YEAR POPULATION ESTIMATES
#----------------------------------------------------------------------------
MYPE.F <- fread(paste0(wd.data, "/Population/MYPE_19_F.csv"))
names(MYPE.F)[!(names(MYPE.F) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))] <- paste0("F_", names(MYPE.F)[!(names(MYPE.F) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))])
names(MYPE.F) <- gsub("\\+", "_", names(MYPE.F))
MYPE.F <- merge(MYPE.F, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="LAD_CD")
MYPE.F <- MYPE.F[!is.na(MYPE.F$RGN_CD),]
MYPE.F$F_19 <- as.numeric(as.character(MYPE.F$F_19))
MYPE.F$F_19[is.na(MYPE.F$F_19)] <- 0
MYPE.F[, population :=rowSums(.SD, na.rm = TRUE), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)]]

MYPE.M <- fread(paste0(wd.data, "/Population/MYPE_19_M.csv"))
names(MYPE.M)[!(names(MYPE.M) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))] <- paste0("M_", names(MYPE.M)[!(names(MYPE.M) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))])
names(MYPE.M) <- gsub("\\+", "_", names(MYPE.M))
MYPE.M <- merge(MYPE.M, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="LAD_CD")
MYPE.M <- MYPE.M[!is.na(MYPE.M$RGN_CD),]
MYPE.M$M_19 <- as.numeric(as.character(MYPE.M$M_19))
MYPE.M$M_19[is.na(MYPE.M$M_19)] <- 0
MYPE.M[, population :=rowSums(.SD, na.rm = TRUE), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)]]

MYPE.T <- fread(paste0(wd.data, "/Population/MYPE_19_T.csv"))
names(MYPE.T)[!(names(MYPE.T) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))] <- paste0("T_", names(MYPE.T)[!(names(MYPE.T) %in% c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM"))])
names(MYPE.T) <- gsub("\\+", "_", names(MYPE.T))
MYPE.T <- merge(MYPE.T, unique(geographic.lookup[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="LAD_CD")
MYPE.T <- MYPE.T[!is.na(MYPE.T$RGN_CD),]
MYPE.T$T_19 <- as.numeric(as.character(MYPE.T$T_19))
MYPE.T$T_19[is.na(MYPE.T$T_19)] <- 0
MYPE.T[, population :=rowSums(.SD, na.rm = TRUE), .SDcols = names(sapply(MYPE.T, is.numeric))[sapply(MYPE.T, is.numeric)]]
MYPE.T <- MYPE.T[,c("LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD", "LSOA_CD", "population")]
MYPE.T <- merge(MYPE.T, data.frame(LSOA.sp)[,c("LSOA_CD", "area_ha")], all.x=TRUE, sort=FALSE, by="LSOA_CD")
MYPE.T$pop.dens <- MYPE.T$population/MYPE.T$area_ha
#----------------------------------------------------------------------------

# LAND REGISTRY HOUSING DATA
#----------------------------------------------------------------------------
# uklr.pp <- fread(paste0(gsub("ADRUK Covid/Local Profiles", "Housing Indicators/Data", wd), "/pp-complete.csv"), header=FALSE, na.strings=c("","NA", "-", "- -", "."), stringsAsFactors=TRUE)
# colnames(uklr.pp) <- c("id", "price", "date", "postcode", "type", "new", "tenure", "paon", "saon",
#   "street", "locality", "town", "district", "county", "ppd", "status")
# uklr.pp <- subset(uklr.pp, as.Date(uklr.pp$date) <= as.Date("2018-12-31"))
# uklr.pp2019 <- fread(paste0(gsub("ADRUK Covid/Local Profiles", "Housing Indicators/Data", wd), "/pp-2019.csv"), header=FALSE, na.strings=c("","NA", "-", "- -", "."), stringsAsFactors=TRUE)
# colnames(uklr.pp2019) <- c("id", "price", "date", "postcode", "type", "new", "tenure", "paon", "saon",
#   "street", "locality", "town", "district", "county", "ppd", "status")
# uklr.pp2020 <- fread(paste0(gsub("ADRUK Covid/Local Profiles", "Housing Indicators/Data", wd), "/pp-2020.csv"), header=FALSE, na.strings=c("","NA", "-", "- -", "."), stringsAsFactors=TRUE)
# colnames(uklr.pp2020) <- c("id", "price", "date", "postcode", "type", "new", "tenure", "paon", "saon",
#   "street", "locality", "town", "district", "county", "ppd", "status")
# uklr.pp2021 <- fread(paste0(gsub("ADRUK Covid/Local Profiles", "Housing Indicators/Data", wd), "/pp-monthly-update-new-version.csv"), header=FALSE, na.strings=c("","NA", "-", "- -", "."), stringsAsFactors=TRUE)
# colnames(uklr.pp2021) <- c("id", "price", "date", "postcode", "type", "new", "tenure", "paon", "saon",
#   "street", "locality", "town", "district", "county", "ppd", "status")
# uklr.pp <- rbind(uklr.pp, uklr.pp2019)
# uklr.pp <- rbind(uklr.pp, uklr.pp2020)
# uklr.pp <- rbind(uklr.pp, uklr.pp2021)
# rm(uklr.pp2019, uklr.pp2020, uklr.pp2021); gc()
# 
# uklr.pp <- subset(uklr.pp, as.Date(uklr.pp$date) >= as.Date("2015-01-01"))
# 
# NSPL <- fread(gsub("Research/ADRUK Covid/Local Profiles", "Liverpool/GDSL/Lookups/NSPL_FEB_2020_UK.csv", wd))
# NSPL <- NSPL[,c("pcd", "oa11")]
# 
# uklr.pp$pc_match <- as.factor(gsub(" ", "", as.character(uklr.pp$postcode)))
# NSPL$pc_match <- as.factor(gsub(" ", "", as.character(NSPL$pcd)))
# 
# uklr.pp <- merge(uklr.pp, NSPL[,c("pc_match", "oa11")], by="pc_match", all.x=T, sort=F)
# rm(NSPL); gc()
#
# uklr.pp$date <- as.character(uklr.pp$date)
# uklr.pp$month <- as.Date(paste0(substr(uklr.pp$date, 1, 7), "-01"), format='%Y-%m-%d')
# uklr.pp$year <- as.Date(paste0(substr(uklr.pp$date, 1, 4), "-01-01"), format='%Y-%m-%d')
# uklr.pp$day <- as.Date(substr(uklr.pp$date, 1, 10), format='%Y-%m-%d')
# uklr.pp$quarter <- as.Date(gsub(paste(unlist(list("-02-", "-03-")), collapse = "|"), "-01-", 
#   gsub(paste(unlist(list("-05-", "-06-")), collapse = "|"), "-04-", 
#   gsub(paste(unlist(list("-08-", "-09-")), collapse = "|"), "-07-", 
#   gsub(paste(unlist(list("-11-", "-12-")), collapse = "|"), "-10-", as.character(uklr.pp$month))))), format='%Y-%m-%d')
# 
# uklr.pp$new <- ifelse(uklr.pp$new=="Y", 1, 0)
# 
# uklr.pp$type_detached <- ifelse(uklr.pp$type=="D", 1, 0)
# uklr.pp$type_flats <- ifelse(uklr.pp$type=="F", 1, 0)
# uklr.pp$type_other <- ifelse(uklr.pp$type=="O", 1, 0)
# uklr.pp$type_semi <- ifelse(uklr.pp$type=="S", 1, 0)
# uklr.pp$type_terraced <- ifelse(uklr.pp$type=="T", 1, 0)
# 
# uklr.pp$tenure_freehold <- ifelse(uklr.pp$tenure=="F", 1, 0)
# uklr.pp$tenure_leashold <- ifelse(uklr.pp$tenure=="L", 1, 0)
# 
# uklr.pp$type <- ifelse(uklr.pp$type=="D", "detached", 
#   ifelse(uklr.pp$type=="F", "flat", 
#   ifelse(uklr.pp$type=="O", "other", 
#   ifelse(uklr.pp$type=="S", "semi", 
#   ifelse(uklr.pp$type=="T", "terraced", NA)))))
# uklr.pp$tenure <- ifelse(uklr.pp$tenure=="F", "freehold", 
#   ifelse(uklr.pp$tenure=="L", "leashold", NA))
# 
# uklr.pp <- uklr.pp[,c("price", "oa11", "new", "month", "quarter", "year", 
#   "type", "type_detached", "type_flats", "type_other", "type_semi", "type_terraced", 
#   "tenure", "tenure_freehold", "tenure_leashold")]
# 
# fwrite(uklr.pp, file=paste0(wd.data, "/Housing/Price_Paid_2015_2021.csv")
uklr.pp <- fread(paste0(gsub("OneDrive - The University of Liverpool", "Dropbox/Research/ADRUK Covid", wd.data), "/Housing/Price_Paid_2015_2021.csv"))
uklr.pp <- merge(uklr.pp, unique(geographic.lookup[,c("OA_CD", "LSOA_CD", "LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by.x="oa11", by.y="OA_CD")
uklr.pp <- merge(uklr.pp, MYPE.T[,c("LSOA_CD", "pop.dens")], all.x=T, sort=F, by="LSOA_CD")
#----------------------------------------------------------------------------

# AFFORDABILITY - EXISTING
#----------------------------------------------------------------------------
# Gross.Income <- list()
# Gross.Income[[1]] <- fread(file=paste0(wd.data, "/Housing/LAD_Income.csv"))
# Gross.Income[[1]] <- merge(Gross.Income[[1]], unique(geographic.lookup[,c("LAD_NM", "LAD_CD", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="LAD_NM")
# Gross.Income[[2]] <- fread(file=paste0(wd.data, "/Housing/LEP_Income.csv"))
# Gross.Income[[2]] <- merge(Gross.Income[[2]], unique(geographic.lookup[,c("LEP_NM1", "LEP_CD1", "LEP_CD2", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="LEP_NM1")
# Gross.Income[[3]] <- fread(file=paste0(wd.data, "/Housing/CA_Income.csv"))
# Gross.Income[[3]] <- merge(Gross.Income[[3]], unique(geographic.lookup[,c("CA_NM", "CA_CD", "RGN_CD")]), all.x=T, sort=F, by="CA_NM")
# names(Gross.Income) <- c("LAD", "LEP", "CA")

affordability <- list()
affordability[[1]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "5a", range = "A7:W343"))
names(affordability[[1]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_M_", 2002:2020))
affordability[[1]] <- melt(affordability[[1]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[1]]$Variable <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[1]]))
affordability[[1]]$Quantile <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[2]]))
affordability[[1]]$Year <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[3]]))
affordability[[1]]$variable <- NULL
affordability[[2]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "5b", range = "A7:W343"))
names(affordability[[2]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_M_", 2002:2020))
affordability[[2]] <- melt(affordability[[2]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[2]]$Variable <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[1]]))
affordability[[2]]$Quantile <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[2]]))
affordability[[2]]$Year <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[3]]))
affordability[[2]]$variable <- NULL
affordability[[3]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "5c", range = "A7:W343"))
names(affordability[[3]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_M_", 2002:2020))
affordability[[3]] <- melt(affordability[[3]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[3]]$Variable <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[1]]))
affordability[[3]]$Quantile <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[2]]))
affordability[[3]]$Year <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[3]]))
affordability[[3]]$variable <- NULL
affordability[[4]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "6a", range = "A7:W343"))
names(affordability[[4]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_Q_", 2002:2020))
affordability[[4]] <- melt(affordability[[4]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[4]]$Variable <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[1]]))
affordability[[4]]$Quantile <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[2]]))
affordability[[4]]$Year <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[3]]))
affordability[[4]]$variable <- NULL
affordability[[5]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "6b", range = "A7:W343"))
names(affordability[[5]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_Q_", 2002:2020))
affordability[[5]] <- melt(affordability[[5]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[5]]$Variable <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[1]]))
affordability[[5]]$Quantile <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[2]]))
affordability[[5]]$Year <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[3]]))
affordability[[5]]$variable <- NULL
affordability[[6]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepriceexistingtoresidencebasedearnings.xlsx"), sheet = "6c", range = "A7:W343"))
names(affordability[[6]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_Q_", 2002:2020))
affordability[[6]] <- melt(affordability[[6]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[6]]$Variable <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[1]]))
affordability[[6]]$Quantile <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[2]]))
affordability[[6]]$Year <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[3]]))
affordability[[6]]$variable <- NULL

affordability.db <- rbind(affordability[[1]], affordability[[2]])
affordability.db <- rbind(affordability.db, affordability[[3]])
affordability.db <- rbind(affordability.db, affordability[[4]])
affordability.db <- rbind(affordability.db, affordability[[5]])
affordability.db <- rbind(affordability.db, affordability[[6]])

affordability.db$value <- as.numeric(as.character(affordability.db$value))
affordability.db$Year <- as.Date(paste0(as.character(affordability.db$Year), "-01-01"), format="%Y-%m-%d")
affordability.db$Variable[affordability.db$Variable=="Inc"] <- "Income"
affordability.db$Variable[affordability.db$Variable=="Prc"] <- "Price"
affordability.db$Variable[affordability.db$Variable=="Aff"] <- "Affordability"
affordability.db$Quantile[affordability.db$Quantile=="M"] <- "Median (50)"
affordability.db$Quantile[affordability.db$Quantile=="Q"] <- "Lower Quartile (25)"
rm(affordability)
affordability.db$Index <- "Existing Dwellings"
#----------------------------------------------------------------------------

# AFFORDABILITY - NEW
#----------------------------------------------------------------------------
affordability <- list()
affordability[[1]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "5a", range = "A7:W343"))
names(affordability[[1]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_M_", 2002:2020))
affordability[[1]] <- melt(affordability[[1]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[1]]$Variable <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[1]]))
affordability[[1]]$Quantile <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[2]]))
affordability[[1]]$Year <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[3]]))
affordability[[1]]$variable <- NULL
affordability[[2]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "5b", range = "A7:W343"))
names(affordability[[2]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_M_", 2002:2020))
affordability[[2]] <- melt(affordability[[2]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[2]]$Variable <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[1]]))
affordability[[2]]$Quantile <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[2]]))
affordability[[2]]$Year <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[3]]))
affordability[[2]]$variable <- NULL
affordability[[3]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "5c", range = "A7:W343"))
names(affordability[[3]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_M_", 2002:2020))
affordability[[3]] <- melt(affordability[[3]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[3]]$Variable <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[1]]))
affordability[[3]]$Quantile <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[2]]))
affordability[[3]]$Year <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[3]]))
affordability[[3]]$variable <- NULL
affordability[[4]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "6a", range = "A7:W343"))
names(affordability[[4]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_Q_", 2002:2020))
affordability[[4]] <- melt(affordability[[4]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[4]]$Variable <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[1]]))
affordability[[4]]$Quantile <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[2]]))
affordability[[4]]$Year <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[3]]))
affordability[[4]]$variable <- NULL
affordability[[5]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "6b", range = "A7:W343"))
names(affordability[[5]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_Q_", 2002:2020))
affordability[[5]] <- melt(affordability[[5]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[5]]$Variable <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[1]]))
affordability[[5]]$Quantile <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[2]]))
affordability[[5]]$Year <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[3]]))
affordability[[5]]$variable <- NULL
affordability[[6]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricenewlybuilttoresidencebasedearnings.xlsx"), sheet = "6c", range = "A7:W343"))
names(affordability[[6]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_Q_", 2002:2020))
affordability[[6]] <- melt(affordability[[6]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[6]]$Variable <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[1]]))
affordability[[6]]$Quantile <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[2]]))
affordability[[6]]$Year <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[3]]))
affordability[[6]]$variable <- NULL

affordability.db1 <- rbind(affordability[[1]], affordability[[2]])
affordability.db1 <- rbind(affordability.db1, affordability[[3]])
affordability.db1 <- rbind(affordability.db1, affordability[[4]])
affordability.db1 <- rbind(affordability.db1, affordability[[5]])
affordability.db1 <- rbind(affordability.db1, affordability[[6]])

affordability.db1$value <- as.numeric(as.character(affordability.db1$value))
affordability.db1$Year <- as.Date(paste0(as.character(affordability.db1$Year), "-01-01"), format="%Y-%m-%d")
affordability.db1$Variable[affordability.db1$Variable=="Inc"] <- "Income"
affordability.db1$Variable[affordability.db1$Variable=="Prc"] <- "Price"
affordability.db1$Variable[affordability.db1$Variable=="Aff"] <- "Affordability"
affordability.db1$Quantile[affordability.db1$Quantile=="M"] <- "Median (50)"
affordability.db1$Quantile[affordability.db1$Quantile=="Q"] <- "Lower Quartile (25)"
rm(affordability)
affordability.db1$Index <- "New Dwellings"
#----------------------------------------------------------------------------

# AFFORDABILITY - OVERALL
#----------------------------------------------------------------------------
affordability <- list()
affordability[[1]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "5a", range = "A7:W343"))
names(affordability[[1]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_M_", 2002:2020))
affordability[[1]] <- melt(affordability[[1]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[1]]$Variable <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[1]]))
affordability[[1]]$Quantile <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[2]]))
affordability[[1]]$Year <- do.call(c, lapply(str_split(affordability[[1]]$variable, "_"), function(x) x[[3]]))
affordability[[1]]$variable <- NULL
affordability[[2]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "5b", range = "A7:W343"))
names(affordability[[2]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_M_", 2002:2020))
affordability[[2]] <- melt(affordability[[2]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[2]]$Variable <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[1]]))
affordability[[2]]$Quantile <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[2]]))
affordability[[2]]$Year <- do.call(c, lapply(str_split(affordability[[2]]$variable, "_"), function(x) x[[3]]))
affordability[[2]]$variable <- NULL
affordability[[3]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "5c", range = "A7:W343"))
names(affordability[[3]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_M_", 2002:2020))
affordability[[3]] <- melt(affordability[[3]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[3]]$Variable <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[1]]))
affordability[[3]]$Quantile <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[2]]))
affordability[[3]]$Year <- do.call(c, lapply(str_split(affordability[[3]]$variable, "_"), function(x) x[[3]]))
affordability[[3]]$variable <- NULL
affordability[[4]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "6a", range = "A7:W343"))
names(affordability[[4]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Prc_Q_", 2002:2020))
affordability[[4]] <- melt(affordability[[4]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[4]]$Variable <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[1]]))
affordability[[4]]$Quantile <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[2]]))
affordability[[4]]$Year <- do.call(c, lapply(str_split(affordability[[4]]$variable, "_"), function(x) x[[3]]))
affordability[[4]]$variable <- NULL
affordability[[5]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "6b", range = "A7:W343"))
names(affordability[[5]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Inc_Q_", 2002:2020))
affordability[[5]] <- melt(affordability[[5]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[5]]$Variable <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[1]]))
affordability[[5]]$Quantile <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[2]]))
affordability[[5]]$Year <- do.call(c, lapply(str_split(affordability[[5]]$variable, "_"), function(x) x[[3]]))
affordability[[5]]$variable <- NULL
affordability[[6]] <- data.table(read_xlsx(paste0(wd.data, "/Housing/ratioofhousepricetoresidencebasedearnings.xlsx"), sheet = "6c", range = "A7:W343"))
names(affordability[[6]]) <- c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM", paste0("Aff_Q_", 2002:2020))
affordability[[6]] <- melt(affordability[[6]], id.vars=c("RGN_CD", "RGN_NM", "LAD_CD", "LAD_NM"))
affordability[[6]]$Variable <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[1]]))
affordability[[6]]$Quantile <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[2]]))
affordability[[6]]$Year <- do.call(c, lapply(str_split(affordability[[6]]$variable, "_"), function(x) x[[3]]))
affordability[[6]]$variable <- NULL

affordability.db2 <- rbind(affordability[[1]], affordability[[2]])
affordability.db2 <- rbind(affordability.db2, affordability[[3]])
affordability.db2 <- rbind(affordability.db2, affordability[[4]])
affordability.db2 <- rbind(affordability.db2, affordability[[5]])
affordability.db2 <- rbind(affordability.db2, affordability[[6]])

affordability.db2$value <- as.numeric(as.character(affordability.db2$value))
affordability.db2$Year <- as.Date(paste0(as.character(affordability.db2$Year), "-01-01"), format="%Y-%m-%d")
affordability.db2$Variable[affordability.db2$Variable=="Inc"] <- "Income"
affordability.db2$Variable[affordability.db2$Variable=="Prc"] <- "Price"
affordability.db2$Variable[affordability.db2$Variable=="Aff"] <- "Affordability"
affordability.db2$Quantile[affordability.db2$Quantile=="M"] <- "Median (50)"
affordability.db2$Quantile[affordability.db2$Quantile=="Q"] <- "Lower Quartile (25)"
rm(affordability)
affordability.db2$Index <- "Overall"
#----------------------------------------------------------------------------

# CLEANED AFFORDABILITY TABLES
#----------------------------------------------------------------------------
afford.price <- rbind(affordability.db[affordability.db$Variable=="Price",], 
  affordability.db1[affordability.db1$Variable=="Price",])
afford.price <- rbind(afford.price, affordability.db2[affordability.db2$Variable=="Price",])
afford.price$Variable <- NULL

afford.incm <- rbind(affordability.db[affordability.db$Variable=="Income",], 
  affordability.db1[affordability.db1$Variable=="Income",])
afford.incm <- rbind(afford.incm, affordability.db2[affordability.db2$Variable=="Income",])
afford.incm$Variable <- NULL
afford.incm <- unique(afford.incm[,-c("Index")])

afford.index <- rbind(affordability.db[affordability.db$Variable=="Affordability",], 
  affordability.db1[affordability.db1$Variable=="Affordability",])
afford.index <- rbind(afford.index, affordability.db2[affordability.db2$Variable=="Affordability",])
afford.index$Variable <- NULL
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 1: POPULATION PYRAMID
###

# (MYPE) GENERATE LOCAL, REGIONAL, NATIONAL DBs 
#----------------------------------------------------------------------------
national1 <- MYPE.F[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)]]
national1 <- as.data.table(cbind(colnames(national1), t(national1)))
names(national1) <- c("age", "Pop_ENG")
national1$age <- as.numeric(as.character(gsub("F", "", gsub("\\_", "", national1$age))))
national1$gender <- "Female"
national2 <- MYPE.M[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)]]
national2 <- as.data.table(cbind(colnames(national2), t(national2)))
names(national2) <- c("age", "Pop_ENG")
national2$age <- as.numeric(as.character(gsub("M", "", gsub("\\_", "", national2$age))))
national2$gender <- "Male"

national <- rbind(national1, national2); rm(national1, national2)
national$Pop_ENG <- as.numeric(as.character(national$Pop_ENG))/sum(as.numeric(as.character(national$Pop_ENG)))*100

regional1 <- MYPE.F[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)], by=list(RGN_CD)]
regional1 <- split(regional1, regional1$RGN_CD)
regional1 <- lapply(regional1, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
regional1 <- lapply(regional1, function(x) {x <- x[-c(1),]; return(x)})
regional1 <- lapply(regional1, function(x) {names(x) <- c("age", "Pop_RGN"); return(x)})
regional1 <- lapply(regional1, function(x) {x$age <- as.numeric(as.character(gsub("F", "", gsub("\\_", "", x$age)))); return(x)})
regional1 <- lapply(regional1, function(x) {x$gender <- "Female"; return(x) })
regional2 <- MYPE.M[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)], by=list(RGN_CD)]
regional2 <- split(regional2, regional2$RGN_CD)
regional2 <- lapply(regional2, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
regional2 <- lapply(regional2, function(x) {x <- x[-c(1),]; return(x)})
regional2 <- lapply(regional2, function(x) {names(x) <- c("age", "Pop_RGN"); return(x)})
regional2 <- lapply(regional2, function(x) {x$age <- as.numeric(as.character(gsub("M", "", gsub("\\_", "", x$age)))); return(x)})
regional2 <- lapply(regional2, function(x) {x$gender <- "Male"; return(x) })

regional <- lapply(as.list(names(regional1)), function(x) rbind(regional1[[which(names(regional1)==x)]], regional2[[which(names(regional2)==x)]]))
names(regional) <- names(regional1); rm(regional1, regional2)
regional <- lapply(regional, function(x) {x$Pop_RGN <- as.numeric(x$Pop_RGN)/sum(as.numeric(x$Pop_RGN))*100; return(x)})

LAD.1 <- MYPE.F[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)], by=list(LAD_CD)]
LAD.1 <- split(LAD.1, LAD.1$LAD_CD)
LAD.1 <- lapply(LAD.1, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
LAD.1 <- lapply(LAD.1, function(x) {x <- x[-c(1),]; return(x)})
LAD.1 <- lapply(LAD.1, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
LAD.1 <- lapply(LAD.1, function(x) {x$age <- as.numeric(as.character(gsub("F", "", gsub("\\_", "", x$age)))); return(x)})
LAD.1 <- lapply(LAD.1, function(x) {x$gender <- "Female"; return(x) })
LAD.2 <- MYPE.M[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)], by=list(LAD_CD)]
LAD.2 <- split(LAD.2, LAD.2$LAD_CD)
LAD.2 <- lapply(LAD.2, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
LAD.2 <- lapply(LAD.2, function(x) {x <- x[-c(1),]; return(x)})
LAD.2 <- lapply(LAD.2, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
LAD.2 <- lapply(LAD.2, function(x) {x$age <- as.numeric(as.character(gsub("M", "", gsub("\\_", "", x$age)))); return(x)})
LAD.2 <- lapply(LAD.2, function(x) {x$gender <- "Male"; return(x) })

LAD.dbs <- lapply(as.list(names(LAD.1)), function(x) rbind(LAD.1[[which(names(LAD.1)==x)]], LAD.2[[which(names(LAD.2)==x)]]))
names(LAD.dbs) <- names(LAD.1); rm(LAD.1, LAD.2)
LAD.dbs <- lapply(LAD.dbs, function(x) {x$Pop_LCL <- as.numeric(x$Pop_LCL)/sum(as.numeric(x$Pop_LCL))*100; return(x)})

LEP.1 <- MYPE.F[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)], by=list(LEP_CD1)]
LEP.1 <- split(LEP.1, LEP.1$LEP_CD1)
LEP.1 <- lapply(LEP.1, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
LEP.1 <- lapply(LEP.1, function(x) {x <- x[-c(1),]; return(x)})
LEP.1 <- lapply(LEP.1, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
LEP.1 <- lapply(LEP.1, function(x) {x$age <- as.numeric(as.character(gsub("F", "", gsub("\\_", "", x$age)))); return(x)})
LEP.1 <- lapply(LEP.1, function(x) {x$gender <- "Female"; return(x) })
LEP.2 <- MYPE.M[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)], by=list(LEP_CD1)]
LEP.2 <- split(LEP.2, LEP.2$LEP_CD1)
LEP.2 <- lapply(LEP.2, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
LEP.2 <- lapply(LEP.2, function(x) {x <- x[-c(1),]; return(x)})
LEP.2 <- lapply(LEP.2, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
LEP.2 <- lapply(LEP.2, function(x) {x$age <- as.numeric(as.character(gsub("M", "", gsub("\\_", "", x$age)))); return(x)})
LEP.2 <- lapply(LEP.2, function(x) {x$gender <- "Male"; return(x) })

LEP.dbs <- lapply(as.list(names(LEP.1)), function(x) rbind(LEP.1[[which(names(LEP.1)==x)]], LEP.2[[which(names(LEP.2)==x)]]))
names(LEP.dbs) <- names(LEP.1); rm(LEP.1, LEP.2)
LEP.dbs <- lapply(LEP.dbs, function(x) {x$Pop_LCL <- as.numeric(x$Pop_LCL)/sum(as.numeric(x$Pop_LCL))*100; return(x)})

CA.1 <- MYPE.F[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.F, is.numeric))[sapply(MYPE.F, is.numeric)], by=list(CA_CD)]
CA.1 <- split(CA.1, CA.1$CA_CD)
CA.1 <- lapply(CA.1, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
CA.1 <- lapply(CA.1, function(x) {x <- x[-c(1),]; return(x)})
CA.1 <- lapply(CA.1, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
CA.1 <- lapply(CA.1, function(x) {x$age <- as.numeric(as.character(gsub("F", "", gsub("\\_", "", x$age)))); return(x)})
CA.1 <- lapply(CA.1, function(x) {x$gender <- "Female"; return(x) })
CA.2 <- MYPE.M[, lapply(.SD, sum), .SDcols = names(sapply(MYPE.M, is.numeric))[sapply(MYPE.M, is.numeric)], by=list(CA_CD)]
CA.2 <- split(CA.2, CA.2$CA_CD)
CA.2 <- lapply(CA.2, function(x) {x <- as.data.table(cbind(colnames(x), t(x))); return(x)})
CA.2 <- lapply(CA.2, function(x) {x <- x[-c(1),]; return(x)})
CA.2 <- lapply(CA.2, function(x) {names(x) <- c("age", "Pop_LCL"); return(x)})
CA.2 <- lapply(CA.2, function(x) {x$age <- as.numeric(as.character(gsub("M", "", gsub("\\_", "", x$age)))); return(x)})
CA.2 <- lapply(CA.2, function(x) {x$gender <- "Male"; return(x) })

CA.dbs <- lapply(as.list(names(CA.1)), function(x) rbind(CA.1[[which(names(CA.1)==x)]], CA.2[[which(names(CA.2)==x)]]))
names(CA.dbs) <- names(CA.1); rm(CA.1, CA.2)
CA.dbs <- lapply(CA.dbs, function(x) {x$Pop_LCL <- as.numeric(x$Pop_LCL)/sum(as.numeric(x$Pop_LCL))*100; return(x)})
#----------------------------------------------------------------------------

# POPULATION PYRAMID
#----------------------------------------------------------------------------
fig.1 <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  
  pop.plot <- merge(LAD.dbs[[names.lookup$LAD_CD]], regional[[names.lookup$RGN_CD]], all.x=T, sort=F, by=c("age", "gender"))
  pop.plot <- merge(pop.plot, national, all.x=T, sort=F, by=c("age", "gender"))
  pop.plot <- pop.plot[!is.na(pop.plot$age),]
  
  t <- ggplot(pop.plot, aes(x = age, fill = gender, y = ifelse(test = gender == "Male", yes = Pop_LCL, no = -Pop_LCL))) + 
    geom_bar(stat = "identity", alpha=0.4) +
    geom_line(aes(x = as.numeric(age), y = ifelse(test = gender == "Male", yes = Pop_ENG, no = -Pop_ENG)), colour="black") +
    geom_line(aes(x = as.numeric(age), y = ifelse(test = gender == "Male", yes = Pop_RGN, no = -Pop_RGN)), colour="white") +
    scale_y_continuous(labels = function(x) paste0(round(abs(x), 3), "%"), limits = max(pop.plot$Pop_LCL) * c(-1,1)) +
    scale_x_continuous(expand = c(0,0), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
      labels = c("0", "10", "20", "30", "40", "50", "60", "70", "80", "90+")) +
    geom_vline(xintercept = seq(from=10, to=90, by=10), linetype="dotted", alpha=0.3) +
    labs(title = paste0("Local Population Pyramid 2019"), 
      subtitle = paste0("Comparison with England (Black) and ", names.lookup$RGN_NM, " Region (White) Aggregates"), x = "Age", y = "Percent of Population",
      caption = paste0("Source: 2019 Mid-Year Population Estimates")) +
    scale_colour_manual(values = c(palette[[1]][palette[[1]]$Colour=="Green",]$Col, palette[[1]][palette[[1]]$Colour=="Purple",]$Col), aesthetics = c("colour", "fill")) +
    theme_classic() +
    theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank(), plot.caption = element_text(face = "italic"),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3")) +
    coord_flip()
  return(t) })
names(fig.1) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 2: POPULATION DENSITY DISTRIBUTRIONS
###

# (MYPE Densities) GET PEAK DENSITY AND POPULATION RANKINGS
#----------------------------------------------------------------------------
Dens.LAD <- split(MYPE.T, MYPE.T$LAD_CD)[which(do.call(c, lapply(split(MYPE.T, MYPE.T$LAD_CD), function(x) dim(x)[1]))>10)]
Dens.LAD <- lapply(Dens.LAD, function(K) return(cbind(K$LAD_CD[1], density(log(K$pop.dens))$x, density(log(K$pop.dens))$y)) )
Dens.LAD <- data.table(do.call(rbind, Dens.LAD))
names(Dens.LAD) <- c("LAD_CD", "x.val", "y.val")
Dens.LAD$x.val <- as.numeric(as.character(Dens.LAD$x.val))
Dens.LAD$y.val <- as.numeric(as.character(Dens.LAD$y.val))

peak.x <- split(Dens.LAD, Dens.LAD$LAD_CD)
peak.x <- lapply(peak.x, function(d) d[which.max(d$y.val),]$x.val)
peak.x <- as.data.frame(do.call(rbind, peak.x))
peak.x$LAD_CD <- rownames(peak.x)
names(peak.x) <- c("peak.x", "LAD_CD")
rownames(peak.x) <- NULL

Pop.rank <- merge(peak.x, MYPE.T[,list(population=sum(population)),by=LAD_CD], all.x=T, sort=F, by="LAD_CD")
Pop.rank$peak.x[Pop.rank$peak.x<0] <- 0

DENS.xtr <- Pop.rank[c(which.max(Pop.rank$peak.x), which.min(Pop.rank$peak.x), which.min(abs((Pop.rank$peak.x-mean(Pop.rank$peak.x)))),
  which.min(abs((Pop.rank$peak.x-median(Pop.rank$peak.x))))),]$LAD_CD
TOTL.xtr <- Pop.rank[c(which.max(Pop.rank$population), which.min(Pop.rank$population), which.min(abs((Pop.rank$population-mean(Pop.rank$population)))),
  which.min(abs((Pop.rank$population-median(Pop.rank$population))))),]$LAD_CD
#----------------------------------------------------------------------------

# (MYPE Densities) DENSITY RANKING PLOTS
#----------------------------------------------------------------------------
fig.2 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_NM", "RGN_CD")])
  
  d1 <- ggplot(Pop.rank, aes(x=peak.x, y=reorder(LAD_CD, peak.x))) +
    geom_segment(aes(x=0, xend=peak.x, y=reorder(LAD_CD, peak.x), yend=reorder(LAD_CD, peak.x)), colour="white", alpha=0.3) +
    geom_point(colour="white", size=2, alpha=0.3) +
    geom_segment(data=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,], aes(x=rep(0,4), xend=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$peak.x,
      y=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$LAD_CD, yend=Pop.rank[which(Pop.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colours="red", alpha=0.4) +
    geom_point(data=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,], aes(x=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$peak.x, 
      y=Pop.rank[which(Pop.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colour="red", size=2, alpha=0.4) +
    geom_segment(aes(x=0, xend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$peak.x, y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6) +
    geom_segment(aes(x=0, xend=peak.x, y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6, linetype="dashed") +
    geom_point(data=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,], aes(x=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$peak.x, 
      y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", size=2, alpha=0.6) +    
    scale_y_discrete(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0)) +
    labs(title="", caption="\n", subtitle="LAD Density Rank", x="(log) Density") +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      legend.position="none", plot.caption=element_text(face="italic"), panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"))
  
  d2 <- ggplot(Pop.rank, aes(x=population, y=reorder(LAD_CD, population))) +
    geom_segment(aes(x=0, xend=population, y=reorder(LAD_CD, population), yend=reorder(LAD_CD, population)), colour="white", alpha=0.3) +
    geom_point(colour="white", size=2, alpha=0.3) +
    geom_segment(data=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,], aes(x=rep(0,4), xend=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$population,
      y=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$LAD_CD, yend=Pop.rank[which(Pop.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colours="red", alpha=0.4) +
    geom_point(data=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,], aes(x=Pop.rank[Pop.rank$LAD_CD %in% DENS.xtr,]$population, 
      y=Pop.rank[which(Pop.rank$LAD_CD %in% DENS.xtr),]$LAD_CD), colour="red", size=2, alpha=0.4) +
    geom_segment(aes(x=0, xend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$population, y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6) +
    geom_segment(aes(x=0, xend=population, y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD,
      yend=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", alpha=0.6, linetype="dashed") +
    geom_point(data=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,], aes(x=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$population, 
      y=Pop.rank[Pop.rank$LAD_CD==names.lookup$LAD_CD,]$LAD_CD), colour="black", size=2, alpha=0.6) +    
    scale_y_discrete(expand=c(0,0)) +
    scale_x_continuous(expand=c(0,0), labels=function(x)x/1000) +
    labs(title="", caption="LSOA Population Density Distributions\nSource: Mid-Year Population Estimates 2019", subtitle="LAD Population Rank", x="Total Population (1,000)") +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      legend.position="none", plot.caption=element_text(face="italic"), panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"))
  return(list(d1, d2)) })
fig.2A <- lapply(fig.2, function(x) x[[1]])
names(fig.2A) <- local.profiles

fig.2B <- lapply(fig.2, function(x) x[[2]])
names(fig.2B) <- local.profiles
#----------------------------------------------------------------------------

# (MYPE Densities) DENSITY DISTRIBUTION PLOTS
#----------------------------------------------------------------------------
fig.2 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_NM", "RGN_CD")])
  t1 <- Dens.LAD[Dens.LAD$LAD_CD %in% names.lookup$LAD_CD,]
  t1$Zone <- "Local"
  t2 <- Dens.LAD[Dens.LAD$LAD_CD %in% unique(geographic.lookup[geographic.lookup$RGN_CD==names.lookup$RGN_CD,]$LAD_CD),]
  t2$Zone <- "Regional"
  t3 <- Dens.LAD[Dens.LAD$LAD_CD %in% DENS.xtr,]
  t2$Zone <- "National"
  
  fig.2 <- ggplot() +
    geom_line(t2, mapping=aes(x=x.val, y=y.val, group=LAD_CD), colour="white", alpha=0.6) +
    geom_line(t3, mapping=aes(x=x.val, y=y.val, group=LAD_CD), colour="red", alpha=0.6) +
    geom_area(t1, mapping=aes(x=x.val, y=y.val), colour="black", alpha=0.6) +
    geom_line(t1, mapping=aes(x=x.val, y=y.val), colour="black", alpha=0.6) +
    geom_vline(xintercept = Pop.rank[Pop.rank$LAD_CD==G,]$peak.x, linetype="dotted", alpha=0.3) +
    scale_x_continuous(expand=c(0,0), limits=c(0, ceiling(t1[which.max(t1$x.val),]$x.val*1.5))) +
    scale_y_continuous(expand=c(0,0), limits=c(0, round(max(t2$y.val, na.rm=T), digits=2))) +
    labs(title=paste0("Population Density Distribution"),
      subtitle=paste0("Comparison with ", names.lookup$RGN_NM, " Region LADs (White) and national extremes (red)"), x="(log) People per Hectare", y="Frequency", caption="\n") +
    annotate('text', y=0.98*(max(t2$y.val, na.rm=T)), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("~ Peak LSOA Density Distribution: ", round(exp(Pop.rank[Pop.rank$LAD_CD==G,]$peak.x), digits=2), " ppl per ha.")) +
    annotate('text', y=0.94*(max(t2$y.val, na.rm=T)), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Avg. Peak LSOA Density across LADs: ", round(exp(mean(Pop.rank$peak.x)), digits=2), " ppl per ha.")) +
    annotate('text', y=0.90*(max(t2$y.val, na.rm=T)), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Med. Peak LSOA Density across LADs: ", round(exp(median(Pop.rank$peak.x)), digits=2), " ppl per ha.")) +
    annotate('text', y=0.86*(max(t2$y.val, na.rm=T)), x=0.005, hjust=0, fontface="italic", alpha=0.75, 
      label=paste0("Total Population: ", format(Pop.rank[Pop.rank$LAD_CD==G,]$population, big.mark=","))) +
    theme_classic() +
    theme(legend.position="none", panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), legend.title=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      plot.caption=element_text(face="italic"))
  return(fig.2)
})
names(fig.2) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 3: POPULATION DENSITY MAPS
###

# (MYPE Densities) DENSITY DISTRIBUTION MAPS
#----------------------------------------------------------------------------
fig.3 <- merge(LSOA.sp, MYPE.T[,c("LSOA_CD", "population", "pop.dens")], all.x=T, sort=F)
fig.3$pop.dens[is.na(fig.3$pop.dens)] <- 0
fig.3 <- fig.3[!st_is_empty(fig.3),,drop=FALSE]

density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])
  
  q <- unique(fig.3[fig.3$LEP_CD1==names.lookup$LEP_CD1 | fig.3$LEP_CD2==names.lookup$LEP_CD1 ,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$pop.dens, breaks = unique(quantile(q$pop.dens, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$pop.dens, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r1 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha(palette[[1]][palette[[1]]$Colour=="Red",]$Col_H, 0.6), lwd = 1, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(palette[[1]][palette[[1]]$Colour=="Blue",]$Col_L, palette[[1]][palette[[1]]$Colour=="Blue",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title="Rank"),
      color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Population Density Deciles"), subtitle=paste0("Relative to the broader ", names.lookup$LEP_NM1, " LEP"), x="People per (LSOA) Hectare", y="", 
         caption="Source: Mid-Year Population Estimates 2019") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$pop.dens, breaks = unique(quantile(q$pop.dens, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$pop.dens, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r2 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha(palette[[1]][palette[[1]]$Colour=="Red",]$Col_H, 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(palette[[1]][palette[[1]]$Colour=="Blue",]$Col_L, palette[[1]][palette[[1]]$Colour=="Blue",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title="Rank"),
      color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0(""),
      subtitle=paste0(names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"), x="People per (LSOA) Hectare", y="", caption="Source: Mid-Year Population Estimates 2019") +
    theme_classic() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  return(list(r2, r1)) })
fig.3A <- lapply(density.plots, function(x) x[[1]])
names(fig.3A) <- local.profiles

fig.3B <- lapply(density.plots, function(x) x[[2]])
names(fig.3B) <- local.profiles
rm(density.plots)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 4: LAD TRANSACTIONS AND MEDIAN PRICE
###

# MONTHLY LAND REGISTRY BY LAD & TENURE (BALANCED PANEL)
#----------------------------------------------------------------------------
balanced.panel <- expand.grid(unique(uklr.pp$type), seq(as.Date(min(uklr.pp$month), format="%Y-%m-%d"), as.Date(max(uklr.pp$month), format="%Y-%m-%d"), by="month"))
names(balanced.panel) <- c("type", "month")
balanced.panel <- data.table(balanced.panel)[order(balanced.panel$type), ]
national <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("type", "month")]
national$month <- as.Date(national$month, format="%Y-%m-%d")
national <- merge(balanced.panel, national, all.x=T, sort=F, by=c("type", "month"))
national$N[is.na(national$N)] <- 0
local.total <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("month")]
local.total$month <- as.Date(local.total$month, format="%Y-%m-%d")
local.total <- data.table(local.total)[order(local.total$month), ]
local.total$N[is.na(local.total$N)] <- 0
local.total$type <- "total"
national <- rbind(local.total, national)
rm(balanced.panel, local.total)

balanced.panel <- expand.grid(unique(geographic.lookup$RGN_CD), unique(uklr.pp$type), seq(as.Date(min(uklr.pp$month), format="%Y-%m-%d"), as.Date(max(uklr.pp$month), format="%Y-%m-%d"), by="month"))
names(balanced.panel) <- c("RGN_CD", "type", "month")
balanced.panel <- data.table(balanced.panel)[order(balanced.panel$RGN_CD, balanced.panel$type), ]
regional <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("RGN_CD", "type", "month")]
regional$month <- as.Date(regional$month, format="%Y-%m-%d")
regional <- merge(balanced.panel, regional, all.x=T, sort=F, by=c("RGN_CD", "type", "month"))
regional$N[is.na(regional$N)] <- 0
balanced.panel <- expand.grid(unique(geographic.lookup$RGN_CD), seq(as.Date(min(uklr.pp$month), format="%Y-%m-%d"), as.Date(max(uklr.pp$month), format="%Y-%m-%d"), by="month"))
names(balanced.panel) <- c("RGN_CD", "month")
balanced.panel <- data.table(balanced.panel)[order(balanced.panel$RGN_CD, balanced.panel$month), ]
local.total <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("RGN_CD", "month")]
local.total$month <- as.Date(local.total$month, format="%Y-%m-%d")
local.total <- merge(balanced.panel, local.total, all.x=T, sort=F, by=c("RGN_CD", "month"))
local.total$N[is.na(local.total$N)] <- 0
local.total$type <- "total"
regional <- rbind(local.total, regional)
rm(balanced.panel, local.total)

balanced.panel <- expand.grid(unique(geographic.lookup$LAD_CD), unique(uklr.pp$type), seq(as.Date(min(uklr.pp$month), format="%Y-%m-%d"), as.Date(max(uklr.pp$month), format="%Y-%m-%d"), by="month"))
names(balanced.panel) <- c("LAD_CD", "type", "month")
balanced.panel <- data.table(balanced.panel)[order(balanced.panel$LAD_CD, balanced.panel$type),]
local.authority <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("LAD_CD", "type", "month")]
local.authority$month <- as.Date(local.authority$month, format="%Y-%m-%d")
local.authority <- merge(balanced.panel, local.authority, all.x=T, sort=F, by=c("LAD_CD", "type", "month"))
local.authority$N[is.na(local.authority$N)] <- 0
balanced.panel <- expand.grid(unique(geographic.lookup$LAD_CD), seq(as.Date(min(uklr.pp$month), format="%Y-%m-%d"), as.Date(max(uklr.pp$month), format="%Y-%m-%d"), by="month"))
names(balanced.panel) <- c("LAD_CD", "month")
balanced.panel <- data.table(balanced.panel)[order(balanced.panel$LAD_CD, balanced.panel$month), ]
local.total <- uklr.pp[,.(N=sum(!is.na(price)), Avg=mean(price, na.rm = T), Med=as.double(median(price, na.rm = T))), by=c("LAD_CD", "month")]
local.total$month <- as.Date(local.total$month, format="%Y-%m-%d")
local.total <- merge(balanced.panel, local.total, all.x=T, sort=F, by=c("LAD_CD", "month"))
local.total$N[is.na(local.total$N)] <- 0
local.total$type <- "total"
local.authority <- rbind(local.total, local.authority)
local.authority <- merge(local.authority, unique(geographic.lookup[,c("LAD_CD", "RGN_CD")]), all.x=T, sort=F)
rm(balanced.panel, local.total)
#----------------------------------------------------------------------------

# LOCAL, REGIONAL AND NATIONAL COMPARISON
#----------------------------------------------------------------------------
fig.4 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  LCL <- local.authority[local.authority$LAD_CD==names.lookup$LAD_CD & local.authority$type=="total",]
  LCL$Med[LCL$N<=5] <- NA
  LCL$type <- "Local"
  RGN <- regional[regional$RGN_CD==names.lookup$RGN_CD & regional$type=="total",]
  RGN$Med[RGN$N<=5] <- NA
  RGN$type <- "Regional"
  NTL <- national[national$type=="total",]
  NTL$Med[NTL$N<=5] <- NA
  NTL$type <- "National"
  TOTAL <- rbind(LCL[,-c("LAD_CD", "RGN_CD")], RGN[,-c("RGN_CD")])
  TOTAL <- rbind(TOTAL, NTL); rm(LCL, RGN, NTL)
  TOTAL$type <- factor(TOTAL$type, levels=c("Local", "Regional", "National"))
  
  fig.4A <- ggplot(data=TOTAL, mapping=aes(x=month, y=Med, group=type, colour=type), alpha=0.8, linetype="solid") +
    geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype="dotted", alpha=0.4) +
    geom_line() +
    geom_point() +
    annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2020-07-01"), ymin=0, ymax=Inf, alpha=0.1, fill="black") +
    scale_x_date(expand = c(0,0), date_breaks="3 month", labels=date_format("%B - %Y"), limits = c(as.Date("2015-01-01"), NA)) +
    scale_y_continuous(expand = c(0,0), name = "Median Price", breaks=seq(from=0, to=max(TOTAL$Med, na.rm=T), by=50000), 
      labels=format(seq(from=0, to=max(TOTAL$Med, na.rm=T), by=50000), big.mark = ",")) +
    scale_colour_manual(values=c("#000000", "#33a989", "#e3ba58")) +
    labs(title = paste0("Dwelling Price & Transaction Evolution 2015-2021"), 
      subtitle = paste0(names.lookup$LAD_NM, " LAD & ", names.lookup$RGN_NM, " Region")) +    
    theme_classic() +
      theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"))
  
  t1 <- merge(TOTAL[TOTAL$type=="Regional",c("month", "N")], TOTAL[TOTAL$type=="Local",c("month", "N")], by="month", sort=F)
  t1$N.x <- t1$N.x - t1$N.y
  t11 <- t1[,c("month", "N.x")]; names(t11) <- c("month", "N"); t11$type <- "Regional"
  t12 <- t1[,c("month", "N.y")]; names(t12) <- c("month", "N"); t12$type <- "Local"
  t1 <- rbind(t11, t12); rm(t11, t12)

  fig.4B <- ggplot(data=t1, mapping=aes(x=month, y=N, group=type, fill=type)) +
    geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2021-01-01")), linetype="dotted", alpha=0.4) +
    geom_col(alpha=0.8) +
    scale_fill_manual(values=c("#000000", "#33a989")) +
    annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2020-07-01"), ymin=0, ymax=Inf, alpha=0.1, fill="black") +
    scale_x_date(expand = c(0,0), date_breaks="3 month", labels=date_format("%B - %Y"), limits = c(as.Date("2015-01-01"), NA)) +
    scale_y_continuous(expand = c(0,0), name = "Transaction Counts", labels=function(x) format(x, big.mark = ",", scientific = FALSE)) +
    labs(title = "", 
      subtitle = paste0("Regional and Local Transactions"),
      caption = paste0("Source: HM Land Registry.\nMedian price for >= 5 transactions")) +    
    theme_classic() +
      theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank(),
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  return(list(fig.4A, fig.4B)) })
fig.4A <- lapply(fig.4, function(x) x[[1]])
names(fig.4A) <- local.profiles

fig.4B <- lapply(fig.4, function(x) x[[2]])
names(fig.4B) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 7: HOUSING AFFORDABILITY
###

# HOUSING PRICES AND INCOME - AFFORDABILITY RANK (2018)
#----------------------------------------------------------------------------
nation.incm <- afford.incm[,-c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")][,.(value=mean(value, na.rm=T)),by=.(Quantile, Year)]
nation.price <- afford.price[,-c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")][,.(value=mean(value, na.rm=T)),by=.(Quantile, Year)]
nation.affordability <- afford.index[,-c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")][,.(value=mean(value, na.rm=T)),by=.(Quantile, Year)]

region.incm <- lapply(split(afford.incm[,-c("RGN_NM", "LAD_CD", "LAD_NM")], afford.incm[,-c("RGN_NM", "LAD_CD", "LAD_NM")]$RGN_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(RGN_CD, Quantile, Year)] })
region.price <- lapply(split(afford.price[,-c("LAD_CD", "LAD_NM")], afford.incm[,-c("RGN_NM", "LAD_CD", "LAD_NM")]$RGN_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(RGN_CD, Quantile, Year)] })
region.affordability <- lapply(split(afford.index[,-c("LAD_CD", "LAD_NM")], afford.incm[,-c("RGN_NM", "LAD_CD", "LAD_NM")]$RGN_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(RGN_CD, Quantile, Year)] })

local.incm <- lapply(split(afford.incm[,-c("RGN_CD", "RGN_NM", "LAD_NM")], afford.incm[,-c("RGN_CD", "RGN_NM", "LAD_NM")]$LAD_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(LAD_CD, Quantile, Year)] })
local.price <- lapply(split(afford.price[,-c("RGN_CD", "RGN_NM", "LAD_NM")], afford.incm[,-c("RGN_CD", "RGN_NM", "LAD_NM")]$LAD_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(LAD_CD, Quantile, Year)] })
local.affordability <- lapply(split(afford.index[,-c("RGN_CD", "RGN_NM", "LAD_NM")], afford.incm[,-c("RGN_CD", "RGN_NM", "LAD_NM")]$LAD_CD), function(x) { 
  x[,.(value=mean(value, na.rm=T)),by=.(LAD_CD, Quantile, Year)] })

fig.7 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])

  NTL <- nation.affordability[nation.affordability$Quantile=="Median (50)",]
  RGN <- region.affordability[[names.lookup$RGN_CD]][region.affordability[[names.lookup$RGN_CD]]$Quantile=="Median (50)",]
  LCL <- local.affordability[[names.lookup$LAD_CD]][local.affordability[[names.lookup$LAD_CD]]$Quantile=="Median (50)",]

  LCL.1 <- local.incm[[names.lookup$LAD_CD]][local.incm[[names.lookup$LAD_CD]]$Quantile=="Median (50)",]
  LCL.2 <- local.price[[names.lookup$LAD_CD]][local.price[[names.lookup$LAD_CD]]$Quantile=="Median (50)",]

  
  Inc.scaled <- (0 + ((max(LCL.2$value, na.rm=T)-0)/(max(LCL.1$value, na.rm=T)-min(LCL.1$value, na.rm=T)))*(LCL.1$value - min(LCL.1$value, na.rm=T)))
  Inc.scaled <- data.table(cbind(Year=as.character(unique(LCL.1$Year)), Inc.scaled=Inc.scaled, Inc=LCL.1$value))
  Inc.scaled$Year <- as.Date(Inc.scaled$Year, format="%Y-%m-%d")
  Inc.scaled$Inc.scaled <- as.numeric(Inc.scaled$Inc.scaled)
  Inc.scaled$Inc <- as.numeric(Inc.scaled$Inc)
    
  fig.7A <- ggplot(LCL.1, aes(x=Year)) +
    geom_bar(data=Inc.scaled, mapping = aes(x = Year, y = Inc.scaled), stat = "identity", fill = "white", size=0.1, alpha=0.6) +
    geom_vline(xintercept = as.numeric(as.Date("2005-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", alpha=0.4) +
    geom_line(data=LCL.2, mapping=aes(x=Year, y=value)) +
    geom_point(data=LCL.2, mapping=aes(x=Year, y=value)) +
    # scale_colour_manual(values=unique(LCL[,c("type", "colour", "order")])$colour[rank(unique(LCL[,c("type", "colour", "order")])$order)][-1]) +
    #geom_line(data=LCL[LCL$type=="Total",], mapping=aes(x=month, y=Med), colour="black", alpha=1, linetype="dashed") +
    scale_x_date(expand = c(0,0), date_breaks="1 year", labels=date_format("%Y"), limits = c(as.Date("2003-01-01"), NA)) +
    scale_y_continuous(expand = c(0,0), name = "Median Price (lines)", 
                       breaks=seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000), 
                       labels=format(seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000), big.mark = ","), 
      sec.axis = sec_axis(~.*1, name = "Median Income (bars)", 
      breaks=round(seq(0, max(Inc.scaled$Inc.scaled, na.rm=T), by = max(Inc.scaled$Inc.scaled, na.rm=T)/length(seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000))))[-length(round(seq(0, max(Inc.scaled$Inc.scaled, na.rm=T), by = max(Inc.scaled$Inc.scaled, na.rm=T)/length(seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000)))))], 
      labels=round(seq(0, max(Inc.scaled$Inc, na.rm=T), by = max(Inc.scaled$Inc, na.rm=T)/length(seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000))))[-length(round(seq(0, max(Inc.scaled$Inc, na.rm=T), by = max(Inc.scaled$Inc, na.rm=T)/length(seq(from=0, to=max(LCL.2$value, na.rm=T), by=10000)))))] ) ) + 
    labs(title = paste0("Dwelling Price and Local Income Evolution 2015-2021"),
      subtitle = paste0("Median Values"),
      caption = paste0("Line plot tracking of median prices (left-axis) with underlying bar plot tracking median income")) +    
    theme_classic() +
    theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, vjust = 0.5, hjust=0.5), axis.title.x=element_blank(),
      axis.ticks.x = element_blank(), panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic"))
 
  
  fig.7B <- ggplot(LCL, mapping=aes(x=Year, y=value)) +
    geom_vline(xintercept = as.numeric(as.Date("2005-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2010-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype="dotted", alpha=0.4) +
    geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted", alpha=0.4) +
    geom_line(data=NTL, mapping=aes(x=Year, y=value), colour="black") +
    geom_point(data=NTL, mapping=aes(x=Year, y=value), colour="black") +
    geom_line(data=RGN, mapping=aes(x=Year, y=value), colour="white") +
    geom_point(data=RGN, mapping=aes(x=Year, y=value), colour="white") +
    geom_line(data=LCL, mapping=aes(x=Year, y=value), colour="red") +
    geom_point(data=LCL, mapping=aes(x=Year, y=value), colour="red") +
    scale_x_date(expand = c(0,0), date_breaks="1 year", labels=date_format("%Y"), limits = c(as.Date("2003-01-01"), NA)) +
    scale_y_continuous(expand = c(0,0)) + 
    labs(title = paste0("Dwelling Index of Affordability"), y="Index of Affordability (Price/Income)",
      subtitle = paste0("Median Values"),
      caption = paste0("Comparison with national and regional averages")) +    
    theme_classic() +
    theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, vjust = 0.5, hjust=0.5), axis.title.x=element_blank(),
      axis.ticks.x = element_blank(), panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic"))
 
  
  return(list(fig.7A, fig.7B)) })
fig.7A <- lapply(fig.7, function(x) x[[1]])
names(fig.7A) <- local.profiles

fig.7B <- lapply(fig.7, function(x) x[[2]])
names(fig.7B) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 5: PRICES BY TYPOLOGY
###

# TYPOLOGY EVOLUTION
#----------------------------------------------------------------------------
fig.5 <- lapply(as.list(local.profiles), function(G){

  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  LCL <- local.authority[local.authority$LAD_CD==names.lookup$LAD_CD,]
  LCL$Med[LCL$N<=5] <- NA
  LCL$type[LCL$type=="detached"] <- "Detached"
  LCL$type[LCL$type=="flat"] <- "Flat"
  LCL$type[LCL$type=="other"] <- "Other"
  LCL$type[LCL$type=="semi"] <- "Semi Detached"
  LCL$type[LCL$type=="terraced"] <- "Terraced"
  LCL$type[LCL$type=="total"] <- "Total"
  LCL <- LCL[LCL$type!="Other",]
  
  t <- LCL[, sum(is.na(Med), na.rm=T)/length(unique(LCL$month)), by=list(type)]
  t <- t$type[t$V1<0.8]

  LCL <- LCL[LCL$type %in% t,]; rm(t)
  
  if(dim(LCL)[1]==0){
    return(NA)
  } else {
    N.scaled <- (0 + ((max(LCL$Med, na.rm=T)-0)/(max(LCL[LCL$type=="Total",]$N, na.rm=T)-min(LCL[LCL$type=="Total",]$N, na.rm=T)))*(LCL[LCL$type=="Total",]$N - min(LCL[LCL$type=="Total",]$N, na.rm=T)))
    N.scaled <- data.table(cbind(month=as.character(unique(LCL$month)), N.scaled=N.scaled, N=LCL[LCL$type=="Total",]$N))
    N.scaled$month <- as.Date(N.scaled$month, format="%Y-%m-%d")
    N.scaled$N.scaled <- as.numeric(N.scaled$N.scaled)
    N.scaled$N <- as.numeric(N.scaled$N)
    
    LCL <- merge(LCL, housing.lookup, all.x=T, sort=F, by="type")
    LCL$type <- factor(LCL$type, levels = unique(LCL[,c("type", "colour", "order")])[order(as.numeric(unique(LCL[,c("type", "colour", "order")])$order)),]$type)
    
    fig.5 <- ggplot(LCL, aes(x=month)) +
      geom_bar(data=N.scaled, mapping = aes(x = month, y = N.scaled), stat = "identity", fill = "white", size=0.1, alpha=0.6) +
      geom_vline(xintercept = as.numeric(as.Date("2015-01-01")), linetype="dotted", alpha=0.4) +
      geom_vline(xintercept = as.numeric(as.Date("2016-01-01")), linetype="dotted", alpha=0.4) +
      geom_vline(xintercept = as.numeric(as.Date("2017-01-01")), linetype="dotted", alpha=0.4) +
      geom_vline(xintercept = as.numeric(as.Date("2018-01-01")), linetype="dotted", alpha=0.4) +
      geom_vline(xintercept = as.numeric(as.Date("2019-01-01")), linetype="dotted", alpha=0.4) +
      geom_vline(xintercept = as.numeric(as.Date("2020-01-01")), linetype="dotted") +
      annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2020-07-01"), ymin=0, ymax=Inf, alpha=0.1, fill="black") +
      geom_line(data=LCL[LCL$type!="Total",], mapping=aes(x=month, y=Med, group=type, colour=type)) +
      scale_colour_manual(values=unique(LCL[,c("type", "colour", "order")])$colour[rank(unique(LCL[,c("type", "colour", "order")])$order)][-1]) +
      geom_line(data=LCL[LCL$type=="Total",], mapping=aes(x=month, y=Med), colour="black", alpha=1, linetype="dashed") +
      scale_x_date(expand = c(0,0), date_breaks="3 month", labels=date_format("%B - %Y"), limits = c(as.Date("2015-01-01"), NA)) +
      scale_y_continuous(expand = c(0,0), name = "Median Price (lines)", breaks=seq(from=0, to=max(LCL$Med, na.rm=T), by=100000), labels=format(seq(from=0, to=max(LCL$Med, na.rm=T), by=100000), big.mark = ","), 
        sec.axis = sec_axis(~.*1, name = "Total Transactions (bars)", 
        breaks=round(seq(0, max(N.scaled$N.scaled, na.rm=T), by = max(N.scaled$N.scaled, na.rm=T)/length(seq(from=0, to=max(LCL$Med, na.rm=T), by=100000))))[-length(round(seq(0, max(N.scaled$N.scaled, na.rm=T), by = max(N.scaled$N.scaled, na.rm=T)/length(seq(from=0, to=max(LCL$Med, na.rm=T), by=100000)))))], 
        labels=round(seq(0, max(N.scaled$N, na.rm=T), by = max(N.scaled$N, na.rm=T)/length(seq(from=0, to=max(LCL$Med, na.rm=T), by=100000))))[-length(round(seq(0, max(N.scaled$N, na.rm=T), by = max(N.scaled$N, na.rm=T)/length(seq(from=0, to=max(LCL$Med, na.rm=T), by=100000)))))] ) ) + 
      labs(title = paste0("Dwelling Price Evolution 2015-2021"),
        subtitle = paste0("Breakdown by typology and overall average (dashed)"),
        caption = paste0("Line plot tracking of median prices (left-axis) with underlying bar plot tracking transaction churn (right-axis)\nMedian price for >= 5 transactions")) +    
      theme_classic() +
      theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, vjust = 0.5, hjust=0.5), axis.title.x=element_blank(),
        plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
        axis.ticks.x = element_blank(), panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic"))
    return(fig.5)} })
names(fig.5) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 6: TYPOLOGY DISTRIBUTION
###

# LOCAL, REGIONAL AND NATIONAL COMPARISON
#----------------------------------------------------------------------------
fig.6 <- lapply(as.list(local.profiles), function(G){
  
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  LCL <- local.authority[local.authority$LAD_CD==names.lookup$LAD_CD,]
  names(LCL)[names(LCL)=="type"] <- "dwelling_type"
  LCL$Med[LCL$N<=5] <- NA
  LCL$Year <- substr(as.character(LCL$month), 1, 4)
  LCL <- LCL[LCL$Year %in% c("2019", "2020", "2021"), c("N", "dwelling_type", "Year")]
  LCL <- LCL[,.(N=sum(N, na.rm=T)), by=.(dwelling_type, Year)]
  LCL <- LCL[LCL$dwelling_type!="total",]
  LCL$Type <- "Local"

  RGN <- regional[regional$RGN_CD==names.lookup$RGN_CD,]
  names(RGN)[names(RGN)=="type"] <- "dwelling_type"
  RGN$Med[RGN$N<=5] <- NA
  RGN$Year <- substr(as.character(RGN$month), 1, 4)
  RGN <- RGN[RGN$Year %in% c("2019", "2020", "2021"), c("N", "dwelling_type", "Year")]
  RGN <- RGN[,.(N=sum(N, na.rm=T)), by=.(dwelling_type, Year)]
  RGN <- RGN[RGN$dwelling_type!="total",]
  RGN$Type <- "Regional"

  NTL <- national
  names(NTL)[names(NTL)=="type"] <- "dwelling_type"
  NTL$Med[NTL$N<=5] <- NA
  NTL$Year <- substr(as.character(NTL$month), 1, 4)
  NTL <- NTL[NTL$Year %in% c("2019", "2020", "2021"), c("N", "dwelling_type", "Year")]
  NTL <- NTL[,.(N=sum(N, na.rm=T)), by=.(dwelling_type, Year)]
  NTL <- NTL[NTL$dwelling_type!="total",]
  NTL$Type <- "National"

  t <- rbind(LCL, RGN)
  t <- rbind(t, NTL); rm(LCL, RGN, NTL)
  t$dwelling_type[t$dwelling_type=="detached"] <- "Detached"
  t$dwelling_type[t$dwelling_type=="flat"] <- "Flat"
  t$dwelling_type[t$dwelling_type=="other"] <- "Other"
  t$dwelling_type[t$dwelling_type=="semi"] <- "Semi Detached"
  t$dwelling_type[t$dwelling_type=="terraced"] <- "Terraced"
  t$dwelling_type[t$dwelling_type=="total"] <- "Total"

  t$dwelling_type <- factor(as.character(t$dwelling_type), 
    levels=housing.lookup[housing.lookup$type %in% unique(as.character(t$dwelling_type)),][order(as.numeric(housing.lookup[housing.lookup$type %in% unique(as.character(t$dwelling_type)),]$order)),]$type )
  t$Type <- factor(t$Type, levels=c("Local", "Regional", "National"))
  t$Year <- factor(t$Year, levels=c("2019", "2020", "2021"))

  fig.6 <- ggplot(t, aes(x=Type, y=N, fill=dwelling_type)) +
    geom_bar(stat="identity", position="fill", width=0.4) +
    labs(title = "Typology Transaction Breakdown", subtitle = "Previous 3 year comparison", x = "", y = "Proportions",
      caption="Source: HM Land Registry.\nLimited transactions up to February 2021") +
    scale_fill_manual(values=housing.lookup[housing.lookup$type %in% unique(as.character(t$dwelling_type)),][order(as.numeric(housing.lookup[housing.lookup$type %in% unique(as.character(t$dwelling_type)),]$order)),]$colour) +
    scale_x_discrete(expand = c(0.5,0.5)) +
    scale_y_continuous(expand = c(0,0), labels=function(x) paste0(x*100, "%")) +
    theme_classic() +
    theme(axis.ticks.x=element_blank(), legend.title = element_blank(), plot.caption = element_text(face = "italic"), 
      plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
      axis.text.x = element_text(angle = 45, hjust = 1), panel.background=element_rect(fill="#E5E5E3")) +
    guides(fill = guide_legend(reverse = FALSE)) +
    facet_wrap(~ Year, nrow = 1)
  return(fig.6) })
names(fig.6) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 7: TRANSACTIONS BY DENSITY QUARTILE
###

#----------------------------------------------------------------------------


#----------------------------------------------------------------------------



#############################################################################
##### GENERATE POPULATION AND HOUSING LOCAL PROFILE
###

# COMPILING LOCAL AUTHORITY DISTRICTS
#----------------------------------------------------------------------------
for(S in local.profiles){
  locality <- unique(geographic.lookup[geographic.lookup$LAD_CD==S, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CIS_CD", "CIS_NM")])
  
  rmarkdown::render(paste0(wd, "/Code/Population and Housing/PopulationHousing_Template.Rmd"),
    output_file=paste0(S, "_PopulationHousing.html"), 
    output_dir=paste0(wd, "/Outputs/", gsub(" ", "_", locality$RGN_NM), "/LAD/", S, "/"))
}
#----------------------------------------------------------------------------


# COMPILING LOCAL ENTERPRISE PARTNERSHIPS
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------


# COMPILING COMBINED AUTHORITIES
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------