#############################################################################
#############################################################################
###########  
#########    SOCIAL ECONOMY: RETAIL + HOSPITALITY & ACCOMODATION
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
geographic.lookup <- fread(file=paste0(wd.data, "/Boundaries & Lookups/Geographic_Lookup_X.csv"))
LAD_CIS <- fread(file=paste0(wd.data, "/Boundaries & Lookups/Local_Authority_District__May_2020__to_Covid_Infection_Survey__October_2020__Lookup_in_England.csv"))[,c("LAD20CD", "CIS20CD")]
names(LAD_CIS) <- c("LAD_CD", "CIS_CD")
LAD_CA <- fread(file=paste0(wd.data, "/Boundaries & Lookups/Local_Authority_District_to_Combined_Authority__December_2020__Lookup_in_England.csv"))[,c("LAD20CD", "CAUTH20CD", "CAUTH20NM")]
names(LAD_CA) <- c("LAD_CD", "CA_CD", "CA_NM")
LSOA_LEP <- fread(file=paste0(wd.data, "/Boundaries & Lookups/Lower_Layer_Super_Output_Area__2011__to_Local_Enterprise_Partnership__April_2020__Lookup_in_England.csv"), na.strings = c(""))[,c("LSOA11CD", "LEP20CD1", "LEP20NM1", "LEP20CD2", "LEP20NM2")]
names(LSOA_LEP) <- c("LSOA_CD", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2")
geographic.lookup <- merge(geographic.lookup, LAD_CIS, all.x=T, sort=F, by="LAD_CD")
geographic.lookup <- merge(geographic.lookup, LAD_CA, all.x=T, sort=F, by="LAD_CD")
geographic.lookup <- merge(geographic.lookup, LSOA_LEP, all.x=T, sort=F, by="LSOA_CD")
CIS.naming <- fread(file=paste0(wd.data, "/Boundaries & Lookups/CIS_Naming.csv"))
geographic.lookup <- merge(geographic.lookup, CIS.naming, all.x=T, sort=F, by="CIS_CD")
rm(LAD_CIS, LAD_CA, LSOA_LEP, CIS.naming); gc()

SIC.lookup <- fread(file=paste0(wd.data, "/Boundaries & Lookups/SIC_Lookup_X.csv"))
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
WZ.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Workplace_Zones_December_2011_Generalised_Clipped_Boundaries_in_England_and_Wales.shp"),
  stringsAsFactors = F, quiet = T), crs=27700)[,c("wz11cd", "lad11cd")]
names(WZ.sp)[names(WZ.sp)=="wz11cd"] <- "WZ_CD"
names(WZ.sp)[names(WZ.sp)=="lad11cd"] <- "LAD_CD"
WZ.sp <- merge(WZ.sp, unique(geographic.lookup[,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]),
  all.x=T, sort=F, by="LAD_CD")

LSOA.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/LSOA_2011_EW_BFC.shp"),
  stringsAsFactors = F, quiet = T), crs=27700)[,c("LSOA11CD")]
names(LSOA.sp)[names(LSOA.sp)=="LSOA11CD"] <- "LSOA_CD"
LSOA.sp <- merge(LSOA.sp, unique(geographic.lookup[,c("LSOA_CD", "LSOA_NM", "LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]),
  all.x=T, sort=F, by="LSOA_CD")
LSOA.sp$area_ha <- as.numeric(st_area(LSOA.sp))*0.0001

LAD.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Local_Authority_Districts_(December_2020)_UK_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("LAD20CD")]
names(LAD.sp)[names(LAD.sp)=="LAD20CD"] <- "LAD_CD"
LAD.sp <- merge(LAD.sp, unique(geographic.lookup[,c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CA_CD", "CA_NM", "LEP_CD1", "LEP_NM1", "LEP_CD2", "LEP_NM2", "CIS_CD", "CIS_NM")]), 
  all.x=T, sort=F, by="LAD_CD")

LEP.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Local_Enterprise_Partnerships_(April_2020)_Boundaries_EN_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("lep20cd", "lep20nm")]
names(LEP.sp)[names(LEP.sp)=="lep20cd"] <- "LEP_CD1"
names(LEP.sp)[names(LEP.sp)=="lep20nm"] <- "LEP_NM1"
t <- unique(geographic.lookup[,c("RGN_CD", "RGN_NM", "LEP_CD1", "LEP_NM1")])
t <- t[-c(17, 18, 21, 41, 42),]
LEP.sp <- merge(LEP.sp, t, all.x=T, sort=F, by="LEP_CD1"); rm(t)

CA.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Combined_Authorities_(December_2019)_Boundaries_EN_BFC.shp"),
  stringsAsFactors = F, quiet = T), crs=27700)[,c("cauth19cd", "cauth19nm")]
names(CA.sp)[names(CA.sp)=="cauth19cd"] <- "CA_CD"
names(CA.sp)[names(CA.sp)=="cauth19nm"] <- "CA_NA"
CA.sp <- merge(CA.sp, unique(geographic.lookup[,c("RGN_CD", "RGN_NM", "CA_CD", "CA_NM")]),
  all.x=T, sort=F, by="CA_CD")

RGN.sp <- st_transform(st_read(paste0(wd.data, "/Boundaries & Lookups/Regions_(December_2019)_Boundaries_EN_BFC.shp"), 
  stringsAsFactors = F, quiet = T), crs=27700)[,c("rgn19cd", "rgn19nm")]
names(RGN.sp)[names(RGN.sp)=="rgn19cd"] <- "RGN_CD"
names(RGN.sp)[names(RGN.sp)=="rgn19nm"] <- "RGN_NM"

Retail.sp <- st_transform(st_read(gsub("ADRUK Covid/Local Profiles", "Retail/Exports/Pre Release/Retail_Centres_UKv3.gpkg", wd),
  stringsAsFactors = F, quiet = T), crs=27700)
names(Retail.sp)[names(Retail.sp)=="geom"] <- c("geometry")
st_geometry(Retail.sp) <- "geometry"
# t <- fread(gsub("ADRUK Covid/Local Profiles", "Retail/Exports/Pre Release/Retail_Centres_Statistics.csv", wd))
# Retail.sp <- merge(Retail.sp, t[,c("RC_ID", "tr_VOA_retailN", "tr_OSM_retailN", "tr_LDC_retailN")], all.x=T, sort=F, by="RC_ID")
# t <- fread(gsub("ADRUK Covid/Local Profiles", "Retail/Exports/Pre Release/RC_OA_Lookup.csv", wd))
# names(t) <- c("RC_ID", "OA_CD")
# Retail.sp <- merge(Retail.sp, t, all.x=T, sort=F, by="RC_ID"); rm(t)
Retail.sp1 <- st_join(st_centroid(Retail.sp), LAD.sp[,c("LAD_CD")], join=st_intersects)
Retail.sp <- merge(Retail.sp, data.table(Retail.sp1)[,c("RC_ID", "LAD_CD")], all.x=T, sort=F, by="RC_ID"); rm(Retail.sp1)
Retail.sp <- merge(Retail.sp, unique(geographic.lookup[,c("LAD_CD", "RGN_CD")]), all.x=T, sort=F, by="LAD_CD")
Retail.sp$Classification <- factor(Retail.sp$Classification, levels=c("Regional Centre", "Major Town Centre", "Town Centre", "Market Town", "District Centre", "Local Centre", "Local Retail Park"))
Retail.sp <- Retail.sp[!duplicated(Retail.sp$RC_ID),]
Retail.sp <- Retail.sp[!is.na(Retail.sp$RC_ID),]

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
# local.profiles <- unique(geographic.lookup$LAD_CD)[!is.na(unique(geographic.lookup$LAD_CD))]
# local.profiles <- unique(LDS.lookup$LAD_CD)
local.profiles <- c("E09000033", "E08000025", "E06000033", "E06000014", "E07000131",
  unique(geographic.lookup[geographic.lookup$CA_NM=="Liverpool City Region",]$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="Yorkshire and The Humber",]$LAD_CD),
  unique(geographic.lookup[geographic.lookup$RGN_NM=="East of England",]$LAD_CD))
local.profiles <- local.profiles[!(local.profiles %in% c("E06000053", "E06000029", "E07000051", "E07000049", "E07000205", "E07000190",
  "E06000028", "E07000201", "E07000206", "E07000204", "E07000053", "E07000052",
  "E07000191", "E07000191", "E07000048", "E07000050"))]

# local.profiles <- unique(LDS.lookup$LAD_CD)
# set.seed(8675309)
# local.profiles <- unique(geographic.lookup[geographic.lookup$LAD_CD %in% local.profiles,c("LAD_CD", "RGN_NM")])[,.SD[sample(.N, min(2,.N))],by = RGN_NM]$LAD_CD
#----------------------------------------------------------------------------

#############################################################################
##### SECTION 2: LOAD IN AND FORMAT CLEANED (NON-DISCLOSIVE) DATASETS
###
  
# ONLY NON-DISCLOSIVE AGGREGATE DATASETS USED FOR THESE VISUALS AND EXPORTING
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

BRES.LSOA <- fread(file=paste0(wd.data, "/Economic/BRES_LSOA_SIC1.csv"), stringsAsFactors=FALSE, header=F, sep=",", skip="2011 super output area - lower layer")
BRES.LSOA <- BRES.LSOA[BRES.LSOA$V1!="Column Total", -c(paste0("V", seq(3, 41, 2)))]
names(BRES.LSOA) <- c("LSOA_CD", paste0("SIC_", LETTERS[1:21]))
BRES.LSOA$LSOA_CD <- do.call(c, lapply(str_split(BRES.LSOA$LSOA_CD, " : "), function(x) x[1]))
BRES.LSOA[, Total:=rowSums(.SD), .SDcols = 2:dim(BRES.LSOA)[2]]
BRES.LSOA2 <- fread(file=paste0(wd.data, "/Economic/BRES_LSOA_SIC2.csv"), stringsAsFactors=FALSE, header=F, sep=",", skip="2011 super output area - lower layer")
BRES.LSOA2 <- BRES.LSOA2[BRES.LSOA2$V1!="Column Total", -c(paste0("V", seq(3, 9, 2)))]
names(BRES.LSOA2) <- c("LSOA_CD", paste0("SIC_", c("45", "46", "47", "55", "56")))
BRES.LSOA2$LSOA_CD <- do.call(c, lapply(str_split(BRES.LSOA2$LSOA_CD, " : "), function(x) x[1]))
BRES.LSOA <- merge(BRES.LSOA, BRES.LSOA2, all.x=T, sort=F, by="LSOA_CD")
BRES.LSOA <- merge(BRES.LSOA, unique(geographic.lookup[,c("LSOA_CD", "LAD_CD", "RGN_CD", "LEP_CD1", "LEP_CD2", "CA_CD")]), all.x=T, sort=F, by="LSOA_CD")
BRES.LSOA <- BRES.LSOA[,c("LSOA_CD", "Total", "SIC_G", "SIC_45", "SIC_46", "SIC_47", "SIC_I", "SIC_55", "SIC_56")]
rm(BRES.LSOA2)

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
#----------------------------------------------------------------------------

# READ IN AND CLEAN UP NATIONAL RETAIL SALES INDEX TABLES
#----------------------------------------------------------------------------
Retail.Index <- fread(file=paste0(wd.data, "/Retail & Fast Indicators/RSI Time Series.csv"), stringsAsFactors=FALSE, header=FALSE)

Retail.IndexLU <- data.table(t(Retail.Index[c(1, 2, 5, 6),]))
names(Retail.IndexLU) <- unlist(Retail.IndexLU[1,])
Retail.IndexLU <- Retail.IndexLU[-1,]

# Retail Sales Index Internet Sales
# VALUE SEASONALLY ADJUSTED AVERAGE WEEKLY SALES IN POUNDS MILLION (MZ..)
# INTERNET RETAIL SALES (Non-seasonally adjusted) (JE/4..)
# VALUE OF INTERNET SALES AT CURRENT PRICES SEASONALLY ADJUSTED (KP..) (Sales Index == 2018)
RSI.Int_LU <- Retail.IndexLU[Retail.IndexLU$CDID %in% c("MZX6", "MZX7", "MZX8", "MZX9", "MZXV", 
  "MZY2", "MZY3", "MZY4", "JE4W", "JE2J", "J4MC", "KPC4", "KPB8", "KPB9", "KPB7", "KPC5", "KPC6", 
  "KPC2", "KPC3"),]
RSI.Int <- Retail.Index[,c(1, (which(Retail.IndexLU$CDID %in% RSI.Int_LU$CDID)+1)),with=F]
RSI.Int <- RSI.Int[-c(1,3:7),]
names(RSI.Int) <- unlist(RSI.Int[1,])
names(RSI.Int)[names(RSI.Int)=="CDID"] <- "MONTH"
RSI.Int <- RSI.Int[-1,]
RSI.Int <- RSI.Int[which(RSI.Int$MONTH=="2006 JAN"):dim(RSI.Int)[1],]
RSI.Int$MONTH <- paste0(str_to_title(RSI.Int$MONTH), " 01")
RSI.Int$MONTH <- as.Date(RSI.Int$MONTH, "%Y %b %d")
RSI.Int[,(names(RSI.Int)[sapply(RSI.Int, is.character)]):= lapply(.SD, as.numeric), .SDcols = names(RSI.Int)[sapply(RSI.Int, is.character)]]
RSI.Int <- RSI.Int[RSI.Int$MONTH >= as.Date("2006-11-01"),]


# Table 1 M of the Retail Sales Index release
# CHAINED VOLUME OF RETAIL SALES SEASONALLY ADJUSTED
# Index numbers of sales per week (Sales Index == 2018)
RSI.T1_LU <- Retail.IndexLU[Retail.IndexLU$CDID %in% c("J5EK", "J467", "EAPT", "JO59", "JO5U", 
  "JO5V", "EAPV", "EAPU", "EAPX", "JO5C", "JO5I", "JO5J", "EAPY", "JO5G", "JO5F", 
  "JO5D", "JO5Y", "EAPW", "JO5K", "JO5L", "JO5M", "JO5Z", "JO5E", "JO64", "JO62", 
  "JO5N", "JO5O", "JO5P", "JO5Q", "J5DZ", "JO5R", "JO5S", "JO5A"),]
RSI.T1 <- Retail.Index[,c(1, (which(Retail.IndexLU$CDID %in% RSI.T1_LU$CDID)+1)),with=F]
RSI.T1 <- RSI.T1[-c(1,3:7),]
names(RSI.T1) <- unlist(RSI.T1[1,])
names(RSI.T1)[names(RSI.T1)=="CDID"] <- "MONTH"
RSI.T1 <- RSI.T1[-1,]
RSI.T1 <- RSI.T1[which(RSI.T1$MONTH=="1988 JAN"):dim(RSI.T1)[1],]
RSI.T1$MONTH <- paste0(str_to_title(RSI.T1$MONTH), " 01")
RSI.T1$MONTH <- as.Date(RSI.T1$MONTH, "%Y %b %d")
RSI.T1[,(names(RSI.T1)[sapply(RSI.T1, is.character)]):= lapply(.SD, as.numeric), .SDcols = names(RSI.T1)[sapply(RSI.T1, is.character)]]


# Table 2 M of the Retail Sales Index release
# VALUE OF RETAIL SALES AT CURRENT PRICES SEASONALLY ADJUSTED
# Index value of sales per week (Sales Index == 2018)
RSI.T2_LU <- Retail.IndexLU[Retail.IndexLU$CDID %in% c("J5C4", "J468", "EAQW", "JO2F", "JO32", 
  "JO33", "EAQY", "EAQX", "EARA", "JO2I", "JO2O", "JO2P", "EARB", "JO2M", "JO2L", 
  "JO2J", "JO36", "EAQZ", "JO2Q", "JO2R", "JO2S", "JO37", "JO2K", "JO3A", "JO38", 
  "JO2T", "JO2U", "JO2V", "JO2W", "J5BI", "JO2X", "JO2Y", "JO2G"),]
RSI.T2 <- Retail.Index[,c(1, (which(Retail.IndexLU$CDID %in% RSI.T2_LU$CDID)+1)),with=F]
RSI.T2 <- RSI.T2[-c(1,3:7),]
names(RSI.T2) <- unlist(RSI.T2[1,])
names(RSI.T2)[names(RSI.T2)=="CDID"] <- "MONTH"
RSI.T2 <- RSI.T2[-1,]
RSI.T2 <- RSI.T2[which(RSI.T2$MONTH=="1988 JAN"):dim(RSI.T2)[1],]
RSI.T2$MONTH <- paste0(str_to_title(RSI.T2$MONTH), " 01")
RSI.T2$MONTH <- as.Date(RSI.T2$MONTH, "%Y %b %d")
RSI.T2[,(names(RSI.T2)[sapply(RSI.T2, is.character)]):= lapply(.SD, as.numeric), .SDcols = names(RSI.T2)[sapply(RSI.T2, is.character)]]

Retail.IndexLU <- rbindlist(list(cbind(RSI.Int_LU, table="Internet"), cbind(RSI.T1_LU, table="Table 1"), cbind(RSI.T2_LU, table="Table 2")))
rm(Retail.Index)

Retail.IndexLU[Retail.IndexLU$CDID %in% c("J5A3", "J3L6", "IV3G", "IV3I", "IV2X", 
  "IW6X", "IW6Y", "IW6U", "J58P", "IZ57"),]
#----------------------------------------------------------------------------








#############################################################################
##### FIGURE 0: SOCIAL SECTOR SIC CODES
###

# SECTOR COLOUR LOOKUP
#----------------------------------------------------------------------------
Social.Lookup <- unique(SIC.lookup[SIC.lookup$BIG_CD=="BIG 9" | SIC.lookup$BIG_CD=="BIG 7", c("BIG_CD", "BIG_NM", "Division_CD", "Division_NM", "Colour")])
Social.Lookup$Col_H <- c("#2E94AB", "#C46827", "#C46827")
Social.Lookup$Col_L <- c("#ECFBFE", "#FEF3EA", "#FEF3EA")
Social.Lookup$Bar <- as.numeric(5)
Social.Lookup$Col_SIC <- c(Social.Lookup$Colour[1], colorRampPalette(c(unique(Social.Lookup[Social.Lookup$BIG_CD=="BIG 9",]$Col_L), unique(Social.Lookup[Social.Lookup$BIG_CD=="BIG 9",]$Colour)))(10)[5], Social.Lookup$Colour[3])
Social.Lookup$label <- paste0(Social.Lookup$Division_NM, " (", Social.Lookup$BIG_CD, ")")

Social.Lookup_plot <- ggplot(Social.Lookup, aes(x=Division_CD, y=Bar, fill=Division_NM), colour="#E5E5E3") +
  geom_bar(stat = "identity", fill = Social.Lookup$Col_SIC, alpha=0.6) +
  geom_text(data=Social.Lookup, mapping=aes(x=Division_CD, y=0, label=label), fontface="italic", colour="black", hjust=0, alpha=0.8) +
  labs(title = "Social Sector SIC Codes", subtitle = "Colour matching guide", x = "", y = "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_flip() +
  guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1)),
    color = guide_legend(override.aes = list(size = 1))) +
  theme_classic() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
    legend.position = "none", legend.title = element_blank(), panel.background=element_rect(fill="#E5E5E3"))
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE 1: LOCAL PROPORTION OF RETAIL + ACCOMODATION
###

# CLEANED AGGREGATED BSD DATA
#----------------------------------------------------------------------------
local.social <- rbind(BSD.sic2[["LAD"]][BSD.sic2[["LAD"]]$sic.division %in% c("47", "55", "56"),],
  cbind(BSD.sic2[["LAD"]][,.(business=sum(business, na.rm=T), employment=sum(employment, na.rm=T)),by=.(LAD_CD, snapshot, RGN_CD)], sic.division="Total"))
local.social <- local.social[order(local.social$LAD_CD, local.social$snapshot, local.social$sic.division),]
local.social <- dcast(local.social, LAD_CD + snapshot + RGN_CD ~ sic.division, value.var=c("business", "employment"))
names(local.social) <- c("LAD_CD", "snapshot", "RGN_CD", "B_LCL_SIC47", "B_LCL_SIC55", "B_LCL_SIC56", "B_LCL_Total", "E_LCL_SIC47", "E_LCL_SIC55", "E_LCL_SIC56", "E_LCL_Total")

regional <- local.social[,-c("LAD_CD")][,.(B_RGN_SIC47=sum(B_LCL_SIC47, na.rm=T), B_RGN_SIC55=sum(B_LCL_SIC55, na.rm=T), B_RGN_SIC56=sum(B_LCL_SIC56, na.rm=T), B_RGN_Total=sum(B_LCL_Total, na.rm=T), 
  E_RGN_SIC47=sum(E_LCL_SIC47, na.rm=T), E_RGN_SIC55=sum(E_LCL_SIC55, na.rm=T), E_RGN_SIC56=sum(E_LCL_SIC56, na.rm=T), E_RGN_Total=sum(E_LCL_Total, na.rm=T)), by=.(snapshot, RGN_CD)]
  
national <- regional[,-c("RGN_CD")][,.(B_NTL_SIC47=sum(B_RGN_SIC47, na.rm=T), B_NTL_SIC55=sum(B_RGN_SIC55, na.rm=T), B_NTL_SIC56=sum(B_RGN_SIC56, na.rm=T), B_NTL_Total=sum(B_RGN_Total, na.rm=T), 
  E_NTL_SIC47=sum(B_RGN_SIC47, na.rm=T), E_NTL_SIC55=sum(B_RGN_SIC55, na.rm=T), E_NTL_SIC56=sum(B_RGN_SIC56, na.rm=T), E_NTL_Total=sum(E_RGN_Total, na.rm=T)), by=.(snapshot)]

local.social <- merge(local.social, regional, all.x=T, sort=F, by=c("RGN_CD", "snapshot"))
local.social <- merge(local.social, national, all.x=T, sort=F, by=c("snapshot"))
rm(regional, national)
#----------------------------------------------------------------------------

# PROPORTION OF TOTAL LOCAL WORKFORCE IN EACH SECTOR
#----------------------------------------------------------------------------
fig.1 <- local.social[snapshot==2020,]
fig.1$B_LCL_SIC47 <- fig.1$B_LCL_SIC47/fig.1$B_LCL_Total
fig.1$B_LCL_SIC55 <- fig.1$B_LCL_SIC55/fig.1$B_LCL_Total
fig.1$B_LCL_SIC56 <- fig.1$B_LCL_SIC56/fig.1$B_LCL_Total
fig.1$B_RGN_SIC47 <- fig.1$B_RGN_SIC47/fig.1$B_RGN_Total
fig.1$B_RGN_SIC55 <- fig.1$B_RGN_SIC55/fig.1$B_RGN_Total
fig.1$B_RGN_SIC56 <- fig.1$B_RGN_SIC56/fig.1$B_RGN_Total
fig.1$B_NTL_SIC47 <- fig.1$B_NTL_SIC47/fig.1$B_NTL_Total
fig.1$B_NTL_SIC55 <- fig.1$B_NTL_SIC55/fig.1$B_NTL_Total
fig.1$B_NTL_SIC56 <- fig.1$B_NTL_SIC56/fig.1$B_NTL_Total
fig.1$E_LCL_SIC47 <- fig.1$E_LCL_SIC47/fig.1$E_LCL_Total
fig.1$E_LCL_SIC55 <- fig.1$E_LCL_SIC55/fig.1$E_LCL_Total
fig.1$E_LCL_SIC56 <- fig.1$E_LCL_SIC56/fig.1$E_LCL_Total
fig.1$E_RGN_SIC47 <- fig.1$E_RGN_SIC47/fig.1$E_RGN_Total
fig.1$E_RGN_SIC55 <- fig.1$E_RGN_SIC55/fig.1$E_RGN_Total
fig.1$E_RGN_SIC56 <- fig.1$E_RGN_SIC56/fig.1$E_RGN_Total
fig.1$E_NTL_SIC47 <- fig.1$E_NTL_SIC47/fig.1$E_NTL_Total
fig.1$E_NTL_SIC55 <- fig.1$E_NTL_SIC55/fig.1$E_NTL_Total
fig.1$E_NTL_SIC56 <- fig.1$E_NTL_SIC56/fig.1$E_NTL_Total

fig.1A <- fig.1[,-c("B_LCL_Total", "E_LCL_Total", "B_RGN_Total", "E_RGN_Total", "B_NTL_Total" ,"E_NTL_Total")]
#----------------------------------------------------------------------------

# PROPORTION OF TOTAL SECTOR WORKFORCE IN EACH LOCALITY
#----------------------------------------------------------------------------
fig.1 <- local.social[snapshot==2020,]
fig.1$B_LCL_SIC47 <- fig.1$B_LCL_SIC47/sum(fig.1$B_LCL_SIC47, na.rm=T)
fig.1$B_LCL_SIC55 <- fig.1$B_LCL_SIC55/sum(fig.1$B_LCL_SIC55, na.rm=T)
fig.1$B_LCL_SIC56 <- fig.1$B_LCL_SIC56/sum(fig.1$B_LCL_SIC56, na.rm=T)
fig.1$B_RGN_SIC47 <- fig.1$B_RGN_SIC47/sum(unique(fig.1[,c("RGN_CD", "B_RGN_SIC47")])$B_RGN_SIC47, na.rm=T)
fig.1$B_RGN_SIC55 <- fig.1$B_RGN_SIC55/sum(unique(fig.1[,c("RGN_CD", "B_RGN_SIC55")])$B_RGN_SIC55, na.rm=T)
fig.1$B_RGN_SIC56 <- fig.1$B_RGN_SIC56/sum(unique(fig.1[,c("RGN_CD", "B_RGN_SIC56")])$B_RGN_SIC56, na.rm=T)
fig.1$B_NTL_SIC47 <- fig.1$B_NTL_SIC47/unique(fig.1[,c("B_NTL_SIC47")])$B_NTL_SIC47
fig.1$B_NTL_SIC55 <- fig.1$B_NTL_SIC55/unique(fig.1[,c("B_NTL_SIC55")])$B_NTL_SIC55
fig.1$B_NTL_SIC56 <- fig.1$B_NTL_SIC56/unique(fig.1[,c("B_NTL_SIC56")])$B_NTL_SIC56
fig.1$E_LCL_SIC47 <- fig.1$E_LCL_SIC47/sum(fig.1$E_LCL_SIC47, na.rm=T)
fig.1$E_LCL_SIC55 <- fig.1$E_LCL_SIC55/sum(fig.1$E_LCL_SIC55, na.rm=T)
fig.1$E_LCL_SIC56 <- fig.1$E_LCL_SIC56/sum(fig.1$E_LCL_SIC56, na.rm=T)
fig.1$E_RGN_SIC47 <- fig.1$E_RGN_SIC47/sum(unique(fig.1[,c("RGN_CD", "E_RGN_SIC47")])$E_RGN_SIC47, na.rm=T)
fig.1$E_RGN_SIC55 <- fig.1$E_RGN_SIC55/sum(unique(fig.1[,c("RGN_CD", "E_RGN_SIC55")])$E_RGN_SIC55, na.rm=T)
fig.1$E_RGN_SIC56 <- fig.1$E_RGN_SIC56/sum(unique(fig.1[,c("RGN_CD", "E_RGN_SIC56")])$E_RGN_SIC56, na.rm=T)
fig.1$E_NTL_SIC47 <- fig.1$E_NTL_SIC47/unique(fig.1[,c("E_NTL_SIC47")])$E_NTL_SIC47
fig.1$E_NTL_SIC55 <- fig.1$E_NTL_SIC55/unique(fig.1[,c("E_NTL_SIC55")])$E_NTL_SIC55
fig.1$E_NTL_SIC56 <- fig.1$E_NTL_SIC56/unique(fig.1[,c("E_NTL_SIC56")])$E_NTL_SIC56

fig.1B <- fig.1[,-c("B_LCL_Total", "E_LCL_Total", "B_RGN_Total", "E_RGN_Total", "B_NTL_Total" ,"E_NTL_Total")]
#----------------------------------------------------------------------------

# GENERATE FIGURE 1 COMPONENTS
#----------------------------------------------------------------------------
fig.1 <- lapply(as.list(local.profiles), function(G){

  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM")])
  labels <- local.social[local.social$snapshot==2020 & local.social$LAD_CD==G,]
  
  plot1 <- melt(fig.1A[fig.1A$LAD_CD==G,][,-c(1:3)])
  plot1$Zone <- do.call(c, lapply(str_split(plot1$variable, "_"), function(x) x[2]))
  plot1$SIC <- do.call(c, lapply(str_split(plot1$variable, "_"), function(x) x[3]))
  plot1$variable <- do.call(c, lapply(str_split(plot1$variable, "_"), function(x) x[1]))
  plot1$variable[plot1$variable=="E"] <- "Employment"
  plot1$variable[plot1$variable=="B"] <- "Business"
  plot1$variable <- factor(plot1$variable, levels=c("Business", "Employment"))
  plot1$Zone[plot1$Zone=="LCL"] <- "Local"
  plot1$Zone[plot1$Zone=="RGN"] <- "Regional"
  plot1$Zone[plot1$Zone=="NTL"] <- "National"
  plot1$Zone <- factor(plot1$Zone, levels=c("Local", "Regional", "National"))
  plot1$SIC[plot1$SIC=="SIC47"] <- "Retail"
  plot1$SIC[plot1$SIC=="SIC55"] <- "Accommodation"
  plot1$SIC[plot1$SIC=="SIC56"] <- "Food & Beverage"
  plot1$SIC <- factor(plot1$SIC, levels=c("Retail", "Food & Beverage", "Accommodation"))
  
  plot2 <- melt(fig.1B[fig.1B$LAD_CD==G,][,-c(1:3)])
  plot2$Zone <- do.call(c, lapply(str_split(plot2$variable, "_"), function(x) x[2]))
  plot2$SIC <- do.call(c, lapply(str_split(plot2$variable, "_"), function(x) x[3]))
  plot2$variable <- do.call(c, lapply(str_split(plot2$variable, "_"), function(x) x[1]))
  plot2 <- plot2[plot2$Zone!="NTL",]
  plot2$variable[plot2$variable=="E"] <- "Employment"
  plot2$variable[plot2$variable=="B"] <- "Business"
  plot2$variable <- factor(plot2$variable, levels=c("Business", "Employment"))
  plot2$Zone[plot2$Zone=="LCL"] <- "Local"
  plot2$Zone[plot2$Zone=="RGN"] <- "Regional"
  plot2$Zone <- factor(plot2$Zone, levels=c("Local", "Regional"))
  plot2$SIC[plot2$SIC=="SIC47"] <- "Retail"
  plot2$SIC[plot2$SIC=="SIC55"] <- "Accommodation"
  plot2$SIC[plot2$SIC=="SIC56"] <- "Food & Beverage"
  plot2$SIC <- factor(plot2$SIC, levels=c("Retail", "Food & Beverage", "Accommodation"))
  
  fig.1A_B <- ggplot() +
    geom_col(data=plot1[plot1$variable=="Business"], aes(x=Zone, y=value, fill=SIC), alpha=0.5) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    labs(title=paste0("Registered Businesses - ", names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"),
      subtitle=paste0("Proportion of Area's Business in Social Sector"), ylab="Percent of Workforce") +
    scale_fill_manual(values=c(unique(SIC.lookup[SIC.lookup$BIG_CD=="BIG 7", c("BIG_CD", "Colour")])$Colour, unique(SIC.lookup[SIC.lookup$BIG_CD=="BIG 9", c("BIG_CD", "Colour")])$Colour, "#F8A86F"), aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none",
      panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3")) +
    facet_wrap(~SIC, ncol=3)
  
  fig.1B_B <- ggplot() +
    geom_col(data=plot2[plot2$variable=="Business"], aes(x=Zone, y=value, fill=SIC), alpha=0.5) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    labs(title=paste0("Registered Businesses - ", names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"),
         subtitle=paste0("Proportion of Area's Workforce in Social Sector"), ylab="Percent of Workforce") +
    scale_fill_manual(values=c(unique(SIC.lookup[SIC.lookup$BIG_CD=="BIG 7", c("BIG_CD", "Colour")])$Colour, unique(SIC.lookup[SIC.lookup$BIG_CD=="BIG 9", c("BIG_CD", "Colour")])$Colour, "#F8A86F"), aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none",
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3")) +
    coord_flip() +
    facet_wrap(~SIC, ncol=3)
  
  fig.1A_E <- ggplot() +
    geom_col(data=plot1[plot1$variable=="Employment"], aes(x=Zone, y=value, fill=SIC), alpha=0.5) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    labs(title=paste0("Registered Employment - ", names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"),
         subtitle=paste0("Proportion of Area's Workforce in Social Sector"), ylab="Percent of Workforce") +
    scale_fill_manual(values=c("#42d4f4", "#f58231", "#F8A86F"), aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none",
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3")) +
    facet_wrap(~SIC, ncol=3)
  
  fig.1B_E <- ggplot() +
    geom_col(data=plot2[plot2$variable=="Employment"], aes(x=Zone, y=value, fill=SIC), alpha=0.5) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), labels = scales::percent) +
    labs(title=paste0("Registered Employment - ", names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"),
         subtitle=paste0("Proportion of Area's Workforce in Social Sector"), ylab="Percent of Workforce") +
    scale_fill_manual(values=c("#42d4f4", "#f58231", "#F8A86F"), aesthetics=c("fill")) +
    theme_classic() +
    theme(axis.title.y=element_blank(), axis.title.x=element_blank(), legend.position="none",
          panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3")) +
    coord_flip() +
    facet_wrap(~SIC, ncol=3)
  
  #geom_text(data=totals.B, x=totals.B$x.coord, y=totals.B$y.coord, aes(label=totals.B$label), alpha=0.4, fontface="italic", hjust=0) +
  #"Total: ", format(sum(totals.B$business), big.mark=",")
  
  return(list(fig.1A_B, fig.1B_B, fig.1A_E, fig.1B_E))
})

fig.1A_BUS <- lapply(fig.1, function(x) x[[1]])
names(fig.1A_BUS) <- local.profiles

fig.1B_BUS <- lapply(fig.1, function(x) x[[2]])
names(fig.1B_BUS) <- local.profiles

fig.1A_EMP <- lapply(fig.1, function(x) x[[3]])
names(fig.1A_EMP) <- local.profiles

fig.1B_EMP <- lapply(fig.1, function(x) x[[4]])
names(fig.1B_EMP) <- local.profiles
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE X: NATIONAL CARD SPEND DATA
###

# CHAPS Card Spend Data (ONS Fast Indicators)
#----------------------------------------------------------------------------
# Next Release 15 April 2021
card.spend <- fread(file=paste0(wd.data, "/Retail & Fast Indicators/Card_Spend.csv"))
card.spend$Date <- as.Date(card.spend$Date, format="%Y-%m-%d")
card.spend <- melt(card.spend, id.vars="Date", variable.name="Type", value.name="Index")
card.spend$Type <- factor(as.character(card.spend$Type), levels=c("Aggregate", "Work", "Staple", "Social", "Delayable"))

card.spend <- ggplot() +
  geom_line(card.spend[card.spend$Type=="Aggregate",], mapping=aes(x=Date, y=Index), colour="black", linetype="dashed", alpha=0.6, lwd=1.25) +
  geom_line(card.spend[card.spend$Type!="Aggregate",], mapping=aes(x=Date, y=Index, group=Type, colour=Type), alpha=0.6, lwd=1.25) +
  scale_colour_manual(values=c(palette[[1]][palette[[1]]$Colour=="Blue",]$Col, palette[[1]][palette[[1]]$Colour=="Red",]$Col, 
    palette[[1]][palette[[1]]$Colour=="Yellow",]$Col, palette[[1]][palette[[1]]$Colour=="Green",]$Col)) +
  geom_hline(yintercept = 100, linetype="dotted", alpha=0.5) +
  annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2020-07-01"), ymin=0, ymax=Inf, alpha=0.1, fill="black") +
  scale_x_date(expand = c(0,0), date_breaks="1 month", labels=date_format("%B - %Y"), limits = c(as.Date("2020-01-01"), NA)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "National Card Spend Changes", 
       subtitle = paste0("Indexed Spending: (Feb. 2020 = 100)"),
       caption = paste0("Source: Clearing House Automated Payment System (CHAPS)")) +    
  theme_classic() +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank(),
        plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
        panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic"))
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE X: RETAIL SALES INDEX
###

# TABLE INDEX OF RETAIL SALES (2018 = 100)
#----------------------------------------------------------------------------
t1 <- RSI.T1[,c("MONTH", "J5EK", "J467", "EAPT", "EAPV", "J5DZ")]
names(t1) <- c("MONTH", "all", "all_Nfuel", "food", "non_food", "non_store")
t1 <- melt(t1, id.vars="MONTH", value.name="Index", variable.name="Type")
t1$Variable <- "Volume Index"

t2 <- RSI.T2[,c("MONTH", "J5C4", "J468", "EAQW", "EAQY", "J5BI")]
names(t2) <- c("MONTH", "all", "all_Nfuel", "food", "non_food", "non_store")
t2 <- melt(t2, id.vars="MONTH", value.name="Index", variable.name="Type")
t2$Variable <- "Value Index"

t <- rbind(t1, t2); rm(t1, t2)
t$Type <- as.character(t$Type)
t <- t[t$Type!="all",]
t$Type[t$Type=="all_Nfuel"] <- "All"
t$Type[t$Type=="food"] <- "Food Retail"
t$Type[t$Type=="non_food"] <- "Non-Food Retail"
t$Type[t$Type=="non_store"] <- "Non-Store Retail"
t$Type <- factor(as.character(t$Type), levels=c("All", "Food Retail", "Non-Food Retail", "Non-Store Retail"))
#----------------------------------------------------------------------------

# VALUE AND VOLUME OF RETAIL SALES INDEX
#----------------------------------------------------------------------------
RSI.plot <- ggplot() +
  geom_line(t[t$Type=="All",], mapping=aes(x=MONTH, y=Index), colour="black", linetype="dashed", alpha=0.6, lwd=1.25) +
  geom_line(t[t$Type!="All",], mapping=aes(x=MONTH, y=Index, group=Type, colour=Type), alpha=0.6, lwd=1.25) +
  scale_colour_manual(values=c(palette[[1]][palette[[1]]$Colour=="Blue",]$Col, palette[[1]][palette[[1]]$Colour=="Red",]$Col, 
    palette[[1]][palette[[1]]$Colour=="Green",]$Col)) +
  geom_hline(yintercept = 100, linetype="dotted", alpha=0.5) +
  annotate("rect", xmin=as.Date("2020-04-01"), xmax=as.Date("2020-07-01"), ymin=0, ymax=Inf, alpha=0.1, fill="black") +
  scale_x_date(expand = c(0,0), date_breaks="1 month", labels=date_format("%B - %Y"), limits = c(as.Date("2019-01-01"), NA)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(title = "Retail Sales Index", 
    subtitle = paste0("Feb. 2018 = 100"),
    caption = paste0("Source: ONS Retail Sales Index")) +
  facet_wrap(~Variable, nrow=2) +
  theme_classic() +
  theme(legend.position="bottom", legend.title=element_blank(), axis.text.x=element_text(angle=45, hjust=1), axis.title.x=element_blank(),
    plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
    panel.background = element_rect(fill="#E5E5E3", colour="#E5E5E3"), plot.caption = element_text(face = "italic")) 
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE X: BRES SMALL AREA WORKFORCE DISTRIBUTION
###

# MERGE SPATIAL DATA 
#----------------------------------------------------------------------------
fig.3 <- merge(LSOA.sp, BRES.LSOA[,c("LSOA_CD", "Total", "SIC_47", "SIC_I")], all.x=T, sort=F)
fig.3$dens <- fig.3$Total/fig.3$area_ha
fig.3$dens[is.na(fig.3$dens)] <- 0
fig.3$densBIG7 <- fig.3$SIC_47/fig.3$area_ha
fig.3$densBIG7[is.na(fig.3$densBIG7)] <- 0
fig.3$densBIG9 <- fig.3$SIC_I/fig.3$area_ha
fig.3$densBIG9[is.na(fig.3$densBIG9)] <- 0
fig.3 <- fig.3[!st_is_empty(fig.3),,drop=FALSE]
#----------------------------------------------------------------------------

# TOTAL WORKFORCE DENSITY PLOTS
#----------------------------------------------------------------------------
density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$dens, breaks = unique(quantile(q$dens, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$dens, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r1 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c("Grey", "Black"))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title.position="top", title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Workforce Density Deciles (Rank)"),
         subtitle=paste0("Total workers per ha."), x="", y="", caption="\n") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          legend.position="bottom", panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG7, breaks = unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r2 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title.position="top", title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title="",
         subtitle=paste0("Retail workers per ha."), x="", y="", caption="\n") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          legend.position="bottom", panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG9, breaks = unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r3 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title.position="top", title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title="",
         subtitle=paste0("Accomodation workers per ha."), x="", y="", caption="LSOA Worker Density Distributions\nSource: ONS SRS - Business Registry and Employment Survey 2019") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          legend.position="bottom", panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  return(list(r1, r2, r3)) })
fig.3A <- lapply(density.plots, function(x) x[[1]])
names(fig.3A) <- local.profiles

fig.3B <- lapply(density.plots, function(x) x[[2]])
names(fig.3B) <- local.profiles

fig.3C <- lapply(density.plots, function(x) x[[3]])
names(fig.3C) <- local.profiles
rm(density.plots)
#----------------------------------------------------------------------------

  

#############################################################################
##### FIGURE X: BROADER RETAIL WORKFORCE
###

# RETAIL WORKFORCE DENSITY PLOTS
#----------------------------------------------------------------------------
density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])
  
  q <- unique(fig.3[fig.3$LEP_CD1==names.lookup$LEP_CD1 | fig.3$LEP_CD2==names.lookup$LEP_CD1 ,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG7, breaks = unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r1 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 1, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title.position="top", title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title="", subtitle=paste0("Relative to the broader ", names.lookup$LEP_NM1, " LEP"), x="", y="", caption="Source: ONS SRS - Business Registry and Employment Survey 2019") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG7, breaks = unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r2 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1)),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Retail Workforce Density Deciles"),
         subtitle=paste0(names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"), x="", y="", caption="") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          legend.position="none", legend.title = element_blank(),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  return(list(r2, r1)) })
fig.4A <- lapply(density.plots, function(x) x[[1]])
names(fig.4A) <- local.profiles

fig.4B <- lapply(density.plots, function(x) x[[2]])
names(fig.4B) <- local.profiles
rm(density.plots)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE X: BROADER ACCOMODATION WORKFORCE
###

# ACCOMODATION WORKFORCE DENSITY PLOTS
#----------------------------------------------------------------------------
density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])
  
  q <- unique(fig.3[fig.3$LEP_CD1==names.lookup$LEP_CD1 | fig.3$LEP_CD2==names.lookup$LEP_CD1 ,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG9, breaks = unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)), 
                    labels = 1:(length(unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r1 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 1, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title.position="top", title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title="", subtitle=paste0("Relative to the broader ", names.lookup$LEP_NM1, " LEP"), x="", y="", caption="Source: ONS SRS - Business Registry and Employment Survey 2019") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG9, breaks = unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)), 
                    labels = 1:(length(unique(quantile(q$densBIG9, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r2 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="55",]$Col_H))(10), na.translate=FALSE) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1)),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Accomodation Workforce Density Deciles"),
         subtitle=paste0(names.lookup$LAD_NM, " (LAD: ", names.lookup$LAD_CD, ")"), x="", y="", caption="") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          legend.position="none", legend.title = element_blank(),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  return(list(r2, r1)) })
fig.5A <- lapply(density.plots, function(x) x[[1]])
names(fig.5A) <- local.profiles

fig.5B <- lapply(density.plots, function(x) x[[2]])
names(fig.5B) <- local.profiles
rm(density.plots)
#----------------------------------------------------------------------------



#############################################################################
##### FIGURE X: RETAIL EMPLOYMENT AND LOCAL RETAIL CENTRES
###

# RETAIL CENTRES AND RETAIL WORKFORCE (2019)
#----------------------------------------------------------------------------
density.plots <- lapply(as.list(local.profiles), function(G){
  names.lookup <- unique(geographic.lookup[geographic.lookup$LAD_CD==G, c("LAD_CD", "LAD_NM", "LEP_CD1", "LEP_NM1")])

  RC <- Retail.sp[Retail.sp$LAD_CD==names.lookup$LAD_CD,]
  RC <- RC[!is.na(RC$RC_ID),]
  
  q <- unique(fig.3[fig.3$LAD_CD==names.lookup$LAD_CD,])
  q <- q[!is.na(q$LSOA_CD),]
  q$D.decile <- cut(q$densBIG7, breaks = unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)), 
    labels = 1:(length(unique(quantile(q$densBIG7, probs = 0:10/10, na.rm=T)))-1), right=F, include.lowest=T)
  r2 <- ggplot() +
    geom_sf(data = q, aes(fill = D.decile), lwd = 0, alpha=0.2) +
    scale_fill_manual(values=colorRampPalette(c(Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_L, Social.Lookup[Social.Lookup$Division_CD=="47",]$Col_H))(10), na.translate=FALSE) +
    geom_sf(data = RC, mapping=aes(colour = Classification), lwd = 0.75, alpha=0.3) +
    scale_colour_manual(values=palette[[2]][1:7], na.translate=FALSE) +
    geom_sf(data =  LAD.sp[LAD.sp$LAD_CD==G,], colour=alpha("Black", 0.6), lwd = 0.75, alpha=0) +
    scale_x_continuous(expand = c(0,0)) +
    coord_sf() +
    guides(shape = guide_legend(override.aes = list(size = 1)), fill = guide_legend(override.aes = list(size = 1), title="Rank"),
           color = guide_legend(override.aes = list(size = 1))) +
    labs(title=paste0("Local Retail Centres"),
         subtitle=paste0("Delineated areas of retail unit clusters"), x="", y="", caption="Source: CDRC Retail Centre Boundaries") +
    theme_classic() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          plot.title = element_text(face = "bold"), plot.subtitle = element_text(face = "italic"),
          panel.background=element_rect(fill="#E5E5E3"), plot.caption = element_text(face = "italic"))
  
  return(r2) })
fig.RC <- density.plots
names(fig.RC) <- local.profiles
rm(density.plots)
#----------------------------------------------------------------------------

  
  
  






#############################################################################
##### FIGURE 2: FULL/PART TIME & BUSINESS SIZE
###

# CLEANED AGGREGATED BRES DATA
#----------------------------------------------------------------------------
local.social <- rbind(BRES.Counts[["LAD"]][BRES.Counts[["LAD"]]$BIG_CD %in% c("BIG 7", "BIG 9"), c("LAD_CD", "snapshot", "RGN_CD", "weighted_ftempee", "weighted_ptempee", "weighted_totempee", "NE.small", "BIG_CD")],
  cbind(BRES.Counts[["LAD"]][,.(weighted_ftempee=sum(weighted_ftempee, na.rm=T), weighted_ptempee=sum(weighted_ptempee, na.rm=T),
    weighted_totempee=sum(weighted_totempee, na.rm=T), NE.small=sum(NE.small, na.rm=T)),by=.(LAD_CD, snapshot, RGN_CD)], BIG_CD="Total"))
local.social <- local.social[order(local.social$LAD_CD, local.social$snapshot, local.social$BIG_CD),]
local.social <- dcast(local.social, LAD_CD + snapshot + RGN_CD ~ BIG_CD, value.var=c("weighted_ftempee", "weighted_ptempee", "weighted_totempee", "NE.small"))
names(local.social) <- c("LAD_CD", "snapshot", "RGN_CD", "FT_LCL_BIG7", "FT_LCL_BIG9", "FT_LCL_Total", "PT_LCL_BIG7", "PT_LCL_BIG9", "PT_LCL_Total", 
  "N_LCL_BIG7", "N_LCL_BIG9", "N_LCL_Total", "Nsm_LCL_BIG7", "Nsm_LCL_BIG9", "Nsm_LCL_Total")

regional <- local.social[,-c("LAD_CD")][,.(FT_RGN_BIG7=sum(FT_LCL_BIG7, na.rm=T), FT_RGN_BIG9=sum(FT_LCL_BIG9, na.rm=T), FT_RGN_Total=sum(FT_LCL_Total, na.rm=T), 
  PT_RGN_BIG7=sum(PT_LCL_BIG7, na.rm=T), PT_RGN_BIG9=sum(PT_LCL_BIG9, na.rm=T), PT_RGN_Total=sum(PT_LCL_Total, na.rm=T), 
  N_RGN_BIG7=sum(N_LCL_BIG7, na.rm=T), N_RGN_BIG9=sum(N_LCL_BIG9, na.rm=T), N_RGN_Total=sum(N_LCL_Total, na.rm=T), 
  Nsm_RGN_BIG7=sum(Nsm_LCL_BIG7, na.rm=T), Nsm_RGN_BIG9=sum(Nsm_LCL_BIG9, na.rm=T), Nsm_RGN_Total=sum(Nsm_LCL_Total, na.rm=T)), by=.(snapshot, RGN_CD)]

national <- regional[,-c("RGN_CD")][,.(FT_NTL_BIG7=sum(FT_RGN_BIG7, na.rm=T), FT_NTL_BIG9=sum(FT_RGN_BIG9, na.rm=T), FT_NTL_Total=sum(FT_RGN_Total, na.rm=T), 
  PT_NTL_BIG7=sum(PT_RGN_BIG7, na.rm=T), PT_NTL_BIG9=sum(PT_RGN_BIG9, na.rm=T), PT_NTL_Total=sum(PT_RGN_Total, na.rm=T), 
  N_NTL_BIG7=sum(N_RGN_BIG7, na.rm=T), N_NTL_BIG9=sum(N_RGN_BIG9, na.rm=T), N_NTL_Total=sum(N_RGN_Total, na.rm=T), 
  Nsm_NTL_BIG7=sum(Nsm_RGN_BIG7, na.rm=T), Nsm_NTL_BIG9=sum(Nsm_RGN_BIG9, na.rm=T), Nsm_NTL_Total=sum(Nsm_RGN_Total, na.rm=T)), by=.(snapshot)]

local.social <- merge(local.social, regional, all.x=T, sort=F, by=c("RGN_CD", "snapshot"))
local.social <- merge(local.social, national, all.x=T, sort=F, by=c("snapshot"))
rm(regional, national)
#----------------------------------------------------------------------------












#############################################################################
##### FIGURE X: EMPLOYMENT DENSITIES - SPATIAL GRADIENT DECAY
###

#----------------------------------------------------------------------------
split(BSD.DensSP[[1]], BSD.DensSP[[1]]$LAD_CD)

t <- rbind(as.data.frame(cbind(Model="Linear", 
    x=seq(0.01, 200, by = 0.01), 
    y=exp(as.numeric(summary(M1)$coefficients[1,1]))*exp(as.numeric(summary(M1)$coefficients[2,1])*seq(0.01, 200, by = 0.01)), 
    Rsq=summary(M1)$adj.r.squared)),
  as.data.frame(cbind(Model="Squared", 
    x=seq(0.01, 200, by = 0.01), 
    y=exp(as.numeric(summary(M2)$coefficients[1,1]))*exp(as.numeric(summary(M2)$coefficients[2,1])*seq(0.01, 200, by = 0.01) + as.numeric(summary(M2)$coefficients[3,1])*(seq(0.01, 200, by = 0.01)^2)),
    Rsq=summary(M2)$adj.r.squared)),
  as.data.frame(cbind(Model="Cubed", 
    x=seq(0.01, 200, by = 0.01), 
    y=exp(as.numeric(summary(M3)$coefficients[1,1]))*exp(as.numeric(summary(M3)$coefficients[2,1])*seq(0.01, 200, by = 0.01) + as.numeric(summary(M3)$coefficients[3,1])*(seq(0.01, 200, by = 0.01)^2) + as.numeric(summary(M3)$coefficients[4,1])*(seq(0.01, 200, by = 0.01)^3)), 
    Rsq=summary(M3)$adj.r.squared)),
  as.data.frame(cbind(Model="Log", 
    x=seq(0.01, 200, by = 0.01), 
    y=exp(as.numeric(summary(M4)$coefficients[1,1]) + as.numeric(summary(M4)$coefficients[2,1])*log(seq(0.01, 200, by = 0.01))), 
    Rsq=summary(M4)$adj.r.squared)))
t$x <- as.numeric(as.character(t$x))
t$y <- as.numeric(as.character(t$y))
t$Rsq <- as.numeric(as.character(t$Rsq))
t$Model <- as.factor(t$Model)
rm(M1, M2, M3, M4)
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
ggplot(data=t, aes(x=x)) +
  geom_point(data=data.table(WZ.db)[,c("density", "dist.CBD")], mapping=aes(x=dist.CBD, y=density), alpha=0.2) +
  geom_line(aes(y = y, group = Model, color = Model)) +
  scale_color_manual(values = c("#ebdc87", "#ffa36c", "#d54062", "#799351")) +
  scale_y_continuous(limits = c(0, 2000), expand = c(0,0)) +
  scale_x_continuous(limits = c(0, 50), expand = c(0,0)) +
  labs(y = "Density per ha.", x = "Distance to City Centre (km)", fill = "Functional",
    title = "Estimated Employment Density Gradients", subtitle = paste0("London, England: (N = ", as.character(dim(WZ.db)[1]), ")"),
    caption = paste0("R-Squared: Linear=", unique(signif(t[t$Model=="Linear",]$Rsq, 4)),
    " ; Squared=", unique(signif(t[t$Model=="Squared",]$Rsq, 4)),
    " ; Cubed=", unique(signif(t[t$Model=="Cubed",]$Rsq, 4)),
    " ; Log=", unique(signif(t[t$Model=="Log",]$Rsq, 4)))) +
  theme_classic() +
  theme(legend.position="bottom", plot.caption = element_text(face = "italic"), 
    legend.title = element_blank(), panel.background=element_rect(fill="#E5E5E3"))
#----------------------------------------------------------------------------






#############################################################################
##### GENERATE SOCIAL ECONOMY LOCAL PROFILE
###

# COMPILING LOCAL AUTHORITY DISTRICTS
#----------------------------------------------------------------------------
for(S in local.profiles){
  locality <- unique(geographic.lookup[geographic.lookup$LAD_CD==S, c("LAD_CD", "LAD_NM", "RGN_CD", "RGN_NM", "CIS_CD", "CIS_NM")])
  
  rmarkdown::render(paste0(wd, "/Code/Social Economy/Social_Economy_Template.Rmd"),
    output_file=paste0(S, "_SocialEconomy.html"), 
    output_dir=paste0(wd, "/Outputs/", gsub(" ", "_", locality$RGN_NM), "/LAD/", S, "/"))
}
#----------------------------------------------------------------------------


# COMPILING LOCAL ENTERPRISE PARTNERSHIPS
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------


# COMPILING COMBINED AUTHORITIES
#----------------------------------------------------------------------------


#----------------------------------------------------------------------------