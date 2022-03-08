library(data.table)
library(lubridate)
library(sf)
library(parallel)
library(curl)
library(stringr)

options(scipen = 999)
options(digits=5)
gc()
#- Clear workspace
rm(list=ls())
####################
# Selecting the LA #
# ##################
#la_name <- 'Market Harborough'
la_off_name <- 'gmr'
la_dir_name <- 'gmr'
gc()
# Directory setting
cDIR <- 
   "/media/maurizio/BETA_Seagate/work/UCL/Local Data Spaces/LDS_data_package/LA/gmr"
#- Settings paths
cdataF               <- paste0(cDIR, "/commondata/")    # R scripts location
ladataf              <- paste0(cDIR, la_dir_name,'/')
# Setting the number of cores
ncoreZ <- detectCores()-1
setwd(cdataF)
# Loading the events and UK Bank Holidays for 2020
covid.dates <- 
   fread('COVID-19_important_dates.csv')
# Loading the UK Bank Holidays
uk.bank.holidays <- 
   fread('UK Bank Holidays.csv')[`Bank Holiday`==1 & year(Date)>=2020,]
names(uk.bank.holidays) <- tolower(names(uk.bank.holidays))
# Loading the Tier evolution
la.tier.evolution <- 
   fread('GEOLYTIX - OpenCovid19Lockdown - UK.zip.csv')
la.tier.evolution <- 
   la.tier.evolution[,.(local_area,t_20201209,t_20201218,
                        t_20201230,t_20210105)]
la.tier.evolution.charts <- 
   melt(la.tier.evolution,
        measure.vars = names(la.tier.evolution)[2:5],
        variable.name = 'Date',
        value.name = 'Tier')
la.tier.evolution.charts[,Date:=gsub('t_','',Date)]
la.tier.evolution.charts[,Date:=ymd(Date)]
la.tier.evolution.charts[Date==as.IDate('2021-01-05'),
                         Tier:='L']
la.tier.evolution.charts[,order:=seq(1:nrow(la.tier.evolution.charts))]
la.tier.evolution.charts[,y:=1]
tier.alpha <- seq(from=0.2,to=1,by=0.2)
tier.lbl   <- seq(from=1,to=5,by=1) 
tier.lbl[tier.lbl=='5'] <- 'L'
tier.color <- as.data.table(cbind(tier.lbl,tier.alpha))
la.tier.evolution.charts <- merge(la.tier.evolution.charts,
                                  tier.color,by.x='Tier',
                                  by.y='tier.lbl')
tier.color <- NULL;tier.alpha <- NULL;tier.lbl <- NULL;gc()
la.tier.evolution.charts[,tier.alpha:=as.numeric(tier.alpha)]
#fwrite(la.tier.evolution.charts,'la.tier.evolution.charts.csv')
# Loading the google mobility reports data
# GMR in wide form
gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
setwd(cdataF)
#download.file(gmr,paste0('google_mobility_report_raw.csv'))
gmr_uk <- fread('google_mobility_report_raw.csv')[country_region=='United Kingdom',]
#gmr_uk <- gmr[country_region=='United Kingdom',]
setorder(gmr_uk,date)
#Not all places have complete coverage of the data
uniquedaysplaceid <- unique(gmr_uk[,.(numberodays=length(.SD$date)),by=place_id])
#gmr_uk_la_list <- sort(unique(gmr_uk$sub_region_2))
min.date.csv.save <- min(gmr_uk$date)
max.date.csv.save <- max(gmr_uk$date)
fwrite(gmr_uk,paste0('google_gmr_uk_by_day_',
                     min.date.csv.save,
                     '-',
                     max.date.csv.save,
                     '.csv'))
###############################################
# Merge Google mobility reports with LAD LUT #
###############################################
# Reference:
download.file("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/geography/google_mobility_lad_lookup_200903.csv", 
              destfile = "google_mobility_lad_lookup_200903.csv", method = "curl")
gmr_uk_lad_lut <- 
   fread('google_mobility_lad_lookup_200903.csv')
gmr_lad_lut_profiles <- fread('google-mobility-profiles-uk-all-trends-local-regional.csv')

lad_region <- fread('Ward_to_Local_Authority_District_to_County_to_Region_to_Country_(December_2019)_Lookup_in_United_Kingdom.csv')
names(lad_region) <- tolower(names(lad_region))
lad_region$fid <- NULL
lad_region$wd19cd <- NULL
lad_region$wd19nm <- NULL
lad_region$ctry19cd <- NULL
lad_region$ctry19nm <- NULL
lad_region <- unique(lad_region)

# gmr_uk_lad_lut <- 
# gmr_uk_lad_lut[!is.na(lad19cd),]
download.file('https://github.com/datasciencecampus/google-mobility-reports-data/blob/master/geography/google_mobility_lad_boundary_200903.gpkg?raw=true',
              "google_mobility_lad_boundary_200903.gpkg")
gmr_uk_sf <- 
   st_read('google_mobility_lad_boundary_200903.gpkg')

gmr_uk_lad_lut_geo <- merge(gmr_uk_lad_lut,
                            gmr_uk_sf[,c("lad19cd","lad19nm","geom")],
                            by='lad19cd')

gmr_uk[,id_join:=tolower(
   gsub(' ','',
        paste0(
               ifelse(is.na(sub_region_1),'',sub_region_1),
               ifelse(is.na(sub_region_2),'',sub_region_2))))]
gmr_uk_codes <- 
   unique(gmr_uk[,.(sub_region_1,sub_region_2,iso_3166_2_code)])
gmr_uk_lad_lut_geo$id_join <- tolower(
   gsub(' ','',
        paste0(
               ifelse(is.na(gmr_uk_lad_lut_geo$sub_region_1),'',
                      gmr_uk_lad_lut_geo$sub_region_1),
               ifelse(is.na(gmr_uk_lad_lut_geo$sub_region_2),'',
                      gmr_uk_lad_lut_geo$sub_region_2))))
length(unique(gmr_uk_lad_lut_geo$id_join))
unique(gmr_uk_lad_lut_geo$id_join)
length(unique(gmr_uk$id_join))

gmr_uk[!id_join%in%unique(gmr_uk_lad_lut_geo$id_join) &id_join!='',]
gmr_uk_lad_lut_geo[!id_join%in%unique(gmr_uk$id_join),]

correctionsthelens <- unique(gmr_uk[sub_region_1=='Merseyside'&sub_region_2=="Metropolitan Borough of St Helens",id_join])
gmr_uk_lad_lut_geo[!id_join%in%unique(gmr_uk$id_join),
                   id_join:=correctionsthelens]
# Totals for UK
gmr_uk_totals <- gmr_uk[id_join=='',]
# Creating the spatial object
gmr_uk_inner_sf <- 
   merge(gmr_uk,
         gmr_uk_lad_lut_geo[,c('id_join','lad19nm','lad19cd','geom')],
         by='id_join')
# Some days are missing
length(unique(gmr_uk_inner_sf$id_join))
length(unique(gmr_uk_inner_sf$lad19cd))
# The coding convention used by Google is a mix of different geographies
# https://en.wikipedia.org/wiki/ISO_3166-2:GB

gmr_uk_inner_sf <- merge(gmr_uk_inner_sf,lad_region[,!c('lad19nm'),with=F],
                         by='lad19cd')

gmr_uk_inner_sf$sub_region_1 <- gmr_uk_inner_sf$rgn19nm
gmr_uk_inner_sf$sub_region_2 <- gmr_uk_inner_sf$cty19nm
#gmr_uk_inner_sf$rgn19nm <- NULL;

gmr_uk_inner <- gmr_uk_inner_sf
gmr_uk_inner$geom <- NULL;

# Saving the objects
fwrite(gmr_uk_inner,paste0('google_gmr_uk_lad_by_day_',
                           min.date.csv.save,
                           '-',
                           max.date.csv.save,
                           '.csv'))
st_write(gmr_uk_inner_sf,paste0('google_gmr_uk_lad_by_day_',
                                min.date.csv.save,
                                '-',
                                max.date.csv.save,
                                '.shp'),append=F)
gmr_uk_inner        <- NULL
gmr_uk_inner_sf     <- NULL
gmr_uk              <- NULL
gmr_uk_sf           <- NULL
gmr_uk_lad_lut      <- NULL
gmr_uk_lad_lut_geo  <- NULL
gc()
# LOAD THE DATA COME HERE IF NOT THE FIRST TIME ----
gmr_lad_by_day <- 
   fread(paste0('google_gmr_uk_lad_by_day_',
                min.date.csv.save,
                '-',
                max.date.csv.save,
                '.csv'))
# GMR spatial object in wide form
gmr_lad_by_day_sf <- 
   st_read(paste0('google_gmr_uk_lad_by_day_',
                  min.date.csv.save,
                  '-',
                  max.date.csv.save,
                  '.shp'))
# Creating the lookup table between gmr geography and LAD geography
lad.list <- cbind(unique(gmr_lad_by_day_sf$lad19cd),
                  unique(gmr_lad_by_day_sf$lad19nm))
# Creating the lookup table between gmr geography and LAD geography
gmr.lad.list <- data.table('lad19cd'=unique(gmr_lad_by_day_sf$lad19cd),
                              'lad19nm'=unique(gmr_lad_by_day_sf$lad19nm))
# Let's have a look at the spatial object 
# and do some house cleaning
gmr_lad_by_day_sf$metro_r <- NULL; 
gmr_lad_by_day_sf$sb_rg_1 <- NULL;
gmr_lad_by_day_sf$sb_rg_2 <- NULL;
gmr_lad_by_day_sf$cntry__ <- NULL;
gmr_lad_by_day_sf$cnss_f_ <- NULL;
names(gmr_lad_by_day_sf)[3] <- 'cntry_lbl'
st_write(gmr_lad_by_day_sf,
         'google_gmr_uk_lad_by_day_2020-02-15-2021-03-13_cleaned.shp',
         append= F)
# Changing the shape of the wide dataset to long
measure.vars.melt <- names(gmr_lad_by_day)[12:17]
gmr_lad_by_day_charts <- melt(gmr_lad_by_day,
                             measure.vars = measure.vars.melt,
                             variable.name = 'ff_type',
                             value.name = 'ff_value')
# Fixing the category names
gmr_lad_by_day_charts[,ff_type:=gsub('_',' ',
                                    gsub('_percent_change_from_baseline',
                                         '',ff_type))]
gmr_lad_by_day_charts$fl_2018 <- NULL;gc() 
gmr_lad_by_day_charts$cntry__ <- NULL;gc()
# Fixing the fields names
gmr_lad_by_day_charts[,ff_type:=str_to_title(ff_type)]
gmr_lad_by_day_charts[,ff_type:=gsub('And','and',ff_type)]

# Adding the UK Bank Holidays
gmr_lad_by_day_charts <-
   merge(gmr_lad_by_day_charts,
         uk.bank.holidays[, .(date, 'ukhol' = name)],
         by = 'date',
         all.x = T)
# Adding the day of the week and factor
gmr_lad_by_day_charts[,dayoftheweek:=weekdays(date)]
gmr_lad_by_day_charts[,dayoftheweek:=factor(dayoftheweek,
                                            levels = c("Monday", "Tuesday", "Wednesday",
                                                       "Thursday", "Friday", "Saturday", "Sunday"))]
gmr_lad_by_day_charts[,ff_type:=factor(ff_type,levels = c("Retail and Recreation",
                                                       "Grocery and Pharmacy",
                                                       "Parks",
                                                       "Transit Stations",
                                                       "Residential",
                                                       "Workplaces"))]
# Calculating the national summary statistics by category
# and date. I will use this to compare the local trends with 
# the national ones
gmr_uk_by_day_charts  <- 
   gmr_lad_by_day_charts[,.(
      ff_min     =ifelse(length(unique(ff_value))==1,
                           ff_value,
                           min(ff_value,na.rm=T)),
      ff_mean    =round(mean(ff_value,na.rm=T),0),
      ff_median  =round(median(ff_value,na.rm=T),0),
      ff_max     =ifelse(length(unique(ff_value))==1,
                           ff_value,
                           max(ff_value,na.rm=T)),
      ff_mad     =round(mad(ff_value,na.rm = T),0),
      ff_var     =round(var(ff_value,na.rm=T),0)),
      by=.(country_region_code,
           date,
           dayoftheweek,
           ukhol,
           ff_type)]
# Saving object
fwrite(gmr_uk_by_day_charts,'gmr_uk_by_day_charts.csv')

# Same for the regional aggregations.
gmr_sub_region_1_by_day_charts <- 
   gmr_lad_by_day_charts[,.(
      ff_min     =ifelse(length(unique(ff_value))==1,
                         ff_value,
                         min(ff_value,na.rm=T)),
      ff_mean    =round(mean(ff_value,na.rm=T),0),
      ff_median  =round(median(ff_value,na.rm=T),0),
      ff_max     =ifelse(length(unique(ff_value))==1,
                         ff_value,
                         max(ff_value,na.rm=T)),
      ff_mad     =round(mad(ff_value,na.rm = T),0),
      ff_var     =round(var(ff_value,na.rm=T),0)),
      by=.(sub_region_1,
           date,
           dayoftheweek,
           ukhol,
           ff_type)]
fwrite(gmr_sub_region_1_by_day_charts,
        'gmr_sub_region_1_by_day_charts.csv')

# Creating the holidays dataset
gmr_lad_holidays_charts <- 
   gmr_lad_by_day_charts[ukhol!='',]

gmr_sub_region_1_holidays_charts <- 
   gmr_sub_region_1_by_day_charts[ukhol!='',]

gmr_uk_holidays_charts <- 
   gmr_uk_by_day_charts[ukhol!='',]

gmr_holidays_charts_subrgn1 <- 
   merge(gmr_lad_holidays_charts,
         gmr_sub_region_1_holidays_charts[,.(sub_region_1,
                                           ukhol,ff_type,
                                           'ff_median_subrgn1'=
                                              ff_median)],
         by=c('sub_region_1','ukhol','ff_type'))
gmr_holidays_charts <- 
   merge(gmr_holidays_charts_subrgn1,
         gmr_uk_holidays_charts[,.(ukhol,ff_type,
                                 'ff_median_uk'=
                                    ff_median)],
         by=c('ukhol','ff_type'))
gmr_holidays_charts <- 
   gmr_holidays_charts[,.(lad19cd,lad19nm,sub_region_1,date,
                        ukhol,ff_type,ff_value,
                        ff_median_subrgn1,ff_median_uk)]

gmr_holidays_charts_subrgn1 <- NULL;
gmr_uk_holidays_charts      <- NULL;
gmr_holidays_charts_subrgn1 <- NULL;
gmr_start_date <- min(gmr_lad_by_day$date)
gmr_holidays_charts[,
                    ukhol:=factor(ukhol,
                                  levels = setorder(uk.bank.holidays[date>=gmr_start_date,],date)$name)]
unique(gmr_holidays_charts$ukhol)
fwrite(gmr_holidays_charts,'gmr_holidays_charts.csv')
# We will now calculate summary statistics for all categories
# in the covid periods contained in the text file we will load.
# gmr_lad_by_day <- NULL;gc()
# I will make a graph for the time series in the LA.
# Overlay the regional and national behaviour.
# Loading the date object
# I need to calculate the min and max date of the gmr reports and create 
# and additional series of events to catch all the data in the time series

# Preparing the covid dates object ----
gmr_end_date <- max(gmr_lad_by_day$date)
new_events <- data.table(
   phase=c('Phase zero',
           'Phase six','Phase six',
           'Phase six','Phase six','Phase seven'),
   event=c('Before First National Lockdown',
           'Schools and care homes reopen','Rule of six outdoor',
           'Shops reopen','Rule of six indoor','Back to normal'),
   start_date=c(as.IDate('2020-01-01'),as.IDate('2021-03-08'),
                as.IDate('2021-03-29'),as.IDate('2021-04-12'),
                as.IDate('2021-05-17'),as.IDate('2021-06-21')),
   end_date=c(as.IDate('2020-03-23'),as.IDate('2021-03-29'),
              as.IDate('2021-04-12'),as.IDate('2021-05-17'),
              as.IDate('2021-06-21'),as.IDate('2022-01-01'))
)
covid.better.dates <- 
   fread('COVID-19_important_dates_gantrify_project.csv')
covid.better.dates[event=='Third National Lockdown',
                   end_date:=as.IDate('2021-03-08')]
# Removing the event 75% of the country in Tier 4 ----
covid.better.dates <- covid.better.dates[!12]
covid.better.dates[event=='Tier 4 introduced',
                   end_date:=as.IDate('2021-01-06')]
covid.better.dates <- 
   rbind(covid.better.dates,new_events)
new_events <- NULL;gc()
setorder(covid.better.dates,start_date)
covid.better.datesL <- as.list(covid.better.dates)
#i <- 10
gmr_lad_events_charts <- as.data.table(rbindlist(
   lapply(1:length(covid.better.datesL$event),function(i){
   phaseL <- covid.better.datesL$phase[i]
   eventL <- covid.better.datesL$event[i]
   startdateL <- covid.better.datesL$start_date[i]
   enddateL   <- covid.better.datesL$end_date[i]
   gmr_lad_events_charts <- 
   gmr_lad_by_day_charts[date>=as.IDate(startdateL)&
                            date<as.IDate(enddateL),
                         .(phase      =as.character(phaseL),
                           event      =as.character(eventL),
                           start_date =startdateL,
                           end_date   =enddateL,
                           ff_min     =as.integer(min(ff_value,na.rm=T)),
                           ff_mean    =round(mean(ff_value,na.rm=T),0),
                           ff_median  =round(median(ff_value,na.rm=T),0),
                           ff_max     =as.integer(max(ff_value,na.rm=T)),
                           ff_mad     =round(mad(ff_value,na.rm = T),0),
                           ff_var     =round(var(ff_value,na.rm=T),0)),
                         by=.(lad19cd,lad19nm,sub_region_1,
                              sub_region_2,ff_type)]
   gmr_lad_by_day_charts[date>=as.IDate(startdateL)&date<as.IDate(enddateL),event:=eventL,]
   gmr_lad_by_day_charts[date>=as.IDate(startdateL)&date<as.IDate(enddateL),phase:=phaseL,]
   return(gmr_lad_events_charts)
      })
))
fwrite(gmr_lad_events_charts,'gmr_lad_events_charts.csv')
fwrite(gmr_lad_by_day_charts,'gmr_lad_by_day_charts.csv')
# Summary statistics sub region layer ----
gmr_sub_region_1_events_charts <- 
   gmr_lad_by_day_charts[,.(
                         start_date =first(date),
                         end_date   =last(date),
                         ff_min     =ifelse(length(unique(ff_value))==1,
                                            ff_value,
                                            min(ff_value,na.rm=T)),
                         ff_mean    =round(mean(ff_value,na.rm=T),0),
                         ff_median  =round(median(ff_value,na.rm=T),0),
                         ff_max     =ifelse(length(unique(ff_value))==1,
                                            ff_value,
                                            max(ff_value,na.rm=T)),
                         ff_mad     =round(mad(ff_value,na.rm = T),0),
                         ff_var     =round(var(ff_value,na.rm=T),0)
                         ),
                         by=.(sub_region_1,
                              phase,
                              event,
                              ff_type)]

fwrite(gmr_sub_region_1_events_charts,'gmr_sub_region_1_events_charts.csv')

# Summary statistics for the UK layer ----
gmr_uk_events_charts <- 
   gmr_lad_by_day_charts[,.(
      start_date =first(date),
      end_date   =last(date),
      ff_min     =ifelse(length(unique(ff_value))==1,
                         ff_value,
                         min(ff_value,na.rm=T)),
      ff_mean    =round(mean(ff_value,na.rm=T),0),
      ff_median  =round(median(ff_value,na.rm=T),0),
      ff_max     =ifelse(length(unique(ff_value))==1,
                         ff_value,
                         max(ff_value,na.rm=T)),
      ff_mad     =round(mad(ff_value,na.rm = T),0),
      ff_var     =round(var(ff_value,na.rm=T),0)
   ),
   by=.(country_region_code,
        phase,
        event,
        ff_type)]
fwrite(gmr_uk_events_charts,'gmr_uk_events_charts.csv')

# At the moment I created the following data.tables:
# 1 Sub_region_2 dataset
# 2 Sub_region_1 dataset
# 3 UK dataset
# 4 Sub_region_2 dataset with summary statistics for the covid periods
# 5 Sub_region_1 dataset with summary statistics for the covid periods
# 6 UK dataset with summary statistics for the period.
# 7 A tier evolution dataset in both long and wide form
# 8 LAD sf object containing the lad codes and labels
# 9 COVID dates dataset
# 10 UK bank holidays dataset
# I will now create the day of the week analysis for the same datasets
# I will use radial graphs for that.
# Put together the events data
gmr_events_charts.subrgn1 <- 
   merge(gmr_lad_events_charts,
         gmr_sub_region_1_events_charts[,.(sub_region_1,
                                           event,ff_type,
                                           'ff_median_subrgn1'=
                                              ff_median)],
         by=c('sub_region_1','event','ff_type'))
gmr_events_charts <- 
   merge(gmr_events_charts.subrgn1,
         gmr_uk_events_charts[,.(event,ff_type,
                                           'ff_median_uk'=
                                              ff_median)],
         by=c('event','ff_type'))
gmr_events_charts <- 
   gmr_events_charts[,.(lad19cd,lad19nm,sub_region_1,
                        event,ff_type,ff_median,
                        ff_median_subrgn1,ff_median_uk)]

gmr_events_charts.subrgn1 <- NULL
unique(gmr_events_charts$event)
eventlevels <- unique(covid.better.dates$event)

covid.better.dates[,eventf:=factor(event,levels=c(eventlevels))]
gmr_events_charts[,event:=factor(event,
                                 levels =
                                 covid.better.dates[start_date<=gmr_end_date,eventf])]
unique(gmr_events_charts$ff_type)
unique(gmr_events_charts$event)

lad2019 <- 
   st_read('Local_Authority_Districts_(December_2019)_Boundaries_UK_BGC.shp')
lad.list <- unique(lad2019$lad19nm)
lad.codes <- unique(lad2019$lad19cd)
lad.code.list <- as.data.table(cbind(lad.list,lad.codes))

gmr.lad.lut <- 
   merge(gmr.lad.list,lad_region[,!c('lad19nm'),with=F],by='lad19cd')


# LADs in LDS
gmr.lad.lut.lds <- gmr.lad.lut[rgn19nm %in%c('East of England','South East',
                          'London')|lad19nm %in%c('Harborough','York',
                                                  'Hackney','Westminster')]
# Creating the LAD folders----
mclapply(1:length(gmr.lad.lut.lds$lad19cd),function(l){
   #l <- 75
   setwd(cDIR)
   lad.name <- gsub(' ','_',gmr.lad.lut.lds[l,]$lad19nm)
   lad.code <- gsub(' ','_',gmr.lad.lut.lds[l,]$lad19cd)
   cnt.name <- gmr.lad.lut.lds[l,]$cty19nm
   cnt.code <- gmr.lad.lut.lds[l,]$cty19cd
   rgn.name <- gsub(' ','_',gmr.lad.lut.lds[l,]$rgn19nm)
   rgn.code <- gsub(' ','_',gmr.lad.lut.lds[l,]$rgn19cd)
   folderpath <- 
      ifelse(cnt.code!='',paste0(rgn.code,'_',rgn.name,'/',
                                 cnt.code,'_',cnt.name,'/',
                                 lad.code,'_',lad.name),
             paste0(rgn.code,'_',rgn.name,'/',lad.code,'_',lad.name))
   if(!dir.exists(folderpath)) {dir.create(file.path(folderpath),
                           recursive = T)}
   setwd(folderpath)
   dir.create(path='./data/')
   dir.create(path='./img/')
   setwd('./data/')
   fwrite(covid.better.dates,'covid.better.dates.csv')
   fwrite(la.tier.evolution.charts,'la.tier.evolution.charts.csv')
   file.copy(paste0(cdataF,'covid19importantdates.png'),
                    '../img/',overwrite = T)
   file.copy(paste0(cdataF,'reportlogos.png'),
             '../img/',overwrite = T)
   
   file.copy(paste0(cDIR,'/commonfiles/','html.report.1.Rmd'),
             '../',overwrite = T)
   file.rename('../html.report.1.Rmd',paste0('../',lad.code,' - ',lad.name,'-mobility-report.Rmd'))
   file.copy(paste0(cDIR,'/commonfiles/','/1.charts.preparation.R'),
             '../',overwrite = T)
   # Loading the UK Bank Holidays
   fwrite(uk.bank.holidays,
          'UK Bank Holidays.csv')
   fwrite(gmr_holidays_charts[lad19cd==lad.code],
          'gmr_holidays_charts.csv')
   fwrite(gmr_events_charts[lad19cd==lad.code],
          'gmr_events_charts.csv')
   fwrite(gmr_lad_by_day[lad19cd==lad.code],
          'gmr_lad_by_day.csv')
   fwrite(gmr_lad_by_day_charts[lad19cd==lad.code],
          'gmr_lad_by_day_charts.csv')
   sub_region_1_name <- unique(gmr_lad_by_day_charts[lad19cd==lad.code,sub_region_1])
   fwrite(gmr_sub_region_1_by_day_charts[sub_region_1==sub_region_1_name],
          'gmr_sub_region_1_by_day_charts.csv')
   fwrite(gmr_sub_region_1_events_charts[sub_region_1==sub_region_1_name],
          'gmr_sub_region_1_events_charts.csv')
   fwrite(gmr_uk_by_day_charts,
          'gmr_uk_by_day_charts.csv')
   gmr.lad.lut.lds$sub_region_1 <- sub_region_1_name
   fwrite(gmr.lad.lut.lds[l,],'gmr.lad.lut.csv')
   lad2019.chart <- 
      lad2019[lad2019$lad19nm==lad.name,]
   ladnn <- st_touches(
      lad2019,
      lad2019.chart,dist=0)
   ladnn <- as.data.table(t(ladnn))
   lad2019.chart <- rbind(lad2019[ladnn$V1,],lad2019.chart)              
   lad2019_tier_chart <- 
      merge(lad2019.chart,
            la.tier.evolution.charts,
            by.x='lad19nm',
            by.y='local_area')
   lad2019_tier_chart <- 
      lad2019_tier_chart[,c("lad19nm","lad19cd","Tier","Date","tier.alpha")]
   st_write(lad2019_tier_chart,'lad2019_tier_chart.shp',
            append = F)
   setwd('../')
   system('Rscript 1.charts.preparation.R',wait = T)
   #source('1.charts.preparation.R')
   },mc.cores = ncoreZ,mc.preschedule = T)
