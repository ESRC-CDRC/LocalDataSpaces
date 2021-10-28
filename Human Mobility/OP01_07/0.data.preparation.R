library(data.table)
library(lubridate)
library(sf)
library(parallel)
library(ggplot2)
library(ggplotgui)

options(scipen = 999)
options(digits=5)
gc()
#- Clear workspace
rm(list=ls())
####################
# Selecting the LA #
# ##################
la_name <- 'York'
la_dir_name <- tolower(gsub(' ','',la_name))
gc()
# Directory setting
cDIR <- 
   "/media/maurizio/BETA_Seagate/work/UCL/Local Data Spaces/LDS_data_package/LA/"
#- Settings paths
cdataF               <- paste0(cDIR, "/commondata/")    # R scripts location
ladataf              <- paste0(cDIR, la_dir_name,'/')
# Setting the number of cores
ncoreZ <- detectCores()-1
setwd(cdataF)
# Loading the google mobility reports data
gmr_la_by_day <- 
   fread('google_gmr_uk_lad_by_day_2020-02-15-2021-02-13.csv')[lad19nm=='Harborough',]
setwd(ladataf)
fwrite(gmr_la_by_day,'gmr_la_by_day,csv')
# Loading and selecting the CDRC UWB footfall data
setwd(cdataF)
uwb_ff_la_day  <- 
   fread('20210206.wifi.uwb.day.final.csv')[town==la_name,]
uwb_ff_la_5min  <- 
   fread('20210206.wifi.uwb.5min.final.csv')[town==la_name,]
#uwb_meta1 <- fread('LDC_UWB_DATA_2021-02-06.csv')
#uwb_meta2 <- fread('20210206_LDC_UWB_WiFi_Devices_NN_Retail_Class.csv')
fwrite(uwb_ff_la_day,'uwb_ff_la_day.csv')
fwrite(uwb_ff_la_5min,'uwb_ff_la_5min.csv')
# Wifi data active and almost active
wifi_ff_la_day <- 
   fread('20210206.wifi.active.not.same.uwb.location.day.csv')[town==la_name,]
wifi_ff_almost_active_day <- 
   fread('20210206.wifi.almost.active.2015-07-22-2021-02-09.day.csv')[town==la_name,]
setwd(ladataf)
ifelse(nrow(wifi_ff_la_day)>0,
       fwrite(wifi_ff_la_day,'wifi_ff_la_active_day.csv'),
       wifi_ff_la_day <- NULL)
ifelse(nrow(wifi_ff_almost_active_day)>0,
fwrite(wifi_ff_almost_active_day,'wifi_ff_la_almost_active_day.csv'),
wifi_ff_almost_active_day <- NULL)
# Loading proprietary data from local authority
setwd(ladataf)
la_data_by_hr <- 
   fread('york_hourly_footfall_2009-03-30-2021-02-07.csv')
la_data_by_day <- 
   fread('york_daily_footfall_2009-03-30-2021-02-07.csv')
la_ff_locations <- 
   fread('york_ff_locations_clean.csv')
setwd(cdataF)

# Loading the events
covid.dates <- 
   fread('COVID-19_important_dates.csv')
# Loading the UK Bank Holidays
uk.bank.holidays <- fread('UK Bank Holidays.csv')[`Bank Holiday`==1 & year(Date)>=2020,]
# Loading the Tier evolution
la.tier.evolution <- 
   fread('GEOLYTIX - OpenCovid19Lockdown - UK.zip.csv')[local_area==la_name]
la.tier.evolution <- 
   la.tier.evolution[,.(local_area,t_20201209,t_20201218,
                        t_20201230,t_20210105)]

#############################
# PREPARING DATA FOR GRAPHS #
############################# 
# GMR DATA PREPARATION
measure.vars.melt <- names(gmr_la_by_day)[11:16]
gmr_la_by_day_charts <- melt(gmr_la_by_day,
                             measure.vars = measure.vars.melt,
                             variable.name = 'ff_type',
                             value.name = 'ff_value')
gmr_la_by_day_charts[,ff_type:=gsub('_',' ',
                                    gsub('_percent_change_from_baseline',
                                    '',ff_type))]

# LA PROPRIETARY DATA PREPARATION
la_data_by_day[,WeekDay:=factor(WeekDay,
                                levels = c("Monday", "Tuesday", "Wednesday",
                                           "Thursday", "Friday", "Saturday", "Sunday"))]
la_data_by_hr[,WeekDay:=factor(WeekDay,
                               levels = c("Monday", "Tuesday", "Wednesday",
                                          "Thursday", "Friday", "Saturday", "Sunday"))]

# Analysis by day of the week 2020
la_data_by_day_2020 <- 
   la_data_by_day[BRCYear %in% c('2020','2021'),]
la_day_of_the_week <- 
   la_data_by_day_2020[,mean(TotalCount,na.rm=T),
                      by=.(WeekDay,LocationName)]
setnames(la_day_of_the_week,old='V1',new='ff_value')
# Analysis by time of day 2020
la_data_by_hr_2020 <- 
   la_data_by_hr[BRCYear %in% c('2020','2021'),]
la_data_by_hr_2020[,hour:=substr(Date,12,19)]
la_hr_by_day <- 
   la_data_by_hr_2020[,mean(TotalCount,na.rm=T),
                       by=.(WeekDay,hour)]
la_hr_by_day[,hr:=as.numeric(substr(hour,1,2))]
setnames(la_hr_by_day,old='V1',new='ff_value')

locations_sf <- st_as_sf(la_ff_locations,coords=c('X','Y'),
                       crs=4326)

esquisse::esquisser()

#####################
# GRAPH PREPARATION #
#####################
library(ggplot2)
# Google Mobility Report data
ggplot(gmr_la_by_day_charts) +
   aes(x = date, y = ff_value, colour = ff_type) +
   geom_line(size = 1L) +
   scale_color_hue() +
   theme_minimal() +
   facet_wrap(vars(ff_type), nrow = 6L) +
   scale_x_date(breaks=covid.dates$event_date) +
   geom_vline(xintercept = covid.dates$event_date,
              col = "black", lwd = 0.25, alpha = 0.75) +
   theme(legend.position='none',
         legend.title = element_blank(),
         axis.text.x = element_text(angle=90)) +
   xlab('')+
   ylab('')+
   annotate(geom = "text",
            x = covid.dates$event_date ,
            #y = 1,
            label = covid.dates$note, color = "red",
            angle = 90)
##############################
# LA PROPRIETARY DATA GRAPHS #
##############################
# LA PROPRIETARY DAY OF THE WEEK BY LOCATION
ggplot(la_day_of_the_week) +
 aes(x = WeekDay, fill = LocationName, weight = ff_value) +
 geom_bar() +
 scale_fill_hue() +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_grid(vars(LocationName), vars()) +
   theme(legend.position='none',
         legend.title = element_blank(),
         axis.text.x = element_text(angle=90)) +
   xlab('')+
   ylab('')
# LA PROPRIETARY TIME OF THE DAY TOTAL
ggplot(la_hr_by_day) +
   aes(x = hr, y = ff_value) +
   geom_line(size = 1L, colour = "#0c4c8a") +
   theme_minimal() +
   facet_wrap(vars(WeekDay)) +
   scale_x_continuous(breaks=la_hr_by_day$hr) +
   xlab('')+
   ylab('')
