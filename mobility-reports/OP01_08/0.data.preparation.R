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
la_name <- 'Market Harborough'
la_off_name <- 'Harborough'
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
   fread('google_gmr_uk_lad_by_day_2020-02-15-2021-02-13.csv')[lad19nm==la_off_name,]
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
setwd(ladataf)
fwrite(uwb_ff_la_day,'uwb_ff_la_day.csv')
fwrite(uwb_ff_la_5min,'uwb_ff_la_5min.csv')
setwd(cdataF)
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

unique(wifi_ff_la_day$xyid)
unique(wifi_ff_almost_active_day$xyid)
unique(uwb_ff_la_day$xyid)

setwd(cdataF)
la_ff_meta_wifi <- fread('ff_metadata_2021-02-10.csv')
la_ff_meta <- 
   fread('20210206_LDC_UWB_WiFi_Devices_NN_Retail_Class.csv')[town==la_name,]
la_ff_meta.active.almost.active <- 
   la_ff_meta[is.na(todate)|todate>=as.IDate('2019-01-01'),]
sort(unique(la_ff_meta.active.almost.active$xyid))
la_ff_meta_wifi <- 
   la_ff_meta_wifi[xyid %in%unique(la_ff_meta.active.almost.active$xyid),]
sort(unique(la_ff_meta_wifi$xyid))

wifi_ff_almost_active_day <- 
   wifi_ff_almost_active_day[xyid %in%unique(la_ff_meta.active.almost.active$xyid)
]
nrow(unique(wifi_ff_almost_active_day,by=c('date','xyid')))

# Loading the events
covid.dates <- 
   fread('COVID-19_important_dates.csv')
# Loading the UK Bank Holidays
uk.bank.holidays <- fread('UK Bank Holidays.csv')[`Bank Holiday`==1 & year(Date)>=2020,]
# Loading the Tier evolution
la.tier.evolution <- 
   fread('GEOLYTIX - OpenCovid19Lockdown - UK.zip.csv')[local_area==la_off_name]
la.tier.evolution <- 
   la.tier.evolution[,.(local_area,t_20201209,t_20201218,
                        t_20201230,t_20210105)]
#############################
# PREPARING DATA FOR GRAPHS #
############################# 
# Tier Evolution Preparation
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

########################
# TIER EVOLUTION CHART #
########################
ggplot(la.tier.evolution.charts) +
   aes(x = order, y = y, colour = 'red') +
   geom_point(size = 50, alpha=la.tier.evolution.charts$tier.alpha) +
   scale_fill_hue() +
   scale_x_discrete(labels=la.tier.evolution.charts$Date)+
   theme_void()+
   theme(legend.position = "none")+
   geom_text(data=la.tier.evolution.charts,
             aes( x=order, y=1, label= Tier),
             color='black',
             size= 24) +
   geom_text(data=la.tier.evolution.charts,
             aes( x=order, label= paste0('\n\n\n\n\n',Date)),
             color='black',
             size= 6)
setwd(ladataf)
ggsave('./img/la.tier.evolution.png',
       width = 50.08,
       height= 16.09,
       units = 'mm',
       scale = 3.5,
       dpi=300,
       limitsize = T)

##############
# GMR GRAPHS #
##############
measure.vars.melt <- names(gmr_la_by_day)[11:16]
gmr_la_by_day_charts <- melt(gmr_la_by_day,
                             measure.vars = measure.vars.melt,
                             variable.name = 'ff_type',
                             value.name = 'ff_value')
gmr_la_by_day_charts[,ff_type:=gsub('_',' ',
                                    gsub('_percent_change_from_baseline',
                                    '',ff_type))]
# Google Mobility Report data
ggplot(gmr_la_by_day_charts) +
   aes(x = date, y = ff_value, colour = ff_type) +
   geom_line(size = 0.35) +
   scale_color_hue() +
   theme_minimal() +
   facet_wrap(vars(ff_type), nrow = 6L,scales = 'free_y') +
   scale_x_date(breaks=c(min(gmr_la_by_day_charts$date),
                         covid.dates$event_date,
                         max(gmr_la_by_day_charts$date))) +
   geom_vline(xintercept = c(min(gmr_la_by_day_charts$date),
                             covid.dates$event_date,
                             max(gmr_la_by_day_charts$date)),
              col = "black", lwd = 0.25, alpha = 0.75) +
   theme(legend.position='none',
         legend.title = element_blank(),
         axis.text.x = element_text(angle=90,size=8))+
   xlab('')+
   ylab('')
setwd(ladataf)
ggsave('./img/gmr_la_by_day_charts.png',
       width = 150,
       height= 220,
       units = 'mm',
       dpi=300,
       limitsize = T)

########################
# CDRC UWB WiFi GRAPHS #
########################
wifi_ff_almost_active_day <- 
   wifi_ff_almost_active_day[date>=as.IDate('2020-01-01')]
wifi_ff_almost_active_day[,dayoftheweek:=factor(dayoftheweek,
                                            levels = c("Monday", "Tuesday", "Wednesday",
                                                       "Thursday", "Friday", "Saturday", "Sunday"))]
uwb_ff_la_day[,dayoftheweek:=factor(dayoftheweek,
                                    levels = c("Monday", "Tuesday", "Wednesday",
                                               "Thursday", "Friday", "Saturday", "Sunday"))]
unique(wifi_ff_almost_active_day$xyid)
unique(uwb_ff_la_day$xyid)
uwb_ff_la_day[,type:='uwb']
#wifi_ff_la_day[,type:='wifi']
wifi_ff_almost_active_day[,type:='wifi']
wifi_ff_la_day$device <- NULL;gc()
wifi_ff_almost_active_day$device <- NULL;gc()

uwb.wifi.charts <- rbind(uwb_ff_la_day,wifi_ff_almost_active_day)
#  Weekly means for the Wifi devices.
#  I am tryin gto align the two datasets. I know that the measurement
#  is m.w = a + meas + error and then I have the m.u = a + meas +error
#  The only thing I can isolate is probably the fixed part, that should incorpo
#  rate the settings of the machine. To do that I could calculate measurements
#  for a regular time period and then compare them. I am using hte std period as Google
# but for uwb data we do not have it so I will try to select a period
# during restrictions that was close to the regular ones from WiFi.  
  
wifi.day.week.means <- 
   uwb.wifi.charts[type=='wifi'& 
                      date>=as.IDate('2020-01-03')&
                      date<=as.IDate('2020-02-07'),
                   .(median(ff_sum,na.rm=T),
                     mean(ff_sum,na.rm = T)),by=dayoftheweek]
setnames(wifi.day.week.means,old=c('V1','V2'),
         new=c('ff_wifi_median','ff_wifi_mean'))
uwb.day.week.means <- 
   uwb.wifi.charts[type=='uwb'
                   & 
                      date>=as.IDate('2020-05-13')&
                      date<=as.IDate('2020-08-31')
                   ,
                   .(median(ff_sum,na.rm=T),
                     mean(ff_sum,na.rm = T)),by=dayoftheweek]
setnames(uwb.day.week.means,old=c('V1','V2'),
         new=c('ff_uwb_median','ff_uwb_mean'))
uwb.wifi.means <- cbind(uwb.day.week.means,wifi.day.week.means)
uwb.wifi.means$dayoftheweek <- NULL
uwb.wifi.means[,propuwbwifi:= ff_wifi_mean/ff_uwb_mean]
uwb.wifi.charts <- merge(uwb.wifi.charts,uwb.wifi.means,by='dayoftheweek')

uwb.wifi.charts[type=='uwb',ff_align_perc:=round(ff_sum*propuwbwifi,0)]
uwb.wifi.charts[type!='uwb',ff_align_perc:=ff_sum]
uwb.wifi.charts[type=='uwb',
                ff_align_mean_wifi:=ff_sum-ff_uwb_mean+ff_wifi_mean]
uwb.wifi.charts[type!='uwb',
                ff_align_mean_wifi:=ff_sum]
ff_charts <- uwb.wifi.charts[,.(median(ff_align_perc,na.rm=T),
                                mean(ff_align_mean_wifi,na.rm=T)),by=date]
setnames(ff_charts,old=c('V1','V2'),
         new=c('ff_median_perc','ff_mean_mean'))

# Analysis by time of day
uwb_ff_la_5min[,hour:=substring(timestamp,12,13)]
uwb_ff_la_5min[,date:=substring(timestamp,1,10)]
uwb_ff_la_5min[,dayoftheweek:=weekdays(as.Date(substring(timestamp,1,10)))]
uwb.data.hr.charts <- 
   uwb_ff_la_5min[,.(ff_sum=sum(footfall,na.rm=T),
                     ff_mean=round(mean(footfall,na.rm=T),0)),
                     by=.(dayoftheweek,hour,xyid)]
uwb.data.hr.charts.totals <- 
uwb.data.hr.charts[,.(round(mean(ff_sum,na.rm = T),0)),
                      by=.(hour,dayoftheweek)]
setnames(uwb.data.hr.charts.totals,
         old=c('V1'),new=c('ff_mean'))
uwb.data.hr.charts.totals[,dayoftheweek:=factor(dayoftheweek,
                     levels = c("Monday", "Tuesday", "Wednesday",
                                "Thursday", "Friday", "Saturday", "Sunday"))]
uwb.data.day.charts.after <- 
   uwb.wifi.charts[type=='uwb',.(sum(ff_sum,na.rm=T)),
                  by=.(date,xyid)]
setnames(uwb.data.day.charts.after,
         old=c('V1'),new=c('ff_sum'))
uwb.data.day.charts.after$time <- 'After Restrictions'

wifi.day.day.charts.before <- 
uwb.wifi.charts[type=='wifi' &date<=as.IDate('2020-03-26'),
                          .(ff_sum=sum(ff_sum,na.rm=T),
                            ff_mean=round(mean(ff_sum,na.rm=T),0),
                            'time'='Before Restrictions'),
                          by=.(date,xyid)]
uwb.wifi.day.charts.total <- 
   rbind(uwb.data.day.charts.after[,.(date,ff_sum,xyid,time)],
         wifi.day.day.charts.before[,.(date,ff_sum,xyid,time)])

# Analysis by hour by xyid UWB period
ggplot(uwb.data.hr.charts.totals) +
   aes(x = hour, fill=xyid,weight = ff_mean) +
   geom_bar() +
   labs(x = "Hour of the day",
        y = "Average footfall counts in the period") +
   theme_minimal()+
   theme(text = element_text(size=10),
         legend.position='none') +
   facet_grid(vars(dayoftheweek), vars())
setwd(ladataf)
ggsave('./img/uwb.data.hr.charts.png',
       width = 150,
       height= 150,
       units = 'mm',
       dpi=300,
       limitsize = T)
ggplot(uwb.data.hr.charts.totals) +
   aes(x = hour, fill = dayoftheweek, group = dayoftheweek, weight = ff_mean) +
   geom_bar() +
   scale_fill_hue() +
   theme_minimal() +
   theme(legend.position = "none") +
   facet_wrap(vars(dayoftheweek))

# Analysis by day by xyid UWB period
ggplot(uwb.data.day.charts) +
   aes(x = dayoftheweek, fill = xyid, weight = ff_mean_period) +
   geom_bar() +
   scale_fill_hue() +
   labs(x = " ", y = "Average footfall counts in the period") +
   theme_minimal() +
   theme(text = element_text(size=10),
         legend.position='none') +
   facet_grid(vars(xyid), vars())
ggplot(uwb.data.day.charts) +
   aes(x = dayoftheweek, fill = xyid, weight = ff_mean_period) +
   geom_bar(position = "dodge") +
   scale_fill_hue() +
   labs(x = "Locations", y = "Average footfall counts in the period", fill = " ") +
   theme_minimal() +
   theme(text = element_text(size=8),
         legend.position = "bottom")
ggsave('./img/uwb.data.day.charts.png',
       width = 150,
       height= 80,
       units = 'mm',
       dpi=300,
       limitsize = T)

theme_set(theme_classic(base_size=12)+ 
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank()))

ggplot(ff_charts[date >= as.IDate('2020-01-01'),]) +
   aes(x = date, y = ff_median_perc) +
   geom_line(size = 0.5, colour = "#6baed6") +
   labs(x = " ", y = "Median Footfall") +
   theme_classic(base_size = 12) +
   scale_y_continuous(expand = c(0,0)) + 
   scale_x_date(breaks=c(min(ff_charts[date >= as.IDate('2020-01-01'),]$date),
                         covid.dates$event_date,
                         max(ff_charts[date >= as.IDate('2020-01-01'),]$date)),
                expand = c(0,0))+
   geom_vline(
      xintercept = covid.dates$event_date,
      col = "red",
      lwd = 0.5,
      alpha = 0.75) +
     theme(legend.position='none',
         legend.title = element_blank(),
         axis.text.x = element_text(angle=90,size=8))+
ggsave('./img/total_la_footfall.png',
       width = 68,
       height= 22,
       units = 'mm',
       scale = 3.5,
       dpi=300,
       limitsize = T)
esquisser()
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