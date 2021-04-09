library(data.table)
library(sf)
library(parallel)
library(ggplot2)

options(scipen = 999)
options(digits=5)
gc()
#- Clear workspace
rm(list=ls())

dir <- "."
allFiles <- list.files(dir)
rDir <- normalizePath(dirname(allFiles[1])) 
setwd(rDir)

#current_working_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(current_working_dir)
setwd('./data')
# Setting the palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# Loading the events and UK Bank Holidays for 2020
covid.better.dates <- 
   fread('covid.better.dates.csv')
uk.bank.holidays <- 
   fread('UK Bank Holidays.csv')
# Loading the Tier evolution
la.tier.evolution.charts <- 
   fread('la.tier.evolution.charts.csv')
# Loading the gmr data
gmr_lad_by_day_charts <- 
   fread('gmr_lad_by_day_charts.csv')
gmr_uk_by_day_charts <- 
fread('gmr_uk_by_day_charts.csv')
# Loading the events section
gmr_events_charts <- 
   fread('gmr_events_charts.csv')
lad2019_chart_tier <- 
   st_read('lad2019_tier_chart.shp')
unique(as.character(gmr_events_charts$event))
ff_typeL       <- as.list(c("Retail and Recreation",
                           "Grocery and Pharmacy",
                           "Parks",
                           "Transit Stations",
                           "Residential",
                           "Workplaces"))
dayoftheweekL <- as.list(c("Monday", "Tuesday", "Wednesday",
                           "Thursday", "Friday", "Saturday", "Sunday"))
ladname.chart <- fread('gmr.lad.lut.csv')$lad19nm
subregion1.chart <- 
   unique(gmr_lad_by_day_charts[lad19nm==ladname.chart,sub_region_1])
gmr_events_days_charts <- 
      gmr_lad_by_day_charts[lad19nm==ladname.chart,
                            .(ff_median=median(ff_value,na.rm = T)),
                         by=.(event,lad19cd,lad19nm,sub_region_1,dayoftheweek,ff_type)]
gmr_events_days_sub_region_1_charts <- 
      gmr_lad_by_day_charts[sub_region_1==subregion1.chart,
                            .(ff_median_subrgn1=median(ff_value,na.rm = T)),
                      by=.(event,sub_region_1,dayoftheweek,ff_type)]
gmr_events_days_uk_charts <- 
   gmr_lad_by_day_charts[,.(ff_median_uk=median(ff_value,na.rm = T)),
                         by=.(event,dayoftheweek,ff_type)]
gmr_events_days_charts <- 
merge(gmr_events_days_charts,
merge(gmr_events_days_sub_region_1_charts,
      gmr_events_days_uk_charts,
      by=c('event','dayoftheweek','ff_type')),
by=c('event','sub_region_1','dayoftheweek','ff_type'))

covid.better.dates.charts <- 
covid.better.dates[start_date<=max(gmr_lad_by_day_charts$date),]

# Setting the directory
setwd('../')
# Google Mobility Report data Timeseries Graph  LA, Sub Region and UK ----
fig.1_6 <- 
   lapply(1:length(ff_typeL),function(i){
      #i <-1 
      ff_typev <- ff_typeL[[i]]
      ggplot() +
       geom_line(data=gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),],
                 aes(x = date, y = ff_value, colour =cbPalette[i]),
                 size = 0.75) +
     scale_color_manual(values=cbPalette[i])+
      geom_area(data=gmr_uk_by_day_charts[ff_type==ff_typev],
               aes(x = date, y = ff_median, fill = cbPalette[i]),
               stat = 'identity',
               size = 0.55,
               alpha=0.3)+
      scale_fill_manual(values=cbPalette[i])+   
      theme_minimal() +
      facet_wrap(vars(ff_type),
                 ncol = 1,
                 dir='v',
                 scales = 'free_y') +
      scale_x_date(breaks=c(min(gmr_lad_by_day_charts$date),as.Date('2020-08-31'),covid.better.dates.charts$start_date),
                   labels =c(min(gmr_lad_by_day_charts$date),'2020-08-31',covid.better.dates.charts$start_date),
                   expand = c(0,0),guide = guide_axis(check.overlap = TRUE), date_labels = "%b %d")+
      ylim(c(min(gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),]$ff_value,na.rm=T),
                                    max(gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),]$ff_value,na.rm=T)+2))+
      geom_segment(aes(x =c(covid.better.dates.charts[-1]$start_date,
                            covid.better.dates.charts[event=='Eat out to help out']$end_date),
                   xend=c(covid.better.dates.charts[-1]$start_date,
                          covid.better.dates.charts[event=='Eat out to help out']$end_date),
                   y=min(gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),]$ff_value,na.rm=T),
                  yend=max(gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),]$ff_value,na.rm=T)+2),    
        colour="grey69", linetype='dashed', alpha = 1)+
      geom_text(aes(x=covid.better.dates.charts[c(-1,-nrow(covid.better.dates.charts)),]$start_date,
                       y=max(gmr_lad_by_day_charts[(lad19nm==ladname.chart)&(ff_type==ff_typev),]$ff_value,na.rm=T)+2,
                       label=covid.better.dates.charts[c(-1,-nrow(covid.better.dates.charts)),]$event),
                   vjust=0,hjust=0,angle=0,
                   check_overlap = T,
                   size=4,
                   alpha=0.75)+
      theme(legend.position='none',
            legend.title = element_blank(),
            strip.text.x = element_text(size = 16,
                                         colour = "black", 
                                         face='bold'),
            axis.text.x = element_text(size=12,
                                       face = 'bold',
                                       angle = 0,
                                       hjust = 0.5,
                                       vjust=0.5),
            axis.text.y = element_text(size=10))+
      xlab('')+
      ylab('')+
         labs(caption = "Source: Google Mobility Reports")
   ggsave(paste0(getwd(),'/img/',ff_typev,'_line_area_uk.png'),
          width = 200,
          height= 70,
          units = 'mm',
          scale=2,
          dpi=300,
          limitsize = T)
})
# Creating the events list for printing
eventsDT <- 
   data.table('event'=factor(covid.better.dates[eventf%in%unique(gmr_events_charts$event),eventf]),
              'order'=covid.better.dates[eventf%in%unique(gmr_events_charts$event),start_date])
eventsL <-
      unique(gmr_events_charts[, .(event)], by = 'event')

eventsL <-merge(eventsL,eventsDT,by='event')          
setorder(eventsL,order)
gmr_events_charts[,ff_type:=factor(ff_type,levels = c("Retail and Recreation",
                                                    "Grocery and Pharmacy",
                                                    "Parks",
                                                    "Transit Stations",
                                                    "Residential",
                                                    "Workplaces"))]
levels.events <- eventsL$event
gmr_events_charts[,event:=factor(event,levels=levels.events)]
unique(gmr_events_charts$event)
setorder(gmr_events_charts,lad19nm,lad19cd,event,ff_type)
#setwd('../')
# Events figures ----
fig.7_19 <-
   lapply(1:nrow(eventsL),function(j){
  #j <- 1
   event.v <- eventsL[j]$event
   # Events charts ---
   ggplot(gmr_events_charts[
      event==event.v&lad19nm==ladname.chart,]) +
      aes(x = ff_type, fill = ff_type, weight = ff_median) +
      scale_fill_manual(values=cbPalette)+
      geom_bar(position = "dodge")+
      geom_label(aes(x=ff_type,
                    y=ff_median+1*sign(ff_median),
                     label=ff_median),
                size=8)+
       geom_segment(aes(x=as.numeric(rev(gmr_events_charts[event==event.v
                                                       &lad19nm==ladname.chart,]$ff_type))+0.3,
                          xend=as.numeric(rev(gmr_events_charts[event==event.v
                                                            &lad19nm==ladname.chart,]$ff_type))+0.3,
                         y = 0,
                        yend = ff_median_uk-1*sign(ff_median_uk)),
                    linetype='dashed',
                    colour='black')+
       geom_label(aes(x=ff_type,y=ff_median_uk,
                      label=ff_median_uk),
                nudge_x = 0.3,
                nudge_y = 0,
                size=5,
                )+
      annotate(geom='label',
               x=as.numeric(rev(gmr_events_charts[event==event.v
                                                  &lad19nm==ladname.chart,]$ff_type))+0.3,
               y=0,
               fill='white',
              label='UK',
              size=6)+
      geom_segment(aes(x=as.numeric(rev(gmr_events_charts[event==event.v
                                                          &lad19nm==ladname.chart,]$ff_type))-0.3,
                       xend=as.numeric(rev(gmr_events_charts[event==event.v
                                                             &lad19nm==ladname.chart,]$ff_type))-0.3,
                       y = 0,
                       yend = ff_median_subrgn1-1*sign(ff_median_subrgn1)),
                   linetype='dashed',
                   colour='black')+
      geom_label(aes(x=ff_type,y=ff_median_subrgn1,
                     #colour='white',
                     label=ff_median_subrgn1),
                 nudge_x = -0.3,
                 nudge_y = 0,
                 size=5)+
      annotate(geom='label',
               x=as.numeric(rev(gmr_events_charts[event==event.v
                                                  &lad19nm==ladname.chart,]$ff_type))-0.3,
               y=0,
               fill='white',
               label=unique(gmr_events_charts[event==event.v
                                              &lad19nm==ladname.chart,]$sub_region_1),
               size=6)+
      theme_minimal() +
      theme(legend.position = "none",
            panel.spacing = unit(1, "lines"),
            axis.text.x = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.5, vjust = 0.5,
                                       face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 16, angle = 0, hjust = 0.1, vjust = 0.0,
                                       face = "plain"),  
            #plot.margin = unit(c(10,10,10,10), "mm"),
            legend.title = element_blank(),
            strip.text.x = element_text(size = 18,
                                        colour = "black", 
                                        face='bold')) +
      facet_wrap(vars(event), ncol=1,dir='v',
                  scales = "free_y"
                  )+
      scale_x_discrete(limits=
                          rev(levels(gmr_events_charts[event==event.v
                                                             &lad19nm==ladname.chart,]$ff_type)),
                       expand = c(0,0))+
      geom_hline(yintercept =c(-50,50),
                 linetype='dotted',
                 col = "black", lwd = 0.5, alpha = 0.5) +
      scale_y_continuous(breaks = c(-75,-50,0,50,75),
                         labels=c('-75','-50','Baseline: Jan 03-Feb 06 2020','50','75'))+
      xlab('')+
      ylab('')+
      coord_flip()+
      labs(caption = "Source: Google Mobility Reports")
   ggsave(paste0(getwd(),'/img/',j,'_',gsub(' ','_',event.v),'.png'),
          width = 240,
          height= 190,
          units = 'mm',
          #scale=1.5,
          dpi=300,
          limitsize = F)
})   
# Making the tier evolution maps
# Selecting the subregion to map the neighbours
# Tier evolution chart
lad2019_chart_tier$Tier <- gsub('L','Lockdown',lad2019_chart_tier$Tier)
ggplot(data=lad2019_chart_tier) +
   aes(fill = Tier) +
   geom_sf(size = 0.1,
           color='white',
           ) +
   geom_sf(data=lad2019_chart_tier[lad2019_chart_tier$lad19nm==ladname.chart,],
         fill=NA,size = 0.3,color='black') +
   scale_fill_brewer(palette = "Reds") +
   theme_void() +
   theme(legend.position = "bottom",
         legend.title = element_text(size=12),
         legend.text = element_text(size=12),
         plot.caption = element_text(size=8),
         strip.text.x = element_text(size = 14,
                                     colour = "black", 
                                     face='bold')) +
   facet_wrap(vars(Date), ncol = 4L, nrow = 1L)+
   labs(caption = "Source: ONS Geography Open Data and Geolytix")
ggsave(paste0(getwd(),'/img/',gsub(' ','',ladname.chart),'_tier_evolution','.png'),
       width = 180,
       height= 60,
       units = 'mm',
       scale=1,
       dpi=300,
       limitsize = F)
file.remove('Rplots.pdf')
dir <- "."
allFiles <- list.files(dir)
rDir <- normalizePath(dirname(allFiles[1])) 
setwd(rDir)
reportRmd <- list.files(path=rDir,pattern = glob2rx('*.Rmd'))
rmarkdown::render(reportRmd,'html_document')
# # COVID-19 Important dates chart
# library(vistime)
# timeline_data <- data.frame(event = covid.better.dates[-1,]$event,
#                             start = as.character(covid.better.dates[-1,]$start_date), 
#                             end   = as.character(covid.better.dates[-1,]$end_date),
#                             group = "COVID-19")
# library(timelineS)
# df <-   data.frame('Events'=timeline_data$event,
#                    'Event_Dates'=as.Date(timeline_data$start))
# Saving the timeline object
# png(paste0('./img/','covid19importantdates.png'),
#     width = 200,
#     height= 100,
#     res = 300,
#     units = 'mm')
# timelineS(df,
#           line.width = 5, line.color = "gray37",
#           scale = "month", scale.format = "%b %Y",
#           scale.font = 1, scale.orient = 1,
#           buffer.days = 45,
#           scale.above = F, scale.cex = 2, scale.tickwidth = 1,
#           labels = paste0(df[[2]],' ',df[[1]]),
#           label.direction = "up",
#           label.length = rev(seq(from=0.1,to=0.8,by=0.1)),
#           label.position = c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2),
#           label.color = "gray87", label.cex = 1.5, label.font = 1,
#           label.angle =0, pch = 20, point.cex = 1.1,
#           point.color = "gray37")
# dev.off()
# All in one
# ggplot(gmr_events_charts[lad19nm==ladname.chart,]) +
#    aes(x = ff_type, fill = ff_type, weight = ff_median) +
#    geom_bar(position = "dodge",width=0.1) +
#    scale_fill_manual(values=cbPalette)+
#    geom_text(aes(x=ff_type,
#                  y=ff_median+1*sign(ff_median),
#                  label=ff_median),
#              size=2)+
#    theme_minimal()+
#    facet_wrap(vars(event),ncol=1,dir='v',
#               scales = "free_y")+
#    theme(legend.position = "none",
#          #panel.spacing = unit(1, "lines"),
#          axis.text.x = element_text(color = "grey20", size = 12, angle = 0, hjust = 0.5, vjust = 0.5,
#                                     face = "plain"),
#          legend.title = element_blank(),
#          strip.text.x = element_text(size = 12,
#                                      colour = "black", 
#                                      face='bold'))+
#    scale_x_discrete(limits=
#                        rev(levels(gmr_events_charts[lad19nm==ladname.chart,]$ff_type)),
#                     expand = c(0.05,0.05))+
#    geom_hline(yintercept =c(-50,50),
#               linetype='dotted',
#               col = "black", lwd = 0.5, alpha = 0.5) +
#    scale_y_continuous(breaks = c(-75,-50,0,50,75),
#                       labels=c('-75','-50','Baseline: Jan 03 to Feb 06','+50','+75'))+
#    xlab('')+
#    ylab('')+
#    labs(caption = "Source: Google Mobility Reports")+
#    coord_flip()
# ggsave(paste0('./img/gmr_events_charts_all.png'),
#        width = 200,
#        height= 300,
#        units = 'mm',
#        #scale=1.5,
#        dpi=300,
#        limitsize = F)
