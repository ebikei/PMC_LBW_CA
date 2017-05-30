x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)
#setwd('F:\\Research\\PMCoarse_CATLBW\\Data')
setwd('F:\\Research\\PMC_LBW_CA\\PMC_LBW_CA')
#setwd('C:\\Users\\ebike\\OneDrive\\Documents\\Research\\PMC_LBW_CA\\Data')

## PM2.5
load('Data//PM25_88101_Cleaned.RData')#PM25_88101_Cleaned
PM25.DF=mutate(PM25_88101_Cleaned,FIPS_C=substr(FIPSPOC,1,9)) %>%
  filter(PM25<100) %>%
  group_by(Date.Local) %>%
  summarize(PM25=mean(PM25,na.rm=TRUE)) %>%
  mutate(MonthDay_num=as.numeric(format(Date.Local,'%j')),MonthName=factor(months(Date.Local,abbreviate=TRUE)),
         Period=cut(as.numeric(substr(Date.Local,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
title=substitute(paste(PM[2.5],' Trend from 2001 to 2013 in California'))
Y_title=substitute(paste(PM[2.5],' Value'))
ylims=c(floor(min(PM25.DF$PM25)), ceiling(max(PM25.DF$PM25)))
Legend=data.frame(PM25.DF[!duplicated(PM25.DF$MonthName),c('MonthName','MonthDay_num')])


###############################
ggplot(PM25.DF, aes(Date.Local,PM25)) +
  geom_point(na.rm=TRUE)
ggplot(PM25.DF, aes(Date.Local,PM25)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(span=0.05,method='loess',size=2)+
  theme_bw()

#########################
Plot_gg_PM25=ggplot(data=PM25.DF,aes_string(x="MonthDay_num",y="PM25",col="Period"))+
  geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
  scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
  scale_y_continuous(Y_title)+
  ggtitle(title)+
  # coord_cartesian(ylim=c(0,2.5))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5))
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PM25


#########################
## PM2.5 Specific Monitor
#########################

MonList=unique(PM25_88101_Cleaned$FIPSPOC)
p=list()

#pdf("C://Users//ebike//OneDrive//Documents//Research//PMC_LBW_CA//Results//PM25_plots.pdf")
pdf("Results//PM25_plots.pdf")
for (i in 1:length(MonList)){
  PM25.DF=filter(PM25_88101_Cleaned,FIPSPOC==MonList[i]) %>%
    mutate(FIPS_C=substr(FIPSPOC,1,9)) %>%
    group_by(Date.Local) %>%
    summarize(PM25=mean(PM25,na.rm=TRUE)) %>%
    mutate(MonthDay_num=as.numeric(format(Date.Local,'%j')),MonthName=factor(months(Date.Local,abbreviate=TRUE)),
           Period=cut(as.numeric(substr(Date.Local,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
  title=substitute(paste(PM[2.5],' Trend from 2001 to 2013 in California at ',nn1),list(nn1=MonList[i]))
  Y_title=substitute(paste(PM[2.5],' Value'))
  ylims=c(floor(min(PM25.DF$PM25)), ceiling(max(PM25.DF$PM25)))

  Plot_gg_PM25=ggplot(data=PM25.DF,aes_string(x="MonthDay_num",y="PM25",col="Period"))+
    geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
    scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
    scale_y_continuous(Y_title)+
    ggtitle(title)+
    theme_bw()+
    theme(plot.title=element_text(hjust = 0.5))
  p[[i]]=Plot_gg_PM25
  print(Plot_gg_PM25)
  rm(PM25.DF,Plot_gg_PM25)
}
dev.off()

