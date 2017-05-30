x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)
#setwd('F:\\Research\\PMCoarse_CATLBW\\Data')
setwd('F:\\Research\\PMC_LBW_CA\\PMC_LBW_CA')
#setwd('C:\\Users\\ebike\\OneDrive\\Documents\\Research\\PMC_LBW_CA\\Data')

## PM2.5
load('Data//PMC_DF_20170518.RData')#PMC_DF
PMC.DF=PMC_DF %>%
  filter(PMC_Value<500) %>%
  group_by(Date) %>%
  summarize(PMC=mean(PMC_Value,na.rm=TRUE)) %>%
  mutate(MonthDay_num=as.numeric(format(Date,'%j')),MonthName=factor(months(Date,abbreviate=TRUE)),
         Period=cut(as.numeric(substr(Date,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
title=substitute(paste(PM[10-2.5],' Trend from 2001 to 2013 in California'))
Y_title=substitute(paste(PM[10-2.5],' Value'))
ylims=c(floor(min(PMC.DF$PMC)), ceiling(max(PMC.DF$PMC)))
Legend=data.frame(PMC.DF[!duplicated(PMC.DF$MonthName),c('MonthName','MonthDay_num')])


###############################
ggplot(PMC.DF, aes(Date,PMC)) +
  geom_point(na.rm=TRUE)
ggplot(PMC.DF, aes(Date,PMC)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(span=0.05,method='loess',size=2)+
  theme_bw()

#########################
Plot_gg_PMC=ggplot(data=PMC.DF,aes_string(x="MonthDay_num",y="PMC",col="Period"))+
  geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
  scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
  scale_y_continuous(Y_title)+
  ggtitle(title)+
  # coord_cartesian(ylim=c(0,2.5))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5))
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PMC


#########################
## PM2.5 Specific Monitor
#########################

MonList=unique(PMC_DF$FIPS_C)
p=list()

#pdf("C://Users//ebike//OneDrive//Documents//Research//PMC_LBW_CA//Results//PM25_plots.pdf")
pdf("Results//PMC_plots.pdf")
for (i in 1:length(MonList)){
  PMC.DF=filter(PMC_DF,FIPS_C==MonList[i]) %>%
    #mutate(FIPS_C=substr(FIPSPOC,1,9)) %>%
    group_by(Date) %>%
    summarize(PMC=mean(PMC_Value,na.rm=TRUE)) %>%
    mutate(MonthDay_num=as.numeric(format(Date,'%j')),MonthName=factor(months(Date,abbreviate=TRUE)),
           Period=cut(as.numeric(substr(Date,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
  title=substitute(paste(PM[10-2.5],' Trend from 2001 to 2013 in California at ',nn1),list(nn1=MonList[i]))
  Y_title=substitute(paste(PM[10-2.5],' Value'))
  ylims=c(floor(min(PMC.DF$PMC)), ceiling(max(PMC.DF$PMC)))
  
  Plot_gg_PMC=ggplot(data=PMC.DF,aes_string(x="MonthDay_num",y="PMC",col="Period"))+
    geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
    scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
    scale_y_continuous(Y_title)+
    ggtitle(title)+
    theme_bw()+
    theme(plot.title=element_text(hjust = 0.5))
  p[[i]]=Plot_gg_PMC
  print(Plot_gg_PMC)
  rm(PMC.DF,Plot_gg_PMC)
}
dev.off()

