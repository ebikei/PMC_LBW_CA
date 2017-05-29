x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)
#setwd('F:\\Research\\PMCoarse_CATLBW\\Data')
setwd('F:\\Research\\PMC_LBW_CA\\PMC_LBW_CA\\Data')
#setwd('C:\\Users\\ebike\\OneDrive\\Documents\\Research\\PMC_LBW_CA')

## PM10
load('PM10_81102_Cleaned.RData')#PM10_81102_Cleaned
PM10.DF=mutate(PM10_81102_Cleaned,FIPS_C=substr(FIPSPOC,1,9)) %>%
    group_by(Date.Local) %>%
    summarize(PM10=mean(PM10,na.rm=TRUE)) %>%
    mutate(MonthDay_num=as.numeric(format(Date.Local,'%j')),MonthName=factor(months(Date.Local,abbreviate=TRUE)),
      Period=cut(as.numeric(substr(Date.Local,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
title=substitute(paste(PM[10],' Trend from 2001 to 2013 in California'))
Y_title=substitute(paste(PM[10],' Value'))
ylims=c(floor(min(PM10.DF$PM10)), ceiling(max(PM10.DF$PM10)))
Legend=data.frame(PM10.DF[!duplicated(PM10.DF$MonthName),c('MonthName','MonthDay_num')])

Plot_gg_PM10=ggplot(data=PM10.DF,aes_string(x="MonthDay_num",y="PM10",col="Period"))+
  geom_smooth(span=0.4,method='loess',size=2.5,se=FALSE)+
  scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
  scale_y_continuous(Y_title)+
  ggtitle(title)+
 # coord_cartesian(ylim=c(0,2.5))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5))
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PM10

