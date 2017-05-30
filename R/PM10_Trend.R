x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)
#setwd('F:\\Research\\PMCoarse_CATLBW\\Data')
#setwd('F:\\Research\\PMC_LBW_CA\\PMC_LBW_CA\\Data')
setwd('C:\\Users\\ebike\\OneDrive\\Documents\\Research\\PMC_LBW_CA\\Data')

## PM10
load('PM10_81102_Cleaned.RData')#PM10_81102_Cleaned
PM10.DF=mutate(PM10_81102_Cleaned,FIPS_C=substr(FIPSPOC,1,9)) %>%
    filter(PM10<500) %>%
    group_by(Date.Local) %>%
    summarize(PM10=mean(PM10,na.rm=TRUE)) %>%
    mutate(MonthDay_num=as.numeric(format(Date.Local,'%j')),MonthName=factor(months(Date.Local,abbreviate=TRUE)),
      Period=cut(as.numeric(substr(Date.Local,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
title=substitute(paste(PM[10],' Trend from 2001 to 2013 in California'))
Y_title=substitute(paste(PM[10],' Value'))
ylims=c(floor(min(PM10.DF$PM10)), ceiling(max(PM10.DF$PM10)))
Legend=data.frame(PM10.DF[!duplicated(PM10.DF$MonthName),c('MonthName','MonthDay_num')])


###############################
ggplot(PM10.DF, aes(Date.Local,PM10)) +
  geom_point(na.rm=TRUE)
ggplot(PM10.DF, aes(Date.Local,PM10)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(span=0.05,method='loess',size=2)+
  theme_bw()

#########################
Plot_gg_PM10=ggplot(data=PM10.DF,aes_string(x="MonthDay_num",y="PM10",col="Period"))+
  geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
  scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
  scale_y_continuous(Y_title)+
  ggtitle(title)+
 # coord_cartesian(ylim=c(0,2.5))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5))
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PM10

filter(PM10_81102_Cleaned,PM10<500) %>%
    group_by(FIPSPOC) %>%
    summarize(PM10=mean(PM10)) %>%
    arrange(-PM10)

#########################
## PM10 Specific Monitor
#########################

MonList=unique(PM10_81102_Cleaned$FIPSPOC)
p=list()

pdf("C://Users//ebike//OneDrive//Documents//Research//PMC_LBW_CA//Results//PM10_plots.pdf")
for (i in 1:length(MonList)){
PM10.DF=filter(PM10_81102_Cleaned,FIPSPOC==MonList[i]) %>%
  mutate(FIPS_C=substr(FIPSPOC,1,9)) %>%
  group_by(Date.Local) %>%
  summarize(PM10=mean(PM10,na.rm=TRUE)) %>%
  mutate(MonthDay_num=as.numeric(format(Date.Local,'%j')),MonthName=factor(months(Date.Local,abbreviate=TRUE)),
         Period=cut(as.numeric(substr(Date.Local,1,4)),c(2001,2005,2009,2014),labels=c('2001-2004','2005-2008','2009-2013'),right=FALSE,include.highest=TRUE))
title=substitute(paste(PM[10],' Trend from 2001 to 2013 in California at ',nn1),list(nn1=MonList[i]))
Y_title=substitute(paste(PM[10],' Value'))
ylims=c(floor(min(PM10.DF$PM10)), ceiling(max(PM10.DF$PM10)))
Legend=data.frame(PM10.DF[!duplicated(PM10.DF$MonthName),c('MonthName','MonthDay_num')])

#ggplot(PM10.DF, aes(Date.Local,PM10)) +
#  geom_point(na.rm=TRUE)+
#  geom_smooth(span=0.05,method='loess',size=2)+
#  theme_bw()

  Plot_gg_PM10=ggplot(data=PM10.DF,aes_string(x="MonthDay_num",y="PM10",col="Period"))+
    geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
    scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
    scale_y_continuous(Y_title)+
    ggtitle(title)+
    theme_bw()+
    theme(plot.title=element_text(hjust = 0.5))
  p[[i]]=Plot_gg_PM10
  print(Plot_gg_PM10)
 rm(PM10.DF,Plot_gg_PM10)
}
dev.off()

test=do.call(arrangeGrob,c(p,nrow=10,ncol=10))

pdf("C://Users//ebike//OneDrive//Documents//Research//PMC_LBW_CA//Results//PM10_plots.pdf", onefile =FALSE,20,15)
grid.draw(test)
dev.off()

pdf("//Results//PM10_plots.pdf", onefile = TRUE)
for (i in seq(length(p))) {
  do.call("grid.arrange", p[[i]])  
}
dev.off()

###############################
ggplot(PM10.DF, aes(Date.Local,PM10)) +
  geom_point(na.rm=TRUE)
ggplot(PM10.DF, aes(Date.Local,PM10)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(span=0.05,method='loess',size=2)+
  theme_bw()

#########################

Plot_gg_PM10=ggplot(data=PM10.DF,aes_string(x="MonthDay_num",y="PM10",col="Period"))+
  geom_smooth(span=0.6,method='loess',size=2.5,se=FALSE)+
  scale_x_continuous('Month',breaks=Legend$MonthDay_num,labels=Legend$MonthName,expand=c(0,0))+
  scale_y_continuous(Y_title)+
  ggtitle(title)+
  # coord_cartesian(ylim=c(0,2.5))+
  theme_bw()+
  theme(plot.title=element_text(hjust = 0.5))
#	theme(panel.background = element_rect(fill = "black"))
Plot_gg_PM10