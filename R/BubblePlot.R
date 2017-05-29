x<-c("dplyr","ggplot2","gridExtra","grid")
lapply(x, require, character.only=T)
#setwd('F:\\Research\\PMCoarse_CATLBW\\Data')
setwd('C:\\Users\\ebike\\OneDrive\\Documents\\Research\\PMC_LBW_CA')

earth.dist<-function (long1, lat1, long2, lat2){
	rad <- pi/180
	a1 <- lat1 * rad
	a2 <- long1 * rad
	b1 <- lat2 * rad
	b2 <- long2 * rad
	dlon <- b2 - a2
	dlat <- b1 - a1
	a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
	R <- 6378.145
	d <- R * c
	return(d)
}

## PM10
load('PM10_81102_Cleaned.RData')#PM10_81102_Cleaned
df=select(PM10_81102_Cleaned,FIPSPOC,Date.Local,PM10,Latitude,Longitude)
length(unique(df$FIPSPOC))
df2=expand.grid(x=unique(df$FIPSPOC),y=unique(df$FIPSPOC)) %>%
	filter(x!=y)

output=data.frame()
for (i in 1:dim(df2)[1]){
temp1=filter(df,FIPSPOC==df2$x[i]) %>%
	rename(FIPSPOC1=FIPSPOC,PM10_1=PM10,Lat1=Latitude,Long1=Longitude)
temp2=filter(df,FIPSPOC==df2$y[i]) %>%
	rename(FIPSPOC2=FIPSPOC,PM10_2=PM10,Lat2=Latitude,Long2=Longitude)

temp=inner_join(temp1,temp2,by='Date.Local') %>%	
	mutate(dist=earth.dist(Long1,Lat1,Long2,Lat2))
out=data.frame(temp$FIPSPOC1[1],temp$FIPSPOC2[1],temp$dist[1],dim(temp)[1],cor(temp$PM10_1,temp$PM10_2))
names(out)=c('FIPSPOC1','FIPSPOC2','Distance','N','Cor')
output=rbind(output,out)
rm(out)
}
output=arrange(output,FIPSPOC1,FIPSPOC2)

output2=distinct(output,Distance,N,Cor,.keep_all=TRUE) %>%
	filter(Distance<100) %>%
	arrange(FIPSPOC1,FIPSPOC2)

fig1=ggplot(output2,aes(Distance,Cor))+
	geom_point(aes(size=N))+
	coord_cartesian(ylim=c(0,1),xlim=c(0,50))+
	ggtitle(expression(paste(PM[10]," Dispersion in CA")))+
	theme_bw()

## PM2.5
load('PM25_88101_Cleaned.RData')#PM25_88101_Cleaned
df=select(PM25_88101_Cleaned,FIPSPOC,Date.Local,PM25,Latitude,Longitude)
length(unique(df$FIPSPOC))
df2=expand.grid(x=unique(df$FIPSPOC),y=unique(df$FIPSPOC)) %>%
	filter(x!=y)

output=data.frame()
for (i in 1:dim(df2)[1]){
temp1=filter(df,FIPSPOC==df2$x[i]) %>%
	rename(FIPSPOC1=FIPSPOC,PM25_1=PM25,Lat1=Latitude,Long1=Longitude)
temp2=filter(df,FIPSPOC==df2$y[i]) %>%
	rename(FIPSPOC2=FIPSPOC,PM25_2=PM25,Lat2=Latitude,Long2=Longitude)

temp=inner_join(temp1,temp2,by='Date.Local') %>%	
	mutate(dist=earth.dist(Long1,Lat1,Long2,Lat2))
out=data.frame(temp$FIPSPOC1[1],temp$FIPSPOC2[1],temp$dist[1],dim(temp)[1],cor(temp$PM25_1,temp$PM25_2))
names(out)=c('FIPSPOC1','FIPSPOC2','Distance','N','Cor')
output=rbind(output,out)
rm(out)
}
output=arrange(output,FIPSPOC1,FIPSPOC2)

output2=distinct(output,Distance,N,Cor,.keep_all=TRUE) %>%
	filter(Distance<100) %>%
	arrange(FIPSPOC1,FIPSPOC2)

fig2=ggplot(output2,aes(Distance,Cor))+
	geom_point(aes(size=N))+
	coord_cartesian(ylim=c(0,1))+
	ggtitle(expression(paste(PM[2.5]," Dispersion in CA")))+
	theme_bw()

## PM10-2.5
load('PMC_DF_20170518.RData')#PMC_DF
df=select(PMC_DF,FIPS_C,Date,PMC_Value,Lat,Long)
length(unique(df$FIPS_C))
df2=expand.grid(x=unique(df$FIPS_C),y=unique(df$FIPS_C)) %>%
	filter(x!=y)

output=data.frame()
for (i in 1:dim(df2)[1]){
temp1=filter(df,FIPS_C==df2$x[i]) %>%
	rename(FIPS_C1=FIPS_C,PMC_1=PMC_Value,Lat1=Lat,Long1=Long)
temp2=filter(df,FIPS_C==df2$y[i]) %>%
	rename(FIPS_C2=FIPS_C,PMC_2=PMC_Value,Lat2=Lat,Long2=Long)

temp=inner_join(temp1,temp2,by='Date') %>%	
	mutate(dist=earth.dist(Long1,Lat1,Long2,Lat2))
out=data.frame(temp$FIPS_C1[1],temp$FIPS_C2[1],temp$dist[1],dim(temp)[1],cor(temp$PMC_1,temp$PMC_2))
names(out)=c('FIPS_C1','FIPS_C2','Distance','N','Cor')
output=rbind(output,out)
rm(out)
}
output=arrange(output,FIPS_C1,FIPS_C2)

output2=distinct(output,Distance,N,Cor,.keep_all=TRUE) %>%
	filter(Distance<100) %>%
	arrange(FIPS_C1,FIPS_C2)

fig3=ggplot(output2,aes(Distance,Cor))+
	geom_point(aes(size=N))+
	coord_cartesian(ylim=c(0,1),xlim=c(0,50))+
	ggtitle(expression(paste(PM[10-2.5]," Dispersion in CA")))+
	theme_bw()

grid.arrange(fig1,fig2,fig3,ncol=1)
