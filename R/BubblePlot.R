x<-c("dplyr")
lapply(x, require, character.only=T)
setwd('F:\\Research\\PMCoarse_CATLBW\\Data')

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



load('PM10_81102_Cleaned.RData')#PM10_81102_Cleaned

df=select(PM10_81102_Cleaned,FIPSPOC,Date.Local,PM10,Latitude,Longitude)
length(unique(df$FIPSPOC))
df2=expand.grid(x=unique(df$FIPSPOC),y=unique(df$FIPSPOC)) %>%
	filter(x!=y)
