x<-c("dplyr","data.table") 
lapply(x, require, character.only=T)
setwd('K:\\Research\\PMCoarse_TLBWCA\\Data')

load('PMC_Monitor_20170427.RData') #PMC_Obs_List
load('PMC_DF_20170427.RData') #PMC_DF

temp1=select(PMC_DF,FIPS_C,Date,PMC_Value) %>%
	data.table()
setkey(temp1,FIPS_C,Date)
temp2=temp1[CJ(unique(FIPS_C),seq(min(Date),max(Date),by=1))]
temp2[,Pol_Week:=rollapply(PMC_Value,7,mean,align=c("right"),fill=NA,na.rm=TRUE,partial=TRUE),by=FIPS_C]
temp2$Pol_Week[is.nan(temp2$Pol_Week)]=NA

DateList=select(temp2,FIPS_C,LMP=Date) %>% data.table()
setkey(DateList,FIPS_C,LMP)

output=select(DateList,FIPS_C,LMP) %>% data.table()
setkey(output,FIPS_C,LMP)

	for (bb in 1:44){
		temp3=mutate(DateList,Date=LMP+bb*7-1) %>% data.table()
		setkey(temp3,FIPS_C,Date)
		temp4=temp2[temp3] %>% select(FIPS_C,LMP,Date,Pol_Week)
		setkey(temp4,FIPS_C,LMP)
		output=cbind(output,temp4$Pol_Week)
		rm(temp3,temp4)
	}
WkName=paste('Week',c(1:44),sep='')
colnames(output)=c('FIPS_C','LMP',WkName)
output=data.frame(output)

for (cc in 37:44){
	output[[paste0('GestWeek_',cc)]]=ifelse(rowSums(!is.na(output[,5:(2+cc)]))>(cc-2)*0.75,rowMeans(output[,c(5:(2+cc))],na.rm=TRUE),NA)

}	
	
output=output[rowSums(!is.na(output[,3:28]))>0,]
output=output[rowSums(!is.na(output[,5:15]))>8,]
output=output[rowSums(!is.na(output[,16:28]))>9,]

for (dd in 37:44){
	output[,c(dd+10)]=ifelse(rowSums(!is.na(output[,29:(dd+2)]))<((dd-26)*0.75),NA,output[,c(dd+10)])
}

output=output[rowSums(!is.na(output[,47:54]))>0,]
PMC_GestExp=output[,c(1:2,47:54)]

save(PMC_GestExp,file='K:\\Research\\PMCoarse_TLBWCA\\Data\\PMC_GestExp_20170505.RData')

rm(list=ls())
