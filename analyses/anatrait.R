library(dplyr)
library(FactoMineR)
library(missMDA)
#library(mice)
#link trait matrix
# https://docs.google.com/spreadsheets/d/1auNXLqljHhXfPZpL63-og9vwUcvB5mk4QGYJTsPqzxA/edit#gid=0
library(googlesheets4)
#read the data
fishtraitnew<-read_sheet("https://docs.google.com/spreadsheets/d/1auNXLqljHhXfPZpL63-og9vwUcvB5mk4QGYJTsPqzxA/edit#gid=0",sheet=1,na="NA")
fishtrait<-as.data.frame(lapply(fishtraitnew,unlist))
#remove Depth var
fishtrait<-fishtrait[,!grepl("Depth",names(fishtrait))]
fishtrait<-fishtrait[,!grepl("Distrib",names(fishtrait))]
#explo data
diagmiss<-function(fishtrait){
	nbid<-fishtrait%>%summarise_all(n_distinct)%>%t()
	nbNA<-function(a){a[a==""]<-NA;sum(is.na(a))}
	nbNA<-fishtrait%>%summarise_all(nbNA)%>%t()
	return(data.frame(nbid=nbid,nbNA=nbNA))
}
diagmiss(fishtrait)

#numeric in numeric
fishtrait<-fishtrait%>%mutate_at(vars("LifeSpan","FoodTroph","MaturityAge"),as.numeric)
fishtrait<-fishtrait%>%mutate_at(vars("DemersPelag","Migratory","FertilizationType","SpawningFreq"),as.factor)
diagmiss(fishtrait)

#imputation
fishtrait2<-mice::mice(fishtrait,m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
str(fishtrait2$imp$bmi)
fishtrait3<-mice::complete(fishtrait2)
diagmiss(fishtrait3)

fishtrait<-fishtrait3

#categorize numerical values using quantile
#a function to categorize numerical value in quantile interval
catquant<-function(a,value){
	#a<-unique(fishtrait$LifeSpan)
  value<-c(0,as.vector(value))
	print(value)
	lval<-length(value)
	interval<-paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
	a1<-interval[findInterval(a,value,all.inside=T)]
	return(a1)
}
#a quick check for the function
table(fishtrait$MaturityAge,catquant(fishtrait$MaturityAge,quantile(fishtrait$MaturityAge,na.rm=T)))
fishtrait$LifeSpan<-catquant(fishtrait$LifeSpan,quantile(fishtrait$LifeSpan,na.rm=T))
fishtrait$FoodTroph<-catquant(fishtrait$FoodTroph,quantile(fishtrait$FoodTroph,na.rm=T))
fishtrait$MaturityAge<-catquant(fishtrait$MaturityAge,quantile(fishtrait$MaturityAge,na.rm=T))

#explo data
diagmiss(fishtrait)

#selection of some traits where information is consitent (let's say...)
fishtrait<-fishtrait%>%select(Species,LifeSpan,DemersPelag,FoodTroph,Migratory,SpawningFreq,FoodTroph,MaturityAge,IUCN.status)

pipo<-fishtrait[,-1]
row.names(pipo)<-fishtrait[,1]
rez<-MCA(pipo,ncp=6)

matdist<-rez$coord


#imputation
#need to convert character in factor...
pipo<-fishtrait%>%mutate_all(as.factor)%>%data.frame()
#row.names = Species
row.names(pipo)<-pipo[,1]
pipo<-pipo[,-1]
#pipo<-pipo[,-1]
nb<-estim_ncpMCA(pipo)
rez<-imputeMCA(pipo,ncp=4)
rez<-MCA(pipo,tab.disj=rez$tab.disj.comp)
HCPC(rez)




