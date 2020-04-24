library(dplyr)
library(FactoMineR)
library(missMDA)

#read the data
fishtrait<-read.csv("../results/Traits poissons f.csv",stringsAsFactors=F)
#remove long lat info X and depth info (for the time being)
fishtrait<-fishtrait%>%select_if(!grepl("itude",names(.)))%>%select(-X)%>%select_if(!grepl("Depth",names(.)))
#group by Species the trait
#identify unique fishtrait by species (to simplify input)
#function to identify unique occurence or not
is_unique<-function(x){all(x==1)}
is_not_unique<-function(x){any(x!=1)}
#traits with one occurence by species : unique values
fishtraitok<-fishtrait%>%group_by(Species)%>%summarise_all(n_distinct)%>%ungroup()%>%
	select_if(is_unique)
fishtrait1<-fishtrait%>%select(c("Species",names(fishtraitok)))%>%group_by(Species)%>%
	summarise_all(unique,na.rm=T)
#multiple occurence of trait by species : median for numerical value, unique with paste and collapse for categories
fishtraitko<-fishtrait%>%group_by(Species)%>%summarise_all(n_distinct)%>%ungroup()%>%
	select_if(is_not_unique)
#2 treatments : one for numeric one for cat
fishtrait2<-fishtrait%>%select(c("Species",names(fishtraitko)))%>%group_by(Species)%>%
	summarise_if(is.numeric,median,na.rm=T)
pasteunique<-function(a){paste(unique(sort(na.omit(a))),collapse=":")}
fishtrait3<-fishtrait%>%select(c("Species",names(fishtraitko)))%>%group_by(Species)%>%
	summarise_if(is.character,pasteunique)
#rebuild the trait data.frame
fishtrait<-full_join(full_join(fishtrait1,fishtrait2),fishtrait3)
#categorize numerical values using quantile
#a function to categorize numerical value in quantile interval
catquant<-function(a,value){
	#a<-unique(fishtrait$LifeSpan)
	#value<-c(0,quantile(a,na.rm=T))
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

#due to algorithms problem in imputation select less parameter
#nb of distinct value by variable
nbid<-fishtrait%>%summarise_all(n_distinct)%>%t()
nbNA<-function(a){a[a==""]<-NA;sum(is.na(a))}
nbNA<-fishtrait%>%summarise_all(nbNA)%>%t()
data.frame(nbid=nbid,nbNA=nbNA)
#ok large number of missing value 
#selection of some traits where information is consitent (let's say...)
fishtrait<-fishtrait%>%select(Species,LifeSpan,DemersPelag,Migratory,FoodTroph)

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




