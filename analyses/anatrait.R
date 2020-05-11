library(dplyr)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(googlesheets4)

#library(mice)
#read the orginal trait data
fishtraitlocal<-read.csv(file="../data/TabespecesDATRAS.csv", sep=",")

#link trait matrix
#https://docs.google.com/spreadsheets/d/1auNXLqljHhXfPZpL63-og9vwUcvB5mk4QGYJTsPqzxA/edit#gid=0
fishtraitBeuhkhof<-readxl::read_excel("../data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")
fishtrait1<-fishtraitBeuhkhof%>%mutate(Species=paste(genus,species))%>%
	filter(Species%in%fishtraitlocal$Species)%>%
	filter(LME %in% c(22,23,24))
length(unique(fishtrait1$Species))
#11 species missing for LME 22 (North Sea)
#try to get info from other LME : 24 for Celtic seas
missingtrait1<-fishtraitlocal%>%filter(!Species%in%fishtrait1$Species)
fishtrait2<-fishtraitBeuhkhof%>%mutate(Species=paste(genus,species))%>%
	filter(Species%in%missingtrait1$Species)%>%
	filter(LME==24)
length(unique(fishtrait2$Species))
#bind the two files and check missing species
fishtrait3<-rbind(fishtrait1,fishtrait2)%>%distinct()
length(unique(fishtrait3$Species))
#3 species missing
missingtrait3<-fishtraitlocal%>%filter(!Species%in%fishtrait3$Species)
missingtrait3%>%pull(Species)
#ok : let it go and work only on 84 species

#summarise the new trait tables
fishtraitnew<-fishtrait3%>%select(Species,habitat,feeding.mode,tl,
				  #body.shape,offspring.size,
				  #spawning.type,
				  age.maturity,#fecundity,
				  growth.coefficient,length.max,age.max)
#add IUCN status
fishtraitnew<-left_join(fishtraitnew,fishtraitlocal%>%select(Species,IUCN.status))
#explo data
diagmiss<-function(fishtrait){
	nbid<-fishtrait%>%summarise_all(n_distinct)%>%t()
	nbNA<-function(a){a[a==""]<-NA;sum(is.na(a))}
	nbNA<-fishtrait%>%summarise_all(nbNA)%>%t()
	return(data.frame(nbid=nbid,nbNA=nbNA))
}
diagmiss(fishtraitnew)
#2 missing value in age.maturity 
#imputation
fishtraitnew0<-mice::mice(fishtraitnew,m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
fishtraitnew<-mice::complete(fishtraitnew0)
diagmiss(fishtraitnew)

#now categorize stuff
#categorize numerical values 
catvar<-function(a,value){
  	value<-c(0,as.vector(value))
	print(value)
	lval<-length(value)
	interval<-paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
	a1<-interval[findInterval(a,value,all.inside=T)]
	return(a1)
}
#a quick check for the function
table(fishtraitnew$tl,catvar(fishtraitnew$tl,c(1:5)))
fishtraitnew$tl<-catvar(fishtraitnew$tl,c(1:5))
#table(fishtraitnew$offspring.size,catvar(fishtraitnew$offspring.size,c(4,400)))
#fishtraitnew$offspring.size<-catvar(fishtraitnew$offspring.size,c(4,400))
table(fishtraitnew$age.maturity,catvar(fishtraitnew$age.maturity,c(2,3,4,5,14)))
fishtraitnew$age.maturity<-catvar(fishtraitnew$age.maturity,c(2,3,4,5,14))
#table(fishtraitnew$fecundity,catvar(fishtraitnew$fecundity,c(1000,10000,100000,1000000)))
#fishtraitnew$fecundity<-catvar(fishtraitnew$fecundity,c(1000,10000,100000,1000000))
table(fishtraitnew$growth.coefficient,catvar(fishtraitnew$growth.coefficient,c(.1,.2,.3,.4,.5,2)))
fishtraitnew$growth.coefficient<-catvar(fishtraitnew$growth.coefficient,c(.1,.2,.3,.4,.5,2))
table(fishtraitnew$length.max,catvar(fishtraitnew$length.max,c(50,100,200)))
fishtraitnew$length.max<-catvar(fishtraitnew$length.max,c(50,100,200))
table(fishtraitnew$age.max,catvar(fishtraitnew$age.max,c(5,10,20,60)))
fishtraitnew$age.max<-catvar(fishtraitnew$age.max,c(5,10,20,60))


#MCA + classif
pipo<-data.frame(fishtraitnew[,-1])
#pipo<-data.frame(fishtraitnew[,c(1,2,3,4,5,7,8)])
row.names(pipo) <- fishtraitnew$Species
#correspodance anlysis on Burt table
rez<-MCA(pipo,ncp=999,method="Burt")
plotellipses(rez,axes=c(1,2))
plotellipses(rez,axes=c(1,3))

#classif
arbre<-cluster::agnes(rez$ind$coord,method="flexible")
plot(arbre,which=2,hang=-1)
rect.hclust(arbre,k=4)
groupe<-cutree(arbre,k=4)
#optimal number of cluster
#funcluster<-function(a,n){list(cluster=cutree(cluster::agnes(a,method="ward"),k=n))}
#funcluster(rez$ind$coord,2)
#rezopti<-cluster::clusGap(rez$ind$coord,funcluster,K.max=6,B=1000)
#plot(rezopti)
#cluster::maxSE(rezopti$Tab[,3],rezopti$Tab[,4],method="Tibs2001SEmax")
#plot(as.hclust(arbre),hang=-1)#,which=2)
#rect.hclust(arbre,k=4)

#how classif is going in 2D
fviz_mca_ind(rez,repel=T,habillage=as.factor(groupe),addEllipses=F,axes=c(1,2))
fviz_mca_ind(rez,repel=T,habillage=as.factor(groupe),addEllipses=F,axes=c(1,3))
fviz_mca_ind(rez,repel=T,habillage=as.factor(groupe),addEllipses=F,axes=c(2,3))

stop()
#then Hennig, C. (2017) Cluster validation by measurement of clustering
#characteristics relevant to the user. In C. H. Skiadas (ed.) Proceedings of
#ASMDA 2017, 501-520, https://arxiv.org/abs/1703.09282
# using clusterbenchmark from fpc

rez2<-HCPC(rez)
matfin<-rez2$data.clust%>%mutate(clust2=cutree(arbre,k=3),spp=row.names(rez2$data.clust))%>%arrange(clust2)
View(matfin)

#remove Depth var
fishtrait<-fishtrait[,!grepl("Depth",names(fishtrait))]
fishtrait<-fishtrait[,!grepl("Distrib",names(fishtrait))]
diagmiss(fishtrait)

#numeric in numeric
fishtrait<-fishtrait%>%mutate_at(vars("LifeSpan","FoodTroph","MaturityAge"),as.numeric)
#factor conversion
fishtrait<-fishtrait%>%mutate_at(vars("DemersPelag","Migratory","FertilizationType","SpawningFreq"),as.factor)
#ordered factor for UICN
listIUCN<-c("DataDeficient","Least Concern","Near Threatened","Vulnerable","Endangered","Critically Endangered")
fishtrait$IUCN.status<-factor(fishtrait$IUCN.status,levels=listIUCN,ordered=T)

diagmiss(fishtrait)


fishtrait<-fishtrait3
#categorize numerical values 
catquant<-function(a,value){
  	value<-c(0,as.vector(value))
	print(value)
	lval<-length(value)
	interval<-paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
	a1<-interval[findInterval(a,value,all.inside=T)]
	return(a1)
}
#a quick check for the function
table(fishtrait$LifeSpan,catquant(fishtrait$LifeSpan,c(1,2,3,10,90)))
fishtrait$LifeSpan<-catquant(fishtrait$LifeSpan,c(1,2,3,10,90))
table(fishtrait$FoodTroph,catquant(fishtrait$FoodTrop,c(1:5)))
fishtrait$FoodTroph<-catquant(fishtrait$FoodTroph,c(1:5))
table(fishtrait$MaturityAge,catquant(fishtrait$MaturityAge,c(1,2,4,14)))
fishtrait$MaturityAge<-catquant(fishtrait$MaturityAge,c(1,2,4,14))

#explo data
diagmiss(fishtrait)

#selection of some traits where information is consitent (let's say...)
fishtrait<-fishtrait%>%select(Species,LifeSpan,DemersPelag,FoodTroph,Migratory,SpawningFreq,FoodTroph,MaturityAge,IUCN.status)

pipo<-fishtrait[,-1]
row.names(pipo)<-fishtrait[,1]
rez<-MCA(pipo,ncp=800)
rez2<-HCPC(rez)
matfin<-rez2$data.clust%>%mutate(spp=row.names(rez2$data.clust))%>%arrange(clust)
View(matfin)

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




