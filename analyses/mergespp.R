library(dplyr)
#test join with datras data
#name J2
load("../data/J2Datras.RData")
spp0<-J2%>%transmute(spp=ScientificName_WoRMS)%>%distinct()%>%
	tidyr::separate(spp,sep=" ",into=c("genus","species","species2"),remove=FALSE)%>%
	arrange(spp)
#some manual correction
#if two spp take the last one (2 cases)
spp0<-spp0%>%mutate(species=ifelse(!is.na(species2),species2,species))
#if salmo trutta -> Salmo trutta trutta to match Beuhkof name
spp0<-spp0%>%mutate(species=ifelse(species=="trutta","trutta trutta",species))
#if Psetta maxima -> Scophthalmus maximus to match Beuhkof name
spp0<-spp0%>%mutate(genus=ifelse(genus=="Psetta","Scophthalmus",genus))
spp0<-spp0%>%mutate(species=ifelse(genus=="Psetta","maximus",species))
#match Beukhof with spp list
trait<-readxl::read_excel("../data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")%>%
	distinct()
trait22<-trait%>%filter(LME==22)
trait22gen<-trait%>%filter(LME==22&is.na(species))%>%select(-species)
trait22fam<-trait%>%filter(LME==22&is.na(genus))%>%mutate(genus=family)%>%select(-species)
trait24<-trait%>%filter(LME==24)
trait24gen<-trait%>%filter(LME==24&is.na(species))%>%select(-species)
trait24fam<-trait%>%filter(LME==24&is.na(genus))%>%mutate(genus=family)%>%select(-species)

#successive join
#spp ok in LME 22
spp22<-left_join(spp0,trait22,by=c("genus","species"))%>%filter(is.finite(LME))
spptmp<-anti_join(spp0,spp22)
#spp ok in LME 24
spp24<-left_join(spptmp,trait24,by=c("genus","species"))%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,spp24)
#genus in 22 & 24
gen22<-left_join(spptmp,trait22gen,by=c("genus"))%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,gen22)
gen24<-left_join(spptmp,trait24gen,by=c("genus"))%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,gen22)
#family in 22 & 24
fam22<-left_join(spptmp,trait22fam,by=c("genus"="genus"),keep=T)%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,fam22)
fam24<-left_join(spptmp,trait24fam,by=c("genus"="genus"),keep=T)%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,fam24)
#identify cephalo using a file generated manually
listceph<-c("Alloteuthis","Eledone cirrhosa","Loligo","Loligo forbesi","Loligo vulgaris","Sepia officinalis","Sepiola")
listceph<-data.frame(spp=listceph,ceph=1)
spptmp<-anti_join(spptmp,listceph)

#match spptmp with BENTIC: 1 pass
#some manual correction to match BENTIC available species (because genus
#matching is not available
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Maja brachydactyla","squinado",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Pagurus prideaux","prideauxi",species))
spptmp<-spptmp%>%mutate(genus=ifelse(spp=="Mimachlamys varia","Chlamys",genus))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Macropodia rostrata","spp.",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Galathea","spp.",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Pagurus cuanensis","bernhardus",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Hippocampus","hippocampus",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Palaemon serratus","spp.",species))
#for species with NA species put spp.
spptmp<-spptmp%>%mutate(species=ifelse(is.na(species),"spp.",species))%>%
	mutate(spp2=paste(genus,species))
write.csv(spptmp%>%transmute(spp2,spp),file="spptmp.csv",row.names=F)

#use spptmp.csv (after conversion in excel file) to get BIOTIC info from the website then read it
benthos<-read.table("BIOTIC.csv",sep=";",header=T,strip.white=T,fill=T)
benthos<-left_join(benthos,spptmp,by=c("SpeciesName"="spp2"))

#species with no match:
spptmp<- anti_join(spptmp,benthos,by=c("spp"="spp"))


#a summary
sumrez<-data.frame(nbinit=nrow(spp0),
		   nbspp22=nrow(spp22),
		   nbspp24=nrow(spp24),
		   nbgen22=nrow(gen22),
		   nbgen24=nrow(gen24),
		   nbfam22=nrow(fam22),
		   nbfam24=nrow(fam24),
		   nbceph=nrow(listceph),
		   nbbenthos=nrow(benthos)
		   )
sumrez$remain<-sumrez$nbinit-sum(sumrez[2:length(sumrez)])
print(sumrez)

#final file
traitfish<-rbind(spp22,spp24,gen22,fam22)
traitbenthos<-benthos
traitceph<-listceph



