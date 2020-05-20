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

#write.csv(spp0,file="spp0.csv")

#match Beukhof with spp list
trait<-readxl::read_excel("../data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")%>%
	distinct()
trait22<-trait%>%filter(LME==22)
trait22gen<-trait%>%filter(LME==22&is.na(species))
trait22fam<-trait%>%filter(LME==22&is.na(genus))
trait24<-trait%>%filter(LME==24)
trait24gen<-trait%>%filter(LME==24&is.na(species))
trait24fam<-trait%>%filter(LME==24&is.na(genus))

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
fam22<-left_join(spptmp,trait22fam,by=c("genus"="family"))%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,fam22)
fam24<-left_join(spptmp,trait24fam,by=c("genus"="family"))%>%filter(is.finite(LME))
spptmp<-anti_join(spptmp,fam24)
#match spptmp with BENTIC: 1 pass
#some manual correction to match BENTIC available species (because genus
#matching is not available
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Maja brachydactyla","squinado",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Pagurus prideaux","prideauxi",species))
spptmp<-spptmp%>%mutate(genus=ifelse(spp=="Mimachlamys varia","Chlamys",genus))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Macropodia rostrata","spp",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Galathea","spp",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Pagurus cuanensis","bernhardus",species))
spptmp<-spptmp%>%mutate(species=ifelse(spp=="Hippocampus","hippocampus",species))
spptmp<-spptmp%>%mutate(genus=ifelse(is.na(genus),"",genus))%>%
	mutate(spp2=paste(genus,species))
				      
write.csv(spptmp%>%transmute(spp2,spp),file="spptmp.csv",row.names=F)

benthos1<-read.csv("BIOTIC.csv",sep=",",header=T)
spptmp<- anti_join(spptmp,benthos1,by=c("spp2"="BioticID"))
#View(spptmp)

#match spptmp genus with BENTIC: 2 pass
#write.csv(spptmp%>%transmute(genus=paste(genus,genus),spp),file="spptmp.csv")

#benthos<- read.csv2("../data/Traits benthos (BIOTIC).csv")
#sppben<-benthos%>%transmute(spp=SpeciesName)%>%distinct()
#spp


sumrez<-data.frame(nbinit=nrow(spp0),
		   nbspp22=nrow(spp22),
		   nbspp24=nrow(spp24),
		   nbgen22=nrow(gen22),
		   nbgen24=nrow(gen24),
		   nbfam22=nrow(fam22),
		   nbfam24=nrow(fam24),
		   nbbentho1=nrow(benthos1)
		   )
sumrez$remain<-sumrez$nbinit-sum(sumrez[2:length(sumrez)])
print(sumrez)
