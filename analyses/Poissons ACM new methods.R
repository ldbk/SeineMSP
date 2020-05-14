setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(googlesheets4)
library(fpc)
#library(mice)


# Read the species list
fishtraitlocal<- read.csv("TabespecesDATRAS.csv")
names(fishtraitlocal)[2]<- "Species"


# Pangea traits
fishtraitBeuhkhof<- readxl::read_excel("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")
fishtrait1<- fishtraitBeuhkhof %>%
  mutate(Species=paste(genus, species)) %>%
  filter(Species %in% fishtraitlocal$Species)%>%
  filter(LME==22) # North Sea
length(unique(fishtrait1$Species))

#11 species missing for this LME
missingtrait1<- fishtraitlocal %>%
  filter(!Species %in% fishtrait1$Species)

fishtrait2<- fishtraitBeuhkhof %>% 
  mutate(Species=paste(genus, species)) %>%
  filter(Species %in% missingtrait1$Species) %>%
  filter(LME==24) # Celtic seas
length(unique(fishtrait2$Species))


# Bind the two files and check missing species
fishtrait3<- rbind(fishtrait1,fishtrait2) %>% distinct()
length(unique(fishtrait3$Species))

# 3 species missing
missingtrait3<- fishtraitlocal %>% filter(!Species %in% fishtrait3$Species)
missingtrait3 %>% pull(Species)
#ok : let it go and work only on 84 species


# Summarise the new trait table
fishtraitnew<- fishtrait3 %>% select(Species,habitat,feeding.mode,tl,
                                  #body.shape,offspring.size,
                                  #spawning.type,
                                  age.maturity,#fecundity,
                                  growth.coefficient,length.max,age.max)


# Add IUCN status
IUCN<- read_sheet("https://docs.google.com/spreadsheets/d/1auNXLqljHhXfPZpL63-og9vwUcvB5mk4QGYJTsPqzxA/edit#gid=0",sheet=1,na="NA")
names(IUCN)[12]<- "IUCN.status"
fishtraitnew<- left_join(fishtraitnew,IUCN %>% select(Species,IUCN.status))


# Explo data
Summary<-function(fishtrait){
  nbid<-fishtrait%>%summarise_all(n_distinct)%>%t()
  nbNA<-function(a){a[a==""]<-NA;sum(is.na(a))}
  nbNA<-fishtrait%>%summarise_all(nbNA)%>%t()
  return(data.frame(nbid=nbid,nbNA=nbNA))
}


# Imputation
fishtraitnew0<- mice::mice(fishtraitnew,m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
fishtraitnew<- mice::complete(fishtraitnew0)


# Traitement variables quantitatives
catvar<- function(a,value){
  value<- c(0,as.vector(value))
  print(value)
  lval<- length(value)
  interval<- paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
  a1<- interval[findInterval(a,value,all.inside=T)]
  return(a1)
}

fishtraitnew$tl<- catvar(fishtraitnew$tl,c(1:5))
fishtraitnew$age.maturity<- catvar(fishtraitnew$age.maturity,c(2,3,4,5,14))
fishtraitnew$growth.coefficient<- catvar(fishtraitnew$growth.coefficient,c(.1,.2,.3,.4,.5,2))
fishtraitnew$length.max<- catvar(fishtraitnew$length.max,c(50,100,200))
fishtraitnew$age.max<- catvar(fishtraitnew$age.max,c(5,10,20,60))


# MCA 
pipo<- data.frame(fishtraitnew[,-1])
#pipo<-data.frame(fishtraitnew[,c(1,2,3,4,5,7,8)])
row.names(pipo)<- fishtraitnew$Species
rez<- MCA(pipo, ncp=999, method="Burt") 
plotellipses(rez, axes=c(1,2))
plotellipses(rez, axes=c(1,3)) 


# Classification
arbre<- cluster::agnes(rez$ind$coord, method="flexible",par.method=1) 
plot(arbre, which=2, hang=-1)
rect.hclust(arbre, k=4)
groupe<- cutree(arbre, k=4)

fishtraitnew<- fishtraitnew %>% mutate(Cluster= groupe)

save(fishtraitnew, file= "C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/PoissonsClusters.RData")

















