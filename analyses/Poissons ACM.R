setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)

fishtrait<-read.csv("FishTraits - traits.csv", stringsAsFactors=F)

# Nettoyage tableau
fishtrait<- fishtrait %>% select(-c(SpawningGround, Spawningarea, MaturityLocality, Distrib))

# TRAITEMENT TRAITS QUANTITATIFS (en utilisant les quantiles)

catquant<- function(a, value){ # a<- unique(fishtrait$LifeSpan) et value<- c(0, quantile(a, na.rm=T))
  lval<- length(value)
  interval<- paste0("[", round(value[-lval], 3), "," , round(value[-1], 3), "[")
  a1<- interval[findInterval(a, value, all.inside=T)]
  return(a1)
}

table(fishtrait$MaturityAge, catquant(fishtrait$MaturityAge, quantile(fishtrait$MaturityAge, na.rm=T)))

fishtrait$LifeSpan<- catquant(fishtrait$LifeSpan, quantile(fishtrait$LifeSpan, na.rm=T)) 
fishtrait$FoodTroph<- catquant(fishtrait$FoodTroph, quantile(fishtrait$FoodTroph, na.rm=T))
fishtrait$MaturityAge<- catquant(fishtrait$MaturityAge, quantile(fishtrait$MaturityAge, na.rm=T))
fishtrait$DepthShallow<- catquant(fishtrait$DepthShallow, quantile(fishtrait$DepthShallow, na.rm=T))
fishtrait$DepthDeep<- catquant(fishtrait$DepthDeep, quantile(fishtrait$DepthDeep, na.rm=T))



# ACM

{
  nbid<- fishtrait %>%
    summarise_all(n_distinct) %>%
    t()
  nbNA<- function(a){a[a==""]<-NA;sum(is.na(a))}
  nbNA<- fishtrait %>%
    summarise_all(nbNA) %>%
    t()
  data.frame(nbid=nbid,nbNA=nbNA)
  
  summary<- data.frame(nbid=nbid,nbNA=nbNA)
}



pipo<- fishtrait %>% 
  mutate_all(as.factor) %>% 
  data.frame()

row.names(pipo)<- pipo[,1] 
pipo<- pipo[,-1]

nb<- estim_ncpMCA(pipo) 
rez<- imputeMCA(pipo, ncp=4) 
rez<- MCA(pipo, tab.disj=rez$tab.disj.comp) 
HCPC(rez) 


