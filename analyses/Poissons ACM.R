setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)

fishtrait<- read.csv("FishTraits - traits.csv", stringsAsFactors=F)

# Nettoyage tableau
fishtrait<- fishtrait %>% select(-Distrib)

# TRAITEMENT TRAITS QUANTITATIFS (en utilisant les quantiles)

catquant<- function(a, value){ # a<- unique(fishtrait$LifeSpan) et value<- c(0, quantile(a, na.rm=T))
  lval<- length(value)
  interval<- paste0("[", round(value[-lval], 3), "," , round(value[-1], 3), "[")
  a1<- interval[findInterval(a, value, all.inside=T)]
  return(a1)
}

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



fishtrait<- fishtrait %>% 
  mutate_all(as.factor) %>% 
  data.frame()

row.names(fishtrait)<- fishtrait[,1] 
fishtrait<- fishtrait[,-1]

nb<- estim_ncpMCA(fishtrait) 
rez<- imputeMCA(fishtrait, ncp=4)
rez$tab.disj # verif fonctionnement imputation
rez1<- MCA(fishtrait, tab.disj=rez$tab.disj) 
clustering<- HCPC(rez1) 


# Description des cluster par Variables et/ou modalités

Tab<- clustering$data.clust 
Tab<- Tab %>% mutate(Species= row.names(Tab))
Tab <- Tab[, c(12, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]

{
  Cluster1<- Tab %>% filter(clust == 1)
  Cluster2<- Tab %>% filter(clust == 2)
  Cluster3<- Tab %>% filter(clust == 3)
  Cluster4<- Tab %>% filter(clust == 4)
  Cluster5<- Tab %>% filter(clust == 5)
  Cluster6<- Tab %>% filter(clust == 6)
}

clustering$desc.var$category


summaryc1<- as.data.frame(summary(Cluster1))
summaryc2<- as.data.frame(summary(Cluster2))
summaryc3<- as.data.frame(summary(Cluster3))
summaryc4<- as.data.frame(summary(Cluster4))
summaryc5<- as.data.frame(summary(Cluster5))
summaryc6<- as.data.frame(summary(Cluster6))











