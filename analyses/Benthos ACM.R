setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)
library("FactoInvestigate")

benthos<- read.csv("Traits benthos f - Traits benthos f.csv", sep=',')


# TRAITEMENT TRAITS QUANTITATIFS (quantiles)

catquant<- function(a, value){ # a<- unique(fishtrait$LifeSpan) et value<- c(0, quantile(a, na.rm=T))
  lval<- length(value)
  interval<- paste0("[", round(value[-lval], 3), "," , round(value[-1], 3), "[")
  a1<- interval[findInterval(a, value, all.inside=T)]
  return(a1)
}

benthos$DepthShallow<- catquant(benthos$DepthShallow, quantile(benthos$DepthShallow, na.rm=T))
benthos$DepthDeep<- catquant(benthos$DepthDeep, quantile(benthos$DepthDeep, na.rm=T))

benthos<- benthos %>% select(-c(BiogeographicRange, biozone, envpos, feedingmethod))


# ACM

{
  nbid<- benthos %>%
    summarise_all(n_distinct) %>%
    t()
  nbNA<- function(a){a[a==""]<-NA;sum(is.na(a))}
  nbNA<- benthos %>%
    summarise_all(nbNA) %>%
    t()
  
  summary<- data.frame(nbid=nbid,nbNA=nbNA)
}

benthos<- benthos %>%
  mutate_all(as.factor)%>%
  data.frame()

row.names(benthos)<- benthos[,1] 
benthos<- benthos[,-1]
nb<- estim_ncpMCA(benthos)

rez<- imputeMCA(benthos)
rez1<- MCA(benthos, tab.disj=rez$tab.disj)
clustering<- HCPC(rez1)


# Description des cluster par Variables et/ou modalités

Tab<- clustering$data.clust 
Tab<- Tab %>% mutate(Species= row.names(Tab))
Tab <- Tab[, c(11, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]

{
Cluster1<- Tab %>% filter(clust == 1)
Cluster2<- Tab %>% filter(clust == 2)
Cluster3<- Tab %>% filter(clust == 3)
Cluster4<- Tab %>% filter(clust == 4)
Cluster5<- Tab %>% filter(clust == 5)
Cluster6<- Tab %>% filter(clust == 6)
Cluster7<- Tab %>% filter(clust == 7)
}

clustering$desc.var$category

summaryc1<- as.data.frame(summary(Cluster1))
summaryc2<- as.data.frame(summary(Cluster2))
summaryc3<- as.data.frame(summary(Cluster3))
summaryc4<- as.data.frame(summary(Cluster4))



