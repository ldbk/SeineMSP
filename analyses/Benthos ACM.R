setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos f - Traits benthos f.csv", sep=',')

benthos<- benthos %>% select(-c(BiogeographicRange, biozone, envpos, feedingmethod, DepthShallow, DepthDeep))
benthos<- benthos[rowSums(is.na(benthos[,]))<4,]


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
Tab <- Tab[, c(9, 1, 2, 3, 4, 5, 6, 7, 8)]

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
<<<<<<< HEAD
=======


>>>>>>> 355daf5e7fee41680f407f34f1531758570f67dc




