setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos f - Traits benthos f.csv", sep=',')

benthos<- benthos %>% dplyr::select(-c(BiogeographicRange, biozone, envpos, feedingmethod, DepthShallow, DepthDeep))
benthos<- benthos[rowSums(is.na(benthos[,]))<4,]
{
benthos$MaturityAge<- as.character(benthos$MaturityAge)
benthos$MaturityAge<- sub("< 1 year", "[0;2[", benthos$MaturityAge)
benthos$MaturityAge<- sub("1 year", "[0;2[", benthos$MaturityAge)
benthos$MaturityAge<- sub("1-2 years", "[0;2[", benthos$MaturityAge)
benthos$MaturityAge<- sub("2-3 years", "[2;6[", benthos$MaturityAge)
benthos$MaturityAge<- sub("3-5 years", "[2;6[", benthos$MaturityAge)
benthos$MaturityAge<- sub("6-10 years", "[6;10[", benthos$MaturityAge)
}

{
benthos$LifeSpan<- as.character(benthos$LifeSpan)
benthos$LifeSpan<- sub("1-2 years", "[0;6[",benthos$LifeSpan)
benthos$LifeSpan<- sub("3-5 years", "[0;6[",benthos$LifeSpan)
benthos$LifeSpan<- sub("6-10 years", "[6;21[",benthos$LifeSpan)
benthos$LifeSpan<- sub("11-20 years", "[6;21[",benthos$LifeSpan)
benthos$LifeSpan<- sub("21-50 years", "[21;50[",benthos$LifeSpan)
}



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
#nb<- estim_ncpMCA(benthos)

rez<- imputeMCA(benthos, ncp =2)
rez1<- MCA(benthos, tab.disj=rez$tab.disj)
clustering<- HCPC(rez1)


# Description des cluster par Variables et/ou modalités - ancien

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







