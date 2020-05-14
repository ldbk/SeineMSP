setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos f - Traits benthos f.csv", sep=',')

benthos<- benthos %>% dplyr::select(-c(BiogeographicRange, biozone, envpos, feedingmethod, DepthShallow, DepthDeep))
benthos<- benthos[rowSums(is.na(benthos[,]))<4,]
names(benthos)[1]<- "Species"


# Imputation
benthos<- mice::mice(benthos, m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
benthos<- mice::complete(benthos)


# Categories
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

pipob<- data.frame(benthos[,-1])
row.names(pipob)<- benthos$Species


# ACM
rezb<- MCA(pipob, ncp=999, method="Burt") 


# Classification
arbreb<- cluster::agnes(rezb$ind$coord, method="flexible", par.method=1) 
plot(arbreb, which=2, hang=-1)
rect.hclust(arbreb, k=4)
groupeb<- cutree(arbreb, k=4)

benthos<- benthos %>% mutate(Cluster= groupeb)
{
benthos$Cluster<- sub("1", "5", benthos$Cluster)
benthos$Cluster<- sub("2", "6", benthos$Cluster)
benthos$Cluster<- sub("3", "7", benthos$Cluster)
benthos$Cluster<- sub("4", "8", benthos$Cluster)
}

save(benthos, file= "C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/BenthosClusters.RData")





