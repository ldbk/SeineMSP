library(dplyr)
library(FactoMineR)
library(missMDA)
library(cluster)


{
  load(file="data/satellite/Detritus/TabDetfin.Rdata")
  load(file="data/satellite/Primary production/TabPPfin.Rdata")
  load(file="data/satellite/sst/Tabsstfin.Rdata")
  load(file="data/satellite/chl/Tabchlfin.Rdata")
  load(file="data/satellite/Turbidity/TabTurbfin.Rdata")
  load(file="data/satellite/Salinity/TabSalfin.Rdata")
  load(file="data/satellite/Particles/TabPartfin.Rdata")
  load(file="data/satellite/O2/TabO2fin.Rdata")
}

{
  names(TabDetfin)[1]<- "Det"
  names(TabPPfin)[1]<- "PP"
  names(Tabsstfin)[1]<- "sst"
  names(Tabchlfin)[3]<- "chl"
  names(TabTurbfin)[1]<- "Turb"
  names(TabSalfin)[1]<- "Sal"
  names(TabPartfin)[1]<- "part"
  names(TabO2fin)[1]<- "O2"
}


# Full join

fj1<- full_join(TabTurbfin, TabDetfin, by=c("x", "y"))
fj2<- full_join(fj1, TabPartfin, by=c("x", "y"))
fj3<- full_join(fj2, TabO2fin, by=c("x", "y"))
fj4<- full_join(fj3, TabPPfin, by=c("x", "y"))
fj5<- full_join(fj4, TabSalfin, by=c("x", "y"))
fj6<- full_join(fj5, Tabsstfin, by=c("x", "y"))
fj7<- full_join(fj6, Tabchlfin, by=c("x", "y"))







# Suite pas possible pour l'instant




metafj7<- fj7 %>% select(x, y)

fj8<- fj7 %>% select(-c(x, y))

# ACP
      ## First the number of components has to be chosen

nb<- estim_ncpPCA(fj8)# nb=1
      # Imputation

res.imput<- imputePCA(fj8, ncp=1)

      # PCA
res.pca<- PCA(res.imput$completeObs)

tree<- agnes(res.pca$ind$coord, method="ward", par.method=1)

arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
plot(arbre, which=2, hang=-1)
rect.hclust(arbre, k=4)

group4<- cutree(arbre, k=4) #4 clusters
  
  
  
  
  
  
  


res.pca<- PCA(fj7)


traitbenthos1<- mice::mice(traitbenthos, m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
traitbenthos<-mice::complete(traitbenthos1)




