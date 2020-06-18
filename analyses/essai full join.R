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

########################
load(file="data/satellite/Detritus/TabDetfin.Rdata")
load(file="data/satellite/Primary production/TabPPfin.Rdata")
load(file="data/satellite/sst/Tabsstfin.Rdata")
load(file="data/satellite/chl/Tabchlfin.Rdata")
load(file="data/satellite/Turbidity/TabTurbfin.Rdata")
load(file="data/satellite/Salinity/TabSalfin.Rdata")
load(file="data/satellite/Particles/TabPartfin.Rdata")
load(file="data/satellite/O2/TabO2fin.Rdata")

load("data/satellite/Detritus/Det_polygons.Rdata")
load("data/satellite/Primary production/PP_polygons.Rdata")
load("data/satellite/sst/sst_polygons.Rdata")
load("data/satellite/chl/chl_polygons.Rdata")
load("data/satellite/Turbidity/Turb_polygons.Rdata")
load("data/satellite/Salinity/Sal_polygons.Rdata")
load("data/satellite/Particles/Part_polygons.Rdata")
load("data/satellite/O2/O2_polygons.Rdata")

points <- data.frame(Long=runif(500, min(TabSalfin$x), max(TabSalfin$x)),
                     Lat=runif(500, min(TabSalfin$y), max(TabSalfin$y)))


x_range <- as.numeric(c(-99, -80))  # min/max longitude of the interpolation area
y_range <- as.numeric(c(24, 32))  # min/max latitude of the interpolation area

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(TabSalfin$x),
                           to = max(TabSalfin$x), 
                           by = 0.01),
                   Lat = seq(from =min(TabSalfin$y),
                           to = max(TabSalfin$y), 
                           by = 0.01))  # expand points to grid

points <- structure(list(grd$Long, grd$Lat), .Names = c("Long", "Lat"), class = "data.frame", row.names = c(NA, dim(grd)[1]))
spdf <- SpatialPointsDataFrame(coords = points, data = points)

noms <- c("polChl","polDet","polO2","polPart","polPP","polSal","polSST","polTurb")
t <- 0
for (i in list(polChl,polDet,polO2,polPart,polPP,polSal,polSST,polTurb)){
  t <- t+1
  pipo <- sp::over(spdf, i)
  pipo$Clust2 <- paste0(rep(substr(noms[t],4,nchar(noms[t])),dim(pipo)[1]), pipo$Clust)
  names(pipo) <- c("Clust",substr(noms[t],4,nchar(noms[t])))
  grd <- cbind(grd,pipo)
}
dim(na.omit(grd))

grd2 <- na.omit(grd)
plot(grd2[,c(1,2)])
grd2 <- grd2[, !duplicated(colnames(grd2))]
grd2 <- grd2[,-3]

rez<- MCA(grd2[,-c(1,2)], ncp=999, method="Burt", graph=F)
plt1<-plotellipses(rez, axes=c(1,2))
plt2<-plotellipses(rez, axes=c(1,3))

arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
plot(arbre, which=2,hang=-1)
rect.hclust(arbre, k=8)

group8<- cutree(arbre,k=8) #4 clusters

tata <- cbind(grd2[,c(1,2)],Clust=factor(group8))

ggplot(tata)+geom_tile(aes(x=Long,y=Lat,fill=Clust))+geom_polygon(aes(x=Long,y=Lat,group=group))
