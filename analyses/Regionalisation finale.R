library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
library(RColorBrewer)


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

load("data/PolyCut.Rdata")

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(TabSalfin$x),                                                 
                              to = max(TabSalfin$x),                                                    
                              by = 0.01),
                   Lat = seq(from =min(TabSalfin$y),
                             to = max(TabSalfin$y), 
                             by = 0.01))  

points <- structure(list(grd$Long, grd$Lat), .Names = c("Long", "Lat"), 
                    class = "data.frame", row.names = c(NA, dim(grd)[1]))                               
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

rect.hclust(arbre, k=(5))

group6<- cutree(arbre,k=5)

tata <- cbind(grd2[,c(1,2)],Clust=factor(group6))

Allparam<- ggplot(tata)+
  geom_tile(aes(x=Long,y=Lat,fill=Clust))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Final bioregionalization")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

Allparam2<- Allparam +
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 10))


ggsave(plot= Allparam2, filename="All.jpeg", path="results/satellite/zones", width = 13, height = 8)







