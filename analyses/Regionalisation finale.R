library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
library(RColorBrewer)
library(NbClust)
library(fastcluster) # pour hclust

{
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
}

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
grd2<- na.omit(grd)                                                                                           
plot(grd2[,c(1,2)])
grd2<- grd2[, !duplicated(colnames(grd2))]
grd2<- grd2[,-3]                                                                                               


# ACM
rez<- MCA(grd2[,-c(1,2)], ncp=999, method="Burt", graph=F)                                                      
plt1<- plotellipses(rez, axes=c(1,2))
plt2<- plotellipses(rez, axes=c(1,3))


# Classification
arbre<- hclust(dist(rez$ind$coord), method="ward.D2")
plot(arbre, which=2,hang=-1)
#NbClust(rez$ind$coord, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
# According to the majority rule, the best number of clusters is  10 (5 indicateurs, puis 4 indicateurs pour 6, 3 ou 2 clusters)
rect.hclust(arbre, k=6)
groups<- cutree(arbre, k=6)

tata<- cbind(grd2[,c(1,2)],Clust=factor(groups))
save(tata, file="results/satellite/Coordzones.Rdata")

ggplot(tata)+
  geom_tile(aes(x=Long,y=Lat,fill= as.numeric(Clust)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr")) +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 10))



# Découpage tata en fonction de polycut

load("data/res.Rdata")

  # create SpatialPointsDataFrame
tataras<- tata
coordinates(tataras)<- ~ Long + Lat
  # coerce to SpatialPixelsDataFrame
gridded(tataras) <- TRUE
  # coerce to raster
rastertata<- raster(tataras)
rastertata
plot(rastertata, col=brewer.pal(n = 6, name = "YlOrBr"), xlab="Longitude", ylab="Latitude")

rastertata2<- mask(rastertata, res)
plot(rastertata2, col=brewer.pal(n = 6, name = "YlOrBr"), main="Après mask", xlab="Longitude", ylab="Latitude")



# Conversion raster - tableau
fortify.Raster <- function(rastertata2, maxPixel = 1000000) {
  
  if (ncell(rastertata2) > maxPixel) {
    x <- sampleRegular(rastertata2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rastertata2, seq_len(ncell(rastertata2)))
  out <- rastertata2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

tatatab<- fortify(rastertata2)
tatatab<- na.omit(tatatab)

allparam<- ggplot(tatatab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  scale_fill_manual(values = c("#FFFFD4", "#FEE391", "#FEC44F", "#FE9929", "#D95F0E", "#993404"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 10))


ggsave(plot= allparam, filename="All.jpeg", path="results/satellite/zones", width = 13, height = 8)







