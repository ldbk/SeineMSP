library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
library(RColorBrewer)
library(fastcluster)
library(NbClust)

{
load(file="data/ICES/Tabfin1.Rdata")
Tabfin1<- titi
names(Tabfin1)[5]<- "Pred1"
Tabfin1<- Tabfin1 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin2.Rdata")
Tabfin2<- titi
names(Tabfin2)[5]<- "Pred2"
Tabfin2<- Tabfin2 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin3.Rdata")
Tabfin3<- titi
names(Tabfin3)[5]<- "Pred3"
Tabfin3<- Tabfin3 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin4.Rdata")
Tabfin4<- titi
names(Tabfin4)[5]<- "Pred4"
Tabfin4<- Tabfin4 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin5.Rdata")
Tabfin5<- titi
names(Tabfin5)[5]<- "Pred5"
Tabfin5<- Tabfin5 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin6.Rdata")
Tabfin6<- titi
names(Tabfin6)[5]<- "Pred6"
Tabfin6<- Tabfin6 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin7.Rdata")
Tabfin7<- titi
names(Tabfin7)[5]<- "Pred7"
Tabfin7<- Tabfin7 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin8.Rdata")
Tabfin8<- titi
names(Tabfin8)[5]<- "Pred8"
Tabfin8<- Tabfin8 %>% dplyr::select(-Community, -Clust)
load(file="data/ICES/Tabfin9.Rdata")
Tabfin9<- titi
names(Tabfin9)[5]<- "Pred9"
Tabfin9<- Tabfin9 %>% dplyr::select(-Community, -Clust)
}
{
load("results/Communautes bio/Zones/Community1_polygons.Rdata")
pol1<- pol
load("results/Communautes bio/Zones/Community2_polygons.Rdata")
pol2<- pol
load("results/Communautes bio/Zones/Community3_polygons.Rdata")
pol3<- pol
load("results/Communautes bio/Zones/Community4_polygons.Rdata")
pol4<- pol
load("results/Communautes bio/Zones/Community5_polygons.Rdata")
pol5<- pol
load("results/Communautes bio/Zones/Community6_polygons.Rdata")
pol6<- pol
load("results/Communautes bio/Zones/Community7_polygons.Rdata")
pol7<- pol
load("results/Communautes bio/Zones/Community8_polygons.Rdata")
pol8<- pol
load("results/Communautes bio/Zones/Community9_polygons.Rdata")
pol9<- pol
}
load("data/PolyCut.Rdata")



# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(Tabfin1$Longitude),                                                 
                              to = max(Tabfin1$Longitude),                                                    
                              by = 0.01),
                   Lat = seq(from =min(Tabfin1$Latitude),
                             to = max(Tabfin1$Latitude), 
                             by = 0.01))  

points <- structure(list(grd$Long, grd$Lat), .Names = c("Long", "Lat"), 
                    class = "data.frame", row.names = c(NA, dim(grd)[1]))                               
spdf <- SpatialPointsDataFrame(coords = points, data = points)                                          

noms <- c("pol1","pol2","pol3","pol4","pol5","pol6","pol7","pol8", "pol9")
t <- 0
for (i in list(pol1,pol2,pol3,pol4,pol5,pol6,pol7,pol8,pol9)){
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


# ACM
rez<- MCA(grd2[,-c(1,2)], ncp=999, method="Burt", graph=F)                                                      
plt1<- plotellipses(rez, axes=c(1,2))
plt2<- plotellipses(rez, axes=c(1,3))



# Classification
arbre<- hclust(dist(rez$ind$coord), method="ward.D2")
plot(arbre, which=2, hang=-1)
#NbClust(rez$ind$coord, min.nc = 2, max.nc = 10, index="alllong", method = "ward.D2")
rect.hclust(arbre, k=9)
groups<- cutree(arbre, k=9)

tata<- cbind(grd2[,c(1,2)], Clust=factor(groups))
save(tata, file="results/Communautes bio/Zones/Tabttpixel.Rdata")


# Plot
Allcom<- ggplot(tata)+
  geom_tile(aes(x=Long, y=Lat, fill=as.numeric(Clust)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlGnBu")) +
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

ggsave(plot= Allcom, filename="Biorégionalisation.jpeg", path="results/Zones/Communautes bio", width = 13, height = 8)




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
plot(rastertata, col=brewer.pal(n = 9, name = "YlGnBu"), xlab="Longitude", ylab="Latitude")

rastertata2<- mask(rastertata, res)
plot(rastertata2, col=brewer.pal(n = 9, name = "YlGnBu"), main="Après mask", xlab="Longitude", ylab="Latitude")



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

Allcom<- ggplot(tatatab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  scale_fill_manual(values = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58"))+
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

ggsave(plot= Allcom, filename="Biorégionalisation.jpeg", path="results/Zones/Communautes bio", width = 13, height = 8)




































