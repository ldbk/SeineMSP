library(dplyr)
library(tidyr)
library(ggplot2)
library(NbClust)
library(rgdal)
library(rgeos)
library(raster)
library(ggplot2)

load("data/krigeage.Rdata")
names(Kriege.dens)[6]<- "Community"
Kriege.dens$Community<- as.numeric(Kriege.dens$Community)


Longitude<- numeric()
Latitude<- numeric()
Clust<- numeric()
Community<- numeric()


for (j in unique(data.frame(Kriege.dens)[,"Community"])){
  
  Tab1<- Kriege.dens[Kriege.dens$Community==j,] %>% dplyr::select(-Variance, -Community)
  Tab2<- pivot_wider(Tab1, names_from = Year, values_from = Prediction)
  metaTab<- Tab2 %>% dplyr::select(Longitude, Latitude)
  Tab2<- Tab2 %>% dplyr::select(-c(Longitude, Latitude))

  
  # Classification
  
  distance<- dist(Tab2)
  #distance[1:5]
  
  tree<- hclust(distance)
  plot(tree, hang=-1)
  
  Nb1<- Tab1 %>% group_by(Longitude,Latitude) %>% summarize(moyper= mean(Prediction))
  Nb2<- Nb1 %>% ungroup() %>% dplyr::select(moyper)
  PLOM<- NbClust(Nb2, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
  
  rect.hclust(tree, max(PLOM$Best.partition))
  zones<- cutree(tree, max(PLOM$Best.partition))
  
  
  toto<- cbind(metaTab, Clust=factor(zones))        
  toto<- left_join(toto, Kriege.dens[Kriege.dens$Community==j,], by=c("Longitude", "Latitude")) 
  toto<- toto %>% dplyr::select(Longitude, Latitude, Clust, Community)   
  
  Longitude<- c(Longitude, toto$Longitude)
  Latitude<- c(Latitude, toto$Latitude)
  Clust<- c(Clust, toto$Clust)
  Community<- c(Community, toto$Community)

  tata <- left_join(toto, cbind(metaTab, Tab2))         
  tata <- pivot_longer(tata, cols=c(5:36), names_to="Year", values_to = "Prediction")    
  tata <- tata %>% group_by(Year, Clust) %>% summarise(Prediction=mean(Prediction))     
  
  ggtata<-  ggplot(tata)+
    geom_point(aes(x=Year,y=Prediction,col=Clust))+
    geom_line(aes(x=Year,y=Prediction,col=Clust, group=Clust))+
    theme_minimal()+
    facet_wrap(.~Clust)
  
  save(ggtata, file= paste0("results/Communautes bio/Community", j,"_raster.Rdata"))
  
  print(ggtata)
  



}
Tabfaunefin<- data.frame(Longitude=Longitude, Latitude=Latitude, Clust=as.factor(Clust), Community=Community)




# Trait de cote
# 1st Polygon
liste <- with(Tabfaunefin, chull(Longitude, Latitude))
hull <- Tabfaunefin[liste, c("Longitude", "Latitude")]
Poly <- Polygon(hull)

# Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

# Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)




for (j in unique(Tabfaunefin[,"Community"])){


# Raster
  
    # create SpatialPointsDataFrame
toto1<- Tabfaunefin[Tabfaunefin$Community==j,]
coordinates(toto1)<- ~ Longitude + Latitude
    # coerce to SpatialPixelsDataFrame
gridded(toto1) <- TRUE
    # coerce to raster
raster<- raster(toto1)
raster
plot(raster, main="", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dis<- disaggregate(raster, fact=(res(raster)/res(rasterchlnew)))
m<- mask(dis, res)
plot(m)

save(m, file= paste0("results/Communautes bio/Community", j,"_raster.Rdata"))



# Polygons

pol<- rasterToPolygons(m, dissolve=TRUE)
plot(pol, col=pol@data$Clust)

save(pol, file= paste0("results/Communautes bio/Community", j,"_polygons.Rdata"))

}



# Every communities

par(mfrow = c(3, 3))

load("results/Communautes bio/Community1_raster.Rdata")
Com1 <- m 
load("results/Communautes bio/Community2_raster.Rdata")
Com2<- m
load("results/Communautes bio/Community3_raster.Rdata")
Com3<- m
load("results/Communautes bio/Community4_raster.Rdata")
Com4<- m
load("results/Communautes bio/Community5_raster.Rdata")
Com5<- m
load("results/Communautes bio/Community6_raster.Rdata")
Com6<- m
load("results/Communautes bio/Community7_raster.Rdata")
Com7<- m
load("results/Communautes bio/Community8_raster.Rdata")
Com8<- m
load("results/Communautes bio/Community9_raster.Rdata")
Com9<- m


{
  Com1<- raster::plot(Com1, main="Com1", xlab="Longitude", ylab="Latitude")
  Com2<- raster::plot(Com2, main="Com2", xlab="Longitude", ylab="Latitude")
  Com3<- raster::plot(Com3, main="Com3", xlab="Longitude", ylab="Latitude")
  Com4<- raster::plot(Com4, main="Com4", xlab="Longitude", ylab="Latitude")
  Com5<- raster::plot(Com5, main="Com5", xlab="Longitude", ylab="Latitude")
  Com6<- raster::plot(Com6, main="Com6", xlab="Longitude", ylab="Latitude")
  Com7<- raster::plot(Com7, main="Com7", xlab="Longitude", ylab="Latitude")
  Com8<- raster::plot(Com8, main="Com8", xlab="Longitude", ylab="Latitude")
  Com9<-raster::plot(Com9, main="Com9", xlab="Longitude", ylab="Latitude")
}









{
  Com1<- raster::plot(Com1, main="Com1", xlab="Longitude", ylab="Latitude")
  Com2<- raster::plot(Com2, main="Com2", xlab="Longitude", ylab="Latitude")
  Com3<- raster::plot(Com3, main="Com3", xlab="Longitude", ylab="Latitude")
  Com4<- raster::plot(Com4, main="Com4", xlab="Longitude", ylab="Latitude")
  Com5<- raster::plot(Com5, main="Com5", xlab="Longitude", ylab="Latitude")
  Com6<- raster::plot(Com6, main="Com6", xlab="Longitude", ylab="Latitude")
  Com7<- raster::plot(Com7, main="Com7", xlab="Longitude", ylab="Latitude")
  Com8<- raster::plot(Com8, main="Com8", xlab="Longitude", ylab="Latitude")
  Com9<- raster::plot(Com9, main="Com9", xlab="Longitude", ylab="Latitude")
}















