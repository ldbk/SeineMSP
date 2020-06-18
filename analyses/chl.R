library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(NbClust)

chl<- stack("data/satellite/chl/chl")


# Conversion raster - tableau
fortify.Raster <- function(chl, maxPixel = 1000000) {
  
  if (ncell(chl) > maxPixel) {
    x <- sampleRegular(chl, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(chl, seq_len(ncell(chl)))
  out <- chl %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabchl<- fortify(chl)
pixelok<- which(!is.na(apply(Tabchl,1,mean)))
Tabchl<- pivot_longer(Tabchl, cols=1:240, names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)

{
  Tabchl$Date<- sub("values.index_","",Tabchl$Date)
  Tabchl$year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
  Tabchl$month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
}


# Infos
#mean(Tabchl$Chloro)
#min(Tabchl$Chloro)
#max(Tabchl$Chloro)
#sd(Tabchl$Chloro)
#var(Tabchl$Chloro)


# Mean chl per year
Tabchl2<- Tabchl %>% group_by(x,y,year) %>% summarize(moyChl= mean(Chloro))
ggplot(Tabchl2)+
  geom_tile(aes(x=x, y=y, fill=moyChl))+
  ggtitle("Chl moyenne 1997-2017")+
  facet_wrap(. ~ year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="(?g/L)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

ggplot(Tabchl2, aes(x= year, y=moyChl, group=year))+
  geom_boxplot()


# Mean chl 1997-2017
Tabchl3<- Tabchl2 %>% group_by(x,y) %>% summarize(moyper= mean(moyChl))
ggplot(Tabchl3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Chlorophylle moyenne 1997-2017")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="?g/L")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))


# Serie tempo mean chl
Tabchl4<- Tabchl %>% group_by(year) %>% summarize(moybaie= mean(Chloro))
ggplot(Tabchl4)+
  geom_point(aes(x= year, y= moybaie))+
  ggtitle("Chlorophylle moyenne annuelle 1997-2017")+
  xlab("Year")+
  ylab("?g/L")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

save(Tabchl4, file="data/satellite/Chl/Chl_serie.Rdata")



# Partitionnement

Tabchlnew<- pivot_wider(Tabchl2, names_from = year, values_from = moyChl)
Tabchlnew<- na.omit(Tabchlnew)
metaTabnew<- Tabchlnew %>% dplyr::select(x, y) %>% ungroup()
Tabchlnew<- Tabchlnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabchlnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

Tabchl5<- Tabchl3 %>% ungroup() %>% dplyr::select(moyper)
NbClust(Tabchl5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  5

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- chl[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabchl2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyChl)) }

toto2chl<- left_join(toto, essai2, by="Clust")

save(toto2chl, file="data/satellite/chl/toto2chl.Rdata")



# Serie tempo

serie <- left_join(toto, cbind(metaTabnew, Tabchlnew))
serie <- pivot_longer(serie, cols=c(4:24), names_to="Year", values_to = "chl")
serie <- serie %>% group_by(Year, Clust) %>% summarise(chl=mean(chl))

ggseriechl<-  ggplot(serie)+
  geom_point(aes(x=Year,y=chl,col=Clust))+
  geom_line(aes(x=Year,y=chl,col=Clust, group=Clust))+
  theme_minimal()+
  facet_wrap(.~Clust)

save(ggseriechl, file="data/satellite/chl/chl_seriebyzone.Rdata")



# Trait de cote
  # 1st Polygon
liste <- with(toto2chl, chull(x, y))
hull <- toto2chl[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Chl<- ggplot(toto2chl)+ geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean chl")+ theme_minimal()+coord_fixed()+ggtitle("Chlorophyll")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3chl<- toto2chl
coordinates(toto3chl)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3chl) <- TRUE
  # coerce to raster
rasterchlnew<- raster(toto3chl)
rasterchlnew
plot(rasterchlnew, main="Chl", xlab="Longitude", ylab="Latitude")

save(rasterchlnew, file="data/satellite/chl/rasterChlnew.Rdata")

mChl<- mask(rasterchlnew, res)
plot(mChl)

save(mChl, file="data/satellite/chl/chl_raster.Rdata")



# Polygons

polChl<- rasterToPolygons(mChl, dissolve=TRUE)
plot(polChl, col=polChl@data$Clust)

writeOGR(polChl, dsn="data/satellite/chl", layer="Chl", driver="ESRI Shapefile")

save(polChl, file="data/satellite/chl/chl_polygons.Rdata")





