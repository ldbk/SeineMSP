library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridisLite)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(NbClust)

sst<- stack("data/satellite/sst/IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1581929927261.nc")
sst<- sst-275.15


# Conversion raster - tableau
fortify.Raster <- function(sst, maxPixel = 1000000) {
  
  if (ncell(sst) > maxPixel) {
    x <- sampleRegular(sst, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(sst, seq_len(ncell(sst)))
  out <- sst %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabsst<- fortify.Raster(sst)
pixelok<- which(!is.na(apply(Tabsst,1,mean)))
Tabsst<- pivot_longer(Tabsst, cols=1:13232, names_to = "Secondes", values_to = "SST", values_drop_na = TRUE)

{
Tabsst$Secondes<- sub("values.X","",Tabsst$Secondes)
secsst<- as.numeric(Tabsst$Secondes)
Day0<-strptime("1981-01-01", format= "%Y-%m-%d")
Date<- Day0+secsst
Tabsst$Date <- Date
Tabsst$Year <- as.numeric(substr(as.character(Tabsst$Date),1,4))
Tabsst$Month<- as.numeric(substr(as.character(Tabsst$Date), 6,7))
Tabsst$Day  <- as.numeric(substr(as.character(Tabsst$Date), 9,10))
}


# Infos
#mean(Tabsst$SST)
#min(Tabsst$SST)
#max(Tabsst$SST)
#sd(Tabsst$SST)
#var(Tabsst$SST)


# Mean SST per year
Tabsst2<- Tabsst %>% group_by(x,y,Year) %>% summarize(moySST= mean(SST))
#ggplot(Tabsst2)+
#  geom_tile(aes(x=x, y=y, fill=moySST))+
#  ggtitle("SST moyenne 1981-2018")+
#  facet_wrap(. ~ Year)+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="SST (°C)")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

#ggplot(Tabsst2, aes(x= Year, y=moySST, group=Year))+
#  geom_boxplot()


# Mean SST 1981-2018
Tabsst3<- Tabsst2 %>% group_by(x,y) %>% summarize(moyper= mean(moySST))
#ggplot(Tabsst3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("SST moyenne 1981-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="SST (°C)")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean SST
Tabsst4<- Tabsst %>% group_by(Year) %>% summarize(moybaie= mean(SST))
#ggplot(Tabsst4)+
#  geom_line(aes(x= Month, y= moybaie))+
#  ggtitle("SST mensuelle 1981-2018")+
#  xlab("Month")+
#  ylab("°C")+
#  theme_minimal()

save(Tabsst4, file="data/satellite/sst/sst_serie.Rdata")



# Partitionnement

Tabsstnew<- pivot_wider(Tabsst2, names_from = Year, values_from = moySST)
Tabsstnew<- na.omit(Tabsstnew)
metaTabnew<- Tabsstnew %>% dplyr::select(x, y) %>% ungroup()
Tabsstnew<- Tabsstnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabsstnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

Tabsst5<- Tabsst3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(Tabsst5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  3

rect.hclust(tree, 3)
zones<- cutree(tree, 3)

zone<- sst[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabsst2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySST)) }

toto2sst<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, Tabsstnew))
serie<- pivot_longer(serie, cols=c(4:41), names_to="Year", values_to = "sst")
serie<- serie %>% group_by(Year, Clust) %>% summarise(sst=mean(sst))

#ggseriesst<-  ggplot(serie)+
#  geom_point(aes(x=Year,y=sst,col=Clust))+
#  geom_line(aes(x=Year,y=sst,col=Clust, group=Clust))+
#  theme_minimal()+
#  facet_wrap(.~Clust)

save(ggseriesst, file="data/satellite/sst/sst_seriebyzone.Rdata")



# Trait de cote
  # 1st Polygon
liste <- with(toto2sst, chull(x, y))
hull <- toto2sst[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#SST<- ggplot(toto2sst)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean SST (°C)")+theme_minimal()+coord_fixed()+ggtitle("SST")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3sst<- toto2sst
coordinates(toto3sst)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3sst) <- TRUE
  # coerce to raster
rastersst<- raster(toto3sst)
rastersst
plot(rastersst, col= terrain.colors(3), main="SST", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dissst<- disaggregate(rastersst, fact=(res(rastersst)/res(rasterchlnew)))
mSST<- mask(dissst, res)
plot(mSST)

save(mSST, file="data/satellite/sst/sst_raster.Rdata")



# Polygons

polSST<- rasterToPolygons(mSST, dissolve=TRUE)
plot(polSST, col=polSST@data$Clust)

writeOGR(polSST, dsn="data/satellite/sst", layer="SST", driver="ESRI Shapefile")

save(polSST, file="data/satellite/sst/sst_polygons.Rdata")



# Mean sst / zone

summarysst<- toto2sst %>% select(Clust, mean)
summarysst<- unique(summarysst)







