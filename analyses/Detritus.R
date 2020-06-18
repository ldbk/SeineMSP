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

Detrit<- stack("data/satellite/Detritus/cdm443")


# Conversion raster - tableau
fortify.Raster <- function(Detrit, maxPixel = 1000000) {
  
  if (ncell(Detrit) > maxPixel) {
    x <- sampleRegular(Detrit, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Detrit, seq_len(ncell(Detrit)))
  out <- Detrit %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabDet<- fortify(Detrit)
pixelok<- which(!is.na(apply(TabDet,1,mean)))
TabDet<- pivot_longer(TabDet, cols=1:244, names_to = "Date", values_to = "Detritus", values_drop_na = TRUE)

{
TabDet$Date<-sub("values.index_","",TabDet$Date)
TabDet$Year<- as.numeric(substr(as.character(TabDet$Date),1,4))
TabDet$Month<- as.numeric(substr(as.character(TabDet$Date), 6,7))
}


# Infos
#mean(TabDet$Detritus)
#min(TabDet$Detritus)
#max(TabDet$Detritus)
#sd(TabDet$Detritus)
#var(TabDet$Detritus)


# Mean detritus per year
TabDet2<- TabDet %>% group_by(x,y,Year) %>% summarize(moyDet= mean(Detritus))
ggplot(TabDet2)+
  geom_tile(aes(x=x, y=y, fill=moyDet))+
  ggtitle("Detritus moyenne 1997-2017")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

ggplot(TabDet2, aes(x=Year, y=moyDet, group=Year))+
  geom_boxplot()


# Mean detritus 1997-2017
TabDet3<- TabDet2 %>% group_by(x,y) %>% summarize(moyper= mean(moyDet))
ggplot(TabDet3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Detritus moyenne 1997-2017")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean detritus
TabDet4<- TabDet %>% group_by(Year) %>% summarize(moybaie= mean(Detritus))
ggplot(TabDet4)+
  geom_line(aes(x=Year, y= moybaie))+
  ggtitle("Detritus annuels 1997-2017")+
  xlab("Year")+
  ylab("Detritus")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

save(TabDet4, file="data/satellite/Detritus/Det_serie.Rdata")



# Partitionnement

TabDetnew<- pivot_wider(TabDet2, names_from = Year, values_from = moyDet)
TabDetnew<- na.omit(TabDetnew)
metaTabnew<- TabDetnew %>% dplyr::select(x, y)
TabDetnew<- TabDetnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabDetnew)
#distance[1:5]

tree<- hclust(distance, )
plot(tree)

TabDet5<- TabDet3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(TabDet5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  5

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- Detrit[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabDet2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyDet)) }

toto2Det<- left_join(toto, essai2, by="Clust")

save(toto2Det, file="data/satellite/Detritus/toto2Det.Rdata")



# Trait de cote
  # 1st Polygon
liste <- with(toto2Det, chull(x, y))
hull <- toto2Det[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Det<- ggplot(toto2Det)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="")+theme_minimal()+coord_fixed()+ggtitle("Detritus")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3Det<- toto2Det
coordinates(toto3Det)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3Det) <- TRUE
  # coerce to raster
rasterDet<- raster(toto3Det)
rasterDet
plot(rasterDet, col= terrain.colors(5), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disdet<- disaggregate(rasterDet, fact=(res(rasterDet)/res(rasterchlnew)))
mDet<- mask(disdet,res)
plot(mDet)

save(mDet, file="data/satellite/Detritus/Det_raster.Rdata")



# Polygons

polDet<- rasterToPolygons(mDet, dissolve=TRUE)
plot(polDet, col=polDet@data$Clust)

writeOGR(polDet, dsn="data/satellite/Detritus", layer="Det", driver="ESRI Shapefile")


save(polDet, file="data/satellite/Detritus/Det_polygons.Rdata")






