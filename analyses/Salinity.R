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
library(cluster)

Sal<- nc_open("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")
Sal<- stack("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")


# Conversion raster - tableau
fortify.Raster <- function(Sal, maxPixel = 1000000) {
  
  if (ncell(Sal) > maxPixel) {
    x <- sampleRegular(Sal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Sal, seq_len(ncell(Sal)))
  out <- Sal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabSal<- fortify(Sal)
pixelok<- which(!is.na(apply(TabSal,1,mean)))
TabSal<- pivot_longer(TabSal, cols=1:323, names_to = "Secondes", values_to = "Salinite", values_drop_na = TRUE)

{
  TabSal$Secondes<-sub("values.X","",TabSal$Secondes)
  TabSal$Secondes<-sub("e.0","e0",TabSal$Secondes)
  secsst<-as.numeric(TabSal$Secondes)
  Jour0<-strptime("1992-01-16", format= "%Y-%m-%d")
  Date<- Jour0+secsst
  TabSal$Date<- Date
  
  TabSal$Year <- as.numeric(substr(as.character(TabSal$Date),1,4))
  TabSal$Month<- as.numeric(substr(as.character(TabSal$Date), 6,7))
  TabSal$Day  <- as.numeric(substr(as.character(TabSal$Date), 9,10))
}


# Infos
#mean(TabSal$Salinite)
#min(TabSal$Salinite)
#max(TabSal$Salinite)
#sd(TabSal$Salinite)
#var(TabSal$Salinite)


# Mean salinity per year
TabSal2<- TabSal %>% group_by(x,y,Year) %>% summarize(moySal= mean(Salinite))
#ggplot(TabSal2)+
#  geom_tile(aes(x=x, y=y, fill=moySal))+
#  ggtitle("Salinite moyenne 1992-2018")+
#  facet_wrap(. ~ Year)+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

#ggplot(TabSal2, aes(x= Year, y=moySal, group=Year))+
#  geom_boxplot()


# Mean salinity 1998-2018
TabSal3<- TabSal2 %>% group_by(x,y) %>% summarize(moyper= mean(moySal))
#ggplot(TabSal3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Salinite moyenne 1992-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean salinity
TabSal4<- TabSal %>% group_by(Year) %>% summarize(moybaie= mean(Salinite))
#ggplot(TabSal4)+
#  geom_line(aes(x= Year, y= moybaie))+
#  ggtitle("Salinite annuelle 1992-2018")+
#  xlab("Year")+
#  ylab("Salinité")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

save(TabSal4, file="data/satellite/Salinity/Sal_serie.Rdata")



# Partitionnement

TabSalnew<- pivot_wider(TabSal2, names_from = Year, values_from = moySal)
TabSalnew<- na.omit(TabSalnew)
metaTabnew<- TabSalnew %>% dplyr::select(x, y) %>% ungroup()
TabSalnew<- TabSalnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabSalnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1)

TabSal5<- TabSal3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(TabSal5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  4

rect.hclust(tree, 4)
zones<- cutree(tree, 4)

zone<- Sal[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabSal2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySal)) }

toto2Sal<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabSalnew))
serie<- pivot_longer(serie, cols=c(4:30), names_to="Year", values_to = "Sal")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Sal=mean(Sal))

#ggserieSal<-  ggplot(serie)+
#  geom_point(aes(x=Year,y=Sal,col=Clust))+
#  geom_line(aes(x=Year,y=Sal,col=Clust, group=Clust))+
#  theme_minimal()+
#  facet_wrap(.~Clust)

save(ggserieSal, file="data/satellite/Salinity/Sal_seriebyzone.Rdata")



# Trait de cote
  # 1st Polygon
liste <- with(toto2Sal, chull(x, y))
hull <- toto2Sal[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Sal<- ggplot(toto2Sal)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean salinity")+theme_minimal()+coord_fixed()+ggtitle("Salinity")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3Sal<- toto2Sal
coordinates(toto3Sal)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3Sal) <- TRUE
  # coerce to raster
rasterSal<- raster(toto3Sal)
rasterSal
plot(rasterSal, col= terrain.colors(4), main="Salinity", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dissal<- disaggregate(rasterSal, fact=(res(rasterSal)/res(rasterchlnew)))
mSal<- mask(dissal, res)
plot(mSal)

save(mSal, file="data/satellite/Salinity/Sal_raster.Rdata")



# Polygons

polSal<- rasterToPolygons(mSal, dissolve=TRUE)
plot(polSal, col=polSal@data$Clust)

writeOGR(polSal, dsn="data/satellite/Salinity", layer="Sal", driver="ESRI Shapefile")

save(polSal, file="data/satellite/Salinity/Sal_polygons.Rdata")



# Mean salinity / zone

summarySal<- toto2Sal %>% select(Clust, mean)
summarySal<- unique(summarySal)



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mSal, maxPixel = 1000000) {
  
  if (ncell(mSal) > maxPixel) {
    x <- sampleRegular(mSal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mSal, seq_len(ncell(mSal)))
  out <- mSal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabSalfin<- fortify(mSal)

save(TabSalfin, file="data/satellite/Salinity/TabSalfin.Rdata")







