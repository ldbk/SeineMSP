library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)
library(rgeos)
library(RGeostats)
library(rgdal)

PP<- nc_open("data/satellite/Primary production/PP 1998-2018.nc")
PP<- stack("data/satellite/Primary production/PP 1998-2018.nc")


# Conversion raster - tableau
fortify.Raster <- function(PP, maxPixel = 1000000) {
  
  if (ncell(PP) > maxPixel) {
    x <- sampleRegular(PP, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(PP, seq_len(ncell(PP)))
  out <- PP %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabPP<- fortify(PP)
pixelok<- which(!is.na(apply(TabPP,1,mean)))
TabPP<- pivot_longer(TabPP, cols=1:251, names_to = "Secondes", values_to = "PP", values_drop_na = TRUE)

{
TabPP$Secondes<-sub("values.X","",TabPP$Secondes)
sec<-as.numeric(TabPP$Secondes)
Jour0<-strptime("1998-01-01", format= "%Y-%m-%d")
TabPP$Date<- Jour0+sec
}
{
TabPP$Year <- as.numeric(substr(as.character(TabPP$Date),1,4))
TabPP$Month<- as.numeric(substr(as.character(TabPP$Date), 6,7))
TabPP$Day<- as.numeric(substr(as.character(TabPP$Date), 9,10))
}
TabPP<- TabPP[, -c(3, 5)]


# Infos
mean(TabPP$PP)
min(TabPP$PP)
max(TabPP$PP)
sd(TabPP$PP)
var(TabPP$PP)


# Mean PP per year
TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(TabPP2)+
  geom_tile(aes(x=x, y=y, fill=moyPP))+
  ggtitle("PP moyenne 1998-2018")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="PP (mg C/m3/j)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(TabPP2, aes(x= Year, y=moyPP, group=Year))+
  ggtitle("Production primaire 1998-2018")+
  ylab("mg C/m3/j")+
  geom_boxplot()


# Mean PP 1998-2018
TabPP3<- TabPP2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPP))
ggplot(TabPP3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Production Primaire moyenne 1998-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mg C/m3/j")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean PP
TabPP4<- TabPP %>% group_by(Year) %>% summarize(moybaie= mean(PP))
ggplot(TabPP4)+
geom_line(aes(x= Year, y= moybaie))+
  ggtitle("PP annuelle 1998-2018")+
  xlab("Year")+
  ylab("mg C/m3/j")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6)) 

save(TabPP4, file="data/satellite/Primary production/PP_serie.Rdata")



# Partitionnement

TabPPnew<- pivot_wider(TabPP2, names_from = Year, values_from = moyPP)
metaTabnew<- TabPPnew %>% dplyr::select(x, y)
TabPPnew<- TabPPnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabPPnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- PP[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabPP2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPP)) }

toto2PP<- left_join(toto, essai2, by="Clust")




#1st Polygon
liste <- with(toto2PP, chull(x, y))
hull <- toto2PP[liste, c("x", "y")]
Poly <- Polygon(hull)

#Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

#Cut object along coast
coast <- readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- gDifference(buff, coast)
PolyCut <- fortify(res)


#Put polygon in good format for later use
tete <- PolyCut[PolyCut$piece==1,]
db.poly <- polygon.create(tete[,c(1,2)])

PP<- ggplot(toto2PP)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean PP (mg C/m3/j)")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Primary production")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

PP




# Raster

r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3PP<- toto2PP
coordinates(toto3PP)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3PP) <- TRUE
  # coerce to raster
rasterPP<- raster(toto3PP)
rasterPP
raster::plot(rasterPP, col= terrain.colors(5), main="Primary production", xlab="Longitude", ylab="Latitude")

rasterPPnew<- resample(rasterPP, r0, method="ngb")
plot(rasterPPnew, main="Primary production", xlab="Longitude", ylab="Latitude")

save(rasterPPnew, file="data/satellite/Primary production/PP_raster.Rdata")


# old

#r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)













