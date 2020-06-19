library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)
library(rgeos)
library(NbClust)

Part<- stack("data/satellite/Particles/dataset-oc-glo-opt-multi-l4-bbp443_4km_monthly-rep-v02_1592568961250.nc")


# Conversion raster - tableau
fortify.Raster <- function(Part, maxPixel = 1000000) {
  
  if (ncell(Part) > maxPixel) {
    x <- sampleRegular(Part, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Part, seq_len(ncell(Part)))
  out <- Part %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabPart<- fortify(Part)
pixelok<- which(!is.na(apply(TabPart,1,mean)))
TabPart<- pivot_longer(TabPart, cols=1:262, names_to = "Date", values_to = "Particules", values_drop_na = TRUE)

{
  TabPart$Date<- sub("values.X","",TabPart$Date)
  TabPart$Year<- as.numeric(substr(as.character(TabPart$Date), 1,4))
  TabPart$Month<- as.numeric(substr(as.character(TabPart$Date), 6,7))
  TabPart$Day<- as.numeric(substr(as.character(TabPart$Date), 9,10))
}


# Infos
#mean(TabPart$Particules)
#min(TabPart$Particules)
#max(TabPart$Particules)
#sd(TabPart$Particules)
#var(TabPart$Particules)


# Mean particles per year 
TabPart2<- TabPart %>% group_by(x,y,Year) %>% summarize(moyPart= mean(Particules))
#ggplot(TabPart2)+
#  geom_tile(aes(x=x, y=y, fill=moyPart))+
#  ggtitle("PP moyenne 1998-2018")+
#  facet_wrap(. ~Year)+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

#ggplot(TabPart2, aes(x=Year, y=moyPart, group=Year))+
#  geom_boxplot()


# Mean particles 1998-2018
TabPart3<- TabPart2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPart))
#ggplot(TabPart3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Particules moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean particles
TabPart4<- TabPart %>% group_by(Year) %>% summarize(moybaie= mean(Particules))
#ggplot(TabPart4)+
#  geom_line(aes(x= Year, y= moybaie))+
#  ggtitle("Particules annuelles 1997-2017")+
#  xlab("Year")+
#  ylab("Concentration en particules")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

save(TabPart4, file="data/satellite/Particles/part_serie.Rdata")



# Partitionnement

TabPartnew<- pivot_wider(TabPart2, names_from = Year, values_from = moyPart)
TabPartnew<- na.omit(TabPartnew)
metaTabnew<- TabPartnew %>% dplyr::select(x, y) %>% ungroup()
TabPartnew<- TabPartnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabPartnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

TabPart5<- TabPart3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(TabPart5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  4

rect.hclust(tree, 4)
zones<- cutree(tree, 4)

zone<- Part[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabPart2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPart)) }

toto2part<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabPartnew))
serie<- pivot_longer(serie, cols=c(4:26), names_to="Year", values_to = "Part")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Part=mean(Part))

ggseriePart<-  ggplot(serie)+
  geom_point(aes(x=Year,y=Part,col=Clust))+
  geom_line(aes(x=Year,y=Part,col=Clust, group=Clust))+
  theme_minimal()+
  facet_wrap(.~Clust)

save(ggseriePart, file="data/satellite/Particles/Part_seriebyzone.Rdata")



# Trait de cote
# 1st Polygon
liste <- with(toto2part, chull(x, y))
hull <- toto2part[liste, c("x", "y")]
Poly <- Polygon(hull)

# Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

# Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Part<- ggplot(toto2part)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean Particles")+theme_minimal()+coord_fixed()+ggtitle("Particles")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# create SpatialPointsDataFrame
toto3part<- toto2part
coordinates(toto3part)<- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(toto3part) <- TRUE
# coerce to raster
rasterpart<- raster(toto3part)
rasterpart
plot(rasterpart, col= terrain.colors(4), main="Particles", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dispart<- disaggregate(rasterpart, fact=(res(rasterpart)/res(rasterchlnew)))
mPart<- mask(dispart, res)
plot(mPart)

save(mPart, file="data/satellite/Particles/part_raster.Rdata")



# Polygons

polPart<- rasterToPolygons(mPart, dissolve=TRUE)
plot(polPart, col=polPart@data$Clust)

writeOGR(polPart, dsn="data/satellite/Particles", layer="Part", driver="ESRI Shapefile")

save(polPart, file="data/satellite/Particles/part_polygons.Rdata")



# Mean particles / zone

summaryPart<- toto2part %>% select(Clust, mean)
summaryPart<- unique(summaryPart)







