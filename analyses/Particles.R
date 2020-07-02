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
library(grDevices)

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


# Serie tempo mean particles (year)
TabPart4<- TabPart %>% group_by(Year) %>% summarize(moybaie= mean(Particules))
save(TabPart4, file= "results/satellite/series full bay/PartTab.Rdata")

Partseries<- ggplot(TabPart4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean particules 1997-2019")+
  xlab("Year")+
  ylab("m-1")+
  theme_minimal()

save(Partseries, file="results/satellite/series full bay/part_series.Rdata")
ggsave(plot= Partseries, filename="Particles.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean particles (month)
TabPart6<- TabPart %>% group_by(Month) %>% summarize(moybaie= mean(Particules))
save(TabPart6, file= "results/satellite/series full bay/monthly/PartTab.Rdata")

Partseries2<- ggplot(TabPart6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean particules")+
  xlab("Month")+
  ylab("m-1")+
  theme_minimal()

save(Partseries2, file="results/satellite/series full bay/monthly/part_series.Rdata")
ggsave(plot= Partseries2, filename="Particles.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabPartnew<- pivot_wider(TabPart2, names_from = Year, values_from = moyPart)
TabPartnew<- na.omit(TabPartnew)
metaTabnew<- TabPartnew %>% dplyr::select(x, y) %>% ungroup()
TabPartnew<- TabPartnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabPartnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1)

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
  ggtitle("Particles")+
  ylab("m-1")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggseriePart, file="results/satellite/series by zone/Part_seriebyzone.Rdata")
ggsave(plot= ggseriePart, filename="Part_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



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

jpeg(file="results/satellite/zones/Part_raster.jpeg")
plot(mPart, main="Particles", xlab="Longitude", ylab="Latitude")
dev.off()



# Polygons

polPart<- rasterToPolygons(mPart, dissolve=TRUE)
plot(polPart, col=polPart@data$Clust)

writeOGR(polPart, dsn="data/satellite/Particles", layer="Part", driver="ESRI Shapefile")

save(polPart, file="data/satellite/Particles/part_polygons.Rdata")



# Mean particles / zone

summaryPart<- toto2part %>% select(Clust, mean)
summaryPart<- unique(summaryPart)

write.table(summaryPart, file="results/satellite/means by zone/summaryPart.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4part<- toto2part %>% select(-Clust)
coordinates(toto4part)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4part) <- TRUE
  # coerce to raster
rasterpart2<- raster(toto4part)
rasterpart2
plot(rasterpart2, col= terrain.colors(4), main="Particles", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dispart2<- disaggregate(rasterpart2, fact=(res(rasterpart2)/res(rasterchlnew)))
mPart2<- mask(dispart2, res)
plot(mPart2)

save(mPart2, file="results/satellite/means by zone/part_raster.Rdata")

jpeg(file="results/satellite/means by zone/Part_raster.jpeg")
plot(mPart2, main="Particles", xlab="Longitude", ylab="Latitude")
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mPart, maxPixel = 1000000) {
  
  if (ncell(mPart) > maxPixel) {
    x <- sampleRegular(mPart, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mPart, seq_len(ncell(mPart)))
  out <- mPart %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabPartfin<- fortify(mPart)

save(TabPartfin, file="data/satellite/Particles/TabPartfin.Rdata")







