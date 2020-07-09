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
library(RColorBrewer)

Detrit<- stack("data/satellite/Detritus/dataset-oc-glo-opt-multi-l4-cdm443_4km_monthly-rep-v02_1592570192473.nc")


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
TabDet<- pivot_longer(TabDet, cols=1:262, names_to = "Date", values_to = "Detritus", values_drop_na = TRUE)

{
  TabDet$Date<-sub("values.X","",TabDet$Date)
  TabDet$Year<- as.numeric(substr(as.character(TabDet$Date),1,4))
  TabDet$Month<- as.numeric(substr(as.character(TabDet$Date), 6,7))
  TabDet$Day<- as.numeric(substr(as.character(TabDet$Date), 9,10))
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

#ggplot(TabDet2, aes(x=Year, y=moyDet, group=Year))+
#  geom_boxplot()


# Mean detritus 1997-2017
TabDet3<- TabDet2 %>% group_by(x,y) %>% summarize(moyper= mean(moyDet))
#ggplot(TabDet3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Detritus moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean detritus (year)
TabDet4<- TabDet %>% group_by(Year) %>% summarize(moybaie= mean(Detritus))
save(TabDet4, file= "results/satellite/series full bay/DetTab.Rdata")

Detseries<- ggplot(TabDet4)+
  geom_line(aes(x=Year, y= moybaie))+
  ggtitle("Mean detritus 1997-2019")+
  xlab("Year")+
  ylab("m-1")+
  theme_minimal() 

save(Detseries, file="results/satellite/series full bay/Det_series.Rdata")
ggsave(plot= Detseries, filename="Detitrus.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean detritus (month)
TabDet6<- TabDet %>% group_by(Month) %>% summarize(moybaie= mean(Detritus))
save(TabDet6, file= "results/satellite/series full bay/monthly/DetTab.Rdata")

Detseries2<- ggplot(TabDet6)+
  geom_line(aes(x=Month, y= moybaie))+
  ggtitle("Mean detritus")+
  xlab("Month")+
  ylab("m-1")+
  theme_minimal() 

save(Detseries2, file="results/satellite/series full bay/monthly/Det_series.Rdata")
ggsave(plot= Detseries2, filename="Detitrus.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabDetnew<- pivot_wider(TabDet2, names_from = Year, values_from = moyDet)
TabDetnew<- na.omit(TabDetnew)
metaTabnew<- TabDetnew %>% dplyr::select(x, y) %>% ungroup()
TabDetnew<- TabDetnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabDetnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1)

TabDet5<- TabDet3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(TabDet5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  3

rect.hclust(tree, 3)
zones<- cutree(tree, 3)

zone<- Detrit[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabDet2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyDet)) }

toto2Det<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabDetnew))
serie<- pivot_longer(serie, cols=c(4:26), names_to="Year", values_to = "Det")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Det=mean(Det))

ggserieDet<-  ggplot(serie)+
  geom_point(aes(x=Year,y=Det,col=Clust))+
  geom_line(aes(x=Year,y=Det,col=Clust, group=Clust))+
  ggtitle("Detritus")+
  ylab("m-1")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggserieDet, file="results/satellite/series by zone/Det_seriebyzone.Rdata")
ggsave(plot= ggserieDet, filename="Det_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



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
plot(rasterDet, col=brewer.pal(n = 3, name = "YlGn"), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disdet<- disaggregate(rasterDet, fact=(res(rasterDet)/res(rasterchlnew)))
mDet<- mask(disdet,res)
plot(mDet, col=brewer.pal(n = 3, name = "YlGn"))

save(mDet, file="data/satellite/Detritus/Det_raster.Rdata")

jpeg(file="results/satellite/zones/Det_raster.jpeg")
plot(mDet, main="Detritus", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "YlGn"))
dev.off()



# Polygons

polDet<- rasterToPolygons(mDet, dissolve=TRUE)
plot(polDet, col=polDet@data$Clust)

writeOGR(polDet, dsn="data/satellite/Detritus", layer="Det", driver="ESRI Shapefile")


save(polDet, file="data/satellite/Detritus/Det_polygons.Rdata")



# Mean detritus per zone

summaryDet<- toto2Det %>% select(Clust, mean)
summaryDet<- unique(summaryDet)

write.table(summaryDet, file="results/satellite/means by zone/summaryDet.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4Det<- toto2Det %>% select(-Clust)
coordinates(toto4Det)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4Det) <- TRUE
  # coerce to raster
rasterDet2<- raster(toto4Det)
rasterDet2
plot(rasterDet2, col=brewer.pal(n = 3, name = "PuBuGn"), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disdet2<- disaggregate(rasterDet2, fact=(res(rasterDet2)/res(rasterchlnew)))
mDet2<- mask(disdet2,res)
plot(mDet2, col=brewer.pal(n = 3, name = "PuBuGn"))

save(mDet2, file="results/satellite/means by zone/Det_raster.Rdata")

jpeg(file="results/satellite/means by zone/Det_raster.jpeg")
plot(mDet2, main="Detritus", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "PuBuGn"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mDet, maxPixel = 1000000) {
  
  if (ncell(mDet) > maxPixel) {
    x <- sampleRegular(mDet, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mDet, seq_len(ncell(mDet)))
  out <- mDet %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabDetfin<- fortify(mDet)

save(TabDetfin, file="data/satellite/Detritus/TabDetfin.Rdata")







