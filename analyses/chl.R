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
library(grDevices)
library(RColorBrewer)

chl<- stack("data/satellite/Chl/dataset-oc-glo-bio-multi-l4-chl_4km_monthly-rep_1592571915166.nc")


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
Tabchl<- pivot_longer(Tabchl, cols=1:262, names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)

{
  Tabchl$Date<- sub("values.X","",Tabchl$Date)
  Tabchl$year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
  Tabchl$month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
  Tabchl$day<- as.numeric(substr(as.character(Tabchl$Date), 9,10))
}


# Infos
#mean(Tabchl$Chloro)
#min(Tabchl$Chloro)
#max(Tabchl$Chloro)
#sd(Tabchl$Chloro)
#var(Tabchl$Chloro)


# Mean chl per year
Tabchl2<- Tabchl %>% group_by(x,y,year) %>% summarize(moyChl= mean(Chloro))
#ggplot(Tabchl2)+
#  geom_tile(aes(x=x, y=y, fill=moyChl))+
#  ggtitle("Chl moyenne 1997-2017")+
#  facet_wrap(. ~ year)+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="(?g/L)")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))

#ggplot(Tabchl2, aes(x= year, y=moyChl, group=year))+
#  geom_boxplot()


# Mean chl 1997-2017
Tabchl3<- Tabchl2 %>% group_by(x,y) %>% summarize(moyper= mean(moyChl))
#ggplot(Tabchl3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Chlorophylle moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="?g/L")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))


# Serie tempo mean chl (year)
Tabchl4<- Tabchl %>% group_by(year) %>% summarize(moybaie= mean(Chloro))
save(Tabchl4, file= "results/satellite/series full bay/chlTab.Rdata")

Chlseries<- ggplot(Tabchl4)+
  geom_line(aes(x= year, y= moybaie))+
  ggtitle("Mean chlorophyll 1997-2019")+
  xlab("Year")+
  ylab("mg/m3")+
  theme_minimal() 

save(Chlseries, file="results/satellite/series full bay/Chl_series.Rdata")
ggsave(plot= Chlseries, filename="chl.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean chl (month)
Tabchl6<- Tabchl %>% group_by(month) %>% summarize(moybaie= mean(Chloro))
save(Tabchl6, file= "results/satellite/series full bay/monthly/chlTab.Rdata")

Chlseries2<- ggplot(Tabchl6)+
  geom_line(aes(x= month, y= moybaie))+
  ggtitle("Mean chlorophyll")+
  xlab("Month")+
  ylab("mg/m3")+
  theme_minimal() 

Chlseries3<- Chlseries2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

save(Chlseries3, file="results/satellite/series full bay/monthly/Chl_series.Rdata")
ggsave(plot= Chlseries3, filename="chl.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

Tabchlnew<- pivot_wider(Tabchl2, names_from = year, values_from = moyChl)
Tabchlnew<- na.omit(Tabchlnew)
metaTabnew<- Tabchlnew %>% dplyr::select(x, y) %>% ungroup()
Tabchlnew<- Tabchlnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabchlnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1)

Tabchl5<- Tabchl3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(Tabchl5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  6

rect.hclust(tree, 6)
zones<- cutree(tree, 6)

zone<- chl[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabchl2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyChl)) }

toto2chl<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, Tabchlnew))
serie<- pivot_longer(serie, cols=c(4:26), names_to="Year", values_to = "chl")
serie<- serie %>% group_by(Year, Clust) %>% summarise(chl=mean(chl))
save(serie, file="results/satellite/series by zone/chlTab.Rdata")

clust.labs<- c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5", "Zone 6")
names(clust.labs)<- c("1", "2", "3", "4", "5", "6")
colors<- brewer.pal(n = 6, name = "YlGnBu")

ggseriechl<-  ggplot(serie)+
  geom_point(aes(x=Year,y=chl,col=Clust))+
  geom_line(aes(x=Year,y=chl,col=Clust, group=Clust))+
  scale_colour_manual(values=colors)+
  ggtitle("Chlorophyll")+
  ylab("mg/m3")+
  theme_minimal()+
  facet_wrap(.~Clust, labeller = labeller(Clust= clust.labs))+
  theme(strip.text.x = element_text(size = 20))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.text.y = element_text(size = 10))

save(ggseriechl, file="results/satellite/series by zone/chl_seriebyzone.Rdata")
ggsave(plot= ggseriechl, filename="chl_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



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
plot(rasterchlnew, col=brewer.pal(n = 6, name = "YlGnBu"), main="Chl", xlab="Longitude", ylab="Latitude")

save(rasterchlnew, file="data/satellite/chl/rasterChlnew.Rdata")

mChl<- mask(rasterchlnew, res)
plot(mChl, col=brewer.pal(n = 6, name = "YlGnBu"))

save(mChl, file="data/satellite/chl/chl_raster.Rdata")

jpeg(file="results/satellite/zones/chl_raster.jpeg")
plot(mChl, main="Chlorophyll", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 6, name = "YlGnBu"))
dev.off()



# Polygons

polChl<- rasterToPolygons(mChl, dissolve=TRUE)
plot(polChl, col=polChl@data$Clust)

writeOGR(polChl, dsn="data/satellite/chl", layer="Chl", driver="ESRI Shapefile")

save(polChl, file="data/satellite/chl/chl_polygons.Rdata")



# Mean chl / zone

summarychl<- toto2chl %>% select(Clust, mean)
summarychl<- unique(summarychl)

write.table(summarychl, file="results/satellite/means by zone/summarychl.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4chl<- toto2chl %>% select(-Clust)
coordinates(toto4chl)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4chl) <- TRUE
  # coerce to raster
rasterchlnew2<- raster(toto4chl)
rasterchlnew2
plot(rasterchlnew2, col=brewer.pal(n = 6, name = "PuRd"), main="Chl", xlab="Longitude", ylab="Latitude")

mChl2<- mask(rasterchlnew2, res)
plot(mChl2, col=brewer.pal(n = 6, name = "PuRd"))

save(mChl2, file="results/satellite/means by zone/chl_raster.Rdata")

jpeg(file="results/satellite/means by zone/chl_raster.jpeg")
plot(mChl2, col=brewer.pal(n = 6, name = "PuRd"), main="Chlorophyll", xlab="Longitude", ylab="Latitude")
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mChl, maxPixel = 1000000) {
  
  if (ncell(mChl) > maxPixel) {
    x <- sampleRegular(mChl, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mChl, seq_len(ncell(mChl)))
  out <- mChl %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

Tabchlfin<- fortify(mChl)

save(Tabchlfin, file="data/satellite/chl/Tabchlfin.Rdata")







