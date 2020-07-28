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

PP<- nc_open("data/satellite/Primary production/MetO-NWS-BIO-mm-PPRD_1591275638447.nc")
PP<- stack("data/satellite/Primary production/MetO-NWS-BIO-mm-PPRD_1591275638447.nc")


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

TabPP$Year <- as.numeric(substr(as.character(TabPP$Date),1,4))
TabPP$Month<- as.numeric(substr(as.character(TabPP$Date), 6,7))
TabPP$Day<- as.numeric(substr(as.character(TabPP$Date), 9,10))
}


# Infos
#mean(TabPP$PP)
#min(TabPP$PP)
#max(TabPP$PP)
#sd(TabPP$PP)
#var(TabPP$PP)


# Mean PP per year
TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(TabPP2)+
  geom_tile(aes(x=x, y=y, fill=log(moyPP)))+
  ggtitle("Production primaire")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="log10 (Production primaire)")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Greens"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(TabPP2, file="data/satellite/Primary production/TabPP2.Rdata")

#ggplot(TabPP2, aes(x= Year, y=moyPP, group=Year))+geom_boxplot()


# Mean PP 1998-2018
#TabPP3<- TabPP2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPP))
#ggplot(TabPP3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Production Primaire moyenne 1998-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="mg C/m3/j")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean PP (year)
TabPP4<- TabPP %>% group_by(Year) %>% summarize(moybaie= mean(PP))
save(TabPP4, file= "results/satellite/series full bay/PPTab.Rdata")

PPseries<- ggplot(TabPP4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean primary production 1998-2018")+
  xlab("Year")+
  ylab("mg C/m3/j")+
  theme_minimal()

save(PPseries, file="results/satellite/series full bay/PP_series.Rdata")
ggsave(plot= PPseries, filename="PP.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean PP (month)
TabPP6<- TabPP %>% group_by(Month) %>% summarize(moybaie= mean(PP))
save(TabPP6, file= "results/satellite/series full bay/monthly/PPTab.Rdata")


PPseries2<- ggplot(TabPP6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean primary production")+
  xlab("Month")+
  ylab("mg C/m3/j")+
  theme_minimal()

PPseries3<- PPseries2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

save(PPseries3, file="results/satellite/series full bay/monthly/PP_series.Rdata")
ggsave(plot= PPseries3, filename="PP.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabPPnew<- pivot_wider(TabPP2, names_from = Year, values_from = moyPP)
TabPPnew<- na.omit(TabPPnew)
metaTabnew<- TabPPnew %>% dplyr::select(x, y) %>% ungroup()
TabPPnew<- TabPPnew %>% ungroup() %>% dplyr::select(-x, -y)               

distance<- dist(TabPPnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1)

#NbClust(TabPPnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
# According to the majority rule, the best number of clusters is  2

rect.hclust(tree, 2)
zones<- cutree(tree, 2)

zone<- PP[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))      

essai<- left_join(TabPP2, toto, by=c("x", "y"))     

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPP)) }  

toto2PP<- left_join(toto, essai2, by="Clust")                           



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabPPnew))
serie<- pivot_longer(serie, cols=c(4:24), names_to="Year", values_to = "PP")
serie<- serie %>% group_by(Year, Clust) %>% summarise(PP=mean(PP))

ggseriePP<-  ggplot(serie)+
  geom_point(aes(x=Year,y=PP,col=Clust))+
  geom_line(aes(x=Year,y=PP,col=Clust, group=Clust))+
  ggtitle("Primary production")+
  ylab("mg C/m3/j")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggseriePP, file="results/satellite/series by zone/PP_seriebyzone.Rdata")
ggsave(plot= ggseriePP, filename="PP_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
  # 1st Polygon
liste <- with(toto2PP, chull(x, y))
hull <- toto2PP[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#PP<- ggplot(toto2PP)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean PP (mg C/m3/j)")+theme_minimal()+coord_fixed()+ggtitle("Primary production")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3PP<- toto2PP
coordinates(toto3PP)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3PP) <- TRUE
  # coerce to raster
rasterPP<- raster(toto3PP)
rasterPP
raster::plot(rasterPP, col= c("#CCFFCC", "#99CC99"), main="Primary production", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disPP<- disaggregate(rasterPP, fact=(res(rasterPP)/res(rasterchlnew)))
mPP<- mask(disPP, res)
plot(mPP)

save(mPP, file="data/satellite/Primary production/PP_raster.Rdata")

jpeg(file="results/satellite/zones/PP_raster.jpeg")
plot(mPP, main="Primary production", xlab="Longitude", ylab="Latitude")
dev.off()



library(sf)

#test loran
r1<- raster(nrow=20, ncol=20, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
plot(r1)
crs(rasterPP)<-crs(r1)
r1c<- resample(rasterPP, r1, method="ngb")
plot(r1c)

#testrasterize
r1<- raster(nrow=200, ncol=200, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
xy<-data.frame(toto2PP[,1:2])
r2<-rasterize(xy,r1,as.numeric(toto2PP$Clust),background=-999)#init1$Clust)
plot(r2)

#test rasterToPolygons
r1<- raster(nrow=10, ncol=20, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
p1<-rasterToPolygons(rasterPP)
p2<-st_as_sf(p1)
r2<-fasterize(p2,r1,field="Clust")
resample(rasterPP,r2)

#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#<<<<<<< HEAD
plot(r1)
plot(p1,add=T)
r2<-rasterize(p1,r1)
plot(r2)
#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#<<<<<<< HEAD



# Polygons

polPP<- rasterToPolygons(mPP, dissolve=TRUE)
plot(polPP, col=polPP@data$Clust)

writeOGR(polPP, dsn="data/satellite/Primary production", layer="PP", driver="ESRI Shapefile")

save(polPP, file="data/satellite/Primary production/PP_polygons.Rdata")



# Mean PP / zone

summaryPP<- toto2PP %>% select(Clust, mean)
summaryPP<- unique(summaryPP)

write.table(summaryPP, file="results/satellite/means by zone/summaryPP.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4PP<- toto2PP %>% select(-Clust)
coordinates(toto4PP)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4PP) <- TRUE
  # coerce to raster
rasterPP2<- raster(toto4PP)
rasterPP2
raster::plot(rasterPP2, col=c("#CCFFCC", "#99CC99"), main="Primary production", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disPP2<- disaggregate(rasterPP2, fact=(res(rasterPP2)/res(rasterchlnew)))
mPP2<- mask(disPP2, res)
plot(mPP2, col=c("#CCFFCC", "#99CC99"))

save(mPP2, file="results/satellite/means by zone/PP_raster.Rdata")

jpeg(file="results/satellite/means by zone/PP_raster.jpeg")
plot(mPP2, main="Primary production", xlab="Longitude", ylab="Latitude", col=c("#CCFFCC", "#99CC99"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mPP, maxPixel = 1000000) {
  
  if (ncell(mPP) > maxPixel) {
    x <- sampleRegular(mPP, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mPP, seq_len(ncell(mPP)))
  out <- mPP %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabPPfin<- fortify(mPP)

save(TabPPfin, file="data/satellite/Primary production/TabPPfin.Rdata")







