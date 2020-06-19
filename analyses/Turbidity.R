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

Turb<- stack("data/satellite/Turbidity/kd490")


# Conversion raster
fortify.Raster <- function(Turb, maxPixel = 1000000) {
  
  if (ncell(Turb) > maxPixel) {
    x <- sampleRegular(Turb, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Turb, seq_len(ncell(Turb)))
  out <- Turb %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabTurb<-fortify(Turb)
pixelok<- which(!is.na(apply(TabTurb,1,mean)))
TabTurb<- pivot_longer(TabTurb, cols=1:244, names_to = "Date", values_to = "Turbidity", values_drop_na = TRUE)

{
TabTurb$Date<-sub("values.index_","",TabTurb$Date)
TabTurb$Year <- as.numeric(substr(as.character(TabTurb$Date),1,4))
TabTurb$Month<- as.numeric(substr(as.character(TabTurb$Date), 6,7))
}


# Infos
#mean(TabTurb$Turbidity)
#min(TabTurb$Turbidity)
#max(TabTurb$Turbidity)
#sd(TabTurb$Turbidity)
#var(TabTurb$Turbidity)


# Mean turb per year
TabTurb2<- TabTurb %>% group_by(x,y,Year) %>% summarize(moyTurb= mean(Turbidity))
#ggplot(TabTurb2)+
#  geom_tile(aes(x=x, y=y, fill=moyTurb))+
#  ggtitle("Turbidité moyenne 1997-2017")+
#  facet_wrap(. ~Year)+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

#ggplot(TabTurb2, aes(x=Year, y=moyTurb, group=Year))+
#  geom_boxplot()


# Mean turb 1997-2017
TabTurb3<- TabTurb2 %>% group_by(x,y) %>% summarize(moyper= mean(moyTurb))
#ggplot(TabTurb3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Turbidité moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean turb
TabTurb4<- TabTurb %>% group_by(Year) %>% summarize(moybaie= mean(Turbidity))
#ggplot(TabTurb4)+
#  geom_line(aes(x=Year, y=moybaie))+
#  ggtitle("Turbidité mensuelle 1997-2017")+
#  xlab("Year")+
#  ylab("Turbidité")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  

save(TabTurb4, file="data/satellite/Turbidity/Turb_serie.Rdata")



# Partitionnement

TabTurbnew<- pivot_wider(TabTurb2, names_from = Year, values_from = moyTurb)
TabTurbnew<- na.omit(TabTurbnew)
metaTabnew<- TabTurbnew %>% dplyr::select(x, y) %>% ungroup()
TabTurbnew<- TabTurbnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabTurbnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

TabTurb5<- TabTurb3 %>% ungroup() %>% dplyr::select(moyper)
#NbClust(TabTurb5, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  7

rect.hclust(tree, 7)
zones<- cutree(tree, 7)

zone<- Turb[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabTurb2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyTurb)) }

toto2Turb<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabTurbnew))
serie<- pivot_longer(serie, cols=c(4:24), names_to="Year", values_to = "Turb")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Turb=mean(Turb))

#ggserieTurb<-  ggplot(serie)+
#  geom_point(aes(x=Year,y=Turb,col=Clust))+
#  geom_line(aes(x=Year,y=Turb,col=Clust, group=Clust))+
#  theme_minimal()+
#  facet_wrap(.~Clust)

save(ggserieTurb, file="data/satellite/Turbidity/Turb_seriebyzone.Rdata")



# Trait de cote
  # 1st Polygon
liste <- with(toto2Turb, chull(x, y))
hull <- toto2Turb[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Turb<- ggplot(toto2Turb)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean Turbidity")+theme_minimal()+coord_fixed()+ggtitle("Turbidity")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3Turb<- toto2Turb
coordinates(toto3Turb)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3Turb) <- TRUE
  # coerce to raster
rasterTurb<- raster(toto3Turb)
rasterTurb
plot(rasterTurb, col= terrain.colors(7), main="Turbidity", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disturb<- disaggregate(rasterTurb, fact=(res(rasterTurb)/res(rasterchlnew)))
mTurb<- mask(disturb, res)
plot(mTurb)

save(mTurb, file="data/satellite/Turbidity/Turb_raster.Rdata")



# Polygons

polTurb<- rasterToPolygons(mTurb, dissolve=TRUE)
plot(polTurb, col=polTurb@data$Clust)

writeOGR(polTurb, dsn="data/satellite/Turbidity", layer="Turb", driver="ESRI Shapefile")

save(polTurb, file="data/satellite/Turbidity/Turb_polygons.Rdata")



# Mean turbidity / zone

summaryTurb<- toto2Turb %>% select(Clust, mean)
summaryTurb<- unique(summaryTurb)







