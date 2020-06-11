library(raster)
library(rasterVis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(dplyr)
library(tidyr)

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
mean(TabDet$Detritus)
min(TabDet$Detritus)
max(TabDet$Detritus)
sd(TabDet$Detritus)
var(TabDet$Detritus)


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

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- Detrit[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

TabDetnew<- bind_cols(TabDetnew, metaTabnew)
TabDetnew<- pivot_longer(TabDetnew, cols = 1:21, names_to = "Year", values_to = "moyDet")

essai<- left_join(TabDetnew, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyDet)) }

toto2Det<- left_join(toto, essai2, by="Clust")



#1st Polygon
liste <- with(toto2Det, chull(x, y))
hull <- toto2Det[liste, c("x", "y")]
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

Det<- ggplot(toto2Det)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Detritus")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

Det

save(Det, file="data/satellite/Detritus/Det_ggplot.Rdata")



# Raster
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)









# Essai ggplot - polygone

toto2Det<- toto2Det %>% ungroup()
coord<- toto2Det %>% dplyr::select(x, y, Clust)
values<- toto2Det %>% dplyr::select(Clust, mean)

datapoly<- merge(values, coord, by=c("Clust"))


p<- ggplot(datapoly, aes(x=x,y=y))+
  geom_polygon(aes(fill=mean,group=Clust))

p









# Raster 

metaTabutile<- toto2Det %>% dplyr::select(x, y)
clust<- toto2Det[,3]

r02<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r12<- raster::rasterize(metaTabutile, r0, fields=clust, fun=mean)
plot(r12)








