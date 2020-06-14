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

O2<- nc_open("data/satellite/O2/MetO-NWS-BIO-dm-DOXY_1583828769643.nc")
O2<- stack("data/satellite/O2/MetO-NWS-BIO-dm-DOXY_1583828769643.nc")


# Conversion raster - tableau
fortify.Raster <- function(O2, maxPixel = 1000000) {
  
  if (ncell(O2) > maxPixel) {
    x <- sampleRegular(O2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(O2, seq_len(ncell(O2)))
  out <- O2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabO2<- fortify(O2)
pixelok<- which(!is.na(apply(TabO2,1,mean)))
TabO2<- pivot_longer(TabO2, cols=1:7669, names_to = "Secondes", values_to = "O2", values_drop_na = TRUE)

{
  TabO2$Secondes<-sub("values.X","",TabO2$Secondes)
  TabO2$Secondes<-sub("e.0","e0",TabO2$Secondes)
  sec<-as.numeric(TabO2$Secondes)
  Day0<-strptime("1998-01-01", format= "%Y-%m-%d")
  Date<- Day0+sec
  TabO2$Date<- Date
  
  TabO2$Year<- as.numeric(substr(as.character(TabO2$Date),1,4))
  TabO2$Month<- as.numeric(substr(as.character(TabO2$Date), 6,7))
  TabO2$Day<- as.numeric(substr(as.character(TabO2$Date), 9,10))
}

#length(TabO2$O2[TabO2$O2<0])
#NEG<- TabO2 %>% filter(O2<0) %>% select(x, y, O2, Year)
#ggplot(NEG)+ geom_point(aes(x=x, y=y))+ facet_wrap(.~Year)
TabO2<- TabO2 %>% filter(O2>0)


# Infos
mean(TabO2$O2)
min(TabO2$O2)
max(TabO2$O2)
sd(TabO2$O2)
var(TabO2$O2)


# Mean O2 per year
TabO22<- TabO2 %>% group_by(x,y,Year) %>% summarize(moyO2= mean(O2))
ggplot(TabO22)+
  geom_tile(aes(x=x, y=y, fill=moyO2))+
  ggtitle("O2 moyen 1998-2018")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mmol/m3")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

ggplot(TabO22, aes(x=Year, y=moyO2m, group=Year))+
  geom_boxplot()


# Mean O2 1998-2018
TabO23<- TabO22 %>% group_by(x,y) %>% summarize(moyper= mean(moyO2))
ggplot(TabO23)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("O2 moyen 1998-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mmol/m3")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean 02
TabO24<- TabO2 %>% group_by(Year) %>% summarize(moybaie= mean(O2))
ggplot(TabO24)+
  geom_line(aes(x=Year, y=moybaie))+
  ggtitle("O2 annuel 1998-2018")+
  xlab("Year")+
  ylab("mmol/m3")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

save(TabO24, file="data/satellite/O2/O2_serie.Rdata")



# Partitionnement

TabO2new<- pivot_wider(TabO22, names_from = Year, values_from = moyO2)
TabO2new<- na.omit(TabO2new)
metaTabnew<- TabO2new %>% dplyr::select(x, y)
TabO2new<- TabO2new %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabO2new)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- O2[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabO22, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyO2)) }

toto2O2<- left_join(toto, essai2, by="Clust")



#1st Polygon
liste <- with(toto2O2, chull(x, y))
hull <- toto2O2[liste, c("x", "y")]
Poly <- Polygon(hull)

#Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

#Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)
PolyCut <- fortify(res)


#Put polygon in good format for later use
tete <- PolyCut[PolyCut$piece==1,]
db.poly <- polygon.create(tete[,c(1,2)])

O2<- ggplot(toto2O2)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean O2")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("O2")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

O2



# Raster

r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3O2<- toto2O2
coordinates(toto3O2)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3O2) <- TRUE
  # coerce to raster
rasterO2<- raster(toto3O2)
rasterO2
plot(rasterO2, col= terrain.colors(5), main="O2", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disO2<- disaggregate(rasterO2, fact=(res(rasterO2)/res(rasterchlnew)))
mO2<- mask(disO2, res)
plot(mO2)
#disO2<- overlay(disO2,)
#plot(disO2)


save(mO2, file="data/satellite/O2/O2_raster.Rdata")







