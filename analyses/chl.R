library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)
library(viridisLite)

chl<- stack("data/satellite/chl/chl")

# Conversion raster
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
Tabchl<- pivot_longer(Tabchl, cols=1:240, names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)


{
  Tabchl$Date<- sub("values.index_","",Tabchl$Date)
  Tabchl$year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
  Tabchl$month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
}


# Infos
mean(Tabchl$Chloro)
min(Tabchl$Chloro)
max(Tabchl$Chloro)
sd(Tabchl$Chloro)
var(Tabchl$Chloro)


# Mean chl per year
Tabchl2<- Tabchl %>% group_by(x,y,year) %>% summarize(moyChl= mean(Chloro))
ggplot(Tabchl2)+
  geom_tile(aes(x=x, y=y, fill=moyChl))+
  ggtitle("Chl moyenne 1997-2017")+
  facet_wrap(. ~ year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="(?g/L)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

ggplot(Tabchl2, aes(x= year, y=moyChl, group=year))+
  ggtitle("Chlorophylle 1997-2017")+
  ylab("?g/L")+
  geom_boxplot()


# Mean chl 1997-2017
Tabchl3<- Tabchl2 %>% group_by(x,y) %>% summarize(moyper= mean(moyChl))
ggplot(Tabchl3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Chlorophylle moyenne 1997-2017")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="?g/L")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))


# Serie tempo mean chl
Tabchl4<- Tabchl %>% group_by(year) %>% summarize(moybaie= mean(Chloro))
ggplot(Tabchl4)+
  geom_point(aes(x= year, y= moybaie))+
  ggtitle("Chlorophylle moyenne annuelle 1997-2017")+
  xlab("Year")+
  ylab("?g/L")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

save(Tabchl4, file="data/satellite/chl/chl_serie.Rdata")



# Partitionnement

Tabchlnew<- pivot_wider(Tabchl2, names_from = year, values_from = moyChl)
Tabchlnew<- na.omit(Tabchlnew)
metaTabnew<- Tabchlnew %>% dplyr::select(x, y)
Tabchlnew<- Tabchlnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabchlnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- chl[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

Tabchlnew<- bind_cols(Tabchlnew, metaTabnew)
Tabchlnew<- pivot_longer(Tabchlnew, cols = 1:21, names_to = "year", values_to = "moyChl")

essai<- left_join(Tabchlnew, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyChl)) }

toto2chl<- left_join(toto, essai2, by="Clust")




#1st Polygon
liste <- with(toto2chl, chull(x, y))
hull <- toto2chl[liste, c("x", "y")]
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

Chl<- ggplot(toto2chl)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean chl")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Chlorophyll")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

Chl



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
plot(rasterchlnew, main="Chl", xlab="Longitude", ylab="Latitude")

save(rasterchlnew, file="data/satellite/chl/chl_raster.Rdata")


# old

#r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)



# essai changement resolution

# res: 0.01098639, 0.0109864
#obj:res 0.011, 0.011 --> donc disaggregate

#xres(rasterchlnew)
#yres(rasterchlnew)

#res(rasterchlnew)<- c((0.011/xres(rasterchlnew)), (0.011/yres(rasterchlnew)))


#rasterchlessai<- disaggregate(rasterchlnew, fact=c(0.011/xres(rasterchlnew), 0.011/yres(rasterchlnew)))




