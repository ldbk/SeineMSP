library(raster)
library(rasterVis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(dplyr)
library(tidyr)

Part<- stack("data/satellite/Particles/bbp443")


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
TabPart<- pivot_longer(TabPart, cols=1:244, names_to = "Date", values_to = "Particules", values_drop_na = TRUE)

{
TabPart$Date<- sub("values.index_","",TabPart$Date)
TabPart$Year<- as.numeric(substr(as.character(TabPart$Date),1,4))
TabPart$Month<- as.numeric(substr(as.character(TabPart$Date), 6,7))
}

# Infos
mean(TabPart$Particules)
min(TabPart$Particules)
max(TabPart$Particules)
sd(TabPart$Particules)
var(TabPart$Particules)


# Mean particles per year 
TabPart2<- TabPart %>% group_by(x,y,Year) %>% summarize(moyPart= mean(Particules))
ggplot(TabPart2)+
  geom_tile(aes(x=x, y=y, fill=moyPart))+
  ggtitle("PP moyenne 1998-2018")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

ggplot(TabPart2, aes(x=Year, y=moyPart, group=Year))+
  geom_boxplot()


# Mean particles 1998-2018
TabPart3<- TabPart2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPart))
ggplot(TabPart3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Particules moyenne 1997-2017")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mes particles
TabPart4<- TabPart %>% group_by(Month) %>% summarize(moybaie= mean(Particules))
ggplot(TabPart4)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Particules mensuelles 1997-2017")+
  xlab("Mois")+
  ylab("Concentration en particules")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  




# Partitionnement

TabPartnew<- pivot_wider(TabPart2, names_from = Year, values_from = moyPart)
TabPartnew<- na.omit(TabPartnew)
metaTabnew<- TabPartnew %>% dplyr::select(x, y)
TabPartnew<- TabPartnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabPartnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- Part[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")


# Raster
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)

toto <- cbind(metaTabnew, Clust=factor(zones))

TabPartnew<- bind_cols(TabPartnew, metaTabnew)
TabPartnew<- pivot_longer(TabPartnew, cols = 1:21, names_to = "Year", values_to = "moyPart")

essai<- left_join(TabPartnew, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPart)) }

toto2part<- left_join(toto, essai2, by="Clust")





#1st Polygon
liste <- with(toto2part, chull(x, y))
hull <- toto2part[liste, c("x", "y")]
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

Part<- ggplot(toto2part)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean Particles")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Particles")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

Part

save(Part, file="data/satellite/Particles/part_ggplot.Rdata")









