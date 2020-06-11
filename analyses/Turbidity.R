library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)

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

TabTurb$Date<-sub("values.index_","",TabTurb$Date)
TabTurb$Year <- as.numeric(substr(as.character(TabTurb$Date),1,4))
TabTurb$Month<- as.numeric(substr(as.character(TabTurb$Date), 6,7))


# Infos Turb
mean(TabTurb$Turbidity)
min(TabTurb$Turbidity)
max(TabTurb$Turbidity)
sd(TabTurb$Turbidity)
var(TabTurb$Turbidity)


# Mean turb per year
TabTurb2<- TabTurb %>% group_by(x,y,Year) %>% summarize(moyTurb= mean(Turbidity))
ggplot(TabTurb2)+
  geom_tile(aes(x=x, y=y, fill=moyTurb))+
  ggtitle("Turbidité moyenne 1997-2017")+
  facet_wrap(. ~Month)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

TabTurb2<- TabTurb %>% group_by(x,y,Year) %>% summarize(moyTurb= mean(Turbidity))
ggplot(TabTurb2, aes(x=Year, y=moyTurb, group=Year))+
  geom_boxplot()


# Mean turb 1997-2017
TabTurb3<- TabTurb2 %>% group_by(x,y) %>% summarize(moyper= mean(moyTurb))
ggplot(TabTurb3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Turbidité moyenne 1997-2017")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean turb
TabTurb4<- TabTurb %>% group_by(Month) %>% summarize(moybaie= mean(Turbidity))
ggplot(TabTurb4)+
  geom_line(aes(x=Month, y=moybaie))+
  ggtitle("Turbidité mensuelle 1997-2017")+
  xlab("Month")+
  ylab("Turbidité")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  





# Partitionnement

TabTurbnew<- pivot_wider(TabTurb2, names_from = Year, values_from = moyTurb)
TabTurbnew<- na.omit(TabTurbnew)
metaTabnew<- TabTurbnew %>% dplyr::select(x, y)
TabTurbnew<- TabTurbnew %>% ungroup() %>% dplyr::select(-x, -y)


distance<- dist(TabTurbnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- Turb[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")


# Raster
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)

toto <- cbind(metaTabnew, Clust=factor(zones))

TabTurbnew<- bind_cols(TabTurbnew, metaTabnew)
TabTurbnew<- pivot_longer(TabTurbnew, cols = 1:21, names_to = "Year", values_to = "moyTurb")

essai<- left_join(TabTurbnew, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyTurb)) }

toto2Turb<- left_join(toto, essai2, by="Clust")




#1st Polygon
liste <- with(toto2Turb, chull(x, y))
hull <- toto2Turb[liste, c("x", "y")]
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

Turb<- ggplot(toto2Turb)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean Turbidity")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Turbidity")+
  geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")

Turb

save(Turb, file="data/satellite/Turbidity/Turb_ggplot.Rdata")









