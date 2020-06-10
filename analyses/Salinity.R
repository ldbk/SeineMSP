library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)

Sal<- nc_open("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")
Sal<- stack("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")


# Conversion raster
fortify.Raster <- function(Sal, maxPixel = 1000000) {
  
  if (ncell(Sal) > maxPixel) {
    x <- sampleRegular(Sal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Sal, seq_len(ncell(Sal)))
  out <- Sal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

# Traitement tableau
TabSal<- fortify(Sal)
pixelok<- which(!is.na(apply(TabSal,1,mean)))
TabSal<- pivot_longer(TabSal, cols=1:323, names_to = "Secondes", values_to = "Salinite", values_drop_na = TRUE)

{
  TabSal$Secondes<-sub("values.X","",TabSal$Secondes)
  TabSal$Secondes<-sub("e.0","e0",TabSal$Secondes)
  secsst<-as.numeric(TabSal$Secondes)
  Jour0<-strptime("1992-01-16", format= "%Y-%m-%d")
  Date<- Jour0+secsst
  TabSal$Date<- Date
  
  TabSal$Year <- as.numeric(substr(as.character(TabSal$Date),1,4))
  TabSal$Month<- as.numeric(substr(as.character(TabSal$Date), 6,7))
  TabSal$Day  <- as.numeric(substr(as.character(TabSal$Date), 9,10))
}
TabSal<- TabSal[, -c(3, 5)]


# Infos
mean(TabSal$Sal)
min(TabSal$Sal)
max(TabSal$Sal)
sd(TabSal$Sal)
var(TabSal$Sal)


# Salinite moy chaque annee (/ pixel)
TabSal2<- TabSal %>% group_by(x,y,Year) %>% summarize(moySal= mean(Salinite))
ggplot(TabSal2)+
  geom_tile(aes(x=x, y=y, fill=moySal))+
  ggtitle("Salinite moyenne 1992-2018")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# PP moy ens 1998-2018
TabSal3<- TabSal2 %>% group_by(x,y) %>% summarize(moyper= mean(moySal))
ggplot(TabSal3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Salinite moyenne 1992-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# PP moy chaque annee (/ baie)
TabSal4<- TabSal %>% group_by(Month) %>% summarize(moybaie= mean(Salinite))
ggplot(TabSal4)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Salinite mensuelle 1992-2018")+
  xlab("Month")+
  ylab("Salinité")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

TabSal2<- TabSal %>% group_by(x,y,Year) %>% summarize(moySal= mean(Salinite))
ggplot(TabSal2, aes(x= Year, y=moySal, group=Year))+
  geom_boxplot()






# Partitionnement

TabSalnew<- pivot_wider(TabSal2, names_from = Year, values_from = moySal)
metaTabnew<- TabSalnew %>% dplyr::select(x, y)
TabSalnew<- TabSalnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabSalnew)
distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)
print(zones)

zone<- Sal[[1]]
values(zone)<- NA
zone[pixelok]<- zones
plot(zone, xlab="Longitude", ylab="Latitude")


# Raster
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
plot(r1)

toto <- cbind(metaTabnew, Clust=factor(zones))
head(toto)

essai<- left_join(TabSal2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySal)) }

toto2Sal<- left_join(toto, essai2, by="Clust")

ggplot(toto2Sal)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean salinity")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Salinity")


save(toto2Sal, file="data/satellite/Salinity/Sal_ggplot.Rdata")













