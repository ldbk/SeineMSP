setwd("C:/Users/jrivet/Documents/Stage M2/Data/SST")
library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyr)
library(viridisLite)
sst<- stack("IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1581929927261.nc")


sst<- sst-275.15


# Conversion raster - tableau
fortify.Raster <- function(sst, maxPixel = 1000000) {
  
  if (ncell(sst) > maxPixel) {
    x <- sampleRegular(sst, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(sst, seq_len(ncell(sst)))
  out <- sst %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tab<- fortify.Raster(sst)
pixelok<- which(!is.na(apply(Tab,1,mean)))
Tab<- pivot_longer(Tab, cols=1:13232, names_to = "Secondes", values_to = "SST", values_drop_na = TRUE)

{
Tab$Secondes<-sub("values.X","",Tab$Secondes)
Tab$Secondes<-sub("e.0","e0",Tab$Secondes)
secsst<-as.numeric(Tab$Secondes)
Day0<-strptime("1981-01-01", format= "%Y-%m-%d")
Date<- Day0+secsst
Tab$Date<- Date
Tab$Year <- as.numeric(substr(as.character(Tab$Date),1,4))
Tab$Month<- as.numeric(substr(as.character(Tab$Date), 6,7))
Tab$Day<- as.numeric(substr(as.character(Tab$Date), 9,10))
}


# Infos SST
mean(Tab$SST)
min(Tab$SST)
max(Tab$SST)
sd(Tab$SST)
var(Tab$SST)


# SST moy every year (/ pixel)
Tab2<- Tab %>% group_by(x,y,Year) %>% summarize(moySST= mean(SST))
ggplot(Tab2)+
  geom_tile(aes(x=x, y=y, fill=moySST))+
  ggtitle("SST moyenne 1981-2018")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="SST (°C)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# SST moy ens 1981-2018
Tab3<- Tab2 %>% group_by(x,y) %>% summarize(moyper= mean(moySST))
ggplot(Tab3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("SST moyenne 1981-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="SST ?C")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo SST moy (/ baie)
Tab4<- Tab %>% group_by(Month) %>% summarize(moybaie= mean(SST))
ggplot(Tab4)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("SST mensuelle 1981-2018")+
  xlab("Month")+
  ylab("°C")+
  theme_minimal()

Tab2<- Tab %>% group_by(x,y,Year) %>% summarize(moySST= mean(SST))
ggplot(Tab2, aes(x= Year, y=moySST, group=Year))+
  geom_boxplot()




# Partitionnement

Tab2new<- pivot_wider(Tab2, names_from = Year, values_from = moySST)
metaTab2new<- Tab2new %>% select(x, y)
Tab2new<- Tab2new %>% ungroup() %>% select(-x, -y)

# Calcul des distances entre les pixels pour pouvoir les regrouper par zones
distance<- dist(Tab2new)
distance[1:5]


# Construction dendro
arbreclassif<- hclust(distance)
plot(arbreclassif)


# Partitionnement dendro 
rect.hclust(arbreclassif, 5)
zonespixel<-cutree(arbreclassif, 5)
print(zonespixel)

zone<- sst[[1]]
values(zone)<-NA
zone[pixelok]<-zonespixel
plot(zone, xlab="Longitude", ylab="Latitude")


library(raster)
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTab2new, r0, fields=zonespixel, fun=mean)
plot(r1)

toto <- cbind(metaTab2new,Clust=factor(zonespixel))
head(toto)

ggplot(toto)+
  geom_tile(aes(x=x,y=y,fill=Clust))+theme_minimal()+coord_fixed()
