library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(dplyr)
library(tidyr)
library(viridisLite)

sst<- stack("data/satellite/sst/IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1581929927261.nc")
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
Tabsst<- fortify.Raster(sst)
pixelok<- which(!is.na(apply(Tabsst,1,mean)))
Tabsst<- pivot_longer(Tabsst, cols=1:13232, names_to = "Secondes", values_to = "SST", values_drop_na = TRUE)

{
Tabsst$Secondes<- sub("values.X","",Tabsst$Secondes)
secsst<- as.numeric(Tabsst$Secondes)
Day0<-strptime("1981-01-01", format= "%Y-%m-%d")
Date<- Day0+secsst
Tabsst$Date <- Date
Tabsst$Year <- as.numeric(substr(as.character(Tabsst$Date),1,4))
Tabsst$Month<- as.numeric(substr(as.character(Tabsst$Date), 6,7))
Tabsst$Day  <- as.numeric(substr(as.character(Tabsst$Date), 9,10))
}


# Infos
mean(Tabsst$SST)
min(Tabsst$SST)
max(Tabsst$SST)
sd(Tabsst$SST)
var(Tabsst$SST)


# Mean SST per year
Tabsst2<- Tabsst %>% group_by(x,y,Year) %>% summarize(moySST= mean(SST))
ggplot(Tabsst2)+
  geom_tile(aes(x=x, y=y, fill=moySST))+
  ggtitle("SST moyenne 1981-2018")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="SST (°C)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

ggplot(Tabsst2, aes(x= Year, y=moySST, group=Year))+
  geom_boxplot()


# Mean SST 1981-2018
Tabsst3<- Tabsst2 %>% group_by(x,y) %>% summarize(moyper= mean(moySST))
ggplot(Tabsst3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("SST moyenne 1981-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="SST (°C)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean SST
Tabsst4<- Tabsst %>% group_by(Month) %>% summarize(moybaie= mean(SST))
ggplot(Tabsst4)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("SST mensuelle 1981-2018")+
  xlab("Month")+
  ylab("°C")+
  theme_minimal()





# Partitionnement

Tabsstnew<- pivot_wider(Tabsst2, names_from = Year, values_from = moySST)
metaTabnew<- Tabsstnew %>% dplyr::select(x, y)
Tabsstnew<- Tabsstnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabsstnew)
#distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)

zone<- sst[[1]]
values(zone)<- NA
zone[pixelok]<- zones
plot(zone, xlab="Longitude", ylab="Latitude")


# Raster
r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

r1<- raster::rasterize(metaTabnew, r0, fields=zones, fun=mean)
#plot(r1)

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabsst2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySST)) }

toto2sst<- left_join(toto, essai2, by="Clust")

ggplot(toto2sst)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean SST (°C)")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("SST")


save(toto2sst, file="data/satellite/sst/sst_ggplot.Rdata")











  


