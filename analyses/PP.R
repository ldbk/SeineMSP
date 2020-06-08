library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)

PP<- nc_open("data/satellite/Primary production/PP 1998-2018.nc")
PP<- stack("data/satellite/Primary production/PP 1998-2018.nc")


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
TabPP$Secondes<-sub("e.0","e0",TabPP$Secondes)
secsst<-as.numeric(TabPP$Secondes)
Jour0<-strptime("1998-01-01", format= "%Y-%m-%d")
TabPP$Date<- Jour0+secsst
}
{
TabPP$Year <- as.numeric(substr(as.character(TabPP$Date),1,4))
TabPP$Month<- as.numeric(substr(as.character(TabPP$Date), 6,7))
TabPP$Day<- as.numeric(substr(as.character(TabPP$Date), 9,10))
}
TabPP<- TabPP[, -c(3, 5)]


# Infos
mean(TabPP$PP)
min(TabPP$PP)
max(TabPP$PP)
sd(TabPP$PP)
var(TabPP$PP)


# Mean PP per year
TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(TabPP2)+
  geom_tile(aes(x=x, y=y, fill=moyPP))+
  ggtitle("PP moyenne 1998-2018")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="PP (mg C/m3/j)")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  


# Mean PP 1998-2018
TabPP3<- TabPP2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPP))
ggplot(TabPP3)+
  geom_tile(aes(x=x, y=y, fill= moyper))+
  ggtitle("Production Primaire moyenne 1998-2018")+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mg C/m3/j")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

setwd("C:/Users/jrivet/Documents/Stage M2/Data/PP")

save(TabPP3, file = "CartePP.RData")


# Serie tempo mean PP
TabPP4<- TabPP %>% group_by(Month) %>% summarize(moybaie= mean(PP))
ggplot(TabPP4)+
geom_line(aes(x= Month, y= moybaie))+
  ggtitle("PP mensuelle 1998-2018")+
  xlab("Mois")+
  ylab("mg C/m3/j")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))  

TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(Tab2, aes(x= Year, y=moyPP, group=Year))+
  ggtitle("Production primaire 1998-2018")+
  ylab("mg C/m3/j")+
  geom_boxplot()





# Partitionnement

TabPPnew<- pivot_wider(TabPP2, names_from = Year, values_from = moyPP)
metaTabnew<- TabPPnew %>% dplyr::select(x, y)
TabPPnew<- TabPPnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabPPnew)
distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)
print(zones)

zone<- PP[[1]]
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

essai<- left_join(TabPP2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPP)) }

toto2<- left_join(toto, essai2, by="Clust")

ggplot(toto2)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean PP (mg C/m3/j)")+
  theme_minimal()+
  coord_fixed()





