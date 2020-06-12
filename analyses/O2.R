library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)

O2<- nc_open("data/O2/MetO-NWS-BIO-dm-DOXY_1591690331863.nc")
O2<- stack("data/O2/MetO-NWS-BIO-dm-DOXY_1591690331863.nc")


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


# Infos
mean(TabO2$O2)
min(TabO2$O2)
max(TabO2$O2)
sd(TabO2$O2)
var(TabO2$O2)

print(TabO2$O2[TabO2$O2<0]) # PB


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

TabO25<- TabO2 %>% group_by(x,y,Month) %>% summarize(moyO2m= mean(O2))
ggplot(TabO25, aes(x=Month, y=moyO2m, group=Month))+
  ggtitle("Oxygene dissous 1998-2018")+
  ylab("mmol/m3")+
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





# Partitionnement

TabO2new<- pivot_wider(TabO22, names_from = Year, values_from = moyO2)
metaTabnew<- TabO2new %>% dplyr::select(x, y)
TabO2new<- TabO2new %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabO2new)
distance[1:5]

tree<- hclust(distance)
plot(tree)

rect.hclust(tree, 5)
zones<- cutree(tree, 5)
print(zones)

zone<- O2[[1]]
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

essai<- left_join(TabO22, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyO2)) }

toto2<- left_join(toto, essai2, by="Clust")

ggplot(toto2)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean O2")+
  theme_minimal()+
  coord_fixed()











