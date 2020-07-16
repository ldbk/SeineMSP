library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

load("results/satellite/Coordzones.Rdata")

load("data/satellite/Chl/Tabchl2.Rdata")
load("data/satellite/Turbidity/TabTurb2.Rdata")
load("data/satellite/Detritus/TabDet2.Rdata")

load("data/Polycut.Rdata")


ggplot(tata)+
  geom_tile(aes(x=Long,y=Lat,fill= as.numeric(Clust)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr")) +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()


# CHL

TabCHL<- pivot_wider(Tabchl2, names_from = year, values_from = moyChl )
rasterchl<- rasterFromXYZ(TabCHL)
zCHL<- raster::extract(rasterchl, SpatialPoints(tata[,1:2]))
newCHL<- tata %>% cbind(zCHL)
newCHL<- pivot_longer(newCHL, cols = 4:26, names_to = "Year")
new2CHL<- newCHL %>% mutate(Year= as.numeric(sub("X", "", Year))) %>%  group_by(Clust, Year) %>% summarize(moyChl= mean(value))
CHL<- ggplot(new2CHL)+
  geom_boxplot(aes(x=Clust, y=moyChl, fill= as.numeric(Clust)))+
  ggtitle("Chlorophylle")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr"))

ggsave(plot= CHL, filename="CHL.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)



# Turb

TabTURB<- pivot_wider(TabTurb2, names_from = Year, values_from = moyTurb )
rasterturb<- rasterFromXYZ(TabTURB)
zTURB<- raster::extract(rasterturb, SpatialPoints(tata[,1:2]))
newTURB<- tata %>% cbind(zCHL)
newTURB<- pivot_longer(newTURB, cols = 4:26, names_to = "Year")
new2TURB<- newTURB %>% mutate(Year= as.numeric(sub("X", "", Year))) %>%  group_by(Clust, Year) %>% summarize(moyTurb= mean(value))
TURB<- ggplot(new2TURB)+
  geom_boxplot(aes(x=Clust, y=moyTurb, fill= as.numeric(Clust) ))+
  ggtitle("Turbidité")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr"))
  

ggsave(plot= TURB, filename="TURB.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)



# Det

TabDET<- pivot_wider(TabDet2, names_from = Year, values_from = moyDet )
rasterdet<- rasterFromXYZ(TabDET)
zDET<- raster::extract(rasterdet, SpatialPoints(tata[,1:2]))
newDET<- tata %>% cbind(zDET)
newDET<- pivot_longer(newDET, cols = 4:26, names_to = "Year")
new2DET<- newDET %>% mutate(Year= as.numeric(sub("X", "", Year))) %>%  group_by(Clust, Year) %>% summarize(moyDet= mean(value))
DET<- ggplot(new2DET)+
  geom_boxplot(aes(x=Clust, y=moyDet, fill= as.numeric(Clust) ))+
  ggtitle("Detritus")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr"))

ggsave(plot= DET, filename="DET.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)
+
  
  
  
  
  
  
  
  
  
  
  



















































