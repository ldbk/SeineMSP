library(rgdal)
library(rgeos)
library(RGeostats)
library(raster)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

Hab<- readOGR(dsn="data/Habitats/Habitats_BdS.gpkg")          # SpatialPolygonsDataFrame

ggHab<- Hab
ggHab@data$id<- row.names(ggHab@data)                         # Créé nvelle colonne dans laquelle mttre la donnée car ggHab n'a que les polygones

ptHab<- fortify(ggHab, region = "id")                         # Conversion SPDF en dataframe que ggplot peut lire
dfHab<- merge(ptHab, ggHab@data, by="id")                     # Associe les données avec les polygones

dfHab<- dfHab %>% filter(lat<=49.8)                           # Même latitude max que dans Datras donc dans Krigeage donc dans Com
names(dfHab)[27]<- "Substrat"


ggHabitats<- ggplot(dfHab)+
  geom_polygon(aes(x=long, y=lat, group=group, fill= Substrat))+
  scale_fill_manual(values= c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD"))+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 12))

ggsave(plot= ggHabitats, filename="ggHabitats.jpeg", path="results/Habitats/", width = 13, height = 8)

  



# HELP : http://mazamascience.com/WorkingWithData/?p=1494