library(rgdal)
library(rgeos)
library(RGeostats)
library(raster)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(GISTools)

load("data/PolyCut.Rdata")

#Hab<- readOGR(dsn="data/Habitats/Habitats_BdS.gpkg")          # SpatialPolygonsDataFrame
load("data/Habitats/Hab_crop.Rdata")
Hab<- out

ggHab<- Hab
ggHab@data$id<- row.names(ggHab@data)                         # Créé nvelle colonne dans laquelle mttre la donnée car ggHab n'a que les polygones

ptHab<- fortify(ggHab, region = "id")                         # Conversion SPDF en dataframe que ggplot peut lire
dfHab<- merge(ptHab, ggHab@data, by="id")                     # Associe les données avec les polygones

dfHab<- dfHab %>% filter(lat<=49.8)                           # Même latitude max que dans Datras donc dans Krigeage donc dans Com
names(dfHab)[27]<- "Substrat"

dfHab$Substrat<- sub("Coarse sediment",              "Sédiment grossier",     dfHab$Substrat)
dfHab$Substrat<- sub("Mixed sediment",               "Sédiment hétérogène",   dfHab$Substrat)
dfHab$Substrat<- sub("Rock or other hard substrata", "Roche et substrat dur", dfHab$Substrat)
dfHab$Substrat<- sub("Sandy mud to muddy sand",      "Vase sableuse",         dfHab$Substrat)
dfHab$Substrat<- sub("Sand",                         "Sable",                 dfHab$Substrat)
dfHab$Substrat<- sub("Seabed",                       "Fond marin",            dfHab$Substrat)

ggHabitats<- ggplot(dfHab)+
  geom_polygon(aes(x=long, y=lat, group=group, fill= Substrat))+
  scale_fill_manual(values= c("#D53E4F", "#FC8D59", "#FEE08B", "#E6F598", "#99D594", "#3288BD"))+
  theme_minimal()+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(legend.title = element_text(size = 20))+
  theme(legend.text = element_text(size = 15))#+
  #coord_sf(xlim= range(dfHab$long),ylim= c(49.2, 49.7))
  
save(ggHabitats, file="results/Habitats/Habitats.Rdata")
ggsave(plot= ggHabitats, filename="ggHabitats.jpeg", path="results/Habitats/", width = 13, height = 8)



# HELP : http://mazamascience.com/WorkingWithData/?p=1494




