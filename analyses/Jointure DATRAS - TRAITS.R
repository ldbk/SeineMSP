setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP")
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(cowplot)

# Ouvrir PoissonsClusters.RData, BenthosClusters.RData et J2Datras.RData das l'environnement

# Mise en forme 3 tabs
J2<- J2 %>% dplyr::select(Year, moyLat, moyLong, ScientificName_WoRMS, Poids, Nombre, Superficie, DensityWgt, DensityNb)
names(J2)[4]<- "Species"

benthos<- benthos[, c(1, 3, 9,  2, 4, 5, 6, 7, 8)]
benthos$Cluster<- as.factor(benthos$Cluster)

fishtraitnew<- fishtraitnew[, c(1, 5, 10, 2, 3, 4, 6, 7, 8, 9)]
fishtraitnew$Cluster<- as.factor(fishtraitnew$Cluster)
names(fishtraitnew)[2]<- "MaturityAge"


# Jointure Benthos-Poissons
BenthoPoi<- fishtraitnew %>% bind_rows(fishtraitnew, benthos)
#length(which(unique(benthos$Species) %in% unique(fishtraitnew$Species)))
BenthoPoi<- unique(BenthoPoi)


# Jointure Datras-traits
Tot<- J2 %>% left_join(BenthoPoi, by= ("Species"))

sanstrait<- Tot %>% filter(is.na(Cluster))

save(Tot, file="C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/Jointure Datras-Traits.Rdata")


# Essai 1e méthode
Dens<- Tot %>% dplyr::select(Year, moyLong, moyLat, Cluster, DensityWgt, DensityNb) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(TotNb= mean(DensityNb), TotWgt= mean(DensityWgt)) %>% 
  ungroup()

# Essai 2e méthode
Dens2<- Tot %>% dplyr::select(Year, moyLong, moyLat, Cluster, Poids, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= mean(Nombre), Wgt= mean(Poids), Sup= mean(Superficie), TotNb=Nb/Sup, TotWgt= Wgt/Sup ) %>% 
  ungroup()




# Test carto

Jointtest<- Joint %>% dplyr::select(Year, moyLong, moyLat, Cluster)

rx<- range(Dens2$moyLong, na.rm=T)
ry<- range(Dens2$moyLat, na.rm=T)


{
Cluster1<- Dens2 %>% filter(Cluster==1)
Cluster2<- Dens2 %>% filter(Cluster==2)
Cluster3<- Dens2 %>% filter(Cluster==3)
Cluster4<- Dens2 %>% filter(Cluster==4)
Cluster5<- Dens2 %>% filter(Cluster==5)
Cluster6<- Dens2 %>% filter(Cluster==6)
Cluster7<- Dens2 %>% filter(Cluster==7)
Cluster8<- Dens2 %>% filter(Cluster==8)
}


C1<-ggplot(Cluster1)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  ggtitle("C1")+
  scale_color_viridis()+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C2<-ggplot(Cluster2)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C2")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C3<-ggplot(Cluster3)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C3")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C4<-ggplot(Cluster4)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C4")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C5<-ggplot(Cluster5)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C5")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C6<-ggplot(Cluster6)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C6")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")
C7<-ggplot(Cluster7)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  theme_minimal()+
  ggtitle("C7")+
  xlab("Longitude")+
  ylab("Latitude")
C8<-ggplot(Cluster8)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  ggtitle("C8")+
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")


plot_grid(C1, C2, C3, C4, C5, C6, C7, C8, ncol = 3, nrow = 3)


