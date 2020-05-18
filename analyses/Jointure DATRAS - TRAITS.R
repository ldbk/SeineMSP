setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP")
library(dplyr)
library(ggplot2)
library(viridis)
library(gridExtra)
library(cowplot)
library(raster)

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
save(Tot, file="C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/Jointure Datras-Traits.Rdata")

sanstrait<- Tot %>% filter(is.na(Cluster)) %>% dplyr::select(Species)
sanstrait<- unique(sanstrait)
write.csv(sanstrait, file="C:/Users/jrivet/Documents/Stage M2/Data/CGFS/sanstrait.csv")
sanstrait2<- read.csv("C:/Users/jrivet/Documents/Stage M2/Data/CGFS/sanstrait_Worms.csv", sep=";")


{
  nbid<- sanstrait2 %>%
    summarise_all(n_distinct) %>%
    t()
  nbNA<- function(a){a[a==""]<-NA;sum(is.na(a))}
  nbNA<- sanstrait2 %>%
    summarise_all(nbNA) %>%
    t()
  data.frame(nbid=nbid,nbNA=nbNA)
  
  summary<- data.frame(nbid=nbid,nbNA=nbNA)
}



# Essai 1e methode
Dens<- Tot %>% dplyr::select(Year, moyLong, moyLat, Cluster, DensityWgt, DensityNb) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(TotNb= mean(DensityNb), TotWgt= mean(DensityWgt)) %>% 
  ungroup()

# Essai 2e methode
Dens2<- Tot %>% dplyr::select(Year, moyLong, moyLat, Cluster, Poids, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= mean(Nombre), Wgt= mean(Poids), Sup= mean(Superficie), TotNb=Nb/Sup, TotWgt= Wgt/Sup ) %>% 
  ungroup()

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


# Test carto

rx<- range(Dens2$moyLong, na.rm=T)
ry<- range(Dens2$moyLat, na.rm=T)

ggplot(Dens2)+
  geom_point(aes(x= moyLong, y= moyLat, size=TotNb))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  facet_wrap(.~ Cluster)
  theme_minimal()+
  xlab("Longitude")+
  ylab("Latitude")




# Essai raster
 
essairaster<- raster(nrow=11, ncol=66, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)


#res(essairaster)<- 0.04

projection(essairaster)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

values<- setValues(essairaster, Cluster1$TotNb)

r1<- rasterize(Cluster1[,4:3], essairaster, fields=values, fun=sum)





