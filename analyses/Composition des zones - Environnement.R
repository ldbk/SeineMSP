library(raster)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

load("results/satellite/Coordzones.Rdata")
load("data/satellite/Chl/Tabchl2.Rdata")
load("data/satellite/Detritus/TabDet2.Rdata")
load("data/satellite/O2/TabO22.Rdata")
load("data/satellite/Particles/TabPart2.Rdata")
load("data/satellite/Primary production/TabPP2.Rdata")
load("data/satellite/Salinity/TabSal2.Rdata")
load("data/satellite/sst/Tabsst2.Rdata")
load("data/satellite/Turbidity/TabTurb2.Rdata")


# Conversion tata en SpatialPolygonsDataFrame
tata1<- tata
coordinates(tata1)<- ~ Long + Lat
gridded(tata1) <- TRUE
rastertata1<- raster(tata1)
poltata1<- rasterToPolygons(rastertata1, dissolve=TRUE)


# Conversion Tab2 en SpatialPointsDataFrame

{
  Tabchl2<- Tabchl2 %>% ungroup()
  Tabchl2SP<- SpatialPointsDataFrame(as.matrix(Tabchl2[,1:2]), Tabchl2)
  pipo1<- sp::over(Tabchl2SP, poltata1)
  
  finchl<- cbind(pipo1, Tabchl2)
  names(finchl)[1]<- "Zones finales"
  names(finchl)[4]<- "Année"
}
{
  TabDet2<- TabDet2 %>% ungroup()
  TabDet2SP<- SpatialPointsDataFrame(as.matrix(TabDet2[,1:2]), TabDet2)
  pipo1<- sp::over(TabDet2SP, poltata1)
  
  finDet<- cbind(pipo1, TabDet2)
  names(finDet)[1]<- "Zones finales"
  names(finDet)[4]<- "Année"
}
{
  TabO22<- TabO22 %>% ungroup()
  TabO22SP<- SpatialPointsDataFrame(as.matrix(TabO22[,1:2]), TabO22)
  pipo1<- sp::over(TabO22SP, poltata1)
  
  finO2<- cbind(pipo1, TabO22)
  names(finO2)[1]<- "Zones finales"
  names(finO2)[4]<- "Année"
}
{
  TabPart2<- TabPart2 %>% ungroup()
  TabPart2SP<- SpatialPointsDataFrame(as.matrix(TabPart2[,1:2]), TabPart2)
  pipo1<- sp::over(TabPart2SP, poltata1)
  
  finPart<- cbind(pipo1, TabPart2)
  names(finPart)[1]<- "Zones finales"
  names(finPart)[4]<- "Année"
}
{
  TabPP2<- TabPP2 %>% ungroup()
  
  # create SpatialPointsDataFrame
  coordinates(TabPP2)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
  gridded(TabPP2) <- TRUE
  # coerce to raster
  rasterPP<- raster(TabPP2)
  stackPP<- raster::stack(TabPP2, bands = 32)
  
  disPP<- disaggregate(rasterPP, fact=10)
  
  TabPP3<- as.data.frame(rasterToPoints(disPP))
  

  
  
  
  
  
  
  TabPP2SP<- SpatialPointsDataFrame(as.matrix(TabPP2[,1:2]), TabPP2)
  pipo1<- sp::over(TabPP2SP, poltata1)
  
  finPP<- cbind(pipo1, TabPP2)
  names(finPP)[1]<- "Zones_finales"
  names(finPP)[4]<- "Année"
}
{
  TabSal2<- TabSal2 %>% ungroup()
  TabSal2SP<- SpatialPointsDataFrame(as.matrix(TabSal2[,1:2]), TabSal2)
  pipo1<- sp::over(TabSal2SP, poltata1)
  
  finSal<- cbind(pipo1, TabSal2)
  names(finSal)[1]<- "Zones finales"
  names(finSal)[4]<- "Année"
}
{
  Tabsst2<- Tabsst2 %>% ungroup()
  Tabsst2SP<- SpatialPointsDataFrame(as.matrix(Tabsst2[,1:2]), Tabsst2)
  pipo1<- sp::over(Tabsst2SP, poltata1)
  
  finsst<- cbind(pipo1, Tabsst2)
  names(finsst)[1]<- "Zones finales"
  names(finsst)[4]<- "Année"
}
{
  TabTurb2<- TabTurb2 %>% ungroup()
  TabTurb2SP<- SpatialPointsDataFrame(as.matrix(TabTurb2[,1:2]), TabTurb2)
  pipo1<- sp::over(TabTurb2SP, poltata1)
  
  finTurb<- cbind(pipo1, TabTurb2)
  names(finTurb)[1]<- "Zones finales"
  names(finTurb)[4]<- "Année"
}




# Calculs séries temporelles pour chaque zone

{
  finchlbis<-  finchl %>% group_by(`Zones finales`, Année) %>% summarise("Chlorophylle a" = mean(moyChl))
  finchlbis<- na.omit(finchlbis)
  finDetbis<-  finDet %>% group_by(`Zones finales`, Année) %>% summarise("Détritus" = mean(moyDet))
  finDetbis<- na.omit(finDetbis)
  finO2bis<-   finO2 %>% group_by(`Zones finales`, Année) %>% summarise("Oxygène dissous" = mean(moyO2))
  finO2bis<- na.omit(finO2bis)
  finPartbis<- finPart %>% group_by(`Zones finales`, Année) %>% summarise("Particules" = mean(moyPart))
  finPartbis<- na.omit(finPartbis)
  finPPbis<-   finPP %>% group_by(`Zones finales`, Année) %>% summarise("Production primaire" = mean(moyPP))
  finPPbis<- na.omit(finPPbis)
  finSalbis<-  finSal %>% group_by(`Zones finales`, Année) %>% summarise("Salinité" = mean(moySal))
  finSalbis<- na.omit(finSalbis)
  finsstbis<-  finsst %>% group_by(`Zones finales`, Année) %>% summarise("Température de surface" = mean(moySST))
  finsstbis<- na.omit(finsstbis)
  finTurbbis<- finTurb %>% group_by(`Zones finales`, Année) %>% summarise("Turbidité" = mean(moyTurb))
  finTurbbis<- na.omit(finTurbbis)
}
{
  fintot<- finchlbis %>% left_join(finDetbis, by= c("Zones finales", "Année")) # 1 pixel de la Zone 1 a en 1988 une densité de com I égale à 4.08  
  fintot<- fintot %>% left_join(finO2bis, by= c("Zones finales", "Année"))
  fintot<- fintot %>% left_join(finPartbis, by= c("Zones finales", "Année"))
  fintot<- fintot %>% left_join(finPPbis, by= c("Zones finales", "Année"))
  fintot<- fintot %>% left_join(finSalbis, by= c("Zones finales", "Année"))
  fintot<- fintot %>% left_join(finsstbis, by= c("Zones finales", "Année"))
  fintot<- fintot %>% left_join(finTurbbis, by= c("Zones finales", "Année"))

  fintot<- pivot_longer(fintot, cols = 3:10, names_to = "Paramètre", values_to = "Moyenne")
  fintot$`Zones finales`<- sub("1", "Zone 1", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("2", "Zone 2", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("3", "Zone 3", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("4", "Zone 4", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("5", "Zone 5", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("6", "Zone 6", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("7", "Zone 7", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("8", "Zone 8", fintot$`Zones finales`)
  fintot$`Zones finales`<- sub("9", "Zone 9", fintot$`Zones finales`)
}



# Séries tempo de chaque param pour chaque zone finale

chlseriesbyzone<- ggplot(finchlbis)+
  geom_line(aes(x=Année, y= `Chlorophylle a`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Chlorophylle a")+
  ylab("mg/m3")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="pink", size=1.1, linetype="solid"))

ggsave(plot= chlseriesbyzone, filename="chlseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

Detseriesbyzone<- ggplot(finDetbis)+
  geom_line(aes(x=Année, y= `Détritus`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Détritus")+
  ylab("m-1")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="cyan4", size=1.1, linetype="solid"))+
  theme(strip.text.x = element_text(size=12, color="white"))

ggsave(plot= Detseriesbyzone, filename="Detseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

O2seriesbyzone<- ggplot(finO2bis)+
  geom_line(aes(x=Année, y= `Oxygène dissous`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Oxygène dissous")+
  ylab("mmol/m3")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#CCCCFF", size=1.1, linetype="solid"))

ggsave(plot= O2seriesbyzone, filename="O2seriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

Partseriesbyzone<- ggplot(finPartbis)+
  geom_line(aes(x=Année, y= `Particules`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Particules")+
  ylab("m-1")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#FFFFCC", size=1.1, linetype="solid"))

ggsave(plot= Partseriesbyzone, filename="Partseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

PPseriesbyzone<- ggplot(finPPbis)+
  geom_line(aes(x=Année, y= `Production primaire`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Production primaire")+
  ylab("mg C/m3/j")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#CCFFCC", size=1.1, linetype="solid"))

ggsave(plot= PPseriesbyzone, filename="PPseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

Salseriesbyzone<- ggplot(finSalbis)+
  geom_line(aes(x=Année, y= `Salinité`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Salinité")+
  ylab("1E-3")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#CCCCCC", size=1.1, linetype="solid"))

ggsave(plot= Salseriesbyzone, filename="Salseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

sstseriesbyzone<- ggplot(finsstbis)+
  geom_line(aes(x=Année, y= `Température de surface`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Température de surface")+
  ylab("°C")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#FFCCCC", size=1.1, linetype="solid"))

ggsave(plot= sstseriesbyzone, filename="sstseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)

Turbseriesbyzone<- ggplot(finTurbbis)+
  geom_line(aes(x=Année, y= `Turbidité`))+
  facet_grid(`Zones finales`~ .)+
  theme_bw()+
  ggtitle("Turbidité")+
  ylab("m-1")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(strip.background = element_rect(colour="black", fill="#0099CC", size=1.1, linetype="solid"))+
  theme(strip.text.x = element_text(size=12, color="white"))

ggsave(plot= Turbseriesbyzone, filename="Turbseriesbyzone.jpeg", path="results/satellite/zones/Régionalisation finale", width = 13, height = 8)




# Boxplots de chaque param pour chaque zone finale

fintot$`Zones finales`<- sub("Zone 1", "1", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 2", "2", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 3", "3", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 4", "4", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 5", "5", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 6", "6", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 7", "7", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 8", "8", fintot$`Zones finales`)
fintot$`Zones finales`<- sub("Zone 9", "9", fintot$`Zones finales`)


CHL<- ggplot(fintot[fintot$Paramètre=="Chlorophylle a",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Chlorophylle a")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= CHL, filename="CHL.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

DET<- ggplot(fintot[fintot$Paramètre=="Détritus",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Détritus")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= DET, filename="DET.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

O2<- ggplot(fintot[fintot$Paramètre=="Oxygène dissous",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Oxygène dissous")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= O2, filename="O2.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

PART<- ggplot(fintot[fintot$Paramètre=="Particules",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Particules")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= PART, filename="PART.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

PP<- ggplot(fintot[fintot$Paramètre=="Production primaire",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Production primaire")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= PP, filename="PP.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

SAL<- ggplot(fintot[fintot$Paramètre=="Salinité",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Salinité")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= SAL, filename="SAL.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

SST<- ggplot(fintot[fintot$Paramètre=="Température de surface",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Température de surface")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= SST, filename="SST.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)

TURB<- ggplot(fintot[fintot$Paramètre=="Turbidité",])+
  geom_boxplot(aes(x=`Zones finales`, y=Moyenne, fill= as.numeric(`Zones finales`)))+
  ggtitle("Turbidité")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 10))+
  theme(axis.text.x= element_text(size= 20))+
  xlab("Zone")+
  scale_fill_gradientn(colours =brewer.pal(n = 6, name = "YlOrBr"))

ggsave(plot= TURB, filename="TURB.jpeg", path="results/satellite/zones/Boxplot", width = 13, height = 8)





# Essai


polfinfort<- fortify(poltata1)

ggplot(na.omit(finchl))+
  geom_tile(aes(x=x, y=y, fill=moyChl))+
  geom_polygon(data=polfinfort, aes(x=long, y=lat, group=group) ,fill=NA, col="black")+
  theme_minimal()














