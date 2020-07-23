library(dplyr)
library(ggplot2)

load("data/krigeage log.Rdata")
load("results/Communautes bio/Zones/Tabttpixel.Rdata")

# Conversion tata en SpatialPolygonsDataFrame
tata1<- tata
coordinates(tata1)<- ~ Long + Lat
gridded(tata1) <- TRUE
rastertata1<- raster(tata1)
poltata1<- rasterToPolygons(rastertata1, dissolve=TRUE)


# Conversion Kriege.logdens en SpatialPointsDataFrame

{
Kriege.logdens1<- Kriege.logdens %>% filter(Cluster==1)
Kriege.logdens1SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens1[,1:2]), Kriege.logdens1)
pipo1<- sp::over(Kriege.logdens1SP, poltata1)

fin1<- cbind(pipo1, Kriege.logdens1)
names(fin1)[7]<- "Communauté I"
names(fin1)[1]<- "Zones finales"
fin1<- fin1 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens2<- Kriege.logdens %>% filter(Cluster==2)
  Kriege.logdens2SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens2[,1:2]), Kriege.logdens2)
  pipo2<- sp::over(Kriege.logdens2SP, poltata1)

  fin2<- cbind(pipo2, Kriege.logdens2)
  names(fin2)[7]<- "Communauté II"
  names(fin2)[1]<- "Zones finales"
  fin2<- fin2 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens3<- Kriege.logdens %>% filter(Cluster==3)
  Kriege.logdens3SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens3[,1:2]), Kriege.logdens3)
  pipo3<- sp::over(Kriege.logdens3SP, poltata1)
  
  fin3<- cbind(pipo3, Kriege.logdens3)
  names(fin3)[7]<- "Communauté III"
  names(fin3)[1]<- "Zones finales"
  fin3<- fin3 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens4<- Kriege.logdens %>% filter(Cluster==4)
  Kriege.logdens4SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens4[,1:2]), Kriege.logdens4)
  pipo4<- sp::over(Kriege.logdens4SP, poltata1)
  
  fin4<- cbind(pipo4, Kriege.logdens4)
  names(fin4)[7]<- "Communauté IV"
  names(fin4)[1]<- "Zones finales"
  fin4<- fin4 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens5<- Kriege.logdens %>% filter(Cluster==5)
  Kriege.logdens5SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens5[,1:2]), Kriege.logdens5)
  pipo5<- sp::over(Kriege.logdens5SP, poltata1)
  
  fin5<- cbind(pipo5, Kriege.logdens5)
  names(fin5)[7]<- "Communauté V"
  names(fin5)[1]<- "Zones finales"
  fin5<- fin5 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens6<- Kriege.logdens %>% filter(Cluster==6)
  Kriege.logdens6SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens6[,1:2]), Kriege.logdens6)
  pipo6<- sp::over(Kriege.logdens6SP, poltata1)
  
  fin6<- cbind(pipo6, Kriege.logdens6)
  names(fin6)[7]<- "Communauté VI"
  names(fin6)[1]<- "Zones finales"
  fin6<- fin6 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens7<- Kriege.logdens %>% filter(Cluster==7)
  Kriege.logdens7SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens7[,1:2]), Kriege.logdens7)
  pipo7<- sp::over(Kriege.logdens7SP, poltata1)
  
  fin7<- cbind(pipo7, Kriege.logdens7)
  names(fin7)[7]<- "Communauté VII"
  names(fin7)[1]<- "Zones finales"
  fin7<- fin7 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens8<- Kriege.logdens %>% filter(Cluster==8)
  Kriege.logdens8SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens8[,1:2]), Kriege.logdens8)
  pipo8<- sp::over(Kriege.logdens8SP, poltata1)
  
  fin8<- cbind(pipo8, Kriege.logdens8)
  names(fin8)[7]<- "Communauté VIII"
  names(fin8)[1]<- "Zones finales"
  fin8<- fin8 %>% dplyr::select(-Variance)
}
{
  Kriege.logdens9<- Kriege.logdens %>% filter(Cluster==9)
  Kriege.logdens9SP<- SpatialPointsDataFrame(as.matrix(Kriege.logdens9[,1:2]), Kriege.logdens9)
  pipo9<- sp::over(Kriege.logdens9SP, poltata1)
  
  fin9<- cbind(pipo9, Kriege.logdens9)
  names(fin9)[7]<- "Communauté IX"
  names(fin9)[1]<- "Zones finales"
  fin9<- fin9 %>% dplyr::select(-Variance)
}


# Calculs séries temporelles pour chaque zone

{
fin1bis<- fin1 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté I" = mean(Prediction))       # Moyenne (pas somme) pour comparer les pixels entre eux et pas les zones qui sont de tailles différentes
fin2bis<- fin2 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté II" = mean(Prediction))
fin3bis<- fin3 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté III" = mean(Prediction))
fin4bis<- fin4 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté IV" = mean(Prediction))
fin5bis<- fin5 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté V" = mean(Prediction))
fin6bis<- fin6 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VI" = mean(Prediction))
fin7bis<- fin7 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VII" = mean(Prediction))
fin8bis<- fin8 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VIII" = mean(Prediction))
fin9bis<- fin9 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté IX" = mean(Prediction))
}
{
fintot<- fin1bis %>% left_join(fin2bis, by= c("Zones finales", "Year")) # 1 pixel de la Zone 1 a en 1988 une densité de com I égale à 4.08  
fintot<- fintot %>% left_join(fin3bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin4bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin5bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin6bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin7bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin8bis, by= c("Zones finales", "Year"))
fintot<- fintot %>% left_join(fin9bis, by= c("Zones finales", "Year"))

fintot<- pivot_longer(fintot, cols = 3:11, names_to = "Communauté", values_to = "Moyenne")
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

fintot$Communauté<- factor(fintot$Communauté, ordered = TRUE, levels = c("Communauté I", "Communauté II", "Communauté III", "Communauté IV", "Communauté V", "Communauté VI", "Communauté VII", "Communauté VIII", "Communauté IX"))
names(fintot)[2]<- "Année"

ggplot(fintot)+
  geom_line(aes(x=Année, y=Moyenne))+
  facet_grid(`Zones finales` ~ Communauté)+
  guides(x = guide_axis(angle = 90))+
  theme_bw()
  



# Calculs proportion des communautés dans chaque zone

prop<- fintot %>% group_by(`Zones finales`, Communauté) %>% summarise(Moyenne= mean(Moyenne)) # 1 pixel de la Zone 1 a en moyenne sur l'ensemble des 32 années une densité de com I égale à 4.10

{
  prop1<- prop[prop$`Zones finales`=="Zone 1",] 
  prop1<- prop1 %>% mutate(Total= sum(Moyenne))
  prop1<- prop1 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop1<- prop1 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop2<- prop[prop$`Zones finales`=="Zone 2",] 
  prop2<- prop2 %>% mutate(Total= sum(Moyenne))
  prop2<- prop2 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop2<- prop2 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop3<- prop[prop$`Zones finales`=="Zone 3",] 
  prop3<- prop3 %>% mutate(Total= sum(Moyenne))
  prop3<- prop3 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop3<- prop3 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop4<- prop[prop$`Zones finales`=="Zone 4",] 
  prop4<- prop4 %>% mutate(Total= sum(Moyenne))
  prop4<- prop4 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop4<- prop4 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop5<- prop[prop$`Zones finales`=="Zone 5",] 
  prop5<- prop5 %>% mutate(Total= sum(Moyenne))
  prop5<- prop5 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop5<- prop5 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop6<- prop[prop$`Zones finales`=="Zone 6",] 
  prop6<- prop6 %>% mutate(Total= sum(Moyenne))
  prop6<- prop6 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop6<- prop6 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop7<- prop[prop$`Zones finales`=="Zone 7",] 
  prop7<- prop7 %>% mutate(Total= sum(Moyenne))
  prop7<- prop7 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop7<- prop7 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop8<- prop[prop$`Zones finales`=="Zone 8",] 
  prop8<- prop8 %>% mutate(Total= sum(Moyenne))
  prop8<- prop8 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop8<- prop8 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop9<- prop[prop$`Zones finales`=="Zone 9",] 
  prop9<- prop9 %>% mutate(Total= sum(Moyenne))
  prop9<- prop9 %>% mutate(Proportion= (Moyenne*100)/Total)
  prop9<- prop9 %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}

propfin<- rbind(prop1, prop2)
propfin<- rbind(propfin, prop3)
propfin<- rbind(propfin, prop4)
propfin<- rbind(propfin, prop5)
propfin<- rbind(propfin, prop6)
propfin<- rbind(propfin, prop7)
propfin<- rbind(propfin, prop8)
propfin<- rbind(propfin, prop9)


  # Barplots

absolu<- ggplot(data=propfin, aes(x=`Zones finales`, y=Proportion, fill=Communauté)) +
  geom_bar(stat="identity")



# Proportion des communautés dans chaque zone en relatif 

Effectif<- c(25, 12, 45, 7, 6, 20, 3, 2, 7)

{
  prop1b<- prop[prop$`Zones finales`=="Zone 1",]
  prop1b<- prop1b %>% mutate(Effectif= Effectif)
  prop1b<- prop1b %>% mutate(NewMoy= Moyenne/Effectif)
  prop1b<- prop1b %>% mutate(Total= sum(NewMoy))
  prop1b<- prop1b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop1b<- prop1b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
  }
{
  prop2b<- prop[prop$`Zones finales`=="Zone 2",]
  prop2b<- prop2b %>% mutate(Effectif= Effectif)
  prop2b<- prop2b %>% mutate(NewMoy= Moyenne/Effectif)
  prop2b<- prop2b %>% mutate(Total= sum(NewMoy))
  prop2b<- prop2b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop2b<- prop2b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop3b<- prop[prop$`Zones finales`=="Zone 3",]
  prop3b<- prop3b %>% mutate(Effectif= Effectif)
  prop3b<- prop3b %>% mutate(NewMoy= Moyenne/Effectif)
  prop3b<- prop3b %>% mutate(Total= sum(NewMoy))
  prop3b<- prop3b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop3b<- prop3b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop4b<- prop[prop$`Zones finales`=="Zone 4",] 
  prop4b<- prop4b %>% mutate(Effectif= Effectif)
  prop4b<- prop4b %>% mutate(NewMoy= Moyenne/Effectif)
  prop4b<- prop4b %>% mutate(Total= sum(NewMoy))
  prop4b<- prop4b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop4b<- prop4b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop5b<- prop[prop$`Zones finales`=="Zone 5",] 
  prop5b<- prop5b %>% mutate(Effectif= Effectif)
  prop5b<- prop5b %>% mutate(NewMoy= Moyenne/Effectif)
  prop5b<- prop5b %>% mutate(Total= sum(NewMoy))
  prop5b<- prop5b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop5b<- prop5b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop6b<- prop[prop$`Zones finales`=="Zone 6",] 
  prop6b<- prop6b %>% mutate(Effectif= Effectif)
  prop6b<- prop6b %>% mutate(NewMoy= Moyenne/Effectif)
  prop6b<- prop6b %>% mutate(Total= sum(NewMoy))
  prop6b<- prop6b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop6b<- prop6b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop7b<- prop[prop$`Zones finales`=="Zone 7",] 
  prop7b<- prop7b %>% mutate(Effectif= Effectif)
  prop7b<- prop7b %>% mutate(NewMoy= Moyenne/Effectif)
  prop7b<- prop7b %>% mutate(Total= sum(NewMoy))
  prop7b<- prop7b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop7b<- prop7b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop8b<- prop[prop$`Zones finales`=="Zone 8",] 
  prop8b<- prop8b %>% mutate(Effectif= Effectif)
  prop8b<- prop8b %>% mutate(NewMoy= Moyenne/Effectif)
  prop8b<- prop8b %>% mutate(Total= sum(NewMoy))
  prop8b<- prop8b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop8b<- prop8b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}
{
  prop9b<- prop[prop$`Zones finales`=="Zone 9",] 
  prop9b<- prop9b %>% mutate(Effectif= Effectif)
  prop9b<- prop9b %>% mutate(NewMoy= Moyenne/Effectif)
  prop9b<- prop9b %>% mutate(Total= sum(NewMoy))
  prop9b<- prop9b %>% mutate(Proportion= (NewMoy*100)/Total)
  prop9b<- prop9b %>% dplyr::select(`Zones finales`, Communauté, Proportion)  
}

propfinb<- rbind(prop1b, prop2b)
propfinb<- rbind(propfinb, prop3b)
propfinb<- rbind(propfinb, prop4b)
propfinb<- rbind(propfinb, prop5b)
propfinb<- rbind(propfinb, prop6b)
propfinb<- rbind(propfinb, prop7b)
propfinb<- rbind(propfinb, prop8b)
propfinb<- rbind(propfinb, prop9b)


# Barplots

relatif<- ggplot(data=propfinb, aes(x=`Zones finales`, y=Proportion, fill=Communauté)) +
  geom_bar(stat="identity")




# Caractérisation des zones en termes de densité totale et non plus en termes de communautés


{
  fin1ter<- fin1 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté I" = mean(Prediction))           # Sur l'année 1988, la moyenne d'un pixel de la zone 1 est    
  fin2ter<- fin2 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté II" = mean(Prediction))
  fin3ter<- fin3 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté III" = mean(Prediction))
  fin4ter<- fin4 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté IV" = mean(Prediction))
  fin5ter<- fin5 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté V" = mean(Prediction))
  fin6ter<- fin6 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VI" = mean(Prediction))
  fin7ter<- fin7 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VII" = mean(Prediction))
  fin8ter<- fin8 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté VIII" = mean(Prediction))
  fin9ter<- fin9 %>% group_by(`Zones finales`, Year) %>% summarise("Communauté IX" = mean(Prediction))
  }
{
  fin1qua<- fin1ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté I`))                   # La moyenne d'un pixel de la zone 1 est
  fin2qua<- fin2ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté II`))
  fin3qua<- fin3ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté III`))
  fin4qua<- fin4ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté IV`))
  fin5qua<- fin5ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté V`))
  fin6qua<- fin6ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté VI`))
  fin7qua<- fin7ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté VII`))
  fin8qua<- fin8ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté VIII`))
  fin9qua<- fin9ter %>% group_by(`Zones finales`) %>% summarise("Com" = mean(`Communauté IX`))
}

fintotqua<- rbind(fin1qua, fin2qua) 
fintotqua<- rbind(fintotqua, fin3qua)
fintotqua<- rbind(fintotqua, fin4qua)
fintotqua<- rbind(fintotqua, fin5qua)
fintotqua<- rbind(fintotqua, fin6qua)
fintotqua<- rbind(fintotqua, fin7qua)
fintotqua<- rbind(fintotqua, fin8qua)
fintotqua<- rbind(fintotqua, fin9qua)

finfin<- fintotqua %>% group_by(`Zones finales`) %>% summarise(`Densité totale` = sum(Com))

ggplot(data=finfin, aes(x=`Zones finales`, y=`Densité totale`, fill=`Zones finales`)) +
  geom_bar(stat="identity")






# Essai

tabchiant<- cbind(fin1, ComII=fin2$Prediction, ComIII=fin3$Prediction, ComIV=fin4$Prediction, 
                  ComV=fin5$Prediction, ComVI=fin6$Prediction, ComVII=fin7$Prediction, 
                  ComVIII=fin8$Prediction, ComIX=fin9$Prediction)
names(tabchiant)[4]<- "ComI"
tabchiant<- tabchiant %>% select(-`Communauté I`)


tabchiant2<- tabchiant %>% group_by(Longitude, Latitude) %>% mutate(Denstot=sum(ComI, ComII, ComIII, ComIV, ComV, ComVI, ComVII, ComVIII, ComIX))



# Création polygones zones

load("results/Communautes bio/Zones/Tabttpixel.Rdata")
# create SpatialPointsDataFrame
tataras<- tata
coordinates(tataras)<- ~ Long + Lat
# coerce to SpatialPixelsDataFrame
gridded(tataras) <- TRUE
# coerce to raster
rastertatanew<- raster(tataras)
rastertatanew
plot(rastertatanew, col=brewer.pal(n = 9, name = "YlGnBu"), main="", xlab="Longitude", ylab="Latitude")


polfin<- rasterToPolygons(rastertatanew, dissolve=TRUE)
plot(polfin, col=polfin@data$Clust)

polfinfort<- fortify(polfin)

ggplot(tabchiant2)+
  geom_tile(aes(x=Longitude, y=Latitude, fill=Denstot))+
  geom_polygon(data=polfinfort, aes(x=long, y=lat, group=group) ,fill=NA, col="black")+
  theme_minimal()








