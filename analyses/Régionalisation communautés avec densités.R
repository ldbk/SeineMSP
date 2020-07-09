library(dplyr)
library(FactoMineR)
library(missMDA)
library(cluster)
library(ggplot2)
library(RColorBrewer)


{
  load(file="data/ICES/Tabfin1.Rdata")
  Tabfin1<- titi
  names(Tabfin1)[5]<- "Pred1"
  Tabfin1<- Tabfin1 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin2.Rdata")
  Tabfin2<- titi
  names(Tabfin2)[5]<- "Pred2"
  Tabfin2<- Tabfin2 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin3.Rdata")
  Tabfin3<- titi
  names(Tabfin3)[5]<- "Pred3"
  Tabfin3<- Tabfin3 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin4.Rdata")
  Tabfin4<- titi
  names(Tabfin4)[5]<- "Pred4"
  Tabfin4<- Tabfin4 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin5.Rdata")
  Tabfin5<- titi
  names(Tabfin5)[5]<- "Pred5"
  Tabfin5<- Tabfin5 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin6.Rdata")
  Tabfin6<- titi
  names(Tabfin6)[5]<- "Pred6"
  Tabfin6<- Tabfin6 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin7.Rdata")
  Tabfin7<- titi
  names(Tabfin7)[5]<- "Pred7"
  Tabfin7<- Tabfin7 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin8.Rdata")
  Tabfin8<- titi
  names(Tabfin8)[5]<- "Pred8"
  Tabfin8<- Tabfin8 %>% dplyr::select(-Community, -Clust)
  load(file="data/ICES/Tabfin9.Rdata")
  Tabfin9<- titi
  names(Tabfin9)[5]<- "Pred9"
  Tabfin9<- Tabfin9 %>% dplyr::select(-Community, -Clust)
}
{
  load("results/Communautes bio/Community1_polygons_dens.Rdata")
  pol1<- poldens
  load("results/Communautes bio/Community2_polygons_dens.Rdata")
  pol2<- poldens
  load("results/Communautes bio/Community3_polygons_dens.Rdata")
  pol3<- poldens
  load("results/Communautes bio/Community4_polygons_dens.Rdata")
  pol4<- poldens
  load("results/Communautes bio/Community5_polygons_dens.Rdata")
  pol5<- poldens
  load("results/Communautes bio/Community6_polygons_dens.Rdata")
  pol6<- poldens
  load("results/Communautes bio/Community7_polygons_dens.Rdata")
  pol7<- poldens
  load("results/Communautes bio/Community8_polygons_dens.Rdata")
  pol8<- poldens
  load("results/Communautes bio/Community9_polygons_dens.Rdata")
  pol9<- poldens
}
load("data/PolyCut.Rdata")


# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(Tabfin1$Longitude),                                             
                              to = max(Tabfin1$Longitude),                                                
                              by = 0.01),
                   Lat = seq(from = min(Tabfin1$Latitude),
                             to = max(Tabfin1$Latitude), 
                             by = 0.01))  

points <- structure(list(grd$Long, grd$Lat), .Names = c("Longitude", "Latitude"), 
                    class = "data.frame", row.names = c(NA, dim(grd)[1]))                                 
spdf <- SpatialPointsDataFrame(coords = points, data = points)                                          

noms <- c("pol1","pol2","pol3","pol4","pol5","pol6","pol7","pol8", "pol9")
t <- 0
for (i in list(pol1,pol2,pol3,pol4,pol5,pol6,pol7,pol8, pol9)){
  t <- t+1
  pipo <- sp::over(spdf, i)
  grd <- cbind(grd,pipo)
}

dim(na.omit(grd))
grd <- na.omit(grd)                                                                                            
plot(grd[,c(1,2)])


# Classification
{
names(grd)[3]<- "Pred1"
names(grd)[4]<- "Pred2"
names(grd)[5]<- "Pred3"
names(grd)[6]<- "Pred4"
names(grd)[7]<- "Pred5"
names(grd)[8]<- "Pred6"
names(grd)[9]<- "Pred7"
names(grd)[10]<- "Pred8"
names(grd)[11]<- "Pred9"
}
grd2<- grd %>% dplyr::select(-Long, -Lat)

rez<- PCA(grd2, ncp=999, graph=F)                                                      
plt1<-plotellipses(rez, axes=c(1,2))
plt2<-plotellipses(rez, axes=c(1,3))
arbre<- agnes(grd2, method="ward", par.method=1)
#save(arbre, file="data/ICES/arbreavecdens.Rdata")
load("data/ICES/arbreavecdens.Rdata")
plot(arbre, which=2,hang=-1)                                
rect.hclust(arbre, k=4)
groups<- cutree(arbre,k=4)
tutu<- cbind(grd, Zone=factor(groups))

colors<- brewer.pal(n = 4, name = "YlGnBu")
ggplot(tutu)+
  geom_tile(aes(x=Long, y=Lat, fill=Zone))+
  scale_fill_manual(values=colors)+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  theme_minimal()



# Composition des zones en termes de communautes

{
tutuz1<- tutu %>% filter(Zone==1) %>% dplyr::select(-Long, -Lat)
tutuz1<- unique(tutuz1) 
tutuz1<- tutuz1 %>% mutate(mean1= mean(tutuz1$Pred1), 
                           mean2= mean(tutuz1$Pred2), 
                           mean3= mean(tutuz1$Pred3),
                           mean4= mean(tutuz1$Pred4),
                           mean5= mean(tutuz1$Pred5),
                           mean6= mean(tutuz1$Pred6),
                           mean7= mean(tutuz1$Pred7),
                           mean8= mean(tutuz1$Pred8),
                           mean9= mean(tutuz1$Pred9))
tutuz1<- tutuz1 %>% dplyr::select(-c(1,2,3,4,5,6,7,8,9))
tutuz1<- unique(tutuz1)
tutuz1<- tutuz1 %>% dplyr::select(-Zone)
  names(tutuz1)[1]<- "Com 1"
  names(tutuz1)[2]<- "Com 2"
  names(tutuz1)[3]<- "Com 3"
  names(tutuz1)[4]<- "Com 4"
  names(tutuz1)[5]<- "Com 5"
  names(tutuz1)[6]<- "Com 6"
  names(tutuz1)[7]<- "Com 7"
  names(tutuz1)[8]<- "Com 8"
  names(tutuz1)[9]<- "Com 9"
tutuz1<- as.matrix(tutuz1)
bar1<- barplot(tutuz1, main="Proportion des communautes dans la zone 1", ylab = "Densites de capture", col="#FFFFCC")
save(bar1, file="results/Communautes bio/Composition des zones/bar1.Rdata")
}



{
tutuz2<- tutu %>% filter(Zone==2) %>% dplyr::select(-Long, -Lat)
tutuz2<- unique(tutuz2)
tutuz2<- tutuz2 %>% mutate(mean1= mean(tutuz2$Pred1), 
                           mean2= mean(tutuz2$Pred2), 
                           mean3= mean(tutuz2$Pred3),
                           mean4= mean(tutuz2$Pred4),
                           mean5= mean(tutuz2$Pred5),
                           mean6= mean(tutuz2$Pred6),
                           mean7= mean(tutuz2$Pred7),
                           mean8= mean(tutuz2$Pred8),
                           mean9= mean(tutuz2$Pred9))
tutuz2<- tutuz2 %>% dplyr::select(-c(1,2,3,4,5,6,7,8,9))
tutuz2<- unique(tutuz2)
tutuz2<- tutuz2 %>% dplyr::select(-Zone)
  names(tutuz2)[1]<- "Com 1"
  names(tutuz2)[2]<- "Com 2"
  names(tutuz2)[3]<- "Com 3"
  names(tutuz2)[4]<- "Com 4"
  names(tutuz2)[5]<- "Com 5"
  names(tutuz2)[6]<- "Com 6"
  names(tutuz2)[7]<- "Com 7"
  names(tutuz2)[8]<- "Com 8"
  names(tutuz2)[9]<- "Com 9"
tutuz2<- as.matrix(tutuz2)
bar2<- barplot(tutuz2, main="Proportion des communautes dans la zone 2", ylab = "Densites de capture", col="#A1DAB4")
save(bar2, file="results/Communautes bio/Composition des zones/bar2.Rdata")
}



{
tutuz3<- tutu %>% filter(Zone==3) %>% dplyr::select(-Long, -Lat)
tutuz3<- unique(tutuz3)
tutuz3<- tutuz3 %>% mutate(mean1= mean(tutuz3$Pred1), 
                           mean2= mean(tutuz3$Pred2), 
                           mean3= mean(tutuz3$Pred3),
                           mean4= mean(tutuz3$Pred4),
                           mean5= mean(tutuz3$Pred5),
                           mean6= mean(tutuz3$Pred6),
                           mean7= mean(tutuz3$Pred7),
                           mean8= mean(tutuz3$Pred8),
                           mean9= mean(tutuz3$Pred9))
tutuz3<- tutuz3 %>% dplyr::select(-c(1,2,3,4,5,6,7,8,9))
tutuz3<- unique(tutuz3)
tutuz3<- tutuz3 %>% dplyr::select(-Zone)
  names(tutuz3)[1]<- "Com 1"
  names(tutuz3)[2]<- "Com 2"
  names(tutuz3)[3]<- "Com 3"
  names(tutuz3)[4]<- "Com 4"
  names(tutuz3)[5]<- "Com 5"
  names(tutuz3)[6]<- "Com 6"
  names(tutuz3)[7]<- "Com 7"
  names(tutuz3)[8]<- "Com 8"
  names(tutuz3)[9]<- "Com 9"
tutuz3<- as.matrix(tutuz3)
bar3<- barplot(tutuz3, main="Proportion des communautes dans la zone 3", ylab = "Densites de capture", col="#41B6C4")
save(bar3, file="results/Communautes bio/Composition des zones/bar3.Rdata")
}


{
tutuz4<- tutu %>% filter(Zone==4) %>% dplyr::select(-Long, -Lat)
tutuz4<- unique(tutuz4)
tutuz4<- tutuz4 %>% mutate(mean1= mean(tutuz4$Pred1), 
                           mean2= mean(tutuz4$Pred2), 
                           mean3= mean(tutuz4$Pred3),
                           mean4= mean(tutuz4$Pred4),
                           mean5= mean(tutuz4$Pred5),
                           mean6= mean(tutuz4$Pred6),
                           mean7= mean(tutuz4$Pred7),
                           mean8= mean(tutuz4$Pred8),
                           mean9= mean(tutuz4$Pred9))
tutuz4<- tutuz4 %>% dplyr::select(-c(1,2,3,4,5,6,7,8,9))
tutuz4<- unique(tutuz4)
tutuz4<- tutuz4 %>% dplyr::select(-Zone)
  names(tutuz4)[1]<- "Com 1"
  names(tutuz4)[2]<- "Com 2"
  names(tutuz4)[3]<- "Com 3"
  names(tutuz4)[4]<- "Com 4"
  names(tutuz4)[5]<- "Com 5"
  names(tutuz4)[6]<- "Com 6"
  names(tutuz4)[7]<- "Com 7"
  names(tutuz4)[8]<- "Com 8"
  names(tutuz4)[9]<- "Com 9"
tutuz4<- as.matrix(tutuz4)
bar4<- barplot(tutuz4, main="Proportion des communautes dans la zone 4", ylab = "Densites de capture", col="#2C7FB8")
save(bar4, file="results/Communautes bio/Composition des zones/bar4.Rdata")
}



par(mfrow = c(2, 2))

barplot(tutuz1, main="Zone 1", ylab = "Densites de capture", col="#FFFFCC", las=2, ylim=c(0,1.2))
barplot(tutuz2, main="Zone 2", ylab = "Densites de capture", col="#A1DAB4", las=2, ylim=c(0,1.2))
barplot(tutuz3, main="Zone 3", ylab = "Densites de capture", col="#41B6C4", las=2, ylim=c(0,1.2))
barplot(tutuz4, main="Zone 4", ylab = "Densites de capture", col="#2C7FB8", las=2, ylim=c(0,1.2))


par(mfrow = c(1, 1))





