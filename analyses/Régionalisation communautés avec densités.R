library(dplyr)
library(FactoMineR)
library(missMDA)
library(cluster)


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


#pol1@polygons[[1]]@ID<- as.character(unique(Tabfin1$Pred1[Tabfin1$Clust==1]))
#pol1@polygons[[2]]@ID<- as.character(unique(Tabfin1$Pred1[Tabfin1$Clust==2]))

#pol1@data[["Clust"]]<- c(unique(Tabfin1$Pred1[Tabfin1$Clust==1]), unique(Tabfin1$Pred1[Tabfin1$Clust==2]))





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
  #pipo$Clust2 <- paste0(rep(substr(noms[t],4,nchar(noms[t])),dim(pipo)[1]), pipo$Clust)
  #names(pipo) <- c("Clust",substr(noms[t],4,nchar(noms[t])))
  grd <- cbind(grd,pipo)
}

dim(na.omit(grd))
grd <- na.omit(grd)                                                                                            
plot(grd[,c(1,2)])
#grd <- grd[, !duplicated(colnames(grd2))]
#grd <- grd[,-3]

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

#arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
#save(arbre, file="data/ICES/arbreavecdens.Rdata")
load("data/ICES/arbreavecdens.Rdata")
plot(arbre, which=2,hang=-1)                                
rect.hclust(arbre, k=5)

groups<- cutree(arbre,k=5)

tutu<- cbind(grd, Zone=factor(groups))

#tata <- cbind(grd2[,c(1,2)], Clust=factor(groups))

ggplot(tutu)+
  geom_tile(aes(x=Long,y=Lat,fill=Zone))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")


tutuz1<- tutu %>% filter(Zone==1) %>% dplyr::select(-Long, -Lat)
tutuz2<- tutu %>% filter(Zone==2) %>% dplyr::select(-Long, -Lat)
tutuz3<- tutu %>% filter(Zone==3) %>% dplyr::select(-Long, -Lat)
tutuz4<- tutu %>% filter(Zone==4) %>% dplyr::select(-Long, -Lat)
tutuz5<- tutu %>% filter(Zone==5) %>% dplyr::select(-Long, -Lat)

tutuz1<- unique(tutuz1)
tutuz2<- unique(tutuz2)
tutuz3<- unique(tutuz3)
tutuz4<- unique(tutuz4)
tutuz5<- unique(tutuz5)

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

tutuz5<- tutuz5 %>% mutate(mean1= mean(tutuz5$Pred1), 
                           mean2= mean(tutuz5$Pred2), 
                           mean3= mean(tutuz5$Pred3),
                           mean4= mean(tutuz5$Pred4),
                           mean5= mean(tutuz5$Pred5),
                           mean6= mean(tutuz5$Pred6),
                           mean7= mean(tutuz5$Pred7),
                           mean8= mean(tutuz5$Pred8),
                           mean9= mean(tutuz5$Pred9))

tutuz5<- tutuz5 %>% dplyr::select(-c(1,2,3,4,5,6,7,8,9))
tutuz5<- unique(tutuz5)

tutuz5<- as.data.frame(t(tutuz5))
names(tutuz5)[1]<- "Mean"
tutuz5bis<- tutuz5 %>% filter(!Mean==5)
tutuz5bis<- tutuz5bis %>% mutate(Community= c(1,2,3,4,5,6,7,8,9))
#tutuz5bis$Mean<- as.numeric(tutuz5bis$Mean)















