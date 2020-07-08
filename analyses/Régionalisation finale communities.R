library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
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
load("results/Communautes bio/Community1_polygons.Rdata")
pol1<- pol
load("results/Communautes bio/Community2_polygons.Rdata")
pol2<- pol
load("results/Communautes bio/Community3_polygons.Rdata")
pol3<- pol
load("results/Communautes bio/Community4_polygons.Rdata")
pol4<- pol
load("results/Communautes bio/Community5_polygons.Rdata")
pol5<- pol
load("results/Communautes bio/Community6_polygons.Rdata")
pol6<- pol
load("results/Communautes bio/Community7_polygons.Rdata")
pol7<- pol
load("results/Communautes bio/Community8_polygons.Rdata")
pol8<- pol
load("results/Communautes bio/Community9_polygons.Rdata")
pol9<- pol
}
load("data/PolyCut.Rdata")



# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(Tabfin1$Longitude),                                                 
                              to = max(Tabfin1$Longitude),                                                    
                              by = 0.01),
                   Lat = seq(from =min(Tabfin1$Latitude),
                             to = max(Tabfin1$Latitude), 
                             by = 0.01))  

points <- structure(list(grd$Long, grd$Lat), .Names = c("Long", "Lat"), 
                    class = "data.frame", row.names = c(NA, dim(grd)[1]))                               
spdf <- SpatialPointsDataFrame(coords = points, data = points)                                          

noms <- c("pol1","pol2","pol3","pol4","pol5","pol6","pol7","pol8", "pol9")
t <- 0
for (i in list(pol1,pol2,pol3,pol4,pol5,pol6,pol7,pol8,pol9)){
  t <- t+1
  pipo <- sp::over(spdf, i)
  pipo$Clust2 <- paste0(rep(substr(noms[t],4,nchar(noms[t])),dim(pipo)[1]), pipo$Clust)
  names(pipo) <- c("Clust",substr(noms[t],4,nchar(noms[t])))
  grd <- cbind(grd,pipo)
}
dim(na.omit(grd))

grd2 <- na.omit(grd)                                                                                           
plot(grd2[,c(1,2)])
grd2 <- grd2[, !duplicated(colnames(grd2))]
grd2 <- grd2[,-3]                                                                                               

rez<- MCA(grd2[,-c(1,2)], ncp=999, method="Burt", graph=F)                                                      
plt1<- plotellipses(rez, axes=c(1,2))
plt2<- plotellipses(rez, axes=c(1,3))

#arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
#save(arbre, file="data/ICES/arbre_régiona_fin_communi_dens.Rdata")
load("data/ICES/arbre_régiona_fin_communi_dens.Rdata")
plot(arbre, which=2, hang=-1)

rect.hclust(arbre, k=(5))
groups<- cutree(arbre, k=5)

tata<- cbind(grd2[,c(1,2)], Clust=factor(groups))

Allparam<- ggplot(tata)+
  geom_tile(aes(x=Long, y=Lat, fill=Clust))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()

Allparam2<- Allparam +
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 10))


ggsave(plot= Allparam2, filename="Biorégionalisation_dens.jpeg", path="results/Communautes bio", width = 13, height = 8)



zone1<- tata %>% filter(Clust==1)
names(zone1)[1]<- "Longitude"
names(zone1)[2]<- "Latitude"
zone2<- tata %>% filter(Clust==2)
names(zone2)[1]<- "Longitude"
names(zone2)[2]<- "Latitude"
zone3<- tata %>% filter(Clust==3)
names(zone3)[1]<- "Longitude"
names(zone3)[2]<- "Latitude"
zone4<- tata %>% filter(Clust==4)
names(zone4)[1]<- "Longitude"
names(zone4)[2]<- "Latitude"
zone5<- tata %>% filter(Clust==5)
names(zone5)[1]<- "Longitude"
names(zone5)[2]<- "Latitude"

Jzone1<- zone1 %>% left_join(Tabfin1, by=c("Longitude", "Latitude"))
Jzone1<- Jzone1 %>% left_join(Tabfin2, by=c("Longitude", "Latitude"))
Jzone1<- Jzone1 %>% left_join(Tabfin3, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin4, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin5, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin6, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin7, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin8, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)
Jzone1<- Jzone1 %>% left_join(Tabfin9, by=c("Longitude", "Latitude"))
Jzone1<- unique(Jzone1)

Jzone2<- zone2 %>% left_join(Tabfin1, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin2, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin3, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin4, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin5, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin6, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin7, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin8, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
Jzone2<- zone2 %>% left_join(Tabfin9, by=c("Longitude", "Latitude"))
Jzone2<- unique(Jzone2)
#Jzone2<- na.omit(Jzone2)


Jzone3<- zone3 %>% left_join(Tabfin1, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin2, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin3, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin4, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin5, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin6, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin7, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin8, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
Jzone3<- zone3 %>% left_join(Tabfin9, by=c("Longitude", "Latitude"))
Jzone3<- unique(Jzone3)
#Jzone3<- na.omit(Jzone3)




Jzone4<- zone4 %>% left_join(Tabfin1, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin2, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin3, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin4, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin5, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin6, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin7, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin8, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
Jzone4<- zone4 %>% left_join(Tabfin9, by=c("Longitude", "Latitude"))
Jzone4<- unique(Jzone4)
#Jzone4<- na.omit(Jzone4)

Jzone5<- zone5 %>% left_join(Tabfin1, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin2, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin3, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin4, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin5, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin6, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin7, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin8, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
Jzone5<- zone5 %>% left_join(Tabfin9, by=c("Longitude", "Latitude"))
Jzone5<- unique(Jzone5)
#Jzone5<- na.omit(Jzone5)




names(tata)[1]<- "Longitude"
names(tata)[2]<- "Latitude"

# Create the function mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

J1<- Tabfin1 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J1<- J1 %>% select(-c(Longitude, Latitude))
J1$Comun<- as.numeric(J1$Comun)
J1$Clust<- as.numeric(J1$Clust)
J1bis<- J1 %>% group_by(Clust) %>% summarize(Mode= getmode(Comun))
J1bis$Mode<- as.numeric(J1bis$Mode)
names(J1bis)[1]<- "zone final"

J2<- Tabfin2 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J2<- J2 %>% select(-c(Longitude, Latitude))
J2$Comdeux<- as.numeric(J2$Comdeux)
J2$Clust<- as.numeric(J2$Clust)
J2bis<- J2 %>% group_by(Clust) %>% summarize(Mode= getmode(Comdeux))
J2bis$Mode<- as.numeric(J2bis$Mode)
names(J2bis)[1]<- "zone final"

J3<- Tabfin3 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J3<- J3 %>% select(-c(Longitude, Latitude))
J3$Comtrois<- as.numeric(J3$Comtrois)
J3$Clust<- as.numeric(J3$Clust)
J3bis<- J3 %>% group_by(Clust) %>% summarize(Mode= getmode(Comtrois))
J3bis$Mode<- as.numeric(J3bis$Mode)
names(J3bis)[1]<- "zone final"

J4<- Tabfin4 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J4<- J4 %>% select(-c(Longitude, Latitude))
J4$Comquatre<- as.numeric(J4$Comquatre)
J4$Clust<- as.numeric(J4$Clust)
J4bis<- J4 %>% group_by(Clust) %>% summarize(Mode= getmode(Comquatre))
J4bis$Mode<- as.numeric(J4bis$Mode)
names(J4bis)[1]<- "zone final"

J5<- Tabfin5 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J5<- J5 %>% select(-c(Longitude, Latitude))
J5$Comcinq<- as.numeric(J5$Comcinq)
J5$Clust<- as.numeric(J5$Clust)
J5bis<- J5 %>% group_by(Clust) %>% summarize(Mode= getmode(Comcinq))
J5bis$Mode<- as.numeric(J5bis$Mode)
names(J5bis)[1]<- "zone final"

J6<- Tabfin6 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J6<- J6 %>% select(-c(Longitude, Latitude))
J6$Comsix<- as.numeric(J6$Comsix)
J6$Clust<- as.numeric(J6$Clust)
J6bis<- J6 %>% group_by(Clust) %>% summarize(Mode= getmode(Comsix))
J6bis$Mode<- as.numeric(J6bis$Mode)
names(J6bis)[1]<- "zone final"

J7<- Tabfin7 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J7<- J7 %>% select(-c(Longitude, Latitude))
J7$Comsept<- as.numeric(J7$Comsept)
J7$Clust<- as.numeric(J7$Clust)
J7bis<- J7 %>% group_by(Clust) %>% summarize(Mode= getmode(Comsept))
J7bis$Mode<- as.numeric(J7bis$Mode)
names(J7bis)[1]<- "zone final"

J8<- Tabfin8 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J8<- J8 %>% select(-c(Longitude, Latitude))
J8$Comhuit<- as.numeric(J8$Comhuit)
J8$Clust<- as.numeric(J8$Clust)
J8bis<- J8 %>% group_by(Clust) %>% summarize(Mode= getmode(Comhuit))
J8bis$Mode<- as.numeric(J8bis$Mode)
names(J8bis)[1]<- "zone final"

J9<- Tabfin9 %>% inner_join(tata, by=c("Longitude", "Latitude"))
J9<- J9 %>% select(-c(Longitude, Latitude))
J9$Comneuf<- as.numeric(J9$Comneuf)
J9$Clust<- as.numeric(J9$Clust)
J9bis<- J9 %>% group_by(Clust) %>% summarize(Mode= getmode(Comneuf))
J9bis$Mode<- as.numeric(J9bis$Mode)
names(J9bis)[1]<- "zone final"

load(file = "data/ICES/mean_prediction_byzone_1.Rdata")
summarycom1<- tete
summarycom1$Clust<- as.numeric(summarycom1$Clust)
load(file = "data/ICES/mean_prediction_byzone_2.Rdata")
summarycom2<- tete
summarycom2$Clust<- as.numeric(summarycom2$Clust)
load(file = "data/ICES/mean_prediction_byzone_3.Rdata")
summarycom3<- tete
summarycom3$Clust<- as.numeric(summarycom3$Clust)
load(file = "data/ICES/mean_prediction_byzone_4.Rdata")
summarycom4<- tete
summarycom4$Clust<- as.numeric(summarycom4$Clust)
load(file = "data/ICES/mean_prediction_byzone_5.Rdata")
summarycom5<- tete
summarycom5$Clust<- as.numeric(summarycom5$Clust)
load(file = "data/ICES/mean_prediction_byzone_6.Rdata")
summarycom6<- tete
summarycom6$Clust<- as.numeric(summarycom6$Clust)
load(file = "data/ICES/mean_prediction_byzone_7.Rdata")
summarycom7<- tete
summarycom7$Clust<- as.numeric(summarycom7$Clust)
load(file = "data/ICES/mean_prediction_byzone_8.Rdata")
summarycom8<- tete
summarycom8$Clust<- as.numeric(summarycom8$Clust)
load(file = "data/ICES/mean_prediction_byzone_9.Rdata")
summarycom9<- tete
summarycom9$Clust<- as.numeric(summarycom9$Clust)

Mean1<- J1bis %>% full_join(summarycom1, by=c("Mode"="Clust"))
Mean1<- na.omit(Mean1)
Mean1<- Mean1 %>% select(-Mode)
names(Mean1)[2]<- "Com1"
Mean2<- J2bis %>% full_join(summarycom2, by=c("Mode"="Clust"))
Mean2<- na.omit(Mean2)
Mean2<- Mean2 %>% select(-Mode)
names(Mean2)[2]<- "Com2"
Mean3<- J3bis %>% full_join(summarycom3, by=c("Mode"="Clust"))
Mean3<- na.omit(Mean3)
Mean3<- Mean3 %>% select(-Mode)
names(Mean3)[2]<- "Com3"
Mean4<- J4bis %>% full_join(summarycom4, by=c("Mode"="Clust"))
Mean4<- na.omit(Mean4)
Mean4<- Mean4 %>% select(-Mode)
names(Mean4)[2]<- "Com4"
Mean5<- J5bis %>% full_join(summarycom5, by=c("Mode"="Clust"))
Mean5<- na.omit(Mean5)
Mean5<- Mean5 %>% select(-Mode)
names(Mean5)[2]<- "Com5"
Mean6<- J6bis %>% full_join(summarycom6, by=c("Mode"="Clust"))
Mean6<- na.omit(Mean6)
Mean6<- Mean6 %>% select(-Mode)
names(Mean6)[2]<- "Com6"
Mean7<- J7bis %>% full_join(summarycom7, by=c("Mode"="Clust"))
Mean7<- na.omit(Mean7)
Mean7<- Mean7 %>% select(-Mode)
names(Mean7)[2]<- "Com7"
Mean8<- J8bis %>% full_join(summarycom8, by=c("Mode"="Clust"))
Mean8<- na.omit(Mean8)
Mean8<- Mean8 %>% select(-Mode)
names(Mean8)[2]<- "Com8"
Mean9<- J9bis %>% full_join(summarycom9, by=c("Mode"="Clust"))
Mean9<- na.omit(Mean9)
Mean9<- Mean9 %>% select(-Mode)
names(Mean9)[2]<- "Com9"


Jfin <- Mean1 %>% left_join(Mean2, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean3, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean4, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean5, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean6, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean7, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean8, by="zone final") 
Jfin <-   Jfin %>% left_join(Mean9, by="zone final")

Jfin<- Jfin %>% mutate("Community maj"=NA)
Jfin$`Community maj`<- as.character(Jfin$`Community maj`)
Jfin[1, 11]<- "Com2"
  Jfin[2, 11]<- "Com2"
  Jfin[3, 11]<- "Com9"
  Jfin[4, 11]<- "Com2"
  Jfin[5, 11]<-  "Com9"


ggplot(Jfin, aes(x=))






