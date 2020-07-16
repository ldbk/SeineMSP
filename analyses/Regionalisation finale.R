library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
library(RColorBrewer)
library(NbClust)
library(fastcluster) # pour hclust


########################
{
load(file="data/satellite/Detritus/TabDetfin.Rdata")
load(file="data/satellite/Primary production/TabPPfin.Rdata")
load(file="data/satellite/sst/Tabsstfin.Rdata")
load(file="data/satellite/chl/Tabchlfin.Rdata")
load(file="data/satellite/Turbidity/TabTurbfin.Rdata")
load(file="data/satellite/Salinity/TabSalfin.Rdata")
load(file="data/satellite/Particles/TabPartfin.Rdata")
load(file="data/satellite/O2/TabO2fin.Rdata")

load("data/satellite/Detritus/Det_polygons.Rdata")
load("data/satellite/Primary production/PP_polygons.Rdata")
load("data/satellite/sst/sst_polygons.Rdata")
load("data/satellite/chl/chl_polygons.Rdata")
load("data/satellite/Turbidity/Turb_polygons.Rdata")
load("data/satellite/Salinity/Sal_polygons.Rdata")
load("data/satellite/Particles/Part_polygons.Rdata")
load("data/satellite/O2/O2_polygons.Rdata")

load("data/PolyCut.Rdata")
}

# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(Long = seq(from =  min(TabSalfin$x),                                                 
                              to = max(TabSalfin$x),                                                    
                              by = 0.01),
                   Lat = seq(from =min(TabSalfin$y),
                             to = max(TabSalfin$y), 
                             by = 0.01))  

points <- structure(list(grd$Long, grd$Lat), .Names = c("Long", "Lat"), 
                    class = "data.frame", row.names = c(NA, dim(grd)[1]))                               
spdf <- SpatialPointsDataFrame(coords = points, data = points)                                          

noms <- c("polChl","polDet","polO2","polPart","polPP","polSal","polSST","polTurb")
t <- 0
for (i in list(polChl,polDet,polO2,polPart,polPP,polSal,polSST,polTurb)){
  t <- t+1
  pipo <- sp::over(spdf, i)
  pipo$Clust2 <- paste0(rep(substr(noms[t],4,nchar(noms[t])),dim(pipo)[1]), pipo$Clust)
  names(pipo) <- c("Clust",substr(noms[t],4,nchar(noms[t])))
  grd <- cbind(grd,pipo)
}
dim(na.omit(grd))
grd2<- na.omit(grd)                                                                                           
plot(grd2[,c(1,2)])
grd2<- grd2[, !duplicated(colnames(grd2))]
grd2<- grd2[,-3]                                                                                               


# ACM
rez<- MCA(grd2[,-c(1,2)], ncp=999, method="Burt", graph=F)                                                      
plt1<- plotellipses(rez, axes=c(1,2))
plt2<- plotellipses(rez, axes=c(1,3))


# Classification
arbre<- hclust(dist(rez$ind$coord), method="ward.D2")
plot(arbre, which=2,hang=-1)
#NbClust(rez$ind$coord, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
# According to the majority rule, the best number of clusters is  10 (5 indicateurs, puis 4 indicateurs pour 6, 3 ou 2 clusters)
rect.hclust(arbre, k=6)
groups<- cutree(arbre, k=6)


tata<- cbind(grd2[,c(1,2)],Clust=factor(groups))
save(tata, file="results/satellite/Coordzones.Rdata")

Allparam<- ggplot(tata)+
  geom_tile(aes(x=Long,y=Lat,fill= as.numeric(Clust)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  scale_fill_gradientn(colours =brewer.pal(n = 5, name = "YlOrBr")) +
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


ggsave(plot= Allparam2, filename="All.jpeg", path="results/satellite/zones", width = 13, height = 8)





# Caractéristiques de chaque zone

chl<- read.csv("results/satellite/means by zone/summarychl.csv", sep=";")
chl[1,1]<- "Chl1"
chl[2,1]<- "Chl2"
chl[3,1]<- "Chl3"
names(chl)[1]<- "Chl"
names(chl)[2]<- "meanChl"
Det<- read.csv("results/satellite/means by zone/summaryDet.csv", sep=";")
Det[1,1]<- "Det1"
Det[2,1]<- "Det2"
names(Det)[1]<- "Det"
names(Det)[2]<- "meanDet"
Part<- read.csv("results/satellite/means by zone/summaryPart.csv", sep=";")
Part[1,1]<- "Part1"
Part[2,1]<- "Part2"
names(Part)[1]<- "Part"
names(Part)[2]<- "meanPart"
sst<- read.csv("results/satellite/means by zone/summarysst.csv", sep=";")
sst[1,1]<- "sst1"
sst[2,1]<- "sst2"
names(sst)[1]<- "SST"
names(sst)[2]<- "meanSST"
Turb<- read.csv("results/satellite/means by zone/summaryTurb.csv", sep=";")
Turb[1,1]<- "Turb1"
Turb[2,1]<- "Turb2"
Turb[3,1]<- "Turb3"
names(Turb)[1]<- "Turb"
names(Turb)[2]<- "meanTurb"
PP<- read.csv("results/satellite/means by zone/summaryPP.csv", sep=";")
PP[1,1]<- "PP1"
PP[2,1]<- "PP2"
names(PP)[1]<- "PP"
names(PP)[2]<- "meanPP"
O2<- read.csv("results/satellite/means by zone/summaryO2.csv", sep=";")
O2[1,1]<- "O21"
O2[2,1]<- "O22"
O2[3,1]<- "O23"
names(O2)[1]<- "O2"
names(O2)[2]<- "meanO2"
Sal<- read.csv("results/satellite/means by zone/summarySal.csv", sep=";")
Sal[1,1]<- "Sal1"
Sal[2,1]<- "Sal2"
Sal[3,1]<- "Sal3"
names(Sal)[1]<- "Sal"
names(Sal)[2]<- "meanSal"



AAA<- tata %>% left_join(grd2, by=c("Long","Lat"))
#AAA<- AAA %>% dplyr::select(-Long, -Lat)

AAA<- AAA %>% left_join(chl, by="Chl")
AAA<- AAA %>% left_join(Det, by="Det")
AAA<- AAA %>% left_join(sst, by="SST")
AAA<- AAA %>% left_join(Turb, by="Turb")
AAA<- AAA %>% left_join(PP, by="PP")
AAA<- AAA %>% left_join(O2, by="O2")
AAA<- AAA %>% left_join(Sal, by="Sal")
AAA<- AAA %>% left_join(Part, by="Part")


{
  zone1<- AAA %>% filter(Clust==1)
  zone1<- zone1 %>% dplyr::select(-c(1:11))
  
  zone1<- zone1 %>% mutate(moyChl= mean(meanChl))
  zone1<- zone1 %>% mutate(moyDet= mean(meanDet))
  zone1<- zone1 %>% mutate(moysst= mean(meanSST))
  zone1<- zone1 %>% mutate(moyTurb= mean(meanTurb))
  zone1<- zone1 %>% mutate(moyO2= mean(meanO2))
  zone1<- zone1 %>% mutate(moySal= mean(meanSal))
  zone1<- zone1 %>% mutate(moyPart= mean(meanPart))
  zone1<- zone1 %>% mutate(moyPP= mean(meanPP))
  
  zone1<- zone1 %>% dplyr::select(-c(1:8))
  zone1<- unique(zone1)
}

{
zone2<- AAA %>% filter(Clust==2)
zone2<- zone2 %>% dplyr::select(-c(1:11))

zone2<- zone2 %>% mutate(moyChl= mean(meanChl))
zone2<- zone2 %>% mutate(moyDet= mean(meanDet))
zone2<- zone2 %>% mutate(moysst= mean(meanSST))
zone2<- zone2 %>% mutate(moyTurb= mean(meanTurb))
zone2<- zone2 %>% mutate(moyO2= mean(meanO2))
zone2<- zone2 %>% mutate(moySal= mean(meanSal))
zone2<- zone2 %>% mutate(moyPart= mean(meanPart))
zone2<- zone2 %>% mutate(moyPP= mean(meanPP))

zone2<- zone2 %>% dplyr::select(-c(1:8))
zone2<- unique(zone2)
}

{
zone3<- AAA %>% filter(Clust==3)
zone3<- zone3 %>% dplyr::select(-c(1:11))

zone3<- zone3 %>% mutate(moyChl= mean(meanChl))
zone3<- zone3 %>% mutate(moyDet= mean(meanDet))
zone3<- zone3 %>% mutate(moysst= mean(meanSST))
zone3<- zone3 %>% mutate(moyTurb= mean(meanTurb))
zone3<- zone3 %>% mutate(moyO2= mean(meanO2))
zone3<- zone3 %>% mutate(moySal= mean(meanSal))
zone3<- zone3 %>% mutate(moyPart= mean(meanPart))
zone3<- zone3 %>% mutate(moyPP= mean(meanPP))

zone3<- zone3 %>% dplyr::select(-c(1:8))
zone3<- unique(zone3)
}


{
zone4<- AAA %>% filter(Clust==4)
zone4<- zone4 %>% dplyr::select(-c(1:11))

zone4<- zone4 %>% mutate(moyChl= mean(meanChl))
zone4<- zone4 %>% mutate(moyDet= mean(meanDet))
zone4<- zone4 %>% mutate(moysst= mean(meanSST))
zone4<- zone4 %>% mutate(moyTurb= mean(meanTurb))
zone4<- zone4 %>% mutate(moyO2= mean(meanO2))
zone4<- zone4 %>% mutate(moySal= mean(meanSal))
zone4<- zone4 %>% mutate(moyPart= mean(meanPart))
zone4<- zone4 %>% mutate(moyPP= mean(meanPP))

zone4<- zone4 %>% dplyr::select(-c(1:8))
zone4<- unique(zone4)
}

{
zone5<- AAA %>% filter(Clust==5)
zone5<- zone5 %>% dplyr::select(-c(1:11))

zone5<- zone5 %>% mutate(moyChl= mean(meanChl))
zone5<- zone5 %>% mutate(moyDet= mean(meanDet))
zone5<- zone5 %>% mutate(moysst= mean(meanSST))
zone5<- zone5 %>% mutate(moyTurb= mean(meanTurb))
zone5<- zone5 %>% mutate(moyO2= mean(meanO2))
zone5<- zone5 %>% mutate(moySal= mean(meanSal))
zone5<- zone5 %>% mutate(moyPart= mean(meanPart))
zone5<- zone5 %>% mutate(moyPP= mean(meanPP))

zone5<- zone5 %>% dplyr::select(-c(1:8))
zone5<- unique(zone5)
}

{
zone6<- AAA %>% filter(Clust==6)
zone6<- zone6 %>% dplyr::select(-c(1:11))

zone6<- zone6 %>% mutate(moyChl= mean(meanChl))
zone6<- zone6 %>% mutate(moyDet= mean(meanDet))
zone6<- zone6 %>% mutate(moysst= mean(meanSST))
zone6<- zone6 %>% mutate(moyTurb= mean(meanTurb))
zone6<- zone6 %>% mutate(moyO2= mean(meanO2))
zone6<- zone6 %>% mutate(moySal= mean(meanSal))
zone6<- zone6 %>% mutate(moyPart= mean(meanPart))
zone6<- zone6 %>% mutate(moyPP= mean(meanPP))

zone6<- zone6 %>% dplyr::select(-c(1:8))
zone6<- unique(zone6)
}


zone1<- zone1 %>% mutate(Zone= "Zone1")
zone2<- zone2 %>% mutate(Zone= "Zone2")
zone3<- zone3 %>% mutate(Zone= "Zone3")
zone4<- zone4 %>% mutate(Zone= "Zone4")
zone5<- zone5 %>% mutate(Zone= "Zone5")
zone6<- zone6 %>% mutate(Zone= "Zone6")


Zones<- zone1 %>% bind_rows(zone2)
Zones<- Zones %>% bind_rows(zone3)
Zones<- Zones %>% bind_rows(zone4)
Zones<- Zones %>% bind_rows(zone5)
Zones<- Zones %>% bind_rows(zone6)

Zones1<- data.frame(Zones[,-9])
row.names(Zones1)<- Zones[,9]


ggplot(Zones1)+
  geom_point(aes(x=))









