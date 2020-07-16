library(ggplot2)
library(dplyr)
library(FactoMineR)   # pour MCA
library(missMDA)
library(cluster)  # pour agnes
library(RColorBrewer)
library(fastcluster)
library(NbClust)
library(sp)
library(raster)
library(cowplot)
setwd("../")

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
load("results/Communautes bio/Zones/Community1_polygons.Rdata")
pol1<- pol
load("results/Communautes bio/Zones/Community2_polygons.Rdata")
pol2<- pol
load("results/Communautes bio/Zones/Community3_polygons.Rdata")
pol3<- pol
load("results/Communautes bio/Zones/Community4_polygons.Rdata")
pol4<- pol
load("results/Communautes bio/Zones/Community5_polygons.Rdata")
pol5<- pol
load("results/Communautes bio/Zones/Community6_polygons.Rdata")
pol6<- pol
load("results/Communautes bio/Zones/Community7_polygons.Rdata")
pol7<- pol
load("results/Communautes bio/Zones/Community8_polygons.Rdata")
pol8<- pol
load("results/Communautes bio/Zones/Community9_polygons.Rdata")
pol9<- pol
}
load("data/Polycut.Rdata")
r0<-rasterFromXYZ(Tabfin9)
r1<-rasterize(pol1,r0)
r2<-rasterize(pol2,r0)
r3<-rasterize(pol3,r0)
r4<-rasterize(pol4,r0)
r5<-rasterize(pol5,r0)
r6<-rasterize(pol6,r0)
r7<-rasterize(pol7,r0)
r8<-rasterize(pol8,r0)
r9<-rasterize(pol9,r0)
rcom<-stack(r1,r2,r3,r4,r5,r6,r7,r8,r9)
names(rcom)<-c("P1","P2","P3","P4","B1","B2","B3","B4","C")
dcom<-data.frame(rasterToPoints(rcom))
for(i in 3:11){
	dcom[,i]<-paste0(names(dcom)[i],dcom[,i])
}

#read raw kriged data
load("data/krigeage log.RData")
names(Kriege.logdens)[6]<- "com"
Kriege.logdens$Community<- as.numeric(Kriege.logdens$Community)
tcom<-Kriege.logdens%>%
	transmute(x=Longitude,y=Latitude,com=Community,year=Year,pred=Prediction)
#map com name
comid<-data.frame(com=1:9,nom=c("P1","P2","P3","P4","B1","B2","B3","B4","C"))
tcom<-left_join(tcom,comid)%>%mutate(com=nom)%>%dplyr::select(-nom)
#tcom<-tcom%>%tidyr::pivot_wider(names_from=year,values_from=pred)
pipo<-cbind(tcom[,1:2],data.frame(extract(rcom,tcom[,1:2])))%>%
	tidyr::pivot_longer(P1:C,names_to="com",values_to="subcom")%>%
	mutate(subcom=paste0(com,subcom))%>%distinct()
tcom<-left_join(tcom,pipo)

fctgraph<-function(tcom,com0="P1"){
pltcom<-ggplot(tcom%>%filter(com==com0)%>%dplyr::select(x,y,subcom)%>%distinct(),
	     aes(x=x,y=y,fill=subcom))+
	geom_raster()+
	scale_fill_brewer(palette="Set2",name="Zones")+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(tcom$x),ylim=range(tcom$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme_bw()+
	theme(legend.position="bottom")
plttcom<-ggplot(tcom%>%filter(com==com0),#%>%group_by(subcom,year)%>%summarise(m=mean(pred)),
	     aes(x=year,y=pred,color=subcom,group=subcom))+
	scale_color_brewer(palette="Set2",name="Zones")+
	theme_bw()+xlab("Année")+ylab("Densité")+
	ggtitle("Evolutions temporelles par zone")+ geom_point(alpha=.5,color="grey")+
	facet_grid(subcom~.)+
	geom_smooth(span=0.5)+theme(legend.position="none")
bxpltcom<-ggplot(tcom%>%filter(com==com0),#%>%group_by(subcom,year)%>%summarise(m=mean(pred)),
	     aes(x=subcom,y=pred,fill=subcom,group=subcom))+
	geom_boxplot()+
	scale_fill_brewer(palette="Set2",name="Zones")+
	ggtitle("Boxplot par zone")+
	theme_bw()+ theme(legend.position="none")+xlab("Zones")+ylab("Densité")
pltfinal<-ggplot()+
	theme_void()+
	coord_equal(xlim=c(0,100),ylim=c(0,100),expand=F)+
	annotation_custom(ggplotGrob(pltcom),xmin=0,xmax=100,ymin=50,ymax=100)+
	annotation_custom(ggplotGrob(plttcom),xmin=0,xmax=50,ymin=0,ymax=50)+
	annotation_custom(ggplotGrob(bxpltcom),xmin=50,xmax=100,ymin=0,ymax=50)+
	ggtitle(paste0("Zonation ",com0))
ggsave(file=paste0("./results/Communautes bio/Zones/Zonation",com0,".png"),pltfinal)
return(pltfinal)
}
fctgraph(tcom%>%filter(year>1014),"P1")
fctgraph(tcom%>%filter(year>1014),"P2")
fctgraph(tcom%>%filter(year>1014),"P3")
fctgraph(tcom%>%filter(year>1014),"P4")
fctgraph(tcom%>%filter(year>2014),"B1")
fctgraph(tcom%>%filter(year>2014),"B2")
fctgraph(tcom%>%filter(year>2014),"B3")
fctgraph(tcom%>%filter(year>2014),"B4")
fctgraph(tcom%>%filter(year>1014),"C")



#MCA
rez<- MCA(dcom[,-c(1,2)], ncp=999, method="Burt", graph=F)                                                      
#rez<- MCA(dcom[,c("P1","P2","P3","P4")], ncp=999, method="Burt", graph=F)                                                      
plt1<- plotellipses(rez, axes=c(1,2))
plt2<- plotellipses(rez, axes=c(1,3))
plot(rez)
plt1
summary(rez)
library(factoextra)
fviz_eig(rez)
plot(rez$eig[,3])
(rez$eig[,3])
# Classification
arbre<- hclust(dist(rez$ind$coord[,1:12]), method="ward.D2")
#arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
#save(arbre, file="data/ICES/arbre_régiona_fin_communi_dens.Rdata")
#load("data/ICES/arbre_régiona_fin_communi_dens.Rdata")
plot(arbre, which=2, hang=-1)
if(F){
source("./analyses/FastNbClust.R")
reztmp<-FastNbClust(data=rez$ind$coord[,1:12], min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
}
#nb cluster
nbk<-8
rect.hclust(arbre, k=nbk)
groups<- cutree(arbre, k=nbk)
plot(rasterFromXYZ( data.frame(rasterToPoints(rcom))%>%mutate(z=as.numeric(groups)) ))
#a graph to ocmpare stuff
dcom<- dcom%>%mutate(Clust=factor(groups))
Allcom<- ggplot(dcom)+
  geom_raster(aes(x=x, y=y, fill=Clust))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #ggtitle("Final bioregionalization")+
  ggtitle("Regionalisation biologique")+
  #scale_fill_gradientn(colours =brewer.pal(n = nbk, name = "YlGnBu")) +
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()
Allcom

#add clust to tcom then map with time series
cmap<-dcom%>%dplyr::select(x,y,Clust)%>%distinct()%>%rasterFromXYZ()
pipo<-cbind(tcom[,1:2],data.frame(extract(cmap,tcom[,1:2])))%>%distinct()
names(pipo)<-c("x","y","Clust")
tcom<-left_join(tcom,pipo)%>%mutate(Clust=as.character(Clust))
#remove before 2014 for B
tcom1<-tcom%>%filter(grepl("B",com),year>=2014)
tcom2<-tcom%>%filter(!grepl("B",com))
tcom<-rbind(tcom1,tcom2)

pltcom<-ggplot(tcom%>%dplyr::select(x,y,Clust)%>%distinct(),
	     aes(x=x,y=y,fill=Clust))+
	geom_raster()+
	scale_fill_brewer(palette="Set2",name="Zones")+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(tcom$x),ylim=range(tcom$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme_bw()+
	theme(legend.position="right")
bxpltcom<-ggplot(tcom,#%>%filter(com==com0),#%>%group_by(subcom,year)%>%summarise(m=mean(pred)),
	     aes(x=subcom,y=pred,fill=Clust,color=Clust,group=subcom))+
	geom_boxplot(outlier.color="grey")+#outlier.shape=NA,coef=1e30,color="black")+
	scale_y_log10()+
	scale_fill_brewer(palette="Set2",name="Zones")+
	scale_color_brewer(palette="Set2",name="Zones")+
	#ggtitle("Boxplot par zone")+
	facet_grid(Clust~com,scale="free_x",drop=T)+
	theme_bw()+ theme(legend.position="none", axis.text.x = element_text(angle=90, hjust=1))+
	xlab("Zones")+ylab("Densité (log10)")
bxpltcom
pltfinal<-ggplot()+
	theme_void()+
	coord_equal(xlim=c(0,100),ylim=c(0,100),expand=F)+
	annotation_custom(ggplotGrob(pltcom),xmin=0,xmax=100,ymin=70,ymax=100)+
	#annotation_custom(ggplotGrob(plttcom),xmin=0,xmax=50,ymin=0,ymax=50)+
	annotation_custom(ggplotGrob(bxpltcom),xmin=00,xmax=100,ymin=0,ymax=70)+
	ggtitle(paste0("Zonation des communautés biologiques"))
pltfinal

ggsave(file=paste0("./results/Communautes bio/Zones/Zonation","Clust",".png"),pltfinal)



