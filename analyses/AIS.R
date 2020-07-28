#zonal classification of AIS dataaaaaa

library(sf)
library(tidyverse)
library(raster)
library(rasterVis)

#load the data again
allais<-stack("../data/humanactivities/allais")


#buil a stack by maritime activities
#fishing : st_01
#service : Service (pilog, search and rescue, port tender, antipol medi) st_02, 
#		tug towing st_07
#		military st_11
#dredging : st_03
#sailing: sailing st04 + pleasure craft st05
#transport: high-speed st06, passenger st08, cargo st09, tanker st10
#fishing
fishingAIS<-allais[[which(grepl("st_01",names(allais)))]]
levelplot((fishingAIS)+1,zscaleLog=T)
levelplot(mean(fishingAIS)+1,zscaleLog=T)
#service
serviceAIS<-allais[[which(grepl("st_02",names(allais)))]]
tugAIS<-allais[[which(grepl("st_07",names(allais)))]]
milAIS<-allais[[which(grepl("st_11",names(allais)))]]
serviceAIS<-serviceAIS+tugAIS+milAIS
levelplot((serviceAIS)+1,zscaleLog=T)
levelplot(mean(serviceAIS)+1,zscaleLog=T)
#dredging
dredgingAIS<-allais[[which(grepl("st_03",names(allais)))]]
levelplot(stack(dredgingAIS)+1,zscaleLog=T)
#sailing
sailingAIS<-allais[[which(grepl("st_04",names(allais)))]]
pleasureAIS<-allais[[which(grepl("st_05",names(allais)))]]
sailingAIS<-sailingAIS+pleasureAIS
levelplot((sailingAIS)+1,zscaleLog=T)
levelplot(mean(sailingAIS)+1,zscaleLog=T)
#dredging
dredgingAIS<-allais[[which(grepl("st_03",names(allais)))]]
levelplot(stack(dredgingAIS)+1,zscaleLog=T)
#transport
speedAIS<-allais[[which(grepl("st_06",names(allais)))]]
passengerAIS<-allais[[which(grepl("st_08",names(allais)))]]
cargoAIS<-allais[[which(grepl("st_09",names(allais)))]]
tankerAIS<-allais[[which(grepl("st_10",names(allais)))]]
transportAIS<-speedAIS+passengerAIS+cargoAIS+tankerAIS
levelplot((transportAIS)+1,zscaleLog=T)
levelplot(mean(transportAIS)+1,zscaleLog=T)

#test all parameters together annual values
fish=mean(fishingAIS);dred=mean(dredgingAIS)
serv=mean(serviceAIS);trans=mean(transportAIS);sail=mean(sailingAIS)
ais<-stack(fish,dred,serv,trans,sail)
names(ais)<-c("fish","dred","serv","trans","sail")
levelplot((ais)+1,zscaleLog=T)
#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
r0<- raster(nrow=30, ncol=80, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
ais<-crop(ais,extent(r0))
ais<-resample(ais,r0)
levelplot((ais)+1,zscaleLog=T)
plot(log10(1+scale(ais)))
#remove outliers (port machin essentiellement)
quantile(ais,seq(0,1,0.01))
plot(ais$fish)
ais$fish[ais$fish>quantile(ais$fish,.95)]<-median(ais$fish,na.rm=T)
ais$dred[ais$dred>quantile(ais$dred,.95)]<-median(ais$dred,na.rm=T)
ais$serv[ais$serv>quantile(ais$serv,.95)]<-median(ais$serv,na.rm=T)
ais$trans[ais$trans>quantile(ais$trans,.95)]<-median(ais$trans,na.rm=T)
ais$sail[ais$sail>quantile(ais$sail,.95)]<-median(ais$sail,na.rm=T)
#plot(scale(ais))
plot((ais))

#tabais<-rasterToPoints(log10(1+scale(ais)))
tabais<-rasterToPoints((ais))
tabais<-data.frame(tabais)
tabais1<-tabais%>%dplyr::select(-x,-y)


#dais<- dist(tabais1)]
#parallelDist package to speed up computation time
dais<-parallelDist::parDist(as.matrix(tabais1))

#fast clustering
tree<-fastcluster::hclust(dais,method="ward.D2")
plot(tree)
stop("ici")


#if(file.exists("nbclustAIS.rds")){
#	rez<-readRDS("nbclustAIS.rds")
#}else{
#	source("FastNbClust.R")
#	rez<-FastNbClust(tabais1, diss=dais,distance=NULL,min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
#	saveRDS(rez,file="nbclustAIS.rds")
#}
# According to the majority rule, the best number of clusters is  6
plot(tree, which=2,hang=-1)
rect.hclust(tree,3)
#distance[1:5]
rect.hclust(tree,3)
zones<-cutree(tree,3)

pipo<-rasterFromXYZ(data.frame(x=tabais$x,y=tabais$y,z=zones))
plot(stack(ais,pipo))
plot(pipo)

#summary the results to help analyses
tabais1%>%mutate(zones=zones)%>%group_by(zones)%>%
	summarise_all(.funs=list(mean=median))#,med=median,max=max,min=min))%>%t()#(aa=summary(fish))
#raster to pol ?
#pol<-rasterToPolygons(pipo,dissolve=T)
#plot(pipo)
#plot(pol,add=T)
#using sf
#pol1<-st_as_sf(pol)
#plot(pol1)
#smoothing polygon edge
#library(sf);library(smoothr)
#plot(pipo)
#plot(smooth(pol1,method="ksmooth",smoothness=3),add=T,alpha=.3)

#test ggplot complet 
tmp<-tabais%>%mutate(zones=zones)
library(cowplot)
mapz<-ggplot()+
	#geom_sf(st_as_sf(pipo))+
	geom_raster(data=tmp,aes(x=x,y=y,fill=as.character(zones)))+
	scale_fill_brewer(palette="Spectral",name="Zones")+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(tmp$x),ylim=range(tmp$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme(legend.position="bottom")

tmp2<-tmp%>%pivot_longer(fish:sail)
tmp2$name[tmp2$name=="dred"]<-"dragage"
tmp2$name[tmp2$name=="fish"]<-"pêche"
tmp2$name[tmp2$name=="sail"]<-"plaisance"
tmp2$name[tmp2$name=="serv"]<-"services"
tmp2$name[tmp2$name=="trans"]<-"transport"
tmp2<-tmp2%>%mutate(zones=as.factor(zones))

bpall<-ggplot()+
	geom_boxplot(data=tmp2,aes(x=zones,y=value,fill=zones))+
	scale_fill_brewer(palette="Spectral")+
	facet_wrap(~name,scale="free_y",ncol=5)+
	ylab("Densité de navires en heures par km^2")+
	theme(legend.position="none")
	#scale_y_log10()

	

ggplot()+
	theme_void()+
	coord_equal(xlim=c(0,100),ylim=c(0,100),expand=F)+
	annotation_custom(ggplotGrob(mapz),xmin=0,xmax=100,ymin=50,ymax=100)+
	annotation_custom(ggplotGrob(bpall),xmin=0,xmax=100,ymin=0,ymax=50)+
	#ggtitle("Vessels activities zonation using AIS data")
	ggtitle("Zonation des activités maritimes vues par l'AIS")

#save the figures
ggsave(file="../results/AIS/zones.png")
#save the data 
tabais<-tmp
save(tabais,file="../results/AIS/tabais.rdata")






