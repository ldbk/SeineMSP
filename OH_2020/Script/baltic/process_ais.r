# Test script localy
wdpaid <- '18_54_22_56'

#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])
temp_path<- "c:/temp"

library(knitr)
library(kableExtra)

library(rgdal)
library(downloader)
library(ggplot2)
library(mapdata)
library(geojsonio)
library(ggmap)
library(ggrepel)


library("rgdal")
library("rasterVis")
library("downloader")
library("ggplot2")


library("XML")
library("RCurl")
library("bitops")
library("lattice")
library("latticeExtra")
library("RColorBrewer")
library("mapdata")
library("maps")
library("maptools")
library("wq")
library("xtable")
library("zoo")
library("jsonlite")
library(ncdf4)

require(xtable)

library(lattice)

library(dplyr)
library(raster)
library(rworldmap)
library(rworldxtra)
library(NbClust)
library(tidyr)
library(dplyr)
library(ggplot2)
library(fastcluster)
library(FactoMineR)
library(sf)


# Script for Wekeo environment
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

bbox<-paste(xmin,ymin,xmax,ymax,sep=",")


#FUNCTION GET VESSELDENSITY
#=================================================

getvesseldensityLite<-function (name = "emodnet:2017_01_st_All", resolution = "30 arcsec / 900m", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{
bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")                           
con <- paste(ogc_url,"/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",name,"&crs=EPSG:4326&BBOX=", bbox, "&format=image/tiff&interpolation=nearest&resx=0.00833333&resy=0.00833333", sep = "") 
#con <- paste("http://77.246.172.208/geoserver/emodnet/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",name,"&crs=EPSG:3035&BBOX=", bbox, "&format=image/tiff&interpolation=nearest&resx=0.00833333&resy=0.00833333", sep = "") 
nomfich <- paste(name, "img.tiff", sep = "_")
nomfich <- tempfile(nomfich)
download(con, nomfich, quiet = TRUE, mode = "wb")
img <- raster(nomfich)
img[img == 0] <- NA
#img[img < 0] <- 0
#img[img > 100] <- 0
names(img) <- paste(name)
return(img)
}


getvesseldensity_byvesseltype<-function (name = "emodnet:2017_01_st_All", vessel_type= "st_09" ,resolution = "30 arcsec / 900m", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5) 
{
##################### getvesseldensityLite2017
for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')){
	print(paste("The month is", month))
	img<-getvesseldensityLite(name = paste("emodnet:2017_",month,"_",vessel_type, sep=""), resolution = "30 arcsec", xmin, xmax, ymin, ymax)
	names(img) <- paste("2017-",month, sep="")
	if (month == '01') {imgs <- img
} else {
 	imgs <- stack(imgs, img)
	}
}
mpa_vesselAll2017<-imgs
##################### getvesseldensityLite2018
for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')){
	print(paste("The month is", month))
	img<-getvesseldensityLite(name = paste("emodnet:2018_",month,"_",vessel_type, sep=""), resolution = "30 arcsec", xmin, xmax, ymin, ymax)
	names(img) <- paste("2018-",month, sep="")
	if (month == '01') {imgs <- img
} else {
 	imgs <- stack(imgs, img)
	}
}
mpa_vesselAll2018<-imgs
##################### getvesseldensityLite2019
for (month in c('01','02','03','04','05','06','07','08','09','10','11','12')){
	print(paste("The month is", month))
	img<-getvesseldensityLite(name = paste("emodnet:2019_",month,"_",vessel_type, sep=""), resolution = "30 arcsec", xmin, xmax, ymin, ymax)
	names(img) <- paste("2019-",month, sep="")
	if (month == '01') {imgs <- img
} else {
 	imgs <- stack(imgs, img)
	}
}
mpa_vesselAll2019<-imgs

mpa<-stack(mpa_vesselAll2017,mpa_vesselAll2018,mpa_vesselAll2019)

return(mpa)
}



#####################
layer_title<-"Vessel density (Cargo) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_09"

mpa_cargo<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (dredging or underwater ops) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_03"

mpa_dredging<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (High Speed Craft) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_06"

mpa_speedcraft<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Fishing) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_01"

mpa_fishing<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Military and Law Enforcement) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_11"

mpa_military<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Passenger) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_08"

mpa_passenger<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Pleasure Craft) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_05"

mpa_pleasure<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Sailing) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_04"

mpa_sailing<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Service) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_02"

mpa_service<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Tanker) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_10"

mpa_tanker<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Tug and Towing) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_07"

mpa_tug<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#####################
layer_title<-"Vessel density (Unknown) 2017-2019"
ogc_url <- "https://ows.emodnet-humanactivities.eu"
vessel_type <- "st_12"

mpa_unknown<-getvesseldensity_byvesseltype(name, vessel_type, resolution = "30 arcsec/900m", xmin, xmax, ymin, ymax)

#nomobj<-ls(patt="mpa_")
#save(list=nomobj,file="envais.rdata")

###########PROCESSING

#land data to mask data
landmask <- rworldmap::getMap(resolution = "high")

#load data
# Test script localy
#wdpaid <- '18_54_22_56'
load("../../Data/baltic/envais.rdata")

#raster 2 datafram
r2df<-function(r){
	tmp<-raster::as.data.frame(r,xy=T)%>%
		tidyr::pivot_longer(3:(2+dim(r)[3]))
	return(tmp)
}
#group vessel cat + reduce spatial resolution + mask to avoid onland info
aggfac<-4 # reduction factor
#allmpa
allmpa<-list(mpa_fishing,mpa_dredging,mpa_sailing,mpa_pleasure,mpa_speedcraft,mpa_passenger,mpa_cargo,mpa_tanker,mpa_service,mpa_tug,mpa_military)
#aggregate
allmpa<-lapply(allmpa,aggregate,fact=aggfac)
#mask
allmpa<-lapply(allmpa,mask,mask=landmask,inverse=T)
#yearly
fsapply<-function(a){return(stackApply(a,indices=substr(names(a),2,5),fun=mean,na.rm=T))}
fsapply(mpa_fishing)
allmpa<-lapply(allmpa,fsapply)

#individual group or sum
#fishing
fishAIS<-r2df(allmpa[[1]])
#dredging
dredAIS<-r2df(allmpa[[2]])
#sailing
tmp1<-r2df(allmpa[[3]])%>%mutate(value1=value)%>%dplyr::select(-value)
tmp2<-r2df(allmpa[[4]])%>%mutate(value2=value)%>%dplyr::select(-value)
tmp<-full_join(tmp1,tmp2)
tmp$sum<-apply(tmp[,4:5],1,sum,na.rm=T)
sailAIS<-tmp%>%transmute(x,y,name,value=sum)
#transport
tmp1<-r2df(allmpa[[5]])%>%mutate(value1=value)%>%dplyr::select(-value)
tmp2<-r2df(allmpa[[6]])%>%mutate(value2=value)%>%dplyr::select(-value)
tmp3<-r2df(allmpa[[7]])%>%mutate(value3=value)%>%dplyr::select(-value)
tmp4<-r2df(allmpa[[8]])%>%mutate(value4=value)%>%dplyr::select(-value)
tmp<-full_join(full_join(full_join(tmp1,tmp2),tmp3),tmp4)
tmp$sum<-apply(tmp[,4:7],1,sum,na.rm=T)
tranAIS<-tmp%>%transmute(x,y,name,value=sum)
#service
tmp1<-r2df(allmpa[[8]])%>%mutate(value1=value)%>%dplyr::select(-value)
tmp2<-r2df(allmpa[[9]])%>%mutate(value2=value)%>%dplyr::select(-value)
tmp3<-r2df(allmpa[[10]])%>%mutate(value3=value)%>%dplyr::select(-value)
tmp<-full_join(full_join(tmp1,tmp2),tmp3)
tmp$sum<-apply(tmp[,4:6],1,sum,na.rm=T)
servAIS<-tmp%>%transmute(x,y,name,value=sum)
#eliminate pt at land
landmask2<-st_as_sf(landmask)
pt2<-st_as_sf(fishAIS,coords=c("x","y"),crs=crs(landmask2))
landmask2<-st_crop(st_as_sf(landmask),extent(pt2))
#pt at sea 
ptin<-!apply(st_intersects(pt2,landmask2,sparse=F),1,any)
#plot(alldf[[1]][ptin,1:2])
#list of all stuff
alldf<-list(fishAIS[ptin,],dredAIS[ptin,],sailAIS[ptin,],tranAIS[ptin,],servAIS[ptin,])
#fct to add a name
fname<-function(a,nom){a$type<-nom;return(a)}
maptmp<-do.call("rbind",Map(fname,alldf,c("fish","dred","sail","tran","serv")))
ggplot(maptmp,aes(x=x,y=y,fill=value))+geom_raster()+
	scale_fill_distiller(palette="Spectral",name="Zone",trans="log10")+
	facet_grid(type~name)+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(maptmp$x),ylim=range(maptmp$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme_bw()#+
	#theme(legend.position="bottom")

#classif 
classif<-function(tmp){
	#tmp<-alldf[[4]]%>%group_by(name)%>%
	tmp<-tmp%>%group_by(name)%>%
		mutate(m=min(value,na.rm=T),
		       q=quantile(value,.95,na.rm=T))%>%
		ungroup()%>%
		mutate(value=ifelse(is.na(value),0,value))%>%
		dplyr::select(-m,-q)
	#tmp<-alldf[[1]]
	#tmp$value<-log10(tmp$value+1)
	tmp$value<-log10(tmp$value+1)
	mat0<-tmp%>%tidyr::pivot_wider(values_from=value,names_from=name,values_fill=0)
	idptNA<-is.finite(apply(mat0[,-c(1:2)],1,sum))
	#str(mat0[,-c(1:2)])
	d<-stats::dist((mat0[,-c(1:2)]))
	clust <-fastcluster::hclust(d, method = "ward.D2")
	pipo<-data.frame(x=mat0$x,y=mat0$y,zone=cutree(clust,6))
	return(pipo)
}
allzone<-lapply(alldf,classif)
maptmp<-do.call("rbind",Map(fname,allzone,c("fish","dred","sail","tran","serv")))
ggplot(maptmp,
       aes(x=x,y=y,fill=as.factor(zone)))+geom_raster()+
	scale_fill_brewer(palette="Set3",name="Cluster")+#,trans="log10")+
	#de\n navires\n(h.km^2)",trans="log10")+
	facet_wrap(~type,ncol=2)+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(maptmp$x),ylim=range(maptmp$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme_bw()#+

#MCA
uu<-do.call("rbind",Map(fname,allzone,c("fish","dred","sail","tran","serv")))%>%
	mutate(zonetype=paste0(type,"-",zone))%>%
	dplyr::select(-zone)%>%
	pivot_wider(values_from=zonetype,names_from=type)
mca <- MCA(uu[,-c(1,2)], ncp=999, method="Burt", graph=F)
#
#plotellipses(mca, axes = c(1,2))
#plotellipses(mca, axes = c(1,3))

#final classif
d <- dist(mca$ind$coord)
clust <- fastcluster::hclust(d, method="ward.D2")
groups <- cutree(clust,6)

finalzone <- data.frame(uu[,c(1,2)],Cluster=factor(groups))

ggplot(finalzone,
       aes(x=x,y=y,fill=Cluster))+geom_raster()+
	scale_fill_brewer(palette="Set3",name="Cluster")+
	borders("world",fill="grey",colour=NA)+
	coord_sf(xlim=range(finalzone$x),ylim=range(finalzone$y))+
	xlab("Longitude")+ylab("Latitude")+
	theme_bw()#+

#add stuff
allrez<-do.call("rbind",Map(fname,alldf,c("fish","dred","sail","tran","serv")))%>%
	left_join(finalzone)

ggplot()+
	  geom_boxplot(data=allrez,aes(x=type,y=value,fill=type))+
	    facet_wrap(~Cluster,nrow=2)+xlab("type")+ylab("Parameters")+
	    scale_y_log10()+
	      scale_fill_brewer(palette="Set3")
	






