#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])


library(rgdal)
library(downloader)
library(ggplot2)
library(mapdata)
library(geojsonio)
library(ggmap)
library(ggrepel)

# EMIS-R Libraries

library("rasterVis")
library("XML")
library("RCurl")
library("bitops")
library("lattice")
library("latticeExtra")
#library("RColorBrewer")
library("maps")
library("maptools")
library("wq")
#library("xtable")
library("zoo")
#library("jsonlite")
#require(xtable)
library(dplyr)

#Libraries for mask
library(rworldmap)
library(rworldxtra)

library(worms)

# Script for Wekeo environment
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

bbox<-paste(xmin,ymin,xmax,ymax,sep=",")

#Write NA
{
  HH==-9
  which(HH==-9, arr.ind = T)
  HH[which(HH==-9, arr.ind = T)]
  HH[which(HH==-9, arr.ind = T)]<- NA
}

{
  HL==-9
  which(HL==-9, arr.ind = T)
  HL[which(HL==-9, arr.ind = T)]
  HL[which(HL==-9, arr.ind = T)]<- NA
}

HH<- HH %>% dplyr::select(StNo ,HaulNo ,Year ,ShootLat,ShootLong ,HaulLat ,HaulLong ,Distance )
HL<- HL %>% dplyr::select(StNo ,HaulNo ,Year ,SpecCode, TotalNo)
HL<- unique(HL)

Bio <- HL %>% left_join(HH, by=c("StNo"="StNo","HaulNo"="HaulNo","Year"="Year"))

Bio<- Bio %>% mutate(meanLat= (ShootLat+HaulLat)/2, 
                     meanLon= (ShootLong+HaulLong)/2) %>%
  dplyr::select(-ShootLat,-ShootLong ,-HaulLat ,-HaulLong)
Bio <- Bio[Bio$meanLon<xmax & Bio$meanLon>xmin & Bio$meanLat<ymax & Bio$meanLat>ymin,]

Bio.sp <- Bio
Bio <- Bio %>% dplyr::group_by(StNo,HaulNo,Year, meanLon, meanLat) %>% dplyr::summarise(S=length(unique(SpecCode)))

coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))


ggplot(Bio) + geom_polygon(data=coast, aes(x=long,y=lat,group=group),col="black",fill="grey")+
  geom_point(aes(x=meanLon, y=meanLat, size=S, col=S))+scale_color_viridis_c()+ facet_wrap(~Year)+
  xlab("Longitude")+ylab("Latitude")

#Species names
sp <- wormsbyid(unique(Bio.sp$SpecCode))
names(sp)[1] <- "SpecCode"
Bio.sp <- Bio.sp %>% dplyr::left_join(sp[,c(1,3,13,14,15,16,17,18)],by=("SpecCode"))

if(dim(Benthos <- Bio.sp[Bio.sp$phylum!="Chordata",])[1]>500){
  Benthos <- Bio.sp[Bio.sp$phylum!="Chordata",]
}else{Bentho <- data.frame(StNo=numeric(),HaulNo=numeric(),Year=numeric(),meanLon=numeric(),meanLat=numeric(),Abun=numeric(),group=numeric())}

if(dim(Bio.sp[Bio.sp$class=="Elasmobranchii",])[1]>500){
  Elasmo <- Bio.sp[Bio.sp$class=="Elasmobranchii",]
}else{Bentho <- data.frame(StNo=numeric(),HaulNo=numeric(),Year=numeric(),meanLon=numeric(),meanLat=numeric(),Abun=numeric(),group=numeric(),class=character())}


tmp <- Bio.sp[Bio.sp$phylum=="Chordata" &Bio.sp$class!="Elasmobranchii",]
keep <- names(which(table(tmp$order)>100))

Bio.order <- Bio.sp[Bio.sp$order %in% keep,]

Bio.order <- Bio.order %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,order,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat,order) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))
Elasmo <- Elasmo %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,class,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat,class) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))
Benthos <- Benthos %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))

Bio.order$group <- as.integer(as.factor(Bio.order$order))
Bio.order <- Bio.order %>% dplyr::select(-order)
Elasmo$group <- max(Bio.order$group)+1
Elasmo <- Elasmo %>% dplyr::select(-class)
Benthos$group <- max(Bio.order$group)+2


Sp <- rbind(Bio.order,Elasmo,Benthos)
keep <- c(keep,"Elasmonbranchii","Benthic fauna")
