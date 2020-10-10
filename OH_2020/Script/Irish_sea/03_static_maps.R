
#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])

#Libraries
library(rgdal)
library(downloader)
library(ggplot2)
library(mapdata)
library(geojsonio)
library(ggmap)
library(ggrepel)

library(rasterVis)
library(rgeos)
library(sp)
library(raster)

library(sf)
#library(stringi)
library(geojsonR)

library("rgdal")
library("rasterVis")
library("downloader")
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


library(dplyr)
library(tidyr)
library(wdpar)
library(directlabels)
#library(rgl)
library(ncdf4)
require(reshape2)

#Libraries for mask
library(rworldmap)
library(rworldxtra)

#Get data#####

#WDPA#####
# Script for Wekeo environment
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

bbox<-paste(xmin,ymin,xmax,ymax,sep=",")

getPolygeojson_sf<-function(wfs_url, epsg_code, layer, xmin, xmax, ymin, ymax){
  layer <- as.character(layer)
  bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  con<-paste0(wfs_url,"&service=WFS&version=1.1.0&request=GetFeature&srsName=",epsg_code,"&typeName=",layer,"&bbox=",bbox,"&outputFormat=geojson")
  layer <- read_sf(con)
  return(layer)
}

getPolygeojson_rgdal<-function(wfs_url, epsg_code, layer, xmin, xmax, ymin, ymax){
  layer <- as.character(layer)
  bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  con<-paste0(wfs_url,"&service=WFS&version=1.1.0&request=GetFeature&srsName=",epsg_code,"&typeName=",layer,"&bbox=",bbox,"&outputFormat=geojson")
  layer<-rgdal::readOGR(con)
  return(layer)
}

layer_title<-"Protected areas"
layer="ms:wdpa"
wfs_url <- "http://mapserver.marine-analyst.eu/cgi-bin/mapserv?map=/var/www/html/mapserver/wfs/wdpa.map"
epsg_code<-"EPSG:4326"

mpa_wdpa_sf<-getPolygeojson_sf(wfs_url, epsg_code, layer, xmin, xmax, ymin, ymax)
Rgeojson<-getPolygeojson_rgdal(wfs_url, epsg_code, layer, xmin, xmax, ymin, ymax)

mpa_wdpa_sf$NAME <- Rgeojson$NAME
mpa_wdpa_sf$MANG_AUTH <- Rgeojson$MANG_AUTH

mpa_wdpa_sp <- as(mpa_wdpa_sf,"Spatial")
mpa_wdpa_sp<-spTransform(mpa_wdpa_sp,CRS("+proj=longlat +datum=WGS84"))
#####

#Natura2000/Aggregates/#####


# Script for Wekeo environment
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

# define a function to read the WFS data as a geoJSON file
getPolyjson<-function(wfs_url, layer, epsg_code, xmin, xmax, ymin, ymax) {
  layer <- as.character(layer)
  bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  con <- paste0(wfs_url,"/wfs?SERVICE=WFS&VERSION=1.0.0&request=GetFeature&srsName=",epsg_code,"&typeName=",layer,"&OUTPUTFORMAT=application/json&bbox=",bbox,",EPSG:4326")
  
  ogrInfo(dsn=con,layer = 'OGRGeoJSON')
  layer <- rgdal::readOGR(dsn=con,layer = 'OGRGeoJSON', encoding = "UTF-8", use_iconv = TRUE)
  #layer <- rgdal::readOGR(con)
  return(layer)
}

####################### get poly natura2000
layer_title<-"Natura 2000 sites"
layer="natura2000areas"
wfs_url <- "https://ows.emodnet-humanactivities.eu"
epsg_code<-"EPSG:4326"
try(mpa_natura2000 <- getPolyjson(wfs_url, layer, epsg_code, xmin, xmax, ymin, ymax), silent=TRUE)
exists("mpa_natura2000")

####################### get poly activelicenses
layer_title<-"Active licenses"
layer="activelicenses"
wfs_url <- "https://ows.emodnet-humanactivities.eu"
epsg_code<-"EPSG:4326"
try(mpa_activelicenses <- getPolyjson(wfs_url, layer, epsg_code, xmin, xmax, ymin, ymax), silent=TRUE)
exists("mpa_activelicenses")

####################### get poly aggregateareas
layer_title<-"Aggregate Extraction Areas"
layer="aggregateareas"
wfs_url <- "https://ows.emodnet-humanactivities.eu"
epsg_code<-"EPSG:4326"
try(mpa_aggregateareas <- getPolyjson(wfs_url, layer, epsg_code, xmin, xmax, ymin, ymax), silent=TRUE)
exists("mpa_aggregateareas")

####################### get poly windfarmspoly
layer_title<-"Wind Farms (Polygons)"
layer="windfarmspoly"
wfs_url <- "https://ows.emodnet-humanactivities.eu"
epsg_code<-"EPSG:4326"
try(mpa_windfarmspoly <- getPolyjson(wfs_url, layer, epsg_code, xmin, xmax, ymin, ymax), silent=TRUE)
exists("mpa_windfarmspoly")



#####

#Bathy#####
sr=SpatialPolygons(list(Polygons(list(Polygon(cbind(c(xmin, xmin, xmax, xmax),c(ymax, ymin, ymin, ymax)))),"1")))
mpa=SpatialPolygonsDataFrame(sr, data.frame(cbind(1:1), row.names=c("1")))
proj4string(mpa)<-CRS("+proj=longlat +datum=WGS84")

bbox<-paste(xmin,ymin,xmax,ymax,sep=",")


#FUNCTION GET RASTER DATA FROM WCS
#=================================================

getWCS<-function (name = "emodnet:mean", resolution = "0.2km", xmin = 15, xmax = 20.5, ymin = 30, ymax = 32.5)
{
  bbox <- paste(xmin, ymin, xmax, ymax, sep = ",")
  con <- paste(ogc_url,"/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=",name,"&crs=EPSG:4326&BBOX=", bbox, "&format=image/tiff&interpolation=nearest&resx=0.00208333&resy=0.00208333", sep = "")
  nomfich <- paste(name, "img.tiff", sep = "_")
  nomfich <- tempfile(nomfich)
  downloader::download(con, nomfich, quiet = TRUE, mode = "wb")
  img <- raster::raster(nomfich)
  img[img == 0] <- NA
  img[img < 0] <- 0
  names(img) <- paste(name)
  return(img)
}

source_provider_url <- "https://www.emodnet-bathymetry.eu"
layer="emodnet:mean"
layer_title<-"EMODnet Bathymetry 2018"
ogc_url <- "https://ows.emodnet-bathymetry.eu"

map_bathy<- getWCS(layer, resolution = "0.2km", xmin, xmax, ymin, ymax)
#####


###
Bathy <- raster::as.data.frame(map_bathy,xy=T)
names(Bathy)[3] <- "Bathymetry"

coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))


Static <- ggplot(Bathy)+
  geom_raster(aes(x=x,y=y,fill=Bathymetry))+
  scale_fill_viridis_c()+xlab("Longitude")+ylab("Latitude")+
  geom_polygon(data=mpa_windfarmspoly, aes(x = long,y= lat, group=group), col="blue", fill=NA)+
  geom_polygon(data=mpa_natura2000, aes(x = long,y= lat, group=group),col="green", fill=NA)+
  geom_polygon(data=mpa_aggregateareas, aes(x = long,y= lat, group=group),col="red",fill=NA)+
  geom_path(data=coast,aes(x=long, y=lat, group=group),fill=NA, col="yellow")+
  xlim(xmin,xmax)+ylim(ymin,ymax)+
  ggtitle("Bathymetry of the area of interest and human management areas")

print(static)
