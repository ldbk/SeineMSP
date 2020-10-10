# Test script localy
wdpaid <- '11.3_53.6_15.5_55.9'
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

