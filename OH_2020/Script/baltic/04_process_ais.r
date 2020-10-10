#packages
library(dplyr)
library(raster)
library("rworldmap")
library("rworldxtra")
library("NbClust")
#land data to mask data
landmask <- rworldmap::getMap(resolution = "high")



#load data
# Test script localy
#wdpaid <- '18_54_22_56'
load("../../Data/baltic/envais.rdata")

#group vessel cat + reduce spatial resolution + mask to avoid onland info
aggfac<-4 # reduction factor
#fishing
fishingAIS<-aggregate(mpa_fishing,aggfac)
fishingAIS<-mask(fishingAIS,landmask,inverse=T)
#dredging
dredgingAIS<-aggregate(mpa_dredging,aggfac)
dredgingAIS<-mask(dredgingAIS,landmask,inverse=T)
#sailing
sailingAIS<-aggregate(mpa_sailing+mpa_pleasure,aggfac)
sailingAIS<-mask(sailingAIS,landmask,inverse=T)
#transport
transportAIS<-aggregate(mpa_speedcraft+mpa_passenger+mpa_cargo+mpa_tanker,aggfac)
transportAIS<-mask(transportAIS,landmask,inverse=T)
#service
serviceAIS<-aggregate(mpa_service+mpa_tug+mpa_military,aggfac)
serviceAIS<-mask(serviceAIS,landmask,inverse=T)




#test
raster<-fishingAIS
plot(log10(mean(raster)))
method.dist="euclidean"
method.classif="ward.D2"

classif <- function(raster, method.dist="euclidean", method.classif="ward.D2", nb.clust){
	df <- raster::as.data.frame(raster, xy=T)
	d <- dist(df,method=method.dist)
	clust <-fastcluster::hclust(d, method = method.classif)

	f1<-function(df,k,mdist=method.dist,mclassif=method.classifp){
		d <- dist(df,method="euclidean")
		clust <-fastcluster::hclust(d, method = "ward.D2")
		zones <- cutree(clust,k)
		return(zones)
	}

	rez<-NbClust::NbClust(df, diss=d,distance=NULL,min.nc = 2, max.nc = 5, index="gap", method = "ward.D2")
	cluster::clusGap(df,f1,K.max=7)

	zones <- cutree(clust,6)

    return(clust)
}
pixelok <- which(!is.na(apply(raster::as.data.frame(mpa_sst),1,mean)))
zone <- raster[[1]]
values(zone) <- NA
zone <- zones





