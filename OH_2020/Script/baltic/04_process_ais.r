#packages
library(dplyr)
library(raster)
library(rworldmap)
library(rworldxtra)
library(NbClust)
library(tidyr)
library(dplyr)
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

##############################################
#raster 2 datafram
r2df<-function(r){
	r<-fishingAIS
	tmp<-raster::as.data.frame(r,xy=T)%>%
		tidyr::pivot_longer(3:(2+dim(r)[3]))
	return(tmp)
}
#filter quatile 99%
f99<-function(df){
		df<-df%>%group_by(name)%>%
		mutate(qxx=quantile(value,.99,na.rm=T),
		       mxx=mean(value,na.rm=T))%>%
		ungroup()%>%
		mutate(value=ifelse(value>=qxx,
				    mxx,value))%>%
		dplyr::select(-qxx,-mxx)
	return(df)
}
#dataframe 2 df
fd2r<-function(df){
}

tmp<-f99(r2df(fishingAIS))




#fct to trans raster in long truc and filter by high quantile

plt1<-ggplot()+
		geom_raster(data=tmp,aes(x=x,y=y,fill=value+1))+
		facet_wrap(~name,ncol=12)+
		scale_fill_distiller(palette="Spectral",name="Densité de\n navires\n(h.km^2)",trans="log10")+
		#scale_fill_gradient(trans = 'log10')+ 
		#scale_fill_viridis_c(trans="log")+#palette="viridis",name="Zones")+
		#scale_fill_viridis_c(palette="rainbow")+#palette="viridis",name="Zones")+
		borders("world",fill="grey",colour=NA)+
		coord_sf(xlim=range(tmp$x),ylim=range(tmp$y))+
		xlab("Longitude")+ylab("Latitude")#+



#test
raster<-fishingAIS
plot(log10(mean(raster,na.rm=T)))
method.dist="euclidean"
method.classif="ward.D2"

classif <- function(df, method.dist="euclidean", method.classif="ward.D2", nb.clust){
	uu<-raster::as.data.frame(stack(mpa_fishing))
	idptNA<-which(apply(uu,1,sum,na.rm=T)>0)
	uu<-uu[idptNA,]
	aa<-dist(uu)
	clust <-fastcluster::hclust(aa, method = method.classif)

	if(F){
		df<-tmp
	}
	#convert df to long table
	mat<-df%>%tidyr::pivot_wider(values_from=value,names_from=name)
	#identify point with no data
	idptNA<-which(apply(mat[,-c(1:2)],1,sum,na.rm=T)>0)
	mat<-mat[idptNA,-c(1:2)]
	plot(raster((as.matrix(d))))
	

	uu<-raster::as.data.frame(fishingAIS,xy=T)
	d<-dist(uu[,-c(1:2)])

	mat<-as.data.frame(mat)
	#remove xy where no data
	d <- stats::dist(mat,method=method.dist)
	d[!is.finite(d)]<-NaN
	clust <-fastcluster::hclust(d, method = method.classif)

	f1<-function(df,k,mdist=method.dist,mclassif=method.classifp){
		d <- dist(df,method="euclidean")
		clust <-fastcluster::hclust(d, method = "ward.D2")
		zones <- cutree(clust,k)
		return(zones)
	}
	#optimal nbclust 
	rez<-NbClust::NbClust(df, diss=NULL,distance="euclidean",min.nc = 2, max.nc = 5, index=c("dunn"), method = "ward.D2")

	rez<-FastNbClust(NULL, diss=d,distance=NULL,min.nc = 2, max.nc = 5, index=c("frey","mcclain","cindex","silhouette","dunn"), method = "ward.D2")
	rez<-FastNbClust(df, diss=NULL,distance="euclidean",min.nc = 2, max.nc = 5, index=c("gap"), method = "ward.D2")

	zones <- cutree(clust,6)

    return(clust)
}
pixelok <- which(!is.na(apply(raster::as.data.frame(mpa_sst),1,mean)))
zone <- raster[[1]]
values(zone) <- NA
zone <- zones





