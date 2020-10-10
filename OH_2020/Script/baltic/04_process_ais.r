#packages
library(dplyr)
library(raster)
library(rworldmap)
library(rworldxtra)
library(NbClust)
library(tidyr)
library(dplyr)
library(ggplot2)
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
pt2<-st_as_sf(fishAIS,coords=c("x","y"),crs=crs(landmask2))
landmask2<-st_crop(st_as_sf(landmask),extent(pt2))
#pt at sea 
ptin<-!apply(st_intersects(pt2,landmask2,sparse=F),1,any)
#plot(alldf[[1]][ptin,1:2])
#list of all stuff
alldf<-list(fishAIS[ptin,],dredAIS[ptin,],sailAIS[ptin,],tranAIS[ptin,],servAIS[ptin,])

#
classif(alldf[[1]])
ggplot(alldf[[1]],aes(x=x,y=y,fill=value))+geom_raster()+
	scale_fill_distiller(palette="Spectral",name="Zone",trans="log10")+
	#de\n navires\n(h.km^2)",trans="log10")+
	facet_wrap(~name)


##############################################
#filter quatile 99%
f99<-function(df){
		df<-df%>%group_by(name)%>%
		mutate(qxx=quantile(value,.95,na.rm=T),
		       mxx=mean(value,na.rm=T),
		       medxx=median(value,na.rm=T))%>%
		ungroup()%>%
		mutate(value=ifelse(value>=qxx,
				    mxx,value))%>%
		mutate(value2=ifelse(is.finite(value),value,medxx))%>%
		dplyr::select(-qxx,-mxx,-medxx)%>%
		data.frame()
	return(df)
}


#classif 
classif<-function(tmp){
	tmp<-alldf[[1]]
	tmp$value[is.nan(tmp$value)]<-NA
	tmp$value<-log10(tmp$value+1)
	mat0<-tmp%>%tidyr::pivot_wider(values_from=value,names_from=name)
	idptNA<-is.finite(apply(mat0[,-c(1:2)],1,sum))
	d<-dist(mat0[,-c(1:2)])
	clust <-fastcluster::hclust(d, method = "ward.D2")
	pipo<-data.frame(x=mat0$x[idptNA],y=mat0$y[idptNA],zone=cutree(clust,6))
	return(pipo)
}
pipo<-classif(alldf[[1]])

ggplot(pipo,aes(x=x,y=y,fill=zone))+geom_raster()+#facet_wrap(~name)+
		scale_fill_distiller(palette="Spectral",name="Zone")#de\n navires\n(h.km^2)",trans="log10")+

	#identify point with no data
	idptNA<-which(apply(mat0[,-c(1:2)],1,sum,na.rm=T)>0)
	#take the mat with median value
	mat1<-tmp%>%dplyr::select(-value)%>%tidyr::pivot_wider(values_from=value2,names_from=name)
	#take only valid point
	mat<-mat1[idptNA,-c(1:2)]
	d<-dist(mat0)
	clust <-fastcluster::hclust(d, method = "ward.D2")
	pipo<-data.frame(x=mat0$x[idptNA],y=mat0$y[idptNA],zone=cutree(clust,6))
	return(pipo)
}



alldat<-list(fishingAIS,dredgingAIS,transportAIS,serviceAIS,sailingAIS)
alldat1<-lapply(alldat,r2df)
alldat2<-lapply(alldat1,f99)
alldat3<-lapply(alldat2,classif)
f1<-function(a,nom){a$name<-nom;return(a)}
#f1(alldat3[[1]],"pipo")
alldat4<-Map(f1,alldat3,c("Fishing","Dredging","Transport","Service","Sailing"))
rez<-do.call("rbind",alldat4)
ggplot(rez,aes(x=x,y=y,fill=zone))+geom_raster()+facet_wrap(~name)+
		scale_fill_distiller(palette="Spectral",name="Zone")#de\n navires\n(h.km^2)",trans="log10")+

rasterVis::levelplot(stack((lapply(alldat,mean,na.rm=T))),zscaleLog=10)
lapply(alldat2,function(a){summary(a$value2)}


stop("ici")
#stop




#tmp 2 raster
mat0<-tmp%>%dplyr::select(-value2)%>%tidyr::pivot_wider(values_from=value,names_from=name)
#identify point with no data
idptNA<-which(apply(mat[,-c(1:2)],1,sumna.rm=T)>0)
#take the mat with median value
mat1<-tmp%>%dplyr::select(-value)%>%tidyr::pivot_wider(values_from=value2,names_from=name)
#take only valid point
mat<-mat1[idptNA,-c(1:2)]
d<-dist(mat)
clust <-fastcluster::hclust(d, method = "ward.D2")

pipo<-data.frame(x=mat0$x[idptNA],y=mat0$y[idptNA],zone=cutree(clust,6))

ggplot(pipo,aes(x=x,y=y,fill=zone))+geom_raster()



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





