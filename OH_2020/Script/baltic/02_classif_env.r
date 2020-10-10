#Library

library(fastcluster)
source("")


classif <- function(raster, method.dist="euclidean", method.classif="ward.D2", nb.clust){
  df <- raster::as.data.frame(raster, xy=T)
  d <- dist(df,method=method.dist)
  clust <-fastcluster::hclust(d, method = method.classif)
  rez<-FastNbClust(tmp4, diss=dais,distance=NULL,min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  zones <- cutree(clust.classif ,6)
}
  pixelok <- which(!is.na(apply(raster::as.data.frame(mpa_sst),1,mean)))
  zone <- mpa_sst[[1]]
  values(zone) <- NA
  zone <- zones
  return(clust)
}
classif(mpa_sst)

tmp <- data.frame(raster::as.data.frame(mpa_sst,xy=T)[,c(1:2)],clust=zones)

ggplot(tmp)+
  geom_point(aes(x=x,y=y,col=clust))
