#Library

library(fastcluster)


classif <- function(raster, method.dist="euclidean", method.classif="ward.D2", nb.clust){
  df <- raster::as.data.frame(raster, xy=T)
  d <- dist(df,method=method.dist)
  clust <-fastcluster::hclust(d, method = method.classif)
  
  return(clust)
}
zones <- cutree(sst.classif ,6)
pixelok <- which(!is.na(apply(raster::as.data.frame(mpa_sst),1,mean)))
zone <- mpa_sst[[1]]
values(zone) <- NA
zone <- zones

tmp <- data.frame(raster::as.data.frame(mpa_sst,xy=T)[,c(1:2)],clust=zones)

ggplot(tmp)+
  geom_point(aes(x=x,y=y,col=clust))
