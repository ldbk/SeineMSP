#Library

library(fastcluster)
library(rgdal)

coast <- readOGR("OH_2020/Data/EEA_Coastline_WGS84.gpkg")
coast <- raster::crop(coast, raster::extent(-8.1,-2.5,51.5,55.9))


classif <- function(raster, method.dist="euclidean", method.classif="ward.D2", nb.clust){
  df <- raster::as.data.frame(raster, xy=T)
  x <- is.na(maps::map.where(coast, df$x, df$y))
  df <- df[which(x),-c(1,2)]
  d <- dist(df,method=method.dist)
  clust <-fastcluster::hclust(d, method = method.classif)
  
  zones <- cutree(clust ,nb.clust)
  raster.cut <- raster[[1]]
  values(raster.cut) <- NA
  raster.cut[which(x)] <- zones
  return(raster.cut)
}


#CLASSIFICATION of the MODIST SST
#================================================================
sst.classif <- classif(mpa_sst,nb.clust = 5)

#CLASSIFICATION of the PRIMARY PRODUCTION
#================================================================
ppr.classif <- classif(mpa_ppr,nb.clust = 5)

#CLASSIFICATION of the SALINITY
#================================================================
salinity.classif <- classif(mpa_salinity,nb.clust = 5)

#CLASSIFICATION of the CHLA
#================================================================
chla.classif <- classif(mpa_chla,nb.clust = 5)

#CLASSIFICATION of the KD443
#================================================================
kd443.classif <- classif(mpa_kd443,nb.clust = 5)

#CLASSIFICATION of the Surface oxygene
#================================================================
oxygene.classif <- classif(mpa_oxygene,nb.clust = 5)

#CLASSIFICATION of the Surface bbp
#================================================================
bbp.classif <- classif(mpa_bbp,nb.clust = 5)

#CLASSIFICATION of the Surface adg
#================================================================
adg.classif <- classif(mpa_adg,nb.clust = 5)

