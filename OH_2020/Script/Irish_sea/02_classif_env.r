#Library

library(fastcluster)
library(rgdal)
library(FactoMineR)

coast <- readOGR("OH_2020/Data/EEA_Coastline_WGS84.gpkg")
coast <- raster::crop(coast, raster::extent(-8.2,-2.5,51.5,55.9))


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

#FINAL CLASSIFICATION#####
grid <- expand.grid(Long=seq(from=-8.1,to=-2.5,by=round(res(mpa_adg)[1],4)),Lat=seq(from=51.5,to=55.9,by=round(res(mpa_adg)[1],4)))


env.zones <- data.frame(grid,
                        SST=extract(sst.classif,grid),
                        PPR=extract(ppr.classif,grid),
                        SAL=extract(salinity.classif,grid),
                        CHL=extract(chla.classif,grid),
                        KD=extract(kd443.classif,grid),
                        OXY=extract(oxygene.classif,grid),
                        BBP=extract(bbp.classif,grid),
                        ADG=extract(adg.classif,grid))
env.zones <- na.omit(env.zones)

for(i in 3:10){
  env.zones[,i] <- paste(env.zones[,i],names(env.zones)[i],sep="_")
}

#MCA
mca <- MCA(env.zones[,-c(1,2)], ncp=999, method="Burt", graph=F)
#plotellipses(mca, axes = c(1,2))
#plotellipses(mca, axes = c(1,3))

#Final classif
d <- dist(mca$ind$coord)
clust <- fastcluster::hclust(d, method="ward.D2")
groups <- cutree(clust,6)

env.classif <- data.frame(env.zones[,c(1,2)],Clusters=factor(groups))
######

#TIME SERIES AND BOXPLOT DATA####
param <- c("SST","PPR","SAL","CHL","KD","OXY","BBP","ADG")
Env <- data.frame()
x <- is.na(maps::map.where(coast, grid$Long, grid$Lat))
marine.grid <- grid[which(x),]
t <- 0

for (i in list(mpa_sst,mpa_ppr,mpa_salinity,mpa_chla,mpa_kd443,mpa_oxygene,mpa_bbp,mpa_adg)){
  t <- t+1
  tmp <- data.frame(marine.grid,extract(i,marine.grid))
  tmp <- dplyr::left_join(env.classif,tmp,by=c("Long"="Long","Lat"="Lat"))
  tmp <- tidyr::pivot_longer(data=tmp, cols=4:dim(tmp)[2],names_to="Date", values_to="Values")
  tmp <- dplyr::mutate(tmp, Month=substr(Date,7,8),Year=substr(Date,2,5)) 
  tmp <- dplyr::select(tmp, -Date)
  tmp <- dplyr::mutate(tmp, Param=param[t])
  
  Env <- rbind(Env, tmp)
}
#####


#RETURN PLOTS#####

map <- ggplot()+
  geom_raster(data=env.classif,aes(x=Long,y=Lat,fill=Clusters))+
  geom_polygon(data=coast, aes(x=long, y=lat, group=group), fill="grey", col="black")+
  theme_minimal()+ggtitle("Final regionalization of the area with environmental parameters")+
  xlab("Longitude")+ylab("Latitude")+scale_fill_viridis_d()
print(map)

boxplots <- ggplot()+
  geom_boxplot(data=Env,aes(x=Clusters,y=Values,fill=Clusters))+
  facet_wrap(~Param,nrow=2, scale="free_y")+xlab("Clusters")+ylab("Parameters")+
  scale_fill_viridis_d()
print(boxplots)


Env$Date <- as.Date(paste(Env$Year,Env$Month,"01",sep="/"), format=c("%Y/%m/%d"))
ts <- ggplot(data=Env[Env$Param=="SST",])+
  geom_path(aes(x=Date,y=Values,col=Clusters))+
  scale_color_viridis_d()+facet_wrap(~Param, nrow = 8, scale="free_y")
print(ts)
#####