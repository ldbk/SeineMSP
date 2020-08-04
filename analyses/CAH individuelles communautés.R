library(dplyr)
library(tidyr)
library(ggplot2)
library(NbClust)
library(rgdal)
library(rgeos)
library(raster)
library(fastcluster) # pour hclust
library(RColorBrewer)
library(ggdendro)

load("data/krigeage log.RData")


# Fonctions pour visualisation dendrogrammes

dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}

set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}

plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <- pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p
}



names(Kriege.logdens)[6]<- "Community"
Kriege.logdens$Community<- as.numeric(Kriege.logdens$Community)

Longitude<- numeric()
Latitude<- numeric()
Clust<- numeric()
Community<- numeric()


for (j in unique(data.frame(Kriege.logdens)[,"Community"])){
  Tab1<- Kriege.logdens[Kriege.logdens$Community==j,] %>% dplyr::select(-Variance, -Community)
  Tab2<- pivot_wider(Tab1, names_from = Year, values_from = Prediction)
  metaTab<- Tab2 %>% dplyr::select(Longitude, Latitude)
  Tab2<- Tab2 %>% dplyr::select(-c(Longitude, Latitude))

  
  # Classification
  
  distance<- dist(Tab2)
  tree<- fastcluster::hclust(distance, method="ward.D2")  
  #plot(tree, hang=-1, main="Communauté I")
  
  #Nb1<- Tab1 %>% group_by(Longitude,Latitude) %>% summarize(moyper= mean(Prediction))
  if(j %in% c(1,2,3,4,9)){
  	PLOM<- NbClust(Tab2, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  }
  #if(j %in% c(4)){
  #	PLOM<- NbClust(Tab2[,9:32], min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  #}
  if(j %in% c(5)){
  	PLOM<- NbClust(Tab2[,c(27,28,31)], min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  }
  if(j %in% c(6)){
    PLOM<- NbClust(Tab2[,c(27:32)], min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  }
  if(j %in% c(7)){
    PLOM<- NbClust(Tab2[,c(31)], min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
  }
  if(j %in% c(8)){
    PLOM$Best.partition<- 3
  }
	
#index <- c("kl", "ch", "ccc", "cindex", "db", "silhouette", "duda", "pseudot2", "beale", "ratkowsky", "ptbiserial", "mcclain", "gamma", "gplus", "tau", "dindex", "sdbw")  
  
#for (i in 1:length(index)){
#  PLOM<- NbClust(Tab2[,c(27:28)], min.nc = 2, max.nc = 10, index=index[1:i], method = "ward.D2")
#  print(i)
#}

  #treecut<- dendro_data_k(tree, 4)
  
  #firstplot<- plot_ggdendro(treecut,
   #                         direction   = "tb",
    #                        expand.y    = 0.2,
     #                       scale.color = c("black", "#FEEDDE", "#FDBE85", "#FD8D3C", "#D94701"))
  
  #ComIX<- firstplot +
   # ggtitle("Communauté IX")+
    #ylab("Distance")+
    #theme_classic()+
    #theme(plot.title = element_text(size = 40, hjust = 0.5))+
    #theme(axis.title.x = element_blank()) +
    #theme(axis.title.y = element_text(size = 35)) +
    #theme(axis.text.x = element_blank())
  
  #ggsave(plot= ComIX, filename="ComIX.jpeg", path="results/Communautes bio/Zones/Dendrogrammes", width = 13, height = 8)
  
  
  rect.hclust(tree, max(PLOM$Best.partition))
  zones<- cutree(tree, max(PLOM$Best.partition))
  
  
  toto<- cbind(metaTab, Clust=factor(zones))        
  toto<- left_join(toto, Kriege.logdens[Kriege.logdens$Community==j,], by=c("Longitude", "Latitude")) 
  toto<- toto %>% dplyr::select(Longitude, Latitude, Clust, Community)  
  
  
  Longitude<- c(Longitude, toto$Longitude)
  Latitude<- c(Latitude, toto$Latitude)
  Clust<- c(Clust, toto$Clust)
  Community<- c(Community, toto$Community)

  tata <- left_join(toto, cbind(metaTab, Tab2))         
  tata <- pivot_longer(tata, cols=c(5:36), names_to="Year", values_to = "Prediction")    
  tata <- tata %>% group_by(Year, Clust) %>% summarise(Prediction=mean(Prediction))     
  
  ggtata<-  ggplot(tata)+
    geom_point(aes(x=Year, y=Prediction, col=Clust))+
    geom_line(aes(x=Year, y=Prediction, col=Clust, group=Clust))+
    theme_minimal()+
    facet_wrap(.~Clust)
  
  save(ggtata, file= paste0("results/Communautes bio/Community", j,"_plot.Rdata"))
  
  print(ggtata)
  
  
  tete<- tata %>% ungroup() %>% group_by(Clust) %>% summarise(Prediction= mean(Prediction))
  save(tete, file=paste0("data/ICES/mean_prediction_byzone_", j, ".Rdata"))
  
  titi<- toto %>% left_join(tete, by="Clust")
  
  save(titi, file= paste0("data/ICES/Tabfin",j, ".Rdata"))



}
Tabfaunefin<- data.frame(Longitude=Longitude, Latitude=Latitude, Clust=as.factor(Clust), Community=Community)
save(Tabfaunefin, file="results/Communautes bio/Tabfaunefin.Rdata")




# Rasters avec zones

load("data/res.Rdata")

for (j in unique(Tabfaunefin[,"Community"])){
  
    # create SpatialPointsDataFrame
toto1<- Tabfaunefin[Tabfaunefin$Community==j,]
coordinates(toto1)<- ~ Longitude + Latitude
    # coerce to SpatialPixelsDataFrame
gridded(toto1) <- TRUE
    # coerce to raster
raster<- raster(toto1)
raster
plot(raster, main="", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dis<- disaggregate(raster, fact=(res(raster)/res(rasterchlnew)))
m<- raster::mask(dis, res)
plot(m)

save(m, file= paste0("results/Communautes bio/Zones/Community", j,"_rasterzones.Rdata"))


# Polygones

pol<- rasterToPolygons(m, dissolve=TRUE)
plot(pol, col=pol@data$Clust)

save(pol, file= paste0("results/Communautes bio/Zones/Community", j,"_polygons.Rdata"))

}









# Raster and polygons with densities and not zones

load("data/ICES/Tabfin1.Rdata")
Tabfin1<- titi
Tabfin1<- Tabfin1 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin2.Rdata")
Tabfin2<- titi
Tabfin2<- Tabfin2 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin3.Rdata")
Tabfin3<- titi
Tabfin3<- Tabfin3 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin4.Rdata")
Tabfin4<- titi
Tabfin4<- Tabfin4 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin5.Rdata")
Tabfin5<- titi
Tabfin5<- Tabfin5 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin6.Rdata")
Tabfin6<- titi
Tabfin6<- Tabfin6 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin7.Rdata")
Tabfin7<- titi
Tabfin7<- Tabfin7 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin8.Rdata")
Tabfin8<- titi
Tabfin8<- Tabfin8 %>% dplyr::select( -Clust)
load("data/ICES/Tabfin9.Rdata")
Tabfin9<- titi
Tabfin9<- Tabfin9 %>% dplyr::select( -Clust)


Tabfinfin<- dplyr::union(Tabfin1, Tabfin2)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin3)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin4)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin5)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin6)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin7)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin8)
Tabfinfin<- dplyr::union(Tabfinfin, Tabfin9)


for (j in unique(Tabfinfin[,"Community"])){
  
    # create SpatialPointsDataFrame
titi1<- Tabfinfin[Tabfinfin$Community==j,]
titi1<- titi1 %>% dplyr::select(- Community)
coordinates(titi1)<- ~ Longitude + Latitude
    # coerce to SpatialPixelsDataFrame
gridded(titi1) <- TRUE
    # coerce to raster
raster<- raster(titi1)
raster
plot(raster, main="", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dis<- disaggregate(raster, fact=(res(raster)/res(rasterchlnew)))
m<- raster::mask(dis, res)
plot(m)

save(m, file= paste0("results/Communautes bio/Community", j,"_raster.Rdata"))


# Polygons with densities

poldens<- rasterToPolygons(m, dissolve=TRUE)
plot(poldens, col=poldens@data$Prediction)

save(poldens, file= paste0("results/Communautes bio/Community", j,"_polygons_dens.Rdata"))



}




# Zonations des 9 communautés

par(mfrow = c(3, 3))

load("results/Communautes bio/Zones/Community1_rasterzones.Rdata")
Com1 <- m 
load("results/Communautes bio/Zones/Community2_rasterzones.Rdata")
Com2<- m
load("results/Communautes bio/Zones/Community3_rasterzones.Rdata")
Com3<- m
load("results/Communautes bio/Zones/Community4_rasterzones.Rdata")
Com4<- m
load("results/Communautes bio/Zones/Community5_rasterzones.Rdata")
Com5<- m
load("results/Communautes bio/Zones/Community6_rasterzones.Rdata")
Com6<- m
load("results/Communautes bio/Zones/Community7_rasterzones.Rdata")
Com7<- m
load("results/Communautes bio/Zones/Community8_rasterzones.Rdata")
Com8<- m
load("results/Communautes bio/Zones/Community9_rasterzones.Rdata")
Com9<- m


{
  Com1<- raster::plot(Com1, main="Communauté I", xlab="Longitude", ylab="Latitude",   col= brewer.pal(n=7, name = "Greys"))
  Com2<- raster::plot(Com2, main="Communauté II", xlab="Longitude", ylab="Latitude",  col= brewer.pal(n=4, name = "PuBu"))
  Com3<- raster::plot(Com3, main="Communauté III", xlab="Longitude", ylab="Latitude", col= c("#FFFFCC", "#CC6633"))
  Com4<- raster::plot(Com4, main="Communauté IV", xlab="Longitude", ylab="Latitude",  col= brewer.pal(n=3, name="Spectral"))
  Com5<- raster::plot(Com5, main="Communauté V", xlab="Longitude", ylab="Latitude",   col= brewer.pal(n=3, name = "PuRd"))
  Com6<- raster::plot(Com6, main="Communauté VI", xlab="Longitude", ylab="Latitude",  col= brewer.pal(n=6, name = "Blues"))
  Com7<- raster::plot(Com7, main="Communauté VII", xlab="Longitude", ylab="Latitude", col= c("#CCFFCC", "#99CC99"))
  Com8<- raster::plot(Com8, main="Communauté VIII", xlab="Longitude", ylab="Latitude",col= brewer.pal(n=3, name="PiYG"))
  Com9<- raster::plot(Com9, main="Communauté IX", xlab="Longitude", ylab="Latitude",  col= brewer.pal(n=4, name="Oranges"))
}


par(mfrow = c(1, 1))





# Découpage des rasters en fonction de polycut

load("data/res.Rdata")
load("data/PolyCut.Rdata")

  # Disaggregate
{
Com1<- disaggregate(Com1, 10)
Com2<- disaggregate(Com2, 10)
Com3<- disaggregate(Com3, 10)
Com4<- disaggregate(Com4, 10)
Com5<- disaggregate(Com5, 10)
Com6<- disaggregate(Com6, 10)
Com7<- disaggregate(Com7, 10)
Com8<- disaggregate(Com8, 10)
Com9<- disaggregate(Com9, 10)
}

  # Mask
{
ComI<- raster::mask(Com1, res)
ComII<- mask(Com2, res)
ComIII<- mask(Com3, res)
ComIV<- mask(Com4, res)
ComV<- mask(Com5, res)
ComVI<- mask(Com6, res)
ComVII<- mask(Com7, res)
ComVIII<- mask(Com8, res)
ComIX<- mask(Com9, res)
}

  # Conversion raster - tableau
{
fortify.Raster <- function(ComI, maxPixel = 1000000) {
  
  if (ncell(ComI) > maxPixel) {
    x <- sampleRegular(ComI, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComI, seq_len(ncell(ComI)))
  out <- ComI %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComII, maxPixel = 1000000) {
  
  if (ncell(ComII) > maxPixel) {
    x <- sampleRegular(ComII, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComII, seq_len(ncell(ComII)))
  out <- ComII %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComIII, maxPixel = 1000000) {
  
  if (ncell(ComIII) > maxPixel) {
    x <- sampleRegular(ComIII, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComIII, seq_len(ncell(ComIII)))
  out <- ComIII %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComIV, maxPixel = 1000000) {
  
  if (ncell(ComIV) > maxPixel) {
    x <- sampleRegular(ComIV, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComIV, seq_len(ncell(ComIV)))
  out <- ComIV %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComV, maxPixel = 1000000) {
  
  if (ncell(ComV) > maxPixel) {
    x <- sampleRegular(ComV, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComV, seq_len(ncell(ComV)))
  out <- ComV %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComVI, maxPixel = 1000000) {
  
  if (ncell(ComVI) > maxPixel) {
    x <- sampleRegular(ComVI, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComVI, seq_len(ncell(ComVI)))
  out <- ComVI %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComVII, maxPixel = 1000000) {
  
  if (ncell(ComVII) > maxPixel) {
    x <- sampleRegular(ComVII, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComVII, seq_len(ncell(ComVII)))
  out <- ComVII %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComVIII, maxPixel = 1000000) {
  
  if (ncell(ComVIII) > maxPixel) {
    x <- sampleRegular(ComVIII, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComVIII, seq_len(ncell(ComVIII)))
  out <- ComVIII %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(ComIX, maxPixel = 1000000) {
  
  if (ncell(ComIX) > maxPixel) {
    x <- sampleRegular(ComIX, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(ComIX, seq_len(ncell(ComIX)))
  out <- ComIX %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
}


{
Com1tab<- fortify(ComI)
Com2tab<- fortify(ComII)
Com3tab<- fortify(ComIII)
Com4tab<- fortify(ComIV)
Com5tab<- fortify(ComV)
Com6tab<- fortify(ComVI)
Com7tab<- fortify(ComVII)
Com8tab<- fortify(ComVIII)
Com9tab<- fortify(ComIX)

Com1tab<- na.omit(Com1tab)
Com2tab<- na.omit(Com2tab)
Com3tab<- na.omit(Com3tab)
Com4tab<- na.omit(Com4tab)
Com5tab<- na.omit(Com5tab)
Com6tab<- na.omit(Com6tab)
Com7tab<- na.omit(Com7tab)
Com8tab<- na.omit(Com8tab)
Com9tab<- na.omit(Com9tab)
}



{
COMI<- ggplot(Com1tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté I")+
  scale_fill_manual(values = c("#F7F7F7", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMI, filename="COMI.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)
#results/Communautes bio/Zones

COMII<- ggplot(Com2tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté II")+
  scale_fill_manual(values = c("#F1EEF6", "#BDC9E1", "#74A9CF", "#0570B0"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMII, filename="COMII.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMIII<- ggplot(Com3tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté III")+
  scale_fill_manual(values = c("#FFFFCC", "#CC6633"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMIII, filename="COMIII.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMIV<- ggplot(Com4tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté IV")+
  scale_fill_manual(values = c("#FC8D59", "#FFFFBF", "#99D594"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMIV, filename="COMIV.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMV<- ggplot(Com5tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté V")+
  scale_fill_manual(values = c("#E7E1EF", "#C994C7", "#DD1C77"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMV, filename="COMV.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMVI<- ggplot(Com6tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté VI")+
  scale_fill_manual(values = c("#EFF3FF", "#C6DBEF", "#9ECAE1", "#6BAED6", "#3182BD", "#08519C"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMVI, filename="COMVI.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMVII<- ggplot(Com7tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté VII")+
  scale_fill_manual(values = c("#CCFFCC", "#99CC99"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMVII, filename="COMVII.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMVIII<- ggplot(Com8tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté VIII")+
  scale_fill_manual(values = c("#E9A3C9", "#F7F7F7", "#A1D76A"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMVIII, filename="COMVIII.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

COMIX<- ggplot(Com9tab)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Communauté IX")+
  scale_fill_manual(values = c("#FEEDDE", "#FDBE85", "#FD8D3C", "#D94701"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= COMIX, filename="COMIX.jpeg", path="results/Communautes bio/Zones", width = 13, height = 8)

}






















