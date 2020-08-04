library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridis)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(NbClust)
library(cluster)
library(grDevices)
library(RColorBrewer)
library(ggdendro)

Detrit<- stack("data/satellite/Detritus/dataset-oc-glo-opt-multi-l4-cdm443_4km_monthly-rep-v02_1592570192473.nc")


# Conversion raster - tableau
fortify.Raster <- function(Detrit, maxPixel = 1000000) {
  
  if (ncell(Detrit) > maxPixel) {
    x <- sampleRegular(Detrit, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Detrit, seq_len(ncell(Detrit)))
  out <- Detrit %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabDet<- fortify(Detrit)
pixelok<- which(!is.na(apply(TabDet,1,mean)))
TabDet<- pivot_longer(TabDet, cols=1:262, names_to = "Date", values_to = "Detritus", values_drop_na = TRUE)

{
  TabDet$Date<-sub("values.X","",TabDet$Date)
  TabDet$Year<- as.numeric(substr(as.character(TabDet$Date),1,4))
  TabDet$Month<- as.numeric(substr(as.character(TabDet$Date), 6,7))
  TabDet$Day<- as.numeric(substr(as.character(TabDet$Date), 9,10))
}


# Infos
#mean(TabDet$Detritus)
#min(TabDet$Detritus)
#max(TabDet$Detritus)
#sd(TabDet$Detritus)
#var(TabDet$Detritus)


# Mean detritus per year
TabDet2<- TabDet %>% group_by(x,y,Year) %>% summarize(moyDet= mean(Detritus))
ggplot(TabDet2)+
  geom_tile(aes(x=x, y=y, fill= log(moyDet)))+
  ggtitle("Détritus")+
  facet_wrap(. ~Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="log10 (Détritus)")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "PuBuGn"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(TabDet2, file="data/satellite/Detritus/TabDet2.Rdata")

#ggplot(TabDet2, aes(x=Year, y=moyDet, group=Year))+
#  geom_boxplot()


# Mean detritus 1997-2017
#TabDet3<- TabDet2 %>% group_by(x,y) %>% summarize(moyper= mean(moyDet))
#ggplot(TabDet3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Detritus moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean detritus (year)
TabDet4<- TabDet %>% group_by(Year) %>% summarize(moybaie= mean(Detritus))
save(TabDet4, file= "results/satellite/series full bay/DetTab.Rdata")

Detseries<- ggplot(TabDet4)+
  geom_line(aes(x=Year, y= moybaie))+
  ggtitle("Mean detritus 1997-2019")+
  xlab("Year")+
  ylab("m-1")+
  theme_minimal() 

save(Detseries, file="results/satellite/series full bay/Det_series.Rdata")
ggsave(plot= Detseries, filename="Detitrus.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean detritus (month)
TabDet6<- TabDet %>% group_by(Month) %>% summarize(moybaie= mean(Detritus))
save(TabDet6, file= "results/satellite/series full bay/monthly/DetTab.Rdata")

Detseries2<- ggplot(TabDet6)+
  geom_line(aes(x=Month, y= moybaie))+
  ggtitle("Mean detritus")+
  xlab("Month")+
  ylab("m-1")+
  theme_minimal() 

save(Detseries2, file="results/satellite/series full bay/monthly/Det_series.Rdata")
ggsave(plot= Detseries2, filename="Detitrus.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabDetnew<- pivot_wider(TabDet2, names_from = Year, values_from = moyDet)
TabDetnew<- na.omit(TabDetnew)
metaTabnew<- TabDetnew %>% dplyr::select(x, y) %>% ungroup()
TabDetnew<- TabDetnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabDetnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1, main="Détritus", cex.main=2)

#NbClust(TabDetnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
# According to the majority rule, the best number of clusters is  2

rect.hclust(tree, 2)
zones<- cutree(tree, 2)

# Functions for the dendrogram

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


treecut <- dendro_data_k(tree, 2)

firstplot<- plot_ggdendro(treecut,
                          direction   = "tb",
                          expand.y    = 0.2,
                          scale.color = c("#99CCCC", "#336666"))

Detgg<- firstplot +
  ggtitle("Détritus")+
  xlab("Coefficient d'agglomération = 0.99")+
  ylab("Distance")+
  theme_classic()+
  theme(plot.title = element_text(size = 40, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 35)) +
  theme(axis.title.y = element_text(size = 35)) +
  theme(axis.text.x = element_blank())

ggsave(plot= Detgg, filename="Det.jpeg", path="results/satellite/Dendrogrammes", width = 13, height = 8)

zone<- Detrit[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabDet2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyDet)) }

toto2Det<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabDetnew))
serie<- pivot_longer(serie, cols=c(4:26), names_to="Year", values_to = "Det")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Det=mean(Det))

ggserieDet<-  ggplot(serie)+
  geom_point(aes(x=Year,y=Det,col=Clust))+
  geom_line(aes(x=Year,y=Det,col=Clust, group=Clust))+
  ggtitle("Detritus")+
  ylab("m-1")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggserieDet, file="results/satellite/series by zone/Det_seriebyzone.Rdata")
ggsave(plot= ggserieDet, filename="Det_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
  # 1st Polygon
liste <- with(toto2Det, chull(x, y))
hull <- toto2Det[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Det<- ggplot(toto2Det)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="")+theme_minimal()+coord_fixed()+ggtitle("Detritus")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3Det<- toto2Det
coordinates(toto3Det)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3Det) <- TRUE
  # coerce to raster
rasterDet<- raster(toto3Det)
rasterDet
plot(rasterDet, col=c("#99CCCC", "#336666"), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disdet<- disaggregate(rasterDet, fact=(res(rasterDet)/res(rasterchlnew)))
mDet<- mask(disdet,res)
plot(mDet, col=c("#99CCCC", "#336666"))

save(mDet, file="data/satellite/Detritus/Det_raster.Rdata")

jpeg(file="results/satellite/zones/Det_raster.jpeg")
plot(mDet, main="Detritus", xlab="Longitude", ylab="Latitude", col=c("#99CCCC", "#336666"))
dev.off()



# Polygons

polDet<- rasterToPolygons(mDet, dissolve=TRUE)
plot(polDet, col=polDet@data$Clust)

writeOGR(polDet, dsn="data/satellite/Detritus", layer="Det", driver="ESRI Shapefile")


save(polDet, file="data/satellite/Detritus/Det_polygons.Rdata")



# Mean detritus per zone

summaryDet<- toto2Det %>% select(Clust, mean)
summaryDet<- unique(summaryDet)

write.table(summaryDet, file="results/satellite/means by zone/summaryDet.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4Det<- toto2Det %>% select(-Clust)
coordinates(toto4Det)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4Det) <- TRUE
  # coerce to raster
rasterDet2<- raster(toto4Det)
rasterDet2
plot(rasterDet2, col=c("#99CCCC", "#336666"), main="Detritus", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disdet2<- disaggregate(rasterDet2, fact=(res(rasterDet2)/res(rasterchlnew)))
mDet2<- mask(disdet2,res)
plot(mDet2, col=c("#99CCCC", "#336666"))

save(mDet2, file="results/satellite/means by zone/Det_raster.Rdata")

jpeg(file="results/satellite/means by zone/Det_raster.jpeg")
plot(mDet2, main="Detritus", xlab="Longitude", ylab="Latitude", col=c("#99CCCC", "#336666"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mDet, maxPixel = 1000000) {
  
  if (ncell(mDet) > maxPixel) {
    x <- sampleRegular(mDet, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mDet, seq_len(ncell(mDet)))
  out <- mDet %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabDetfin<- fortify(mDet)

save(TabDetfin, file="data/satellite/Detritus/TabDetfin.Rdata")







