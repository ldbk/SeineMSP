library(ncdf4)
library(raster)
library(rasterVis)
library(ggplot2)
library(MASS)
library(viridisLite)
library(dplyr)
library(tidyr)
library(rgdal)
library(rgeos)
library(NbClust)
library(cluster)
library(grDevices)
library(RColorBrewer)
library(ggdendro)

sst<- stack("data/satellite/sst/IFREMER-ATL-SST-L4-REP-OBS_FULL_TIME_SERIE_1581929927261.nc")
sst<- sst-275.15


# Conversion raster - tableau
fortify.Raster <- function(sst, maxPixel = 1000000) {
  
  if (ncell(sst) > maxPixel) {
    x <- sampleRegular(sst, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(sst, seq_len(ncell(sst)))
  out <- sst %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabsst<- fortify.Raster(sst)
pixelok<- which(!is.na(apply(Tabsst,1,mean)))
Tabsst<- pivot_longer(Tabsst, cols=1:13232, names_to = "Secondes", values_to = "SST", values_drop_na = TRUE)

{
Tabsst$Secondes<- sub("values.X","",Tabsst$Secondes)
secsst<- as.numeric(Tabsst$Secondes)
Day0<-strptime("1981-01-01", format= "%Y-%m-%d")
Date<- Day0+secsst
Tabsst$Date <- Date
Tabsst$Year <- as.numeric(substr(as.character(Tabsst$Date),1,4))
Tabsst$Month<- as.numeric(substr(as.character(Tabsst$Date), 6,7))
Tabsst$Day  <- as.numeric(substr(as.character(Tabsst$Date), 9,10))
}

Tabsst<- na.omit(Tabsst)

# Infos
#mean(Tabsst$SST)
#min(Tabsst$SST)
#max(Tabsst$SST)
#sd(Tabsst$SST)
#var(Tabsst$SST)


# Mean SST per year
Tabsst2<- Tabsst %>% group_by(x,y,Year) %>% summarize(moySST= mean(SST))
ggplot(Tabsst2)+
  geom_tile(aes(x=x, y=y, fill=moySST))+
  ggtitle("Température de surface")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="°C")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "YlOrRd"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(Tabsst2, file="data/satellite/sst/Tabsst2.Rdata")

#ggplot(Tabsst2, aes(x= Year, y=moySST, group=Year))+geom_boxplot()


# Mean SST 1981-2018
#Tabsst3<- Tabsst2 %>% group_by(x,y) %>% summarize(moyper= mean(moySST))
#ggplot(Tabsst3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("SST moyenne 1981-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="SST (°C)")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean SST (year)
Tabsst4<- Tabsst %>% group_by(Year) %>% summarize(moybaie= mean(SST))
save(Tabsst4, file= "results/satellite/series full bay/sstTab.Rdata")

SSTseries<- ggplot(Tabsst4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean Sea Surface Temperature 1982-2018")+
  xlab("Year")+
  ylab("°C")+
  theme_minimal()

save(SSTseries, file="results/satellite/series full bay/sst_series.Rdata")
ggsave(plot= SSTseries, filename="SST.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean SST (month)
Tabsst6<- Tabsst %>% group_by(Month) %>% summarize(moybaie= mean(SST))
save(Tabsst6, file= "results/satellite/series full bay/monthly/sstTab.Rdata")

SSTseries2<- ggplot(Tabsst6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean Sea Surface Temperature")+
  xlab("Month")+
  ylab("°C")+
  theme_minimal()

save(SSTseries2, file="results/satellite/series full bay/monthly/sst_series.Rdata")
ggsave(plot= SSTseries2, filename="SST.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

Tabsstnew<- pivot_wider(Tabsst2, names_from = Year, values_from = moySST)
Tabsstnew<- na.omit(Tabsstnew)
metaTabnew<- Tabsstnew %>% dplyr::select(x, y) %>% ungroup()
Tabsstnew<- Tabsstnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabsstnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1, main="Température de surface", cex.main=2)

#NbClust(Tabsstnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
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
                          scale.color = c("#FF9999", "#990000"))

SSTgg<- firstplot +
  ggtitle("Température de surface")+
  xlab("Coefficient d'agglomération = 0.99")+
  ylab("Distance")+
  theme_classic()+
  theme(plot.title = element_text(size = 40, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 35)) +
  theme(axis.title.y = element_text(size = 35)) +
  theme(axis.text.x = element_blank())

ggsave(plot= SSTgg, filename="SST.jpeg", path="results/satellite/Dendrogrammes", width = 13, height = 8)

zone<- sst[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabsst2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySST)) }

toto2sst<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, Tabsstnew))
serie<- pivot_longer(serie, cols=c(4:40), names_to="Year", values_to = "sst")
serie<- serie %>% group_by(Year, Clust) %>% summarise(sst=mean(sst))

ggseriesst<-  ggplot(serie)+
  geom_point(aes(x=Year,y=sst,col=Clust))+
  geom_line(aes(x=Year,y=sst,col=Clust, group=Clust))+
  ggtitle("Sea Surface Temperature")+
  ylab("°C")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggseriesst, file="results/satellite/series by zone/sst_seriebyzone.Rdata")
ggsave(plot= ggseriesst, filename="SST_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
  # 1st Polygon
liste <- with(toto2sst, chull(x, y))
hull <- toto2sst[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#SST<- ggplot(toto2sst)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean SST (°C)")+theme_minimal()+coord_fixed()+ggtitle("SST")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3sst<- toto2sst
coordinates(toto3sst)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3sst) <- TRUE
  # coerce to raster
rastersst<- raster(toto3sst)
rastersst
plot(rastersst, col= c("#FFCCCC", "#FF6666"), main="Température de surface", cex.main= 2, xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dissst<- disaggregate(rastersst, fact=(res(rastersst)/res(rasterchlnew)))
mSST<- mask(dissst, res)
plot(mSST, col= c("#FFCCCC", "#FF6666"), main="Température de surface", cex.main= 2, xlab="Longitude", ylab="Latitude")

save(mSST, file="data/satellite/sst/sst_raster.Rdata")

jpeg(file="results/satellite/zones/SST_raster.jpeg")
plot(mSST, main="Température de surface", cex.main= 2, xlab="Longitude", ylab="Latitude", col= c("#FFCCCC", "#FF6666"))
dev.off()



# Polygons

polSST<- rasterToPolygons(mSST, dissolve=TRUE)
plot(polSST, col=polSST@data$Clust)

writeOGR(polSST, dsn="data/satellite/sst", layer="SST", driver="ESRI Shapefile")

save(polSST, file="data/satellite/sst/sst_polygons.Rdata")



# Mean sst / zone

summarysst<- toto2sst %>% select(Clust, mean)
summarysst<- unique(summarysst)

write.table(summarysst, file="results/satellite/means by zone/summarysst.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4sst<- toto2sst %>% select(-Clust)
coordinates(toto4sst)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4sst) <- TRUE
  # coerce to raster
rastersst2<- raster(toto4sst)
rastersst2
plot(rastersst2, col= c("#FFCCCC", "#FF6666"), main="Température de surface", cex.main= 2,  xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dissst2<- disaggregate(rastersst2, fact=(res(rastersst2)/res(rasterchlnew)))
mSST2<- mask(dissst2, res)
plot(mSST2, col= c("#FFCCCC", "#FF6666"))

save(mSST2, file="results/satellite/means by zone/sst_raster.Rdata")

jpeg(file="results/satellite/means by zone/SST_raster.jpeg")
plot(mSST2, main="Température de surface", cex.main= 2, xlab="Longitude", ylab="Latitude", col= c("#FFCCCC", "#FF6666"), legend(1, 2, legend= "°C"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mSST, maxPixel = 1000000) {
  
  if (ncell(mSST) > maxPixel) {
    x <- sampleRegular(mSST, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mSST, seq_len(ncell(mSST)))
  out <- mSST %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

Tabsstfin<- fortify(mSST)

save(Tabsstfin, file="data/satellite/sst/Tabsstfin.Rdata")







