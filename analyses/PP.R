library(ncdf4)
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

PP<- nc_open("data/satellite/Primary production/MetO-NWS-BIO-mm-PPRD_1591275638447.nc")
PP<- stack("data/satellite/Primary production/MetO-NWS-BIO-mm-PPRD_1591275638447.nc")


# Conversion raster - tableau
fortify.Raster <- function(PP, maxPixel = 1000000) {
  if (ncell(PP) > maxPixel) {
    x <- sampleRegular(PP, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(PP, seq_len(ncell(PP)))
  out <- PP %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabPP<- fortify(PP)
pixelok<- which(!is.na(apply(TabPP,1,mean)))
TabPP<- pivot_longer(TabPP, cols=1:251, names_to = "Secondes", values_to = "PP", values_drop_na = TRUE)

{
TabPP$Secondes<-sub("values.X","",TabPP$Secondes)
sec<-as.numeric(TabPP$Secondes)
Jour0<-strptime("1998-01-01", format= "%Y-%m-%d")
TabPP$Date<- Jour0+sec

TabPP$Year <- as.numeric(substr(as.character(TabPP$Date),1,4))
TabPP$Month<- as.numeric(substr(as.character(TabPP$Date), 6,7))
TabPP$Day<- as.numeric(substr(as.character(TabPP$Date), 9,10))
}


# Infos
#mean(TabPP$PP)
#min(TabPP$PP)
#max(TabPP$PP)
#sd(TabPP$PP)
#var(TabPP$PP)


# Mean PP per year
TabPP2<- TabPP %>% group_by(x,y,Year) %>% summarize(moyPP= mean(PP))
ggplot(TabPP2)+
  geom_tile(aes(x=x, y=y, fill=log(moyPP)))+
  ggtitle("Production primaire")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="log10 (Production primaire)")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Greens"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(TabPP2, file="data/satellite/Primary production/TabPP2.Rdata")

#ggplot(TabPP2, aes(x= Year, y=moyPP, group=Year))+geom_boxplot()


# Mean PP 1998-2018
#TabPP3<- TabPP2 %>% group_by(x,y) %>% summarize(moyper= mean(moyPP))
#ggplot(TabPP3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Production Primaire moyenne 1998-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="mg C/m3/j")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean PP (year)
TabPP4<- TabPP %>% group_by(Year) %>% summarize(moybaie= mean(PP))
save(TabPP4, file= "results/satellite/series full bay/PPTab.Rdata")

PPseries<- ggplot(TabPP4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean primary production 1998-2018")+
  xlab("Year")+
  ylab("mg C/m3/j")+
  theme_minimal()

save(PPseries, file="results/satellite/series full bay/PP_series.Rdata")
ggsave(plot= PPseries, filename="PP.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean PP (month)
TabPP6<- TabPP %>% group_by(Month) %>% summarize(moybaie= mean(PP))
save(TabPP6, file= "results/satellite/series full bay/monthly/PPTab.Rdata")


PPseries2<- ggplot(TabPP6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean primary production")+
  xlab("Month")+
  ylab("mg C/m3/j")+
  theme_minimal()

PPseries3<- PPseries2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

save(PPseries3, file="results/satellite/series full bay/monthly/PP_series.Rdata")
ggsave(plot= PPseries3, filename="PP.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabPPnew<- pivot_wider(TabPP2, names_from = Year, values_from = moyPP)
TabPPnew<- na.omit(TabPPnew)
metaTabnew<- TabPPnew %>% dplyr::select(x, y) %>% ungroup()
TabPPnew<- TabPPnew %>% ungroup() %>% dplyr::select(-x, -y)               

distance<- dist(TabPPnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1, main="Production primaire", cex.main=2)

#NbClust(TabPPnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
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
                          scale.color = c("#CCFFCC", "#99CC99"))

PPgg<- firstplot +
  ggtitle("Production primaire")+
  xlab("Coefficient d'agglomération = 0.99")+
  ylab("Distance")+
  theme_classic()+
  theme(plot.title = element_text(size = 40, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 35)) +
  theme(axis.title.y = element_text(size = 35)) +
  theme(axis.text.x = element_blank())

ggsave(plot= PPgg, filename="PP.jpeg", path="results/satellite/Dendrogrammes", width = 13, height = 8)

zone<- PP[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))      

essai<- left_join(TabPP2, toto, by=c("x", "y"))     

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyPP)) }  

toto2PP<- left_join(toto, essai2, by="Clust")                           



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabPPnew))
serie<- pivot_longer(serie, cols=c(4:24), names_to="Year", values_to = "PP")
serie<- serie %>% group_by(Year, Clust) %>% summarise(PP=mean(PP))

ggseriePP<-  ggplot(serie)+
  geom_point(aes(x=Year,y=PP,col=Clust))+
  geom_line(aes(x=Year,y=PP,col=Clust, group=Clust))+
  ggtitle("Primary production")+
  ylab("mg C/m3/j")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggseriePP, file="results/satellite/series by zone/PP_seriebyzone.Rdata")
ggsave(plot= ggseriePP, filename="PP_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
  # 1st Polygon
liste <- with(toto2PP, chull(x, y))
hull <- toto2PP[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#PP<- ggplot(toto2PP)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean PP (mg C/m3/j)")+theme_minimal()+coord_fixed()+ggtitle("Primary production")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3PP<- toto2PP
coordinates(toto3PP)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3PP) <- TRUE
  # coerce to raster
rasterPP<- raster(toto3PP)
rasterPP
raster::plot(rasterPP, col= c("#CCFFCC", "#99CC99"), main="Primary production", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disPP<- disaggregate(rasterPP, fact=(res(rasterPP)/res(rasterchlnew)))
mPP<- mask(disPP, res)
plot(mPP)

save(mPP, file="data/satellite/Primary production/PP_raster.Rdata")

jpeg(file="results/satellite/zones/PP_raster.jpeg")
plot(mPP, main="Primary production", xlab="Longitude", ylab="Latitude")
dev.off()



library(sf)

#test loran
r1<- raster(nrow=20, ncol=20, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
plot(r1)
crs(rasterPP)<-crs(r1)
r1c<- resample(rasterPP, r1, method="ngb")
plot(r1c)

#testrasterize
r1<- raster(nrow=200, ncol=200, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
xy<-data.frame(toto2PP[,1:2])
r2<-rasterize(xy,r1,as.numeric(toto2PP$Clust),background=-999)#init1$Clust)
plot(r2)

#test rasterToPolygons
r1<- raster(nrow=10, ncol=20, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
values(r1) <- rnorm(ncell(r1))
p1<-rasterToPolygons(rasterPP)
p2<-st_as_sf(p1)
r2<-fasterize(p2,r1,field="Clust")
resample(rasterPP,r2)

#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#<<<<<<< HEAD
plot(r1)
plot(p1,add=T)
r2<-rasterize(p1,r1)
plot(r2)
#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#=======
#>>>>>>> f21cafda9eb0b1cc2c00b81766d367ed685ce785
#<<<<<<< HEAD



# Polygons

polPP<- rasterToPolygons(mPP, dissolve=TRUE)
plot(polPP, col=polPP@data$Clust)

writeOGR(polPP, dsn="data/satellite/Primary production", layer="PP", driver="ESRI Shapefile")

save(polPP, file="data/satellite/Primary production/PP_polygons.Rdata")



# Mean PP / zone

summaryPP<- toto2PP %>% select(Clust, mean)
summaryPP<- unique(summaryPP)

write.table(summaryPP, file="results/satellite/means by zone/summaryPP.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4PP<- toto2PP %>% select(-Clust)
coordinates(toto4PP)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4PP) <- TRUE
  # coerce to raster
rasterPP2<- raster(toto4PP)
rasterPP2
raster::plot(rasterPP2, col=c("#CCFFCC", "#99CC99"), main="Primary production", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

disPP2<- disaggregate(rasterPP2, fact=(res(rasterPP2)/res(rasterchlnew)))
mPP2<- mask(disPP2, res)
plot(mPP2, col=c("#CCFFCC", "#99CC99"))

save(mPP2, file="results/satellite/means by zone/PP_raster.Rdata")

jpeg(file="results/satellite/means by zone/PP_raster.jpeg")
plot(mPP2, main="Primary production", xlab="Longitude", ylab="Latitude", col=c("#CCFFCC", "#99CC99"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mPP, maxPixel = 1000000) {
  
  if (ncell(mPP) > maxPixel) {
    x <- sampleRegular(mPP, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mPP, seq_len(ncell(mPP)))
  out <- mPP %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabPPfin<- fortify(mPP)

save(TabPPfin, file="data/satellite/Primary production/TabPPfin.Rdata")







