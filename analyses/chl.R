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

chl<- stack("data/satellite/Chl/dataset-oc-glo-bio-multi-l4-chl_4km_monthly-rep_1592571915166.nc")


# Conversion raster - tableau
fortify.Raster <- function(chl, maxPixel = 1000000) {
  
  if (ncell(chl) > maxPixel) {
    x <- sampleRegular(chl, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(chl, seq_len(ncell(chl)))
  out <- chl %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
Tabchl<- fortify(chl)
pixelok<- which(!is.na(apply(Tabchl,1,mean)))
Tabchl<- pivot_longer(Tabchl, cols=1:262, names_to = "Date", values_to = "Chloro", values_drop_na = TRUE)

{
  Tabchl$Date<- sub("values.X","",Tabchl$Date)
  Tabchl$year<- as.numeric(substr(as.character(Tabchl$Date),1,4))
  Tabchl$month<- as.numeric(substr(as.character(Tabchl$Date), 6,7))
  Tabchl$day<- as.numeric(substr(as.character(Tabchl$Date), 9,10))
}


# Infos
#mean(Tabchl$Chloro)
#min(Tabchl$Chloro)
#max(Tabchl$Chloro)
#sd(Tabchl$Chloro)
#var(Tabchl$Chloro)


# Mean chl per year
Tabchl2<- Tabchl %>% group_by(x,y,year) %>% summarize(moyChl= mean(Chloro))
ggplot(Tabchl2)+
  geom_tile(aes(x=x, y=y, fill=log(moyChl)))+
  ggtitle("Chlorophylle a")+
  facet_wrap(. ~ year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="log10 (Chlorophylle a)")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "PuRd"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(Tabchl2, file="data/satellite/Chl/Tabchl2.Rdata")

#ggplot(Tabchl2, aes(x= year, y=moyChl, group=year))+
#  geom_boxplot()


# Mean chl 1997-2017
#Tabchl3<- Tabchl2 %>% group_by(x,y) %>% summarize(moyper= mean(moyChl))
#ggplot(Tabchl3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Chlorophylle moyenne 1997-2017")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  labs(fill="?g/L")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))


# Serie tempo mean chl (year)
Tabchl4<- Tabchl %>% group_by(year) %>% summarize(moybaie= mean(Chloro))
save(Tabchl4, file= "results/satellite/series full bay/chlTab.Rdata")

Chlseries<- ggplot(Tabchl4)+
  geom_line(aes(x= year, y= moybaie))+
  ggtitle("Mean chlorophyll 1997-2019")+
  xlab("Year")+
  ylab("mg/m3")+
  theme_minimal() 

save(Chlseries, file="results/satellite/series full bay/Chl_series.Rdata")
ggsave(plot= Chlseries, filename="chl.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean chl (month)
Tabchl6<- Tabchl %>% group_by(month) %>% summarize(moybaie= mean(Chloro))
save(Tabchl6, file= "results/satellite/series full bay/monthly/chlTab.Rdata")

Chlseries2<- ggplot(Tabchl6)+
  geom_line(aes(x= month, y= moybaie))+
  ggtitle("Mean chlorophyll")+
  xlab("Month")+
  ylab("mg/m3")+
  theme_minimal() 

Chlseries3<- Chlseries2 +
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.text.x = element_text(size = 15, colour = "blue"))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_text(size = 15, colour = "red"))

save(Chlseries3, file="results/satellite/series full bay/monthly/Chl_series.Rdata")
ggsave(plot= Chlseries3, filename="chl.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

Tabchlnew<- pivot_wider(Tabchl2, names_from = year, values_from = moyChl)
Tabchlnew<- na.omit(Tabchlnew)
metaTabnew<- Tabchlnew %>% dplyr::select(x, y) %>% ungroup()
Tabchlnew<- Tabchlnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(Tabchlnew)
tree<- agnes(distance, method="ward", par.method=1)
plot(tree, hang=-1, main= "Chlorophylle a", cex.main=2, xlab = NULL)

#NbClust(Tabchlnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
# According to the majority rule, the best number of clusters is  3

rect.hclust(tree, 3)
zones<- cutree(tree, 3)


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


treecut <- dendro_data_k(tree, 3)

firstplot<- plot_ggdendro(treecut,
              direction   = "tb",
              expand.y    = 0.2,
              scale.color = c("black","#EDF8B1", "#7FCDBB", "#2C7FB8"))

chlgg<- firstplot +
  ggtitle("Chlorophylle a")+
  xlab("Coefficient d'agglomération = 1")+
  ylab("Distance")+
  theme_classic()+
  theme(plot.title = element_text(size = 40, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 35)) +
  theme(axis.title.y = element_text(size = 35)) +
  theme(axis.text.x = element_blank())

ggsave(plot= chlgg, filename="chl.jpeg", path="results/satellite/Dendrogrammes", width = 13, height = 8)

zone<- chl[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(Tabchl2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moyChl)) }

toto2chl<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, Tabchlnew))
serie<- pivot_longer(serie, cols=c(4:26), names_to="Year", values_to = "chl")
serie<- serie %>% group_by(Year, Clust) %>% summarise(chl=mean(chl))
save(serie, file="results/satellite/series by zone/chlTab.Rdata")

clust.labs<- c("Zone 1", "Zone 2", "Zone 3")
names(clust.labs)<- c("1", "2", "3")
colors<- brewer.pal(n = 3, name = "YlGnBu")

ggseriechl<-  ggplot(serie)+
  geom_point(aes(x=Year,y=chl,col=Clust))+
  geom_line(aes(x=Year,y=chl,col=Clust, group=Clust))+
  scale_colour_manual(values=colors)+
  ggtitle("Chlorophyll")+
  ylab("mg/m3")+
  theme_minimal()+
  facet_wrap(.~Clust, labeller = labeller(Clust= clust.labs))+
  theme(strip.text.x = element_text(size = 20))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 20))+
  theme(axis.text.y = element_text(size = 10))

save(ggseriechl, file="results/satellite/series by zone/chl_seriebyzone.Rdata")
ggsave(plot= ggseriechl, filename="chl_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
# 1st Polygon
liste <- with(toto2chl, chull(x, y))
hull <- toto2chl[liste, c("x", "y")]
Poly <- Polygon(hull)

# Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

# Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Chl<- ggplot(toto2chl)+ geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean chl")+ theme_minimal()+coord_fixed()+ggtitle("Chlorophyll")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3chl<- toto2chl
coordinates(toto3chl)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3chl) <- TRUE
  # coerce to raster
rasterchlnew<- raster(toto3chl)
rasterchlnew
plot(rasterchlnew, col=brewer.pal(n = 3, name = "YlGnBu"), main="Chl", xlab="Longitude", ylab="Latitude")

save(rasterchlnew, file="data/satellite/chl/rasterChlnew.Rdata")

mChl<- mask(rasterchlnew, res)
plot(mChl, col=brewer.pal(n = 3, name = "YlGnBu"))

save(mChl, file="data/satellite/chl/chl_raster.Rdata")

jpeg(file="results/satellite/zones/chl_raster.jpeg")
plot(mChl, main="Chlorophyll", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "YlGnBu"))
dev.off()



# Polygons

polChl<- rasterToPolygons(mChl, dissolve=TRUE)
plot(polChl, col=polChl@data$Clust)

writeOGR(polChl, dsn="data/satellite/chl", layer="Chl", driver="ESRI Shapefile")

save(polChl, file="data/satellite/chl/chl_polygons.Rdata")



# Mean chl / zone

summarychl<- toto2chl %>% dplyr::select(Clust, mean)
summarychl<- unique(summarychl)

write.table(summarychl, file="results/satellite/means by zone/summarychl.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4chl<- toto2chl %>% select(-Clust)
coordinates(toto4chl)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4chl) <- TRUE
  # coerce to raster
rasterchlnew2<- raster(toto4chl)
rasterchlnew2
plot(rasterchlnew2, col=brewer.pal(n = 3, name = "PuRd"), main="Chl", xlab="Longitude", ylab="Latitude")

mChl2<- mask(rasterchlnew2, res)
plot(mChl2, col=brewer.pal(n = 3, name = "PuRd"))

save(mChl2, file="results/satellite/means by zone/chl_raster.Rdata")

jpeg(file="results/satellite/means by zone/chl_raster.jpeg")
plot(mChl2, col=brewer.pal(n = 3, name = "PuRd"), main="Chlorophyll", xlab="Longitude", ylab="Latitude")
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mChl, maxPixel = 1000000) {
  
  if (ncell(mChl) > maxPixel) {
    x <- sampleRegular(mChl, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mChl, seq_len(ncell(mChl)))
  out <- mChl %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

Tabchlfin<- fortify(mChl)

save(Tabchlfin, file="data/satellite/chl/Tabchlfin.Rdata")







