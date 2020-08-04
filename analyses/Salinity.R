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

Sal<- nc_open("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")
Sal<- stack("data/satellite/Salinity/MetO-NWS-PHY-mm-SAL_1583156080399.nc")


# Conversion raster - tableau
fortify.Raster <- function(Sal, maxPixel = 1000000) {
  
  if (ncell(Sal) > maxPixel) {
    x <- sampleRegular(Sal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(Sal, seq_len(ncell(Sal)))
  out <- Sal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}


# Traitement tableau
TabSal<- fortify(Sal)
pixelok<- which(!is.na(apply(TabSal,1,mean)))
TabSal<- pivot_longer(TabSal, cols=1:323, names_to = "Secondes", values_to = "Salinite", values_drop_na = TRUE)

{
  TabSal$Secondes<-sub("values.X","",TabSal$Secondes)
  TabSal$Secondes<-sub("e.0","e0",TabSal$Secondes)
  secsst<-as.numeric(TabSal$Secondes)
  Jour0<-strptime("1992-01-16", format= "%Y-%m-%d")
  Date<- Jour0+secsst
  TabSal$Date<- Date
  
  TabSal$Year <- as.numeric(substr(as.character(TabSal$Date),1,4))
  TabSal$Month<- as.numeric(substr(as.character(TabSal$Date), 6,7))
  TabSal$Day  <- as.numeric(substr(as.character(TabSal$Date), 9,10))
}


# Infos
#mean(TabSal$Salinite)
#min(TabSal$Salinite)
#max(TabSal$Salinite)
#sd(TabSal$Salinite)
#var(TabSal$Salinite)


# Mean salinity per year
TabSal2<- TabSal %>% group_by(x,y,Year) %>% summarize(moySal= mean(Salinite))
ggplot(TabSal2)+
  geom_tile(aes(x=x, y=y, fill= log(moySal)))+
  ggtitle("Salinité")+
  facet_wrap(. ~ Year)+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="log10 (Salinité)")+
  theme_minimal()+
  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Greys"))+
  theme(strip.text.x = element_text(size = 15))+
  theme(axis.text.x = element_blank())+
  theme(plot.title = element_text(size = 30, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 15))+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.text.y = element_blank())+
  theme(legend.title = element_text(size = 15))

save(TabSal2, file="data/satellite/Salinity/TabSal2.Rdata")

#ggplot(TabSal2, aes(x= Year, y=moySal, group=Year))+geom_boxplot()


# Mean salinity 1998-2018
#TabSal3<- TabSal2 %>% group_by(x,y) %>% summarize(moyper= mean(moySal))
#ggplot(TabSal3)+
#  geom_tile(aes(x=x, y=y, fill= moyper))+
#  ggtitle("Salinite moyenne 1992-2018")+
#  xlab("Longitude")+
#  ylab("Latitude")+
#  theme_minimal()+
#  scale_fill_gradientn(colours = terrain.colors(6))  


# Serie tempo mean salinity (year)
TabSal4<- TabSal %>% group_by(Year) %>% summarize(moybaie= mean(Salinite))
save(TabSal4, file= "results/satellite/series full bay/SalTab.Rdata")

Salseries<- ggplot(TabSal4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Mean salinity 1992-2018")+
  xlab("Year")+
  ylab("1E-3")+
  theme_minimal()  

save(Salseries, file="results/satellite/series full bay/Sal_series.Rdata")
ggsave(plot= Salseries, filename="Salinity.jpeg", path="results/satellite/series full bay", width = 13, height = 8)



# Serie tempo mean salinity (month)
TabSal6<- TabSal %>% group_by(Month) %>% summarize(moybaie= mean(Salinite))
save(TabSal6, file= "results/satellite/series full bay/monthly/SalTab.Rdata")

Salseries2<- ggplot(TabSal6)+
  geom_line(aes(x= Month, y= moybaie))+
  ggtitle("Mean salinity")+
  xlab("Month")+
  ylab("1E-3")+
  theme_minimal()  

save(Salseries2, file="results/satellite/series full bay/monthly/Sal_series.Rdata")
ggsave(plot= Salseries2, filename="Salinity.jpeg", path="results/satellite/series full bay/monthly", width = 13, height = 8)



# Partitionnement

TabSalnew<- pivot_wider(TabSal2, names_from = Year, values_from = moySal)
TabSalnew<- na.omit(TabSalnew)
metaTabnew<- TabSalnew %>% dplyr::select(x, y) %>% ungroup()
TabSalnew<- TabSalnew %>% ungroup() %>% dplyr::select(-x, -y)

distance<- dist(TabSalnew)
#distance[1:5]

tree<- agnes(distance, method="ward", par.method=1)
plot(tree, which=2,hang=-1, main="Salinité", cex.main=2)

#NbClust(TabSalnew, min.nc = 2, max.nc = 10, index="all", method = "ward.D2")
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
                          scale.color = c("black", "#F0F0F0", "#BDBDBD", "#636363"))

Salgg<- firstplot +
  ggtitle("Salinité")+
  xlab("Coefficient d'agglomération = 0.99")+
  ylab("Distance")+
  theme_classic()+
  theme(plot.title = element_text(size = 40, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 35)) +
  theme(axis.title.y = element_text(size = 35)) +
  theme(axis.text.x = element_blank())

ggsave(plot= Salgg, filename="Sal.jpeg", path="results/satellite/Dendrogrammes", width = 13, height = 8)

zone<- Sal[[1]]
values(zone)<- NA
zone[pixelok]<- zones
#plot(zone, xlab="Longitude", ylab="Latitude")

toto <- cbind(metaTabnew, Clust=factor(zones))

essai<- left_join(TabSal2, toto, by=c("x", "y"))

for (k in unique(essai[,"Clust"])){
  essai2<- essai %>%  group_by(Clust) %>% summarise(mean= mean(moySal)) }

toto2Sal<- left_join(toto, essai2, by="Clust")



# Serie tempo / zone

serie<- left_join(toto, cbind(metaTabnew, TabSalnew))
serie<- pivot_longer(serie, cols=c(4:30), names_to="Year", values_to = "Sal")
serie<- serie %>% group_by(Year, Clust) %>% summarise(Sal=mean(Sal))

ggserieSal<-  ggplot(serie)+
  geom_point(aes(x=Year,y=Sal,col=Clust))+
  geom_line(aes(x=Year,y=Sal,col=Clust, group=Clust))+
  ggtitle("Salinity")+
  ylab("1E-3")+
  theme_minimal()+
  facet_wrap(.~Clust)+
  guides(x = guide_axis(angle = 90))

save(ggserieSal, file="results/satellite/series by zone/Sal_seriebyzone.Rdata")
ggsave(plot= ggserieSal, filename="Sal_seriesbyzone.jpeg", path="results/satellite/series by zone", width = 13, height = 8)



# Trait de cote
  # 1st Polygon
liste <- with(toto2Sal, chull(x, y))
hull <- toto2Sal[liste, c("x", "y")]
Poly <- Polygon(hull)

  # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

  # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)

#Sal<- ggplot(toto2Sal)+geom_tile(aes(x=x,y=y,fill=mean))+xlab("Longitude")+ylab("Latitude")+labs(fill="mean salinity")+theme_minimal()+coord_fixed()+ggtitle("Salinity")+geom_polygon(data=tete, aes(x=long,y=lat, group=group),fill=NA,col="black")



# Raster

#r0<- raster(nrow=45, ncol=163, xmn=-1.400764, xmx=0.3900167, ymn=49.30618, ymx=49.80057)
#projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # create SpatialPointsDataFrame
toto3Sal<- toto2Sal
coordinates(toto3Sal)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto3Sal) <- TRUE
  # coerce to raster
rasterSal<- raster(toto3Sal)
rasterSal
plot(rasterSal, col= brewer.pal(n = 3, name = "Greys"), main="Salinity", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dissal<- disaggregate(rasterSal, fact=(res(rasterSal)/res(rasterchlnew)))
mSal<- mask(dissal, res)
plot(mSal, col= brewer.pal(n = 3, name = "Greys"))

save(mSal, file="data/satellite/Salinity/Sal_raster.Rdata")

jpeg(file="results/satellite/zones/Sal_raster.jpeg")
plot(mSal, main="Salinity", xlab="Longitude", ylab="Latitude", col= brewer.pal(n = 3, name = "Greys"))
dev.off()



# Polygons

polSal<- rasterToPolygons(mSal, dissolve=TRUE)
plot(polSal, col=polSal@data$Clust)

writeOGR(polSal, dsn="data/satellite/Salinity", layer="Sal", driver="ESRI Shapefile")

save(polSal, file="data/satellite/Salinity/Sal_polygons.Rdata")



# Mean salinity / zone

summarySal<- toto2Sal %>% select(Clust, mean)
summarySal<- unique(summarySal)

write.table(summarySal, file="results/satellite/means by zone/summarySal.csv", sep = ";", row.names = FALSE)


  # create SpatialPointsDataFrame
toto4Sal<- toto2Sal %>% select(-Clust)
coordinates(toto4Sal)<- ~ x + y
  # coerce to SpatialPixelsDataFrame
gridded(toto4Sal) <- TRUE
  # coerce to raster
rasterSal2<- raster(toto4Sal)
rasterSal2
plot(rasterSal2, col=brewer.pal(n = 3, name = "Greys"), main="Salinity", xlab="Longitude", ylab="Latitude")

dissal2<- disaggregate(rasterSal2, fact=(res(rasterSal2)/res(rasterchlnew)))
mSal2<- mask(dissal2, res)
plot(mSal2, col=brewer.pal(n = 3, name = "Greys"))

save(mSal2, file="results/satellite/means by zone/Sal_raster.Rdata")

jpeg(file="results/satellite/means by zone/Sal_raster.jpeg")
plot(mSal2, main="Salinity", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "Greys"))
dev.off()



# Pour full_join

    # Conversion raster - tableau
fortify.Raster <- function(mSal, maxPixel = 1000000) {
  
  if (ncell(mSal) > maxPixel) {
    x <- sampleRegular(mSal, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(mSal, seq_len(ncell(mSal)))
  out <- mSal %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

TabSalfin<- fortify(mSal)

save(TabSalfin, file="data/satellite/Salinity/TabSalfin.Rdata")







