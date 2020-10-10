#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])

#Libraries

library(icesDatras)
library(rworldmap)
library(rworldxtra)

HH <- data.frame()
for (i in getSurveyList()){
  tmp <- getDATRAS(record = "HH", i, years=1998:2018,1:4)
  HH <- rbind(HH,tmp)
}

HH <- dplyr::mutate(HH, Long=((ShootLong+HaulLong)/2), Lat=((ShootLat+HaulLat)/2))

coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))

x <- is.na(maps::map.where(coast, HH$Long, HH$Lat))
HH.in <- HH[which(x),]

ggplot(HH.in)+geom_point(aes(x=Long,y=Lat))
