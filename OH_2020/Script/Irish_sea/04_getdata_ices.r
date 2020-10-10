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

#Subset hauls in our area
HH.in <- HH[HH$Long<xmax & HH$Long>xmin & HH$Lat<ymax & HH$Lat>ymin,]
coast2<-st_as_sf(coast)
sp<-st_as_sf(HH.in,coords=c("Long","Lat"),crs=crs(coast2))

#Points index to keep 
SeaPt<-!apply(st_intersects(sp,coast2,sparse=F),1,any)
HH.in <- HH.in[SeaPt,]

#ggplot(HH.in)+geom_point(aes(x=Long,y=Lat,col=Survey))

#Which campaign do we keep
campaign <- names(table(HH.in$Survey)[which(table(HH.in$Survey)==max(table(HH.in$Survey)))])
quarter <- as.integer(names(table(HH.in$Quarter[HH.in$Survey==campaign])[which(table(HH.in$Quarter[HH.in$Survey==campaign])==max(table(HH.in$Quarter[HH.in$Survey==campaign])))]))

#Load campaign for last 10 years
HH <- getDATRAS(record = "HH", campaign, years=2008:2018,quarter)
HL <- getDATRAS(record = "HL", campaign, years=2008:2018,quarter)
