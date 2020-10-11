#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])

#Library
library(tidyr)
library(dplyr)

#Libraries for mask
library(rworldmap)
library(rworldxtra)
#Write NA
{
  HH==-9
  which(HH==-9, arr.ind = T)
  HH[which(HH==-9, arr.ind = T)]
  HH[which(HH==-9, arr.ind = T)]<- NA
}

{
  HL==-9
  which(HL==-9, arr.ind = T)
  HL[which(HL==-9, arr.ind = T)]
  HL[which(HL==-9, arr.ind = T)]<- NA
}

HH<- HH %>% dplyr::select(StNo ,HaulNo ,Year ,ShootLat,ShootLong ,HaulLat ,HaulLong ,Distance )
HL<- HL %>% dplyr::select(StNo ,HaulNo ,Year ,SpecCode, TotalNo)
HL<- unique(HL)

Bio <- HL %>% left_join(HH, by=c("StNo"="StNo","HaulNo"="HaulNo","Year"="Year"))

Bio<- Bio %>% mutate(meanLat= (ShootLat+HaulLat)/2, 
                     meanLon= (ShootLong+HaulLong)/2) %>%
  dplyr::select(-ShootLat,-ShootLong ,-HaulLat ,-HaulLong)

Bio <- Bio %>% group_by(StNo,HaulNo,Year, meanLon, meanLat) %>% summarise(S=length(unique(SpecCode)))
Bio <- Bio[Bio$meanLon<xmax & Bio$meanLon>xmin & Bio$meanLat<ymax & Bio$meanLat>ymin,]

coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))


ggplot(Bio) + geom_polygon(data=coast, aes(x=long,y=lat,group=group),col="black",fill="grey")+
  geom_point(aes(x=meanLon, y=meanLat, size=S, col=S))+scale_color_viridis_c()+ facet_wrap(~Year)+
  xlab("Longitude")+ylab("Latitude")
