library(dplyr)
library(tidyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(NbClust)
library(raster)

load("data/krigeage.Rdata")
names(Kriege.logdens)[6]<- "Community"
Kriege.logdens$Community<- as.numeric(Kriege.logdens$Community)

Tabcom1<- Kriege.logdens %>% filter(Community==9) 

Tab1<- Tabcom1 %>% select(-c(Variance, Community))
Tab2<- pivot_wider(Tab1, names_from = Year, values_from = Prediction)
metaTab<- Tab2 %>% select(Longitude, Latitude)
Tab2<- Tab2 %>% select(-c(Longitude, Latitude))



# Classification

distance<- dist(Tab2)
distance[1:5]

tree<- hclust(distance)
plot(tree, hang=-1)


# Determining best number of zones for each community

Nb1<- Tab1 %>% group_by(Longitude,Latitude) %>% summarize(moyper= mean(Prediction))
Nb2<- Nb1 %>% ungroup() %>% dplyr::select(moyper)
NbClust(Nb2, min.nc = 2, max.nc = 10, index="all", method = "ward.D")
# According to the majority rule, the best number of clusters is  3

rect.hclust(tree, 3)
zones<- cutree(tree, 3)

toto<- cbind(metaTab, Clust=factor(zones))
ggplot(toto)+
  geom_tile(aes(x=Longitude, y=Latitude, fill=Clust)) +
  theme_minimal() +
  coord_fixed()+
  scale_fill_manual(values=c("cyan3","red", "chartreuse4", "darkgoldenrod1", "burlywood4"))



# Trait de cote
    # 1st Polygon
liste <- with(toto, chull(Longitude, Latitude))
hull <- toto[liste, c("Longitude", "Latitude")]
Poly <- Polygon(hull)

    # Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

    # Cut object along coast
coast <- rgdal::readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- rgeos::gDifference(buff, coast)



# Raster

    # create SpatialPointsDataFrame
toto1<- toto
coordinates(toto1)<- ~ Longitude + Latitude
    # coerce to SpatialPixelsDataFrame
gridded(toto1) <- TRUE
    # coerce to raster
rastercom9<- raster(toto1)
rastercom9
plot(rastercom5, main="", xlab="Longitude", ylab="Latitude")

load("data/satellite/chl/rasterChlnew.Rdata")

dis<- disaggregate(rastercom9, fact=(res(rastercom9)/res(rasterchlnew)))
mCom9<- mask(dis, res)
plot(mCom9)

save(mCom9, file="data/ICES/Com9_raster.Rdata")



# Polygons

pol<- rasterToPolygons(mCom9, dissolve=TRUE)
plot(pol, col=pol@data$Clust)

writeOGR(pol, dsn="data/ICES", layer="Com9", driver="ESRI Shapefile")







