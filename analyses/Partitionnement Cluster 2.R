
load("data/krigeage.Rdata")

Tab<- Kriege.logdens[Kriege.logdens$Cluster==2,]
Tab<- Tab %>% select(-c(Variance,Cluster))
Tab<- pivot_wider(Tab, names_from = Year, values_from = Prediction)
metaTab<- Tab %>% select(Longitude, Latitude)
Tab<- Tab %>% select(-c(Longitude,Latitude))


# Calcul des distances entre les pixels pour pouvoir les regrouper par zones
distance<- dist(Tab)
distance[1:5]


# Construction dendro
arbreclassif<- hclust(distance)
plot(arbreclassif, hang=-1)


# Partitionnement dendro 
rect.hclust(arbreclassif, 5)
zonespixel<-cutree(arbreclassif, 5)
print(zonespixel)

# zone<- sst[[1]]
# values(zone)<-NA
# zone[pixelok]<-zonespixel
# plot(zone, xlab="Longitude", ylab="Latitude")
# 
# 
# library(raster)
# r0<- raster(nrow=80, ncol=100, xmn=-1.500034, xmx=0.7083337, ymn=49.16667, ymx=49.70833)
# projection(r0)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# 
# r1<- raster::rasterize(metaTab2new, r0, fields=zonespixel, fun=mean)
# plot(r1)

toto <- cbind(metaTab,Clust=factor(zonespixel))
head(toto)

ggplot(toto)+
  geom_tile(aes(x=Longitude,y=Latitude,fill=Clust))+theme_minimal()+coord_fixed()

tata <- left_join(toto,cbind(metaTab,Tab))
tata <- pivot_longer(tata,cols=c(4:35),names_to="Year",values_to = "Prediction")
tata <- tata %>% group_by(Year,Clust) %>% summarise(Prediction=mean(Prediction))

ggplot(tata)+
  geom_point(aes(x=Year,y=Prediction,col=Clust))+
  geom_line(aes(x=Year,y=Prediction,col=Clust, group=Clust))+theme_minimal()+
  facet_wrap(.~Clust)

