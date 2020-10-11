#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
#wdpaid <- 'minlon_minlat_maxlon_maxlat'
wdpaidsplit <- unlist(strsplit(wdpaid, "[_]"))
xmin <- as.numeric(wdpaidsplit[1])
ymin <- as.numeric(wdpaidsplit[2])
xmax <- as.numeric(wdpaidsplit[3])
ymax <- as.numeric(wdpaidsplit[4])

#Library
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(RGeostats)
library(viridis)
#Libraries for mask
library(rworldmap)
library(rworldxtra)


# Etendre le tableau et rajouter lignes de densit? = 0
ttsel <- Sp %>% ungroup() %>% dplyr::select(group, Year, meanLon, meanLat, Abun) %>% distinct() 
codif <- ttsel %>% dplyr::select( Year, meanLon, meanLat) %>% distinct() 
allstrat <- ttsel %>% tidyr::expand(group,Year) 
pipo <- dplyr::left_join(allstrat, codif) %>% dplyr::left_join(ttsel) %>% dplyr::mutate(Abun = ifelse(is.finite(Abun), Abun, 0))
Sp0 <- dplyr::left_join(codif, pipo) %>% na.omit()

#Creation du polygon de krigeage

# 1st Polygon
liste <- with(Sp0, chull(meanLon, meanLat))
hull <- Sp0[liste, c("meanLon", "meanLat")]
Poly <- Polygon(hull)

# Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

# Cut object along coast
coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))

res <- gDifference(buff, coast)
PolyCut <- fortify(res)


# Put polygon in good format for later use
toto <- PolyCut[PolyCut$piece==1,]
db.poly <- polygon.create(toto[,c(1,2)])

data.db <- db.create(Sp0)
data.db <- db.locate(data.db, c("meanLon","meanLat"),"x")
data.db <- db.locate(data.db, "Abun","z")

# Boucle vario/krigeage sur tout les clusters (avec logarithme)

Longitude <- numeric()
Latitude <- numeric()
Prediction.std <- numeric()
Variance.std <- numeric()
Year <- numeric()
Cluster <- character()
Prediction <- numeric()
Variance <- numeric()


# Grille de krigeage
grid.db <- db.grid.init(data.db,nodes=c(100,100))
poly <- polygon.create(x=PolyCut[PolyCut$group=="1.1",1],y=PolyCut[PolyCut$group=="1.1",2]) #RGeostats polygon
grid.db <- db.polygon(grid.db, poly)


# Moving or unique neighbouhood?
nei1 <- neigh.create(ndim=2,type=0) #unique
nei2 <- neigh.create(ndim=2,type=2,nmini=2,nmaxi=8,radius=1) # moving


for (j in unique(pull(Sp0[,"group"]))){
  
  data.db <- db.create(Sp0[Sp0$group==j,]) #Choix du cluster
  data.db <- db.locate(data.db,names=c("meanLon","meanLat"),"x")
  data.db <- db.locate(data.db,names=c("Abun"),"z")
  
  # Standartisation par an
  data.db.std <- data.db
  data.db.std <- db.locate(data.db.std,"Year","code")
  
  # Vario moyen
  vg.data.std <- vario.calc(data.db.std, lag=0.05, nlag=floor(max(dist(data.db@items[,c(3,4)]))/2)/0.05,opt.code=1,tolcode=0) 
  #plot(vg.data.std,npairdw=T,inches=0.1,las=1,add=T,col=2,lwd=2)
  vario1 <- vg.data.std
  vg.mod <- model.auto(vario=vario1, struct=c(1:5), npairdw=TRUE, title= paste0("Communauté", " ", j), size=45,  col="red", inches=.05)
  
  
  data.db.std <- db.locate(data.db.std,"Year",NA)
  
  for (i in sort(unique(data.db[,"Year"]))){
    
    sel <- data.db.std[,"Year"]== i
    
    if (all(sel==F))
    {Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
    Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
    Prediction <- c(Prediction,rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Variance <- c(Variance, rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))}else{
      
      # Ordinary kriging, unique neighbourhood
      sel <- data.db.std[,"Year"]== i
      db.kriege <- db.create(data.db.std[sel])
      db.kriege <- db.locate(db.kriege,names=c("meanLon","meanLat"),"x")
      db.kriege <- db.locate(db.kriege,names=c("Abun"),"z")
      kres <- kriging(db.kriege, grid.db, model = vg.mod, neigh = nei1, uc=c("1"), mean=NA)

      Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
      Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
      Prediction <- c(Prediction, kres@items$Kriging.Abun.estim[kres@items$Polygon==TRUE])
      Variance <- c(Variance, kres@items$Kriging.Abun.stdev[kres@items$Polygon==TRUE])
      Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
      Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))
    }
    
  }
  
}
Kriege.logdens<- data.frame(Longitude=Longitude, Latitude=Latitude, Prediction=Prediction, Variance=Variance, Year=Year, Cluster=factor(Cluster))
#save(Kriege.logdens, file="data/krigeage log.RData")

names(Kriege.logdens)[6]<- "Community"
Kriege.logdens$Community<- as.numeric(Kriege.logdens$Community)

Longitude<- numeric()
Latitude<- numeric()
Clust<- numeric()
Community<- numeric()


for (j in unique(data.frame(Kriege.logdens)[,"Community"])){
  Tab1<- Kriege.logdens[Kriege.logdens$Community==j,] %>% dplyr::select(-Variance, -Community)
  Tab2<- pivot_wider(Tab1, names_from = Year, values_from = Prediction)
  metaTab<- Tab2 %>% dplyr::select(Longitude, Latitude)
  Tab2<- Tab2 %>% dplyr::select(-c(Longitude, Latitude))
  
  #Classification
  
  distance<- dist(Tab2)
  tree<- fastcluster::hclust(distance, method="ward.D2")  
  
  # Dendrogrammes 
  zones<- cutree(tree, 5)
  
  
  toto<- cbind(metaTab, Clust=factor(zones))        
  toto<- left_join(toto, Kriege.logdens[Kriege.logdens$Community==j,], by=c("Longitude", "Latitude")) 
  toto<- toto %>% dplyr::select(Longitude, Latitude, Clust, Community)  
  
  
  Longitude<- c(Longitude, toto$Longitude)
  Latitude<- c(Latitude, toto$Latitude)
  Clust<- c(Clust, toto$Clust)
  Community<- c(Community, toto$Community)
  
  tata <- left_join(toto, cbind(metaTab, Tab2))         
  tata <- pivot_longer(tata, cols=c(5:dim(tata)[2]), names_to="Year", values_to = "Prediction")    
  tata <- tata %>% dplyr::group_by(Year, Clust) %>% dplyr::summarise(Prediction=mean(Prediction))     
  
  ggtata<-  ggplot(tata)+
    geom_point(aes(x=Year, y=Prediction, col=Clust))+
    geom_line(aes(x=Year, y=Prediction, col=Clust, group=Clust))+
    theme_minimal()+
    facet_wrap(.~Clust)
  
  #print(ggtata)
  
  tete<- tata
  names(tete)[2]<- "Zones"
  tete<- tata %>% ungroup() %>% dplyr::group_by(Clust) %>% dplyr::summarise(Prediction= mean(Prediction))
  
  titi<- toto %>% left_join(tete, by="Clust")
  
  
  
}
Tabfaunefin<- data.frame(Longitude=Longitude, Latitude=Latitude, Clust=as.factor(Clust), Community=Community)

for(i in 1:(length(keep)+2)){
  Tabfaunefin$Community[Tabfaunefin$Community==i] <- keep[i]
}

ggplot(Tabfaunefin)+
  geom_raster(aes(x=Longitude,y=Latitude,fill=Clust))+facet_wrap(~Community)+scale_fill_viridis_d()+theme_minimal()+
  geom_polygon(data=PolyCut[PolyCut$group==1.1,],aes(x=long, y=lat, group=group), col="black", fill=NA)+
  ggtitle("Classificiation of biological groups in selected area")
