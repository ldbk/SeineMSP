#Area of interest 
wdpaid <- '-8.02_51.76_-2.63_55.79'
wdpaid <- '-2.8_44.3_-0.2_46.6'
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
library(tidyr)
library(dplyr)
library(ggplot2)
library(worms)
library(rgdal)
library(rgeos)
#library(RGeostats)
library(viridis)


library(ggplot2)
#Libraries for mask
library(rworldmap)
library(rworldxtra)

#
library(worms)

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
Bio <- Bio[Bio$meanLon<xmax & Bio$meanLon>xmin & Bio$meanLat<ymax & Bio$meanLat>ymin,]

Bio.sp <- Bio
Bio <- Bio %>% dplyr::group_by(StNo,HaulNo,Year, meanLon, meanLat) %>% dplyr::summarise(S=length(unique(SpecCode)))

coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))


ggplot(Bio) + geom_polygon(data=coast, aes(x=long,y=lat,group=group),col="black",fill="grey")+
  geom_point(aes(x=meanLon, y=meanLat, size=S, col=S))+scale_color_viridis_c()+ facet_wrap(~Year)+
  xlab("Longitude")+ylab("Latitude")

#Species names
sp <- wormsbyid(unique(Bio.sp$SpecCode))
names(sp)[1] <- "SpecCode"
Bio.sp <- Bio.sp %>% dplyr::left_join(sp[,c(1,3,13,14,15,16,17,18)],by=("SpecCode"))

if(dim(Benthos <- Bio.sp[Bio.sp$phylum!="Chordata",])[1]>500){
  Benthos <- Bio.sp[Bio.sp$phylum!="Chordata",]
}else{Benthos <- data.frame(StNo=numeric(),HaulNo=numeric(),Year=numeric(),meanLon=numeric(),meanLat=numeric(),Abun=numeric(),group=numeric(), TotalNo <- numeric())}

if(dim(Bio.sp[Bio.sp$class=="Elasmobranchii",])[1]>500){
  Elasmo <- Bio.sp[Bio.sp$class=="Elasmobranchii",]
}else{Elasmo <- data.frame(StNo=numeric(),HaulNo=numeric(),Year=numeric(),meanLon=numeric(),meanLat=numeric(),Abun=numeric(),group=numeric(),class=character(), TotalNo <- numeric())}


tmp <- Bio.sp[Bio.sp$phylum=="Chordata" &Bio.sp$class!="Elasmobranchii",]
keep <- names(which(table(tmp$order)>100))

Bio.order <- Bio.sp[Bio.sp$order %in% keep,]

Bio.order <- Bio.order %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,order,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat,order) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))
Elasmo <- Elasmo %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,class,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat,class) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))
Benthos <- Benthos %>% dplyr::select(StNo ,HaulNo,Year,meanLon,meanLat,TotalNo) %>%
  dplyr::group_by(StNo ,HaulNo,Year,meanLon,meanLat) %>% dplyr::summarise(Abun=sum(TotalNo, na.rm=T))

Bio.order$group <- as.integer(as.factor(Bio.order$order))
Bio.order <- Bio.order %>% dplyr::select(-order)
Elasmo$group <- max(Bio.order$group)+1
Elasmo <- Elasmo %>% dplyr::select(-class)
Benthos$group <- max(Bio.order$group)+2


Sp <- rbind(Bio.order,Elasmo,Benthos)
keep <- c(keep,"Elasmonbranchii","Benthic fauna")


# Etendre le tableau et rajouter lignes de densite
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
buff <- raster::buffer(SpPoly, 0.2)

# Cut object along coast
coast <- rworldmap::getMap(resolution = "high")
coast <- raster::crop(coast, raster::extent(xmin-0.1,xmax+0.1,ymin-0.1,ymax+0.1))

res <- gDifference(buff, coast)
PolyCut <- fortify(res)


# Put polygon in good format for later use
toto <- PolyCut[PolyCut$piece==1,]
db.poly <- RGeostats::polygon.create(toto[,c(1,2)])

data.db <- RGeostats::db.create(Sp0)
data.db <- RGeostats::db.locate(data.db, c("meanLon","meanLat"),"x")
data.db <- RGeostats::db.locate(data.db, "Abun","z")

# Boucle vario krigeage sur tout les clusters 

Longitude <- numeric()
Latitude <- numeric()
Prediction.std <- numeric()
Variance.std <- numeric()
Year <- numeric()
Cluster <- character()
Prediction <- numeric()
Variance <- numeric()


# Grille de krigeage
grid.db <- RGeostats::db.grid.init(data.db,nodes=c(100,100))
poly <- RGeostats::polygon.create(x=PolyCut[PolyCut$group=="1.1",1],y=PolyCut[PolyCut$group=="1.1",2]) #RGeostats polygon
grid.db <- RGeostats::db.polygon(grid.db, poly)


# Moving or unique neighbouhood
nei1 <- RGeostats::neigh.create(ndim=2,type=0) #unique
nei2 <- RGeostats::neigh.create(ndim=2,type=2,nmini=2,nmaxi=8,radius=1) # moving


for (j in unique(pull(Sp0[,"group"]))){
  
  data.db <- RGeostats::db.create(Sp0[Sp0$group==j,]) #Choix du cluster
  data.db <- RGeostats::db.locate(data.db,names=c("meanLon","meanLat"),"x")
  data.db <- RGeostats::db.locate(data.db,names=c("Abun"),"z")
  
  # Standartisation par an
  data.db.std <- data.db
  data.db.std <- RGeostats::db.locate(data.db.std,"Year","code")
  
  # Vario moyen
  vg.data.std <- RGeostats::vario.calc(data.db.std, lag=0.05, nlag=floor(max(dist(data.db@items[,c(3,4)]))/2)/0.05,opt.code=1,tolcode=0) 
  #plot(vg.data.std,npairdw=T,inches=0.1,las=1,add=T,col=2,lwd=2)
  vario1 <- vg.data.std
  vg.mod <- RGeostats::model.auto(vario=vario1, struct=c(1:5), npairdw=TRUE, title= paste0("Communaute", " ", j), size=45,  col="red", inches=.05, draw=F)
  
  
  data.db.std <- RGeostats::db.locate(data.db.std,"Year",NA)
  
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
      db.kriege <- RGeostats::db.create(data.db.std[sel])
      db.kriege <- RGeostats::db.locate(db.kriege,names=c("meanLon","meanLat"),"x")
      db.kriege <- RGeostats::db.locate(db.kriege,names=c("Abun"),"z")
      kres <- RGeostats::kriging(db.kriege, grid.db, model = vg.mod, neigh = nei1, uc=c("1"), mean=NA)
      
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

Bio.classif <- ggplot(Tabfaunefin)+
  geom_raster(aes(x=Longitude,y=Latitude,fill=Clust))+facet_wrap(~Community)+scale_fill_viridis_d()+theme_minimal()+
  geom_polygon(data=PolyCut[PolyCut$group==1.1,],aes(x=long, y=lat, group=group), col="black", fill=NA)+
  ggtitle("Classificiation of biological groups in selected area")


print(Bio.classif)