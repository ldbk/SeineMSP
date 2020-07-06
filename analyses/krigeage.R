#Libraries
library(dplyr)
library(ggplot2)
library(rgdal)
library(rgeos)
library(RGeostats)
library(viridis)

#Chargement données
load("data/Denstot.Rdata")

ttsel <- Denstot %>% dplyr::select(Cluster, Year, moyLong, moyLat, DensNb) %>% distinct() 
codif <- ttsel %>% dplyr::select( Year, moyLong, moyLat) %>% distinct() 
allstrat <- ttsel %>% tidyr::expand(Cluster,Year) 
pipo <- left_join(allstrat, codif) %>% left_join(ttsel) %>% mutate(DensNb = ifelse(is.finite(DensNb), DensNb, 0))
Denstot <- left_join(codif, pipo)


#Creation du polygon de krigeage#####


#1st Polygon
liste <- with(Denstot, chull(moyLong, moyLat))
hull <- Denstot[liste, c("moyLong", "moyLat")]
Poly <- Polygon(hull)
#Create SpatialPolygons objects
SpPoly<- SpatialPolygons(list(Polygons(list(Poly), "SpPoly")))
buff <- raster::buffer(SpPoly, 0.1)

#Cut object along coast
coast <- readOGR(dsn="data/Shp_FR/FRA_adm0.shp") #https://www.diva-gis.org/datadown
res <- gDifference(buff, coast)
PolyCut <- fortify(res)

save(PolyCut, file="data/Polycut.Rdata")

#Put polygon in good format for later use
toto <- PolyCut[PolyCut$piece==1,]
db.poly <- polygon.create(toto[,c(1,2)])


ggplot(Denstot)+
  geom_point(aes(x=moyLong,y=moyLat))+
  geom_polygon(data=toto, aes(x=long,y=lat, group=group),fill=NA,col="black")
#####



#Mise en forme data.frame
CGFS <- Denstot %>% distinct()
CGFS <- na.omit(CGFS)


#Test vario/krigeage sur cluster 2#####


#Selection
CGFS$logDens <- log10(CGFS$DensNb+ min(CGFS$DensNb[CGFS$DensNb!=0])) 
db.CGFS <- db.create(CGFS[CGFS$Cluster==2,]) #Choix du cluster
db.CGFS <- db.locate(db.CGFS,names=c("moyLong","moyLat"),"x") #Indications des colonnes coordonnées
db.CGFS <- db.locate(db.CGFS,names=c("logDens"),"z") #Indication de la colonne densité


#Creation du variogramme pour l'année 1996
vg.data <- vario.calc(db.sel(db.CGFS,Year==1996), lag=0.05, nlag=8)  
plot(vg.data,npairdw=T,inches=0.1,las=1,xlab="Distance")

#Boucle des varios standardisés (standardisation avec flag.norm=T)
for(i in unique(db.CGFS[,"Year"])){  
  vg.data <- vario.calc(db.sel(db.CGFS,Year==i), lag=0.05, nlag=8)  
  plot(vg.data,npairdw=T,inches=0.1,col=rgb(0,0,0,0.25),add=!(i==1996),las=1,xlab="Distance")}


#Crétaion de la colonne Densité Standardisée par an
db.CGFS.std <- db.CGFS
for(i in unique(db.CGFS[,"Year"])){  
  sel <- db.CGFS.std[,"Year"]==i   
  sd.year <- sqrt(mean(db.CGFS.std[,"DensNb"][sel]^2) -              
                    mean(db.CGFS.std[,"DensNb"][sel])^2)  
  db.CGFS.std[,"DensNb"][sel] <- db.CGFS.std[,"DensNb"][sel]/sd.year 
}

db.CGFS.std <- db.locate(db.CGFS.std,"Year","code")

#Nouvelle boucle des varios, doit être similaire à la première
for(i in unique(db.CGFS[,"Year"])){  
  vg.data <- vario.calc(db.sel(db.CGFS.std,Year==i), lag=0.05, nlag=8)    
  plot(vg.data,npairdw=T,inches=0.1,col=rgb(0,0,0,0.25),add=!(i==1996),         
       las=1,xlab="Distance (km)") }

#Vario moyen pour toutes les années du cluster choisi
vg.data.std <- vario.calc(db.CGFS.std, lag=0.05, nlag=8, opt.code=1, tolcode=0) 
plot(vg.data.std, npairdw=T, inches=0.1, las=1, add=T, col=2, lwd=2, main= "Variogramme moyen de la communauté 2 (démersale)")


#Ajustement d'un modèle à travers le variogramme moyen
vario1 <- vg.data.std
vg.mod <- model.auto(vario=vario1,struct=c(1:5),npairdw=TRUE,title="",inches=.05, main= "Modèle de la communauté 2 (démersale)", xlab="Distance (km)")



#Krigeage pour le cluster choisi pour une année

#Grille de krigeage
x0 <- -1.4
y0 <- 49.25
dx <- 0.05
dy <- 0.05
nx <- 32
ny <- 13
db.grid <- db.create(flag.grid=T,x0=c(x0,y0),dx=c(dx,dy),nx=c(nx,ny))
db.grid <- db.polygon(db.grid,db.poly)

#Moving or unique neighbouhood?
nei1 <- neigh.create(ndim=2,type=0) #unique
nei2 <- neigh.create(ndim=2,type=2,nmini=2,nmaxi=12,radius=0.45) # moving

db.kriege <- db.sel(db.CGFS.std,Year==1996) #Selection de l'année 1996
kres <- kriging(db.kriege, db.grid, model = vg.mod, neigh = nei2, uc=c("1"), mean=NA) #Krigeage ordianire, voisinnage unique

# Plot kriged estimates: K.estim
plot(kres,name.image=5,title="Valeurs de krigeage estimées",col=topo.colors(20),xlab="Longitude",ylab="Latitude",xlim=c(-1.5,0.5),pos.legend=1)
plot(db.sel(db.CGFS.std, Year==1996),pch=18,add=T,col="red",inches=1.5)
plot(res,add=T)


#####


#Boucle vario/krigeage sur tout les clusters#####

Longitude <- numeric()
Latitude <- numeric()
Prediction.std <- numeric()
Variance.std <- numeric()
Year <- numeric()
Cluster <- character()
Prediction <- numeric()
Variance <- numeric()


#Grille de krigeage
x0 <- -1.4
y0 <- 49.25
dx <- 0.05
dy <- 0.05
nx <- 32
ny <- 13
db.grid <- db.create(flag.grid=T,x0=c(x0,y0),dx=c(dx,dy),nx=c(nx,ny))
db.grid <- db.polygon(db.grid,db.poly)

#Moving or unique neighbouhood?
nei1 <- neigh.create(ndim=2,type=0) #unique
nei2 <- neigh.create(ndim=2,type=2,nmini=2,nmaxi=8,radius=1) # moving


for (j in unique(data.frame(CGFS)[,"Cluster"])){
  
  db.CGFS <- db.create(CGFS[CGFS$Cluster==j,]) #Choix du cluster
  db.CGFS <- db.locate(db.CGFS,names=c("moyLong","moyLat"),"x")
  db.CGFS <- db.locate(db.CGFS,names=c("DensNb"),"z")
  
  #Standartisation par an
  db.CGFS.std <- db.CGFS
  for(k in unique(db.CGFS[,"Year"])){  
    sel <- db.CGFS.std[,"Year"]==k  
    sd.year <- sqrt(mean(db.CGFS.std[,"DensNb"][sel]^2) -              
                      mean(db.CGFS.std[,"DensNb"][sel])^2) 
    db.CGFS.std[,"DensNb"][sel] <- db.CGFS.std[,"DensNb"][sel]/sd.year 
  }
  
  db.CGFS.std <- db.locate(db.CGFS.std,"Year","code")
  db.CGFS.std@items$DensNb <- ifelse(is.finite(db.CGFS.std@items$DensNb), db.CGFS.std@items$DensNb, 0)
  
  #Vario moyen
  vg.data.std <- vario.calc(db.CGFS.std, lag=0.05, nlag=8,opt.code=1,tolcode=0) 
  #plot(vg.data.std,npairdw=T,inches=0.1,las=1,add=T,col=2,lwd=2)
  vario1 <- vg.data.std
  vg.mod <- model.auto(vario=vario1,struct=c(1:5),npairdw=TRUE,title="",inches=.05)
  
  
  db.CGFS.std <- db.locate(db.CGFS.std,"Year",NA)
  
  for (i in sort(unique(db.CGFS[,"Year"]))){
    
    sel <- db.CGFS.std[,"Year"]== i
    
    if (all(sel==F))
    {Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
    Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
    Prediction <- c(Prediction,rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Variance <- c(Variance, rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))}else{
      
      
      #Ordinary kriging, unique neighbourhood
      sel <- db.CGFS.std[,"Year"]== i
      db.kriege <- db.create(db.CGFS.std[sel])
      db.kriege <- db.locate(db.kriege,names=c("moyLong","moyLat"),"x")
      db.kriege <- db.locate(db.kriege,names=c("DensNb"),"z")
      kres <- kriging(db.kriege, db.grid, model = vg.mod, neigh = nei1, uc=c("1"), mean=NA)
      
      
      
      # Plot kriged estimates: K.estim
      #plot(kres,name.image=5,title="K.estim",col=topo.colors(20),xlab="",
      #ylab="",xlim=c(-10,12),pos.legend=5)
      #plot(db.sel(dens.data, annee==i & Nv_Rubbin==j),pch=18,add=T,col="black",inches=1.5)
      #plot(poly.data,add=T)
      # Plot kriging errors: K.std
      #plot(kres,name.image=6,title="K.std",col=rev(gray((0:100)/100)),
      #xlab="",ylab="", xlim=c(-10,12),pos.legend=5)
      #plot(db.sel(plie.data, annee==2010),pch=18,add=T,col="black",inches=1.5)
      #plot(poly.data,add=T)
      
      Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
      Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
      Prediction <- c(Prediction, kres@items$Kriging.DensNb.estim[kres@items$Polygon==TRUE])
      Variance <- c(Variance, kres@items$Kriging.DensNb.stdev[kres@items$Polygon==TRUE])
      Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
      Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))
    }
    
  }
  
}
Kriege.dens<- data.frame(Longitude=Longitude, Latitude=Latitude, Prediction=Prediction, Variance=Variance, Year=Year, Cluster=factor(Cluster))


#Plot du krigeage sur toutes les années pour un cluster
ggplot()+
  geom_raster(data=Kriege.dens[Kriege.dens$Cluster==2 & Kriege.dens$Year==1996,],aes(x= Longitude, y= Latitude, fill = Prediction)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + ggtitle(paste("Estimation des densités du Cluster 1 d'espèces par \n krigeage ordinaire et voisinnage fixe", sep=" ")) + 
  theme_minimal() + geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group),fill=NA, col="black")

save(Kriege.dens, file="data/krigeage.RData")







#Boucle vario/krigeage sur tout les clusters#####

Longitude <- numeric()
Latitude <- numeric()
Prediction.std <- numeric()
Variance.std <- numeric()
Year <- numeric()
Cluster <- character()
Prediction <- numeric()
Variance <- numeric()


#Grille de krigeage
x0 <- -1.4
y0 <- 49.25
dx <- 0.05
dy <- 0.05
nx <- 32
ny <- 13
db.grid <- db.create(flag.grid=T,x0=c(x0,y0),dx=c(dx,dy),nx=c(nx,ny))
db.grid <- db.polygon(db.grid,db.poly)

#Moving or unique neighbouhood?
nei1 <- neigh.create(ndim=2,type=0) #unique
nei2 <- neigh.create(ndim=2,type=2,nmini=2,nmaxi=8,radius=1) # moving


for (j in unique(data.frame(CGFS)[,"Cluster"])){
  
  CGFS$logDens <- log10(CGFS$DensNb+ min(CGFS$DensNb[CGFS$DensNb!=0])) 
  db.CGFS <- db.create(CGFS[CGFS$Cluster==j,]) #Choix du cluster
  db.CGFS <- db.locate(db.CGFS,names=c("moyLong","moyLat"),"x")
  db.CGFS <- db.locate(db.CGFS,names=c("logDens"),"z")
  
  #Standartisation par an
  db.CGFS.std <- db.CGFS
  db.CGFS.std <- db.locate(db.CGFS.std,"Year","code")
 
  #Vario moyen
  vg.data.std <- vario.calc(db.CGFS.std, lag=0.05, nlag=8,opt.code=1,tolcode=0) 
  #plot(vg.data.std,npairdw=T,inches=0.1,las=1,add=T,col=2,lwd=2)
  vario1 <- vg.data.std
  vg.mod <- model.auto(vario=vario1,struct=c(1:5),npairdw=TRUE,title="",inches=.05)
  
  
  db.CGFS.std <- db.locate(db.CGFS.std,"Year",NA)
  
  for (i in sort(unique(db.CGFS[,"Year"]))){
    
    sel <- db.CGFS.std[,"Year"]== i
    
    if (all(sel==F))
    {Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
    Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
    Prediction <- c(Prediction,rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Variance <- c(Variance, rep(NA, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
    Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))}else{
      
      
      #Ordinary kriging, unique neighbourhood
      sel <- db.CGFS.std[,"Year"]== i
      db.kriege <- db.create(db.CGFS.std[sel])
      db.kriege <- db.locate(db.kriege,names=c("moyLong","moyLat"),"x")
      db.kriege <- db.locate(db.kriege,names=c("logDens"),"z")
      kres <- kriging(db.kriege, db.grid, model = vg.mod, neigh = nei1, uc=c("1"), mean=NA)
      
      
      
      # Plot kriged estimates: K.estim
      #plot(kres,name.image=5,title="K.estim",col=topo.colors(20),xlab="",
      #ylab="",xlim=c(-10,12),pos.legend=5)
      #plot(db.sel(dens.data, annee==i & Nv_Rubbin==j),pch=18,add=T,col="black",inches=1.5)
      #plot(poly.data,add=T)
      # Plot kriging errors: K.std
      #plot(kres,name.image=6,title="K.std",col=rev(gray((0:100)/100)),
      #xlab="",ylab="", xlim=c(-10,12),pos.legend=5)
      #plot(db.sel(plie.data, annee==2010),pch=18,add=T,col="black",inches=1.5)
      #plot(poly.data,add=T)
      
      Longitude <- c(Longitude, kres@items$x1[kres@items$Polygon==TRUE])
      Latitude <- c(Latitude, kres@items$x2[kres@items$Polygon==TRUE])
      Prediction <- c(Prediction, kres@items$Kriging.logDens.estim[kres@items$Polygon==TRUE])
      Variance <- c(Variance, kres@items$Kriging.logDens.stdev[kres@items$Polygon==TRUE])
      Year <- c(Year, rep(i, length(kres@items$x1[kres@items$Polygon==TRUE])))
      Cluster <- c(Cluster, rep(j, length(kres@items$x1[kres@items$Polygon==TRUE])))
    }
    
  }
  
}
Kriege.logdens<- data.frame(Longitude=Longitude, Latitude=Latitude, Prediction=Prediction, Variance=Variance, Year=Year, Cluster=factor(Cluster))


#Plot du krigeage sur toutes les années pour un cluster
y <- 9
ggplot()+
  geom_raster(data=Kriege.logdens[Kriege.logdens$Cluster==y,],aes(x= Longitude, y= Latitude, fill = Prediction)) +
  scale_fill_gradientn(colours = terrain.colors(20)) + 
  facet_wrap(.~Year) + ggtitle(paste0("Estimation des densités du Cluster ",y," d'espèces par \n krigeage ordinaire et voisinnage fixe", sep=" ")) + 
  theme_minimal() + geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group),fill=NA, col="black")

save(Kriege.logdens, file="data/krigeage.RData")


