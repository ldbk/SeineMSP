setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(assertthat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(maps)
library(viridis)

# SIH
Captures<- read.csv("SIH Captures original.csv", sep=";")
Trait<- read.csv("SIH Trait original.csv", sep=";")
Taxons<- read.csv("Tab correspondance des taxons original.csv", sep=";")


# SIH Nettoyage tableaux
Captures<- Captures[, -1]
names(Captures)[1]<- "Year"
names(Captures)[2]<- "HaulNo"

Trait<- Trait[, -c(1, 4, 5, 9)]
names(Trait)[1]<- "Year"
names(Trait)[2]<- "HaulNo"

Taxons<- Taxons[, -c(1, 2, 5, 6, 7, 8, 9, 10, 11, 12)]
names(Taxons)[1]<- "Espece"
names(Taxons)[2]<- "SpecCode"
Taxons<- Taxons[!is.na(Taxons$SpecCode),]


# SIH Jointures 
J1SIH<- Captures %>% left_join(Trait, by= c("Year", "HaulNo"))
J1SIH<- J1SIH %>% 
  filter(-1.5<=Long & Long <= 0.5 & 49<=Lat & Lat<=49.8)
plot(Trait$Long, Trait$Lat)

J2SIH<- J1SIH %>% left_join(Taxons, by="Espece")
J2SIH<- J2SIH[, -3] 

load("SIH vérifié.RData")
SIHvérifié<- sp
SIHvérifié<- SIHvérifié[,-c(2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28)]
J3SIH<- J2SIH %>% left_join(SIHvérifié, by=c("SpecCode"))
J3SIH<- J3SIH[!is.na(J3SIH$SpecCode),]

J3SIH<- J3SIH[, -c(5, 9)]
J3SIH<- unique(J3SIH)


# SIH Calcul densit?s
J3SIH<- J3SIH %>%
  mutate(DensityWgt= Poids/SurfaceBalayee) %>%
  mutate(DensityNb= Nombre/SurfaceBalayee)

save(J3SIH, file= "C:/Users/jrivet/Documents/Stage M2/Donn?es/CGFS/J3SIH.RData")


# SIH Carto des traits de chalut

    #a quick and dirty map
plot(J3SIH$Long, J3SIH$Lat, asp=1)
map("world",add=T)

rx1<- range(J3SIH$Long)
ry1<- range(J3SIH$Lat)
    
  # Carte des captures (en masse, toutes esp?ces confondues), par ann?e
ggplot(J3SIH)+
  geom_point(aes(x= Long, y= Lat, size= Poids, col=Poids))+
  facet_wrap(~ Year)+
  scale_color_distiller(palette='Spectral', name="Poids des captures en kg (esp?ces confondues)", trans="log10")+
  borders("world",xlim=rx1, ylim=ry1, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx1, ylim=ry1)+
  theme_bw()+
  ggtitle("Masses des captures") +
  xlab("Longitude") +
  ylab("Latitude")


  # Carte des captures (en masse, toutes esp?ces confondues), sur l'ensemble 1988-2019
ggplot(J3SIH)+
  geom_point(aes(x=Long, y=Lat, size=Poids, col=Poids))+
  scale_color_distiller(palette='Spectral', name="Poids des captures en kg (esp?ces confondues)", trans="log10")+
  borders("world",xlim=rx1, ylim=ry1, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx1, ylim=ry1)+
  theme_bw()+
  ggtitle("Masses moyennes des captures 1988-2019") +
  xlab("Longitude") +
  ylab("Latitude")


  # Essai carte par ann?e et par esp?ce
ggplot(J3SIH)+
  geom_point(aes(x=Long, y=Lat, size=Poids, col=Poids))+
  scale_color_distiller(palette='Spectral', name="Poids des captures en kg", trans="log10")+
  borders("world",xlim=rx1, ylim=ry1, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx1, ylim=ry1)+
  facet_wrap(valid_AphiaID~ Year, drop=FALSE)+
  theme_bw()+
  ggtitle("CGFS position")+
  xlab("Longitude")+
  ylab("Latitude")







# DATRAS
HH<- read.csv(file="Datras HH original.csv", sep=",")
HL<- read.csv(file="Datras HL original.csv", sep=",")
Wing<- read.csv(file= "OuvertureCgfs2015_2019 original.csv", sep=";")


# DATRAS nettoyage tableaux
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

HH<- HH[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 15, 16, 18, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 34, 35, 36, 37, 38, 39, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54,55, 56, 57, 58, 59, 60, 61)] # RecordType, Survey, Quarter, Country, Ship, Gear, SweepLngt, GearExp (NA), DoorType, Mois, Jour, TimeShot, Stratum (NA), DayNight, Depth, HaulVal, HydroStNo, StdSpecRecCode, BycSpecRecCode, DataType, NetOpening, Rigging (Na), Tickler (NA), WarpLgnt, WarpDia, WarpDen (NA), DoorSurface, DoorWgt, DoorSpread, Buoyancy, KiteDim (NA), WgtGroundRope, TowDir, GroundSpeed, SpeedWater, SurCurDir (NA), SurCurSpeed (NA),BotCurDir(NA), BotCurSpeed (NA), WindDir, WindSpeed, SwellDir(NA), SwellHeight(NA), SurTemp, BotTemp, SurSal, BotSal, ThermoCline (NA), ThClineDepth (NA), DateofCalculation. 
HL<- HL[,-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 15, 16, 18, 19, 20, 21, 23, 24, 25, 28)] # RecordType, Survey, Quarter (all 4), Country, Ship (GWD ou THA2), Gear, SweepLngt, GearExp (NA), DoorType (P ou R), SpecCodeType (all W), SpecVal, Sex, CatIdentifier, NoMeas, SubFactor, SubWgt (NA), LngtCode, LngtClass, HLNoAtLngt, Date of calculation.
HL<- unique(HL)

tabessai<- HL %>% dplyr::select(ValidAphiaID, ScientificName_WoRMS, SpecCode)
HL$ValidAphiaID[is.na(HL$ValidAphiaID)]<-	HL$SpecCode[is.na(HL$ValidAphiaID)]
HL<- HL[!is.na(HL$ValidAphiaID),]
HL<- HL[, -4]

Wing<- Wing[, -c(1, 2, 4, 5, 6)]
names(Wing)[1]<- "StNo"
names(Wing)[2]<- "WingSpread"


# DATRAS Jointures
J1<- HH %>% left_join(Wing, by= c("StNo"))
J1$WingSpread.x[is.na(J1$WingSpread.x)]<-	J1$WingSpread.y[is.na(J1$WingSpread.x)]
J1<- J1[, -11]
names(J1)[10]<- "WingSpread"
J1$WingSpread[is.na(J1$WingSpread)]<- 10

J2<- HL %>% left_join(J1, by= c("StNo", "HaulNo", "Year"))
J2<- J2[, -1]
J2<- J2 %>% 
  filter(-1.5<=ShootLong & ShootLong <= 0.5 & 49<=ShootLat & ShootLat<=49.8)%>%
  filter(-1.5<=HaulLong & HaulLong <= 0.5 & 49<=HaulLat & HaulLat<=49.8)


# DATRAS especes
J2esp<- J2 %>% dplyr::select(ScientificName_WoRMS)
J2esp<- unique(J2esp)
write.csv(J2esp, file="C:/Users/jrivet/Documents/Stage M2/Data/CGFS/Tabespeces.csv")


# DATRAS calcul densites
J2$CatCatchWgt<- J2$CatCatchWgt/1000 # Converti g en kg
J2$Distance<- J2$Distance/1000 # Converti m en km 
J2$WingSpread<- J2$WingSpread/1000 # Converti m en km

J2<- J2 %>%
  mutate(Poids= CatCatchWgt*HaulDur/60) %>% # Converti CatCatchWgt par trait de chalut
  mutate(Nombre= TotalNo*HaulDur/60) # Converti TotalNo par trait de chalut


J2<- J2 %>%
  mutate(Superficie= Distance*WingSpread) %>%
  mutate(moyLat= (ShootLat+HaulLat)/2) %>%
  mutate(moyLong= (ShootLong+HaulLong)/2) %>%
  mutate(DensityWgt= Poids/Superficie) %>%
  mutate(DensityNb= Nombre/Superficie)

J2<- J2[, -c(3, 4, 7, 8, 9, 10, 11, 12, 13)]


save(J2, file= "C:/Users/jrivet/Documents/Stage M2/Data/CGFS/J2Datras.RData")







# Richesse sp?cifique N par station
J2N<- J2 %>% dplyr::select(Year, moyLong, moyLat, ScientificName_WoRMS) %>%
  group_by(Year, moyLong, moyLat) %>%
  summarize(N= length(ScientificName_WoRMS))

rx<-range(J2$moyLong, na.rm=T)
ry<-range(J2$moyLat, na.rm=T)

ggplot(J2N)+
  geom_point(aes(x= moyLong, y= moyLat, size= N, col= N))+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  scale_color_viridis()+
  theme_minimal()+
  ggtitle("Richesse sp?cifique")+
  xlab("Longitude")+
  ylab("Latitude")


# Cartes des traits de chalut

    #a quick and dirty map
plot(J2$moyLong, J2$moyLat, asp=1)
map("world",add=T)


    #a less quick and dirty map
ggplot(J2)+ 
  aes(x=moyLong, y=moyLat)+
  geom_point()+
  borders("world", xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=x, ylim=y)+
  facet_wrap(~ Year, drop=FALSE)+
  theme_bw()+
  ggtitle("Position des traits CGFS")+
  xlab("Longitude")+
  ylab("Latitude")



# Cartes de la masse des captures
J2g<- J2 %>%
  group_by(HaulNo, Year, Poids, ValidAphiaID, ScientificName_WoRMS, moyLong, moyLat) %>%
  summarize(TotalPoids= sum(Poids, na.rm=T)) %>%
  ungroup()


      # Carte des captures (en masse, toutes esp?ces confondues), par ann?e
ggplot(J2g)+
  geom_point(aes(x= moyLong, y= moyLat, size= TotalPoids, col=TotalPoids))+
  facet_wrap(~ Year)+
  scale_color_distiller(palette='Spectral', name="Poids des captures en kg (esp?ces confondues)", trans="log10")+
  borders("world",xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  theme_bw()+
  ggtitle("Masses des captures") +
  xlab("Longitude") +
  ylab("Latitude")


# Carte des captures (en masse, toutes esp?ces confondues), sur l'ensemble 1988-2019
ggplot(J2g)+
  geom_point(aes(x= moyLong, y= moyLat, size= TotalPoids, col=TotalPoids))+
  scale_color_distiller(palette='Spectral', name="Poids des captures en kg (esp?ces confondues)", trans="log10")+
  borders("world",xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  theme_bw()+
  ggtitle("Masses moyennes des captures 1988-2019") +
  xlab("Longitude") +
  ylab("Latitude")



# Essai carte par ann?e et par esp?ce
ggplot(J2g)+
  geom_point(aes(x= moyLong, y= moyLat, size= TotalPoids, col= TotalPoids))+
  scale_color_distiller(palette='Spectral', name="total weight", trans="log10")+
  borders("world",xlim=rx, ylim=ry, fill="grey", colour=NA, alpha=1)+
  coord_sf(xlim=rx, ylim=ry)+
  facet_wrap(ScientificName_WoRMS~Year, drop=FALSE)+
  theme_bw()+
  ggtitle("CGFS position")+
  xlab("Longitude")+
  ylab("Latitude")









