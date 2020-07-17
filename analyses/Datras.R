library(assertthat)
library(tidyr)
library(dplyr)
library(ggplot2)
library(raster)
library(maps)
library(viridis)

HH<- read.csv(file="data/ICES/Datras HH original.csv", sep=",")
HL<- read.csv(file="data/ICES/Datras HL original.csv", sep=",")
Wing<- read.csv(file= "data/ICES/OuvertureCgfs2015_2019 original.csv", sep=";")


# Nettoyage tableaux
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

#tabessai<- HL %>% dplyr::select(ValidAphiaID, ScientificName_WoRMS, SpecCode)
HL$ValidAphiaID[is.na(HL$ValidAphiaID)]<-	HL$SpecCode[is.na(HL$ValidAphiaID)]
HL<- HL[!is.na(HL$ValidAphiaID),]
HL<- HL[, -4]

Wing<- Wing[, -c(1, 2, 4, 5, 6)]
names(Wing)[1]<- "StNo"
names(Wing)[2]<- "WingSpread"


# Jointures
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


# Calcul
J2$Distance<- J2$Distance/1000 # Converti m en km 
J2$WingSpread<- J2$WingSpread/1000 # Converti m en km

J2<- J2 %>%
  mutate(Nombre= TotalNo*HaulDur/60) # Converti TotalNo par trait de chalut


J2<- J2 %>%
  mutate(Superficie= Distance*WingSpread) %>%
  mutate(moyLat= (ShootLat+HaulLat)/2) %>%
  mutate(moyLong= (ShootLong+HaulLong)/2)

J2<- J2 %>% dplyr::select(Year, ScientificName_WoRMS, Nombre, Superficie, moyLat, moyLong)


# Final
write.csv(J2, file= "data/J2Datras.csv")







