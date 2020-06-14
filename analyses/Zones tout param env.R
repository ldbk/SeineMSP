library(ggplot2)
library(gridExtra)
library(raster)


# Cartes
{
load("data/satellite/Particles/part_raster.Rdata")
load("data/satellite/chl/chl_raster.Rdata")
load("data/satellite/Primary production/PP_raster.Rdata")
load("data/satellite/Detritus/Det_raster.Rdata")
load("data/satellite/Turbidity/Turb_raster.Rdata")
load("data/satellite/sst/sst_raster.Rdata")
load("data/satellite/Salinity/Sal_raster.Rdata")
}

par(mfrow = c(2, 4))

{
Part<- raster::plot(mPart, main="Particles", xlab="Longitude", ylab="Latitude")
Chl<- raster::plot(mChl, main="Chl", xlab="Longitude", ylab="Latitude")
PP<- raster::plot(mPP, main="Primary production", xlab="Longitude", ylab="Latitude")
Det<- raster::plot(mDet, main="Detritus", xlab="Longitude", ylab="Latitude")
Turb<- raster::plot(mTurb, main="Turbidity", xlab="Longitude", ylab="Latitude")
SST<- raster::plot(mSST, main="SST", xlab="Longitude", ylab="Latitude")
Sal<- raster::plot(mSal, main="Salinity", xlab="Longitude", ylab="Latitude")
}

#grid.arrange(Part, Chl, PP, Det, Turb, SST, Sal, ncol=2, nrow = 4)











# Series temporelles
{
  load("data/satellite/Particles/part_serie.Rdata")
  load("data/satellite/chl/chl_serie.Rdata")
  load("data/satellite/Primary production/PP_serie.Rdata")
  load("data/satellite/Detritus/Det_serie.Rdata")
  load("data/satellite/Turbidity/Turb_serie.Rdata")
  load("data/satellite/sst/sst_serie.Rdata")
  load("data/satellite/Salinity/Sal_serie.Rdata")
}

PartS<- ggplot(TabPart4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Particles 1997-2017")+
  xlab("Year")+
  ylab("Particles")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

ChlS<- ggplot(Tabchl4)+
  geom_line(aes(x= year, y= moybaie))+
  ggtitle("Chlorophyll 1997-2017")+
  xlab("Year")+
  ylab("µg/L")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

PPS<- ggplot(TabPP4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("PP 1998-2018")+
  xlab("Year")+
  ylab("mg C/m3/j")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6)) 

DetS<- ggplot(TabDet4)+
  geom_line(aes(x=Year, y= moybaie))+
  ggtitle("Detritus 1997-2017")+
  xlab("Year")+
  ylab("Detritus")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6)) 

TurbS<- ggplot(TabTurb4)+
  geom_line(aes(x=Year, y=moybaie))+
  ggtitle("Turbidity 1997-2017")+
  xlab("Year")+
  ylab("Turbidity")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

sstS<- ggplot(Tabsst4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("SST 1981-2018")+
  xlab("Year")+
  ylab("°C")+
  theme_minimal()

SalS<- ggplot(TabSal4)+
  geom_line(aes(x= Year, y= moybaie))+
  ggtitle("Salinity 1992-2018")+
  xlab("Year")+
  ylab("Salinity")+
  theme_minimal()+
  scale_fill_gradientn(colours = terrain.colors(6))

grid.arrange(PartS, ChlS, PPS, DetS, TurbS, sstS, SalS, ncol=2, nrow = 4)






  