library(ggplot2)
library(gridExtra)
library(raster)
library(grDevices)


# Cartes
{
load("data/satellite/Particles/part_raster.Rdata")
load("data/satellite/chl/chl_raster.Rdata")
load("data/satellite/Primary production/PP_raster.Rdata")
load("data/satellite/Detritus/Det_raster.Rdata")
load("data/satellite/Turbidity/Turb_raster.Rdata")
load("data/satellite/sst/sst_raster.Rdata")
load("data/satellite/Salinity/Sal_raster.Rdata")
load("data/satellite/O2/O2_raster.Rdata")
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
O2<- raster::plot(mO2, main="O2", xlab="Longitude", ylab="Latitude")
}

#grid.arrange(Part, Chl, PP, Det, Turb, SST, Sal, ncol=2, nrow = 4)

par(mfrow = c(1, 1))





# Series temporelles (ens baie)
{
  load("results/satellite/series full bay/part_series.Rdata")
  load("results/satellite/series full bay/chl_series.Rdata")
  load("results/satellite/series full bay/PP_series.Rdata")
  load("results/satellite/series full bay/Det_series.Rdata")
  load("results/satellite/series full bay/Turb_series.Rdata")
  load("results/satellite/series full bay/sst_series.Rdata")
  load("results/satellite/series full bay/Sal_series.Rdata")
  load("results/satellite/series full bay/O2_series.Rdata")
}


grid.arrange(Partseries, Chlseries, PPseries, Detseries, Turbseries, SSTseries, Salseries, ncol=2, nrow = 4)

par(mfrow = c(1, 1))





# Series tempo (par zone)

{
  load("results/satellite/series by zone/Part_seriebyzone.Rdata")
  load("results/satellite/series by zone/chl_seriebyzone.Rdata")
  load("results/satellite/series by zone/PP_seriebyzone.Rdata")
  load("results/satellite/series by zone/Det_seriebyzone.Rdata")
  load("results/satellite/series by zone/Turb_seriebyzone.Rdata")
  load("results/satellite/series by zone/sst_seriebyzone.Rdata")
  load("results/satellite/series by zone/Sal_seriebyzone.Rdata")
  load("results/satellite/series by zone/O2_seriebyzone.Rdata")
}


grid.arrange(ggseriePart, ggseriechl, ggseriePP, ggserieDet, ncol=2, nrow = 2)

grid.arrange(ggserieTurb, ggseriesst, ggserieSal, ggserieO2, ncol=2, nrow = 2)




  