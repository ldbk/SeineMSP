library(ggplot2)
library(gridExtra)
library(raster)
library(grDevices)
library(cowplot)


# Cartes zones
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



# Ens chl, PP, O2

    # Series tempo mensuelles
{
  load("results/satellite/series full bay/monthly/chl_series.Rdata")
  load("results/satellite/series full bay/monthly/PP_series.Rdata")
  load("results/satellite/series full bay/monthly/O2_series.Rdata")
}

chlPPO2<- plot_grid(Chlseries3, PPseries3, O2series3,  labels=c("A", "B", "C"), ncol = 2, nrow = 2)
ggsave(plot= chlPPO2, filename="chlPPO2.jpeg", path="results/satellite/series full bay/monthly")


    # Series tempo mensuelles sur meme graphe

{
  load("results/satellite/series full bay/monthly/chlTab.Rdata")
  load("results/satellite/series full bay/monthly/PPTab.Rdata")
  load("results/satellite/series full bay/monthly/O2Tab.Rdata")
}

names(Tabchl6)[1]<- "Month"

# The data have a common independent variable (x)
Month<- as.integer(Tabchl6$Month)

# Generate 4 different sets of outputs
Chl<- Tabchl6$moybaie
PP<- TabPP6$moybaie
O2<- TabO26$moybaie

list<- list(Chl, PP, O2)

# Colors for y[[2]], y[[3]], y[[4]] points and axes
colors= c("red", "blue")

# Set the margins of the plot wider
par(oma = c(0, 2, 2, 3))

plot(Month, list[[1]], yaxt = "n", xlab = "Month", ylab = "")
lines(Month, list[[1]])

# We use the "pretty" function go generate nice axes
axis(at = pretty(list[[1]]), side = 2)

# The side for the axes.  The next one will go on 
# the left, the following two on the right side
sides <- list(2, 4, 4)

# The number of "lines" into the margin the axes will be
lines <- list(2, NA, 2)

for(i in 2:3) {
  par(new = TRUE)
  plot(Month, list[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "", main= "Chlorophyll (black), primary production (red), dissolved oxygen (blue)")
  axis(at = pretty(list[[i]]), side = sides[[i-1]], line = lines[[i-1]], 
       col = colors[i - 1])
  lines(Month, list[[i]], col = colors[i - 1])
}



    # Series tempo annuelles sur meme graphe

{
  load("results/satellite/series full bay/chlTab.Rdata")
  load("results/satellite/series full bay/PPTab.Rdata")
  load("results/satellite/series full bay/O2Tab.Rdata")
}

names(Tabchl4)[1]<- "Year"

lignes<- matrix(data=NA, nrow=2, ncol=2, dimnames = list(c("23", "22"), c("Year", "moybaie")))
lignes<- as.data.frame(lignes)
lignes[1, 1]<- 1997
lignes[2, 1]<- 2019
lignes$Year<- as.numeric(lignes$Year)
lignes$moybaie<- as.numeric(lignes$moybaie)

TabO24<- dplyr::union(TabO24, lignes)
TabPP4<- dplyr::union(TabPP4, lignes)


# The data have a common independent variable (x)
Year<- as.numeric(Tabchl4$Year)

# Generate 4 different sets of outputs
Chl<- Tabchl4$moybaie
PP<- TabPP4$moybaie
O2<- TabO24$moybaie

list<- list(Chl, PP, O2)

# Colors for y[[2]], y[[3]], y[[4]] points and axes
colors= c("red", "blue")

# Set the margins of the plot wider
par(oma = c(0, 2, 2, 3))

plot(Year, list[[1]], yaxt = "n", xlab = "Year", ylab = "")
lines(Year, list[[1]])

# We use the "pretty" function go generate nice axes
axis(at = pretty(list[[1]]), side = 2)

# The side for the axes.  The next one will go on 
# the left, the following two on the right side
sides <- list(2, 4, 4)

# The number of "lines" into the margin the axes will be
lines <- list(2, NA, 2)

for(i in 2:3) {
  par(new = TRUE)
  plot(Year, list[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "", main= "Chlorophyll (black), primary production (red), dissolved oxygen (blue)")
  axis(at = pretty(list[[i]]), side = sides[[i-1]], line = lines[[i-1]], 
       col = colors[i - 1])
  lines(Year, list[[i]], col = colors[i - 1])
}


    # Cartes zones (moyennes)
{
  load("results/satellite/means by zone/chl_raster.Rdata")
  load("results/satellite/means by zone/PP_raster.Rdata")
  load("results/satellite/means by zone/O2_raster.Rdata")
}

essai<- par(mfrow = c(2, 2))

Chl<- raster::plot(mChl2, col= terrain.colors(6), main="Chlorophyll", xlab="Longitude", ylab="Latitude")
PP<- raster::plot(mPP2, main="Primary production", xlab="Longitude", ylab="Latitude", col= terrain.colors(3))
O2<- raster::plot(mO22, col= terrain.colors(5), main="O2", xlab="Longitude", ylab="Latitude")








# Ens turbidity, detritus & particles


    # Series tempo mensuelles sur meme graphe

{
  load("results/satellite/series full bay/monthly/DetTab.Rdata")
  load("results/satellite/series full bay/monthly/PartTab.Rdata")
  load("results/satellite/series full bay/monthly/TurbTab.Rdata")
}


lignes<- matrix(data=NA, nrow=1, ncol=2, dimnames = list("12", c("Month", "moybaie")))
lignes<- as.data.frame(lignes)
lignes[1, 1]<- 12
lignes$Month<- as.numeric(lignes$Month)
lignes$moybaie<- as.numeric(lignes$moybaie)

TabDet6<- dplyr::union(TabDet6, lignes)
TabPart6<- dplyr::union(TabPart6, lignes)


# The data have a common independent variable (x)
Month<- as.integer(TabDet6$Month)

# Generate 4 different sets of outputs
Det<- TabDet6$moybaie
Part<- TabPart6$moybaie
Turb<- TabTurb6$moybaie

list<- list(Det, Part, Turb)

# Colors for y[[2]], y[[3]], y[[4]] points and axes
colors= c("red", "blue")

# Set the margins of the plot wider
par(oma = c(0, 2, 2, 3))

plot(Month, list[[1]], yaxt = "n", xlab = "Month", ylab = "")
lines(Month, list[[1]])

# We use the "pretty" function go generate nice axes
axis(at = pretty(list[[1]]), side = 2)

# The side for the axes.  The next one will go on 
# the left, the following two on the right side
sides <- list(2, 4, 4)

# The number of "lines" into the margin the axes will be
lines <- list(2, NA, 2)

for(i in 2:3) {
  par(new = TRUE)
  plot(Month, list[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "", main= "Detitrus (black), particles (red), turbidity (blue)")
  axis(at = pretty(list[[i]]), side = sides[[i-1]], line = lines[[i-1]], 
       col = colors[i - 1])
  lines(Month, list[[i]], col = colors[i - 1])
}



    # Series tempo annuelles sur meme graphe

{
  load("results/satellite/series full bay/DetTab.Rdata")
  load("results/satellite/series full bay/PartTab.Rdata")
  load("results/satellite/series full bay/TurbTab.Rdata")
}


# The data have a common independent variable (x)
Year<- as.numeric(TabDet4$Year)

# Generate 4 different sets of outputs
Det<- TabDet4$moybaie
Part<- TabPart4$moybaie
Turb<- TabTurb4$moybaie

list<- list(Det, Part, Turb)

# Colors for y[[2]], y[[3]], y[[4]] points and axes
colors= c("red", "blue")

# Set the margins of the plot wider
par(oma = c(0, 2, 2, 3))

plot(Year, list[[1]], yaxt = "n", xlab = "Year", ylab = "")
lines(Year, list[[1]])

# We use the "pretty" function go generate nice axes
axis(at = pretty(list[[1]]), side = 2)

# The side for the axes.  The next one will go on 
# the left, the following two on the right side
sides <- list(2, 4, 4)

# The number of "lines" into the margin the axes will be
lines <- list(2, NA, 2)

for(i in 2:3) {
  par(new = TRUE)
  plot(Year, list[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "", main= "Detritus (black), particles (red), turbidity (blue)")
  axis(at = pretty(list[[i]]), side = sides[[i-1]], line = lines[[i-1]], 
       col = colors[i - 1])
  lines(Year, list[[i]], col = colors[i - 1])
}



    # Cartes zones (moyennes)
{
  load("results/satellite/means by zone/Det_raster.Rdata")
  load("results/satellite/means by zone/Part_raster.Rdata")
  load("results/satellite/means by zone/Turb_raster.Rdata")
}

essai<- par(mfrow = c(2, 2))

Det<- raster::plot(mDet2, main="Detritus", xlab="Longitude", ylab="Latitude")
Part<- raster::plot(mPart2, main="Particles", xlab="Longitude", ylab="Latitude")
Turb<- raster::plot(mTurb2, main="Turbidity", xlab="Longitude", ylab="Latitude")








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









  