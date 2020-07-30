library(ggplot2)
library(gridExtra)
library(raster)
library(grDevices)
library(cowplot)
library(RColorBrewer)
library(dplyr)


# Cartes 9 zonations (zones)
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

par(mfrow = c(1, 1))



# Cartes 9 zonations (moyennes)
{
  load("results/satellite/means by zone/chl_raster.Rdata")
  load("results/satellite/means by zone/PP_raster.Rdata")
  load("results/satellite/means by zone/O2_raster.Rdata")
  load("results/satellite/means by zone/Det_raster.Rdata")
  load("results/satellite/means by zone/Part_raster.Rdata")
  load("results/satellite/means by zone/Turb_raster.Rdata")
  load("results/satellite/means by zone/SST_raster.Rdata")
  load("results/satellite/means by zone/Sal_raster.Rdata")
}

par(mfrow = c(4, 2))

{
  Chl<- raster::plot(mChl2, main="Chlorophyll", col=brewer.pal(n = 3, name = "PuRd"))
  Part<- raster::plot(mPart2, main="Particles", col=c("#FFFFCC", "#CC6633"))
  PP<- raster::plot(mPP2, main="Primary production", col=c("#CCFFCC", "#99CC99"))
  Det<- raster::plot(mDet2, main="Detritus", col=c("#99CCCC", "#336666"))
  Turb<- raster::plot(mTurb2, main="Turbidity", col=brewer.pal(n = 3, name = "PuBu"))
  SST<- raster::plot(mSST2, main="SST", col= c("#FFCCCC", "#FF6666"))
  Sal<- raster::plot(mSal2, main="Salinity", xlab="Longitude", ylab="Latitude", col= brewer.pal(n = 3, name = "Greys"))
  O2<- raster::plot(mO22, main="O2", col=brewer.pal(n = 3, name = "Purples"))
}

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
TabO24 <- TabO24[order(TabO24$Year),]
TabPP4 <- TabPP4[order(TabPP4$Year),]


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

par(mfrow = c(2, 2))

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

par(mfrow = c(2, 2))

Det<- raster::plot(mDet2, main="Detritus", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "YlGn"))
Part<- raster::plot(mPart2, main="Particles", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 4, name = "YlOrBr"))
Turb<- raster::plot(mTurb2, main="Turbidity", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "PuBu"))





# Ens Salinity & sst


    # Series tempo annuelles sur meme graphe

{
  load("results/satellite/series full bay/SalTab.Rdata")
  load("results/satellite/series full bay/sstTab.Rdata")
}

lignes<- matrix(data=NA, nrow=10, ncol=2, dimnames = list(c("28", "29", "30", "31", "31", "32", "33", "34", "35", "36"), c("Year", "moybaie")))
lignes<- as.data.frame(lignes)
lignes[1, 1]<- 1982
lignes[2, 1]<- 1983
lignes[3, 1]<- 1984
lignes[4, 1]<- 1985
lignes[5, 1]<- 1986
lignes[6, 1]<- 1987
lignes[7, 1]<- 1988
lignes[8, 1]<- 1989
lignes[9, 1]<- 1990
lignes[10, 1]<- 1991

lignes$Year<- as.numeric(lignes$Year)
lignes$moybaie<- as.numeric(lignes$moybaie)

TabSal4<- dplyr::union(TabSal4, lignes)
TabSal4 <- TabSal4[order(TabSal4$Year),]

# The data have a common independent variable (x)
Year<- as.numeric(Tabsst4$Year)

# Generate 4 different sets of outputs
Sal<- TabSal4$moybaie
sst<- Tabsst4$moybaie

list<- list(Sal, sst)

# Colors for y[[2]], y[[3]], y[[4]] points and axes
colors= c("blue")

# Set the margins of the plot wider
par(oma = c(0, 2, 2, 3))

plot(Year, list[[1]], yaxt = "n", xlab = "Year", ylab = "")
lines(Year, list[[1]])

# We use the "pretty" function go generate nice axes
axis(at = pretty(list[[1]]), side = 4)

# The side for the axes.  The next one will go on 
# the left, the following two on the right side
sides <- list(2, 4, 4)

# The number of "lines" into the margin the axes will be
lines <- list(2, NA, 2)

for(i in 2:2) {
  par(new = TRUE)
  plot(Year, list[[i]], axes = FALSE, col = colors[i - 1], xlab = "", ylab = "", main= "Temperature (blue), salinity (black)")
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

par(mfrow = c(2, 2))

Det<- raster::plot(mDet2, main="Detritus", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "YlGn"))
Part<- raster::plot(mPart2, main="Particles", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 4, name = "YlOrBr"))
Turb<- raster::plot(mTurb2, main="Turbidity", xlab="Longitude", ylab="Latitude", col=brewer.pal(n = 3, name = "PuBu"))















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









# Découpe des rasters avec polycut

load("data/PolyCut.Rdata")
load("data/res.Rdata")

load("data/satellite/chl/chl_raster.Rdata")
#col=brewer.pal(n = 3, name = "YlGnBu")
load("data/satellite/Detritus/Det_raster.Rdata")
#col=c("#99CCCC", "#336666")
load("data/satellite/O2/O2_raster.Rdata")
#col=brewer.pal(n = 3, name = "Purples")
load("data/satellite/Particles/part_raster.Rdata")
#col=c("#FFFFCC", "#CC6633")
load("data/satellite/Primary production/PP_raster.Rdata")
#col= c("#CCFFCC", "#99CC99")
load("data/satellite/Salinity/Sal_raster.Rdata")
#col= brewer.pal(n = 3, name = "Greys")
load("data/satellite/sst/sst_raster.Rdata")
#col= c("#FFCCCC", "#FF6666")
load("data/satellite/Turbidity/Turb_raster.Rdata")
#col=brewer.pal(n = 3, name = "PuBu")

{
rasterchl2<- disaggregate(mChl, 10)
rasterDet2<- disaggregate(mDet, 10)
rasterO22<- disaggregate(mO2, 10)
rasterPart2<- disaggregate(mPart, 10)
rasterPP2<- disaggregate(mPP, 10)
rasterSal2<- disaggregate(mSal, 10)
rasterSST2<- disaggregate(mSST, 10)
rasterTurb2<- disaggregate(mTurb, 10)
}

{
rasterchl2<- mask(rasterchl2, res)
rasterDet2<- mask(rasterDet2, res)
rasterO22<- mask(rasterO22, res)
rasterPart2<- mask(rasterPart2, res)
rasterPP2<- mask(rasterPP2, res)
rasterSal2<- mask(rasterSal2, res)
rasterSST2<- mask(rasterSST2, res)
rasterTurb2<- mask(rasterTurb2, res)
}

#plot(rasterchl2, col=brewer.pal(n = 3, name = "YlGnBu"), main="Après mask", xlab="Longitude", ylab="Latitude")

fortify.Raster <- function(rasterchl2, maxPixel = 1000000) {
  
  if (ncell(rasterchl2) > maxPixel) {
    x <- sampleRegular(rasterchl2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterchl2, seq_len(ncell(rasterchl2)))
  out <- rasterchl2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterDet2, maxPixel = 1000000) {
  
  if (ncell(rasterDet2) > maxPixel) {
    x <- sampleRegular(rasterDet2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterDet2, seq_len(ncell(rasterDet2)))
  out <- rasterDet2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterO22, maxPixel = 1000000) {
  
  if (ncell(rasterO22) > maxPixel) {
    x <- sampleRegular(rasterO22, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterO22, seq_len(ncell(rasterO22)))
  out <- rasterO22 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterPart2, maxPixel = 1000000) {
  
  if (ncell(rasterPart2) > maxPixel) {
    x <- sampleRegular(rasterPart2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterPart2, seq_len(ncell(rasterPart2)))
  out <- rasterPart2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterPP2, maxPixel = 1000000) {
  
  if (ncell(rasterPP2) > maxPixel) {
    x <- sampleRegular(rasterPP2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterPP2, seq_len(ncell(rasterPP2)))
  out <- rasterPP2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterSal2, maxPixel = 1000000) {
  
  if (ncell(rasterSal2) > maxPixel) {
    x <- sampleRegular(rasterSal2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterSal2, seq_len(ncell(rasterSal2)))
  out <- rasterSal2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterSST2, maxPixel = 1000000) {
  
  if (ncell(rasterSST2) > maxPixel) {
    x <- sampleRegular(rasterSST2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterSST2, seq_len(ncell(rasterSST2)))
  out <- rasterSST2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}
fortify.Raster <- function(rasterTurb2, maxPixel = 1000000) {
  
  if (ncell(rasterTurb2) > maxPixel) {
    x <- sampleRegular(rasterTurb2, maxPixel, asRaster=TRUE)
  }
  xy <- xyFromCell(rasterTurb2, seq_len(ncell(rasterTurb2)))
  out <- rasterTurb2 %>%
    getValues() %>%
    data.frame(values = .) %>%
    cbind(xy)
  return(out)
}

{
Chl<- fortify(rasterchl2)
Det<- fortify(rasterDet2)
Part<- fortify(rasterPart2)
O2<- fortify(rasterO22)
Sal<- fortify(rasterSal2)
SST<- fortify(rasterSST2)
PP<- fortify(rasterPP2)
Turb<- fortify(rasterTurb2)
}
{
Chl<- na.omit(Chl)
Det<- na.omit(Det)
Part<- na.omit(Part)
O2<- na.omit(O2)
Sal<- na.omit(Sal)
SST<- na.omit(SST)
PP<- na.omit(PP)
Turb<- na.omit(Turb)
}

CHL<- ggplot(Chl)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Chlorophylle a")+
  scale_fill_manual(values = c("#EDF8B1", "#7FCDBB", "#2C7FB8"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= CHL, filename="CHL.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

DET<- ggplot(Det)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Détritus")+
  scale_fill_manual(values = c("#99CCCC", "#336666"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= DET, filename="DET.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

PART<- ggplot(Part)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Particules")+
  scale_fill_manual(values = c("#FFFFCC", "#CC6633"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= PART, filename="Part.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

PP<- ggplot(PP)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Production primaire")+
  scale_fill_manual(values = c("#CCFFCC", "#99CC99"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= PP, filename="PP.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

SAL<- ggplot(Sal)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Salinité")+
  scale_fill_manual(values = c("#F0F0F0", "#BDBDBD", "#636363"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= SAL, filename="Sal.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

SST<- ggplot(SST)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Température de surface")+
  scale_fill_manual(values = c("#FFCCCC", "#FF6666"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= SST, filename="SST.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

TURB<- ggplot(Turb)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Turbidité")+
  scale_fill_manual(values = c("#ECE7F2", "#A6BDDB", "#2B8CBE"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= TURB, filename="Turb.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)

O2<- ggplot(O2)+
  geom_tile(aes(x=x,y=y,fill= as.factor(values)))+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  ggtitle("Oxygène dissous")+
  scale_fill_manual(values = c("#EFEDF5", "#BCBDDC", "#756BB1"))+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_minimal()+
  labs(fill= "Zones")+
  theme(legend.title = element_text(size = 30))+
  theme(legend.text = element_text(size = 30))+
  theme(plot.title = element_text(size = 35, hjust = 0.5))+
  theme(axis.title.x = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 10))+
  theme(axis.title.y = element_text(size = 25))+
  theme(axis.text.y = element_text(size = 10))

ggsave(plot= O2, filename="O2.jpeg", path="results/satellite/Zones/Decoupe_polycut", width = 13, height = 8)












  