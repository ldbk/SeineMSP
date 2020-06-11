library(ggplot2)
library(gridExtra)

{
load("data/satellite/Particles/part_ggplot.Rdata")
load("data/satellite/chl/chl_ggplot.Rdata")
load("data/satellite/Primary production/PP_ggplot.Rdata")
load("data/satellite/Detritus/Det_ggplot.Rdata")
load("data/satellite/Turbidity/Turb_ggplot.Rdata")
load("data/satellite/sst/sst_ggplot.Rdata")
load("data/satellite/Salinity/Sal_ggplot.Rdata")
}

grid.arrange(Part, Chl, PP, Det, Turb, SST, Sal, ncol=2, nrow = 4)

