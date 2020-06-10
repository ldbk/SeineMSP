library(ggplot2)
library(gridExtra)

load("data/satellite/Particles/part_ggplot.Rdata")
load("data/satellite/chl/chl_ggplot.Rdata")
load("data/satellite/Primary production/PP_ggplot.Rdata")
load("data/satellite/Detritus/Det_ggplot.Rdata")
load("data/satellite/Turbidity/Turb_ggplot.Rdata")
load("data/satellite/sst/sst_ggplot.Rdata")
load("data/satellite/Salinity/Sal_ggplot.Rdata")

Part<- ggplot(toto2part)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean Partcles")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Particles")

Chl<- ggplot(toto2chl)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean chl")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Chlorophyll")

PP<- ggplot(toto2PP)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean PP (mg C/m3/j)")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Primary production")

Det<- ggplot(toto2Det)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Detritus")

Turb<- ggplot(toto2Turb)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean Turbidity")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Turbidity")

SST<- ggplot(toto2sst)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean SST (°C)")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("SST")

Sal<- ggplot(toto2Sal)+
  geom_tile(aes(x=x,y=y,fill=mean))+
  xlab("Longitude")+
  ylab("Latitude")+
  labs(fill="mean salinity")+
  theme_minimal()+
  coord_fixed()+
  ggtitle("Salinity")


grid.arrange(Part, Chl, PP, Det, Turb, SST, Sal, ncol=2, nrow = 4)

