#package
library(sf)
library(tidyverse)
library(ggrepel)
library(rnaturalearthdata)

#read the data
nat2000<-readRDS("../data/humanactivities/nat2000.rds")
aggareas<-readRDS("../data/humanactivities/aggareas.rds")
munpol<-readRDS("../data/humanactivities/munpol.rds")
munpt<-readRDS("../data/humanactivities/munpt.rds")
windfarms<-readRDS("../data/humanactivities/windfarms.rds")


#a map
ggplot()+
	geom_sf(data=nat2000,aes(fill=sitetype))+
	geom_sf(data=aggareas,aes(fill=code))+
	geom_sf(data=munpol,fill="grey")+
	geom_sf(data=munpt,fill="grey")+
	geom_sf(data=windfarms,fill="blue")+
	borders("world",fill="light grey") +coord_sf(ylim=c(49.2,49.8),xlim=c(-1.5,0.5))+
	theme_bw()

