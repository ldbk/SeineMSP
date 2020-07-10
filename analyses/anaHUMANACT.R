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
dredgespoil<-readRDS("../data/humanactivities/dredgespoil.rds")


#a map
ggplot()+
	geom_sf(data=nat2000,aes(fill=sitetype),alpha=.2)+
	geom_sf(data=aggareas%>%mutate(type="agg"),aes(fill=type),alpha=.4)+
	#geom_sf(data=munpol%>%mutate(type="mun"),aes(fill=type),fill="red")+
	#geom_sf(data=munpt,color="red")+
	#geom_sf(data=windfarms,fill="blue")+
	geom_sf(data=dredgespoil%>%mutate(type="agg"),aes(color=type))+
	borders("world",fill="grey") +coord_sf(ylim=c(49.2,49.8),xlim=c(-1.5,0.5))+
	theme_bw()

