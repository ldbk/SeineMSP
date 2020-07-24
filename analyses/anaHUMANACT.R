library(sf)
library(tidyverse)
library(ggrepel)
library(rnaturalearthdata)

load("data/PolyCut.Rdata")

#read the data
nat2000<-readRDS("data/humanactivities/nat2000.rds")
aggareas<-readRDS("data/humanactivities/aggareas.rds")
munpol<-readRDS("data/humanactivities/munpol.rds")
munpt<-readRDS("data/humanactivities/munpt.rds")
windfarms<-readRDS("data/humanactivities/windfarms.rds")
dredgespoil<-readRDS("data/humanactivities/dredgespoil.rds")


#a map
ggplot()+
	geom_sf(data=nat2000,aes(fill=sitetype),alpha=.2)+
	geom_sf(data=aggareas%>%mutate(type="agg"),aes(fill=type),alpha=.4)+
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
	#geom_sf(data=munpol%>%mutate(type="mun"),aes(fill=type),fill="red")+
	#geom_sf(data=munpt,color="red")+
	#geom_sf(data=windfarms,fill="blue")+
	geom_sf(data=dredgespoil%>%mutate(type="agg"),aes(color=type))+
	#borders("world",fill="grey") +
  coord_sf(ylim=c(49.2,49.9),xlim=c(-1.5,0.5))+
	theme_bw()

ggplot()+
  geom_sf(data=nat2000, aes(fill=sitedesc), alpha=.2)+            
  geom_sf(data=aggareas %>% mutate(type="Site d'extraction de granulat"), aes(fill=type), alpha=.4)+ 
  geom_polygon(data=PolyCut, aes(x=long, y=lat, group=group), fill=NA, col="black")+
  #geom_sf(data=munpol%>%mutate(type="mun"),aes(fill=type),fill="red")+
  #geom_sf(data=munpt,color="red")+
  #geom_sf(data=windfarms,fill="blue")+
  geom_sf(data=dredgespoil %>% mutate(type="Site de dépôt"), aes(color=type))+            # Dépôt en mer
  #borders("world", fill="grey") +
  coord_sf(ylim=c(49.2, 49.9),xlim=c(-1.5,0.5))+
  theme_bw()+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 13))+
  theme(axis.title.y = element_text(size = 13))+
  theme(legend.text = element_text(size = 15))

