library(cowplot)
library(ggplot2)

load("results/AIS/AIS.Rdata")
load("results/Communautes bio/Bio.Rdata")
load("results/Habitats/Habitats.Rdata")
load("results/Usages/Usages.Rdata")
load("results/satellite/zones/Env.Rdata")

allparam2<- allparam+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 25))+
  theme(legend.title = element_text(size = 25))+
  theme(axis.title.x= element_text(size= 25))+
  theme(axis.title.y= element_text(size= 25))+
  geom_sf(data=windfarms, fill="blue")

ggsave(plot= allparam2, filename="allparam2.jpeg", path="results/satellite/", width = 13, height = 8)


ggHabitats2<- ggHabitats+
  #theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 25))+
  theme(legend.title = element_text(size = 25))+
  theme(axis.title.x= element_text(size= 25))+
  theme(axis.title.y= element_text(size= 25))+
  geom_sf(data=windfarms, fill="blue")

ggsave(plot= ggHabitats2, filename="ggHabitats2.jpeg", path="results/Habitats/", width = 13, height = 8)


Allcom2<- Allcom+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 25))+
  theme(legend.title = element_text(size = 25))+
  theme(axis.title.x= element_text(size= 25))+
  theme(axis.title.y= element_text(size= 25))+
  geom_sf(data=windfarms, fill="blue")

ggsave(plot= Allcom2, filename="Allcom2.jpeg", path="results/Communautes bio/", width = 13, height = 8)


Usages2<- Usages+
  #theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 25))+
  theme(legend.title = element_text(size = 25))+
  geom_sf(data=windfarms, fill="blue")+
  theme(legend.title = element_blank())

ggsave(plot= Usages2, filename="Usages2.jpeg", path="results/Usages/", width = 13, height = 8)

AIS2<- mapz+
  theme(legend.position="bottom")+
  theme(legend.text = element_text(size = 25))+
  theme(legend.title = element_text(size = 25))+
  theme(axis.title.x= element_text(size= 25))+
  theme(axis.title.y= element_text(size= 25))+
  geom_sf(data=windfarms, fill="blue")

ggsave(plot= AIS2, filename="AIS2.jpeg", path="results/AIS/", width = 13, height = 8)


#plot_grid(allparam2, ggHabitats2, Allcom2, Usages2, mapz, labels=c("A", "B", "C", "D", "E"), ncol = 1, nrow = 3)
#plot_grid(allparam2, Allcom2, AIS2, labels=c("A", "B", "C"), ncol = 1, nrow = 3)












