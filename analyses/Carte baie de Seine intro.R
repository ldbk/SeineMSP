library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(rnaturalearth)
library(ggplot2)
library(rgdal)

world<- ne_countries(scale=10, returnclass = "sf")


# Element to add
#countries <- data.frame(long=c(-2, 2),lat=c(50, 40),legend="FRANCE")
regions <- data.frame(long=0.6,lat=49.2, legend="Normandie")
sea<- data.frame(long=-0.1,lat=49.9, legend="Manche")
rivers <- data.frame(long=c(-1.4, -1, -0.8, -0.45, -0.01, 0.28, 0.9), lat=c(49.4, 49.1, 49.28, 49.2, 49.22, 49.33, 49.55),legend=c("La Douve", "La Vire", "L'Aure", "L'Orne", "La Dives", "La Touques", "La Seine"))
bdv <- data.frame(long=c(-1.1), lat=c(49.5), legend="Baie des\n Veys")
bds <- data.frame(long=c(-0.5), lat=c(49.62), legend="Baie de Seine")
#cotentin <- data.frame(long=c(-1.5),lat=c(49.45),legend=c("Cotentin\n Peninsula"))
portsPTS<- data.frame(long=c(-1.26, -0.75, -0.46, -0.26, 0.23, 0.11), lat=c(49.67, 49.35, 49.33, 49.28, 49.42, 49.49))
ports<- data.frame(   long=c(-1.26, -0.78, -0.46, -0.26, 0.23, 0.11), lat=c(49.71, 49.42, 49.40, 49.32, 49.46, 49.53), legend=c("Barfleur", "Port en\n Bessin", "Courseulles sur\n mer", "Ouistreham", "Honfleur", "Le Havre"))

#stat$Coord_x <- as.numeric(as.character(stat$Coord_x))
#stat$Coord_y <- as.numeric(as.character(stat$Coord_y))
#stat$Coord_y[3] <- stat$Coord_y[3]+0.02 #Correction CAB



# Bathymetry
bathy <- rgdal::readOGR("data/Bathymétrie/bathy_BdS_10-30.gpkg")
bathy10 <- bathy[bathy@data$ELEV==10,]
bathy10 <- fortify(bathy10)
bathy20 <- bathy[bathy@data$ELEV==20,]
bathy20 <- fortify(bathy20)
bathy30 <- bathy[bathy@data$ELEV==30,]
bathy30 <- fortify(bathy30)

bathy10 <- cbind(bathy10,ELEV=10)
bathy20 <- cbind(bathy20,ELEV=20)
bathy30 <- cbind(bathy30,ELEV=30)
bathytot <- rbind(bathy10,bathy20,bathy30)
bathytot$ELEV <- factor(bathytot$ELEV)

bathytot$ELEV <- sub("10", "-10 m", bathytot$ELEV)
bathytot$ELEV <- sub("20", "-20 m", bathytot$ELEV)
bathytot$ELEV <- sub("30", "-30 m", bathytot$ELEV)


ggbathy<- ggplot(bathytot)+
  geom_path(data=bathytot, aes(x=long, y=lat, group=group, lty=ELEV, colour= ELEV), alpha=0.3)+
  theme_classic() +
  xlab("Longitude")+
  ylab("Latitude")+
  theme(axis.title.x= element_text(size = 15))+
  theme(axis.title.y= element_text(size = 15))+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 15))+
  theme(legend.key.size = unit(35, "line"))+
  geom_sf(data = world, fill = "linen") + 
  coord_sf(xlim = c(-1.6, 0.7), ylim = c(49.2, 49.8), expand = FALSE)

ggsave(plot = ggbathy, filename = "Bathymétrie.jpeg", path = "results/Baie de Seine intro", width = 13, height = 8)
  

# Fleuves
#bzh <- readOGR("cours_bzh.gpkg")
#north <- readOGR("cours_north.gpkg")
normandy<- readOGR("data/Carte baie de Seine intro/Cours d'eau.gpkg")


# Plot Channel
EastChan <- ggplot()+ 
  geom_sf(data = world, fill = "linen") + 
  coord_sf(xlim = c(-2, 1.4), ylim = c(49, 50), expand = FALSE)+
  #geom_text(data=countries,aes(x=long,y=lat,label=legend, fontface=2))+
  geom_text(data=regions, aes(x=long, y=lat, label=legend, fontface=1, size= 20, col="red"))+
  geom_text(data=sea, aes(x=long, y=lat, label=legend, fontface=1, size= 20, col="red"))+
  
  geom_text(data=bds,aes(x=long, y=lat,label=legend, fontface=2), size= 6)+
  geom_text(data=bdv,aes(x=long,y=lat,label=legend, fontface=3))+
  
  #geom_text(data=cotentin,aes(x=long,y=lat,label=legend, fontface=3),angle=315)+
  #geom_path(data=bathytot, aes(x=long, y=lat, group=group, lty=ELEV), alpha=0.3)+
  #geom_path(data=bzh, aes(x=long, y=lat,group=group),col="blue")+
  #geom_path(data=north, aes(x=long, y=lat,group=group),col="blue")+
  
  #geom_path(data=bathytot, aes(x=long, y=lat, group=group, lty=ELEV, colour= ELEV), alpha=0.3)+
  
  geom_text(data=rivers,aes(x=long,y=lat,label=legend, fontface=3), size=3)+
  geom_path(data=normandy, aes(x=long, y=lat,group=group),col="blue")+
  
  geom_label(data=ports, aes(x=long,y=lat, label=legend, fontface=1))+
  geom_point(data=portsPTS, aes(x=long, y=lat), shape= 18)+
  
  theme(panel.background = element_rect(fill = "lightskyblue1"), 
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "lightskyblue1"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "black"))+
  theme(legend.position = "none")+
  xlab("Longitude")+
  ylab("Latitude")#+
  #ggsn::scalebar(dist=50, transform=T, dist_unit ="km",location="bottomleft",
  #              x.min=(-4), x.max=3, y.min=48.2, y.max=51, st.size=3)
#ggsn::north(location="topright",x.min=(-4), x.max=2.5, y.min=48.2, y.max=51)
ggsave(plot = EastChan, filename = "Baie de Seine.jpeg", path = "results/Baie de Seine intro", width = 13, height = 8)







# Plot France or Europe  
library(sf)
box <- data.frame(Coord_x=c(-5,3),Coord_y=c(48,51))
pt <- st_as_sf(box, coords = c("Coord_x", "Coord_y"), crs= 4326, agr="constant")
bbox <- st_as_sfc(st_bbox(pt))

FR <- ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(data = bbox, fill = NA, color = "red", size = 1.2)+
  coord_sf(xlim = c(-5, 5), ylim = c(42, 52), expand = FALSE)+
  theme_void()

WestEU <- ggplot() + 
  geom_sf(data = world, fill = "white") + 
  geom_sf(data = bbox, fill = NA, color = "red", size = 1.2)+
  coord_sf(xlim = c(-10, 10), ylim = c(37, 57), expand = FALSE)+
  theme_void()





# Combine plot
library(cowplot)
ggdraw() +
  draw_plot(EastChan) +
  draw_plot(WestEU, x = 0.70,y = 0.17, width = 0.30, height = 0.30)

library(grid)
rect <- grid.rect(
  x = 0.70,
  y = 0.17,
  width = 0.3,
  height =0.3,
  hjust = 0, vjust = 1,
  gp = gpar(alpha = 0.5, col="black",fill="skyblue2"),
  draw = T
)
ggdraw() +
  draw_plot(EastChan) +
  draw_plot(WestEU, x = 0.70,y = 0.17, width = 0.30, height = 0.30)+
  draw_grob(rect)


# Save plot
ggsave(filename = "figure_1_bw.png", 
       plot = gg_inset_map,
       width = 7.05, 
       height = 4,
       dpi = 150)
























