setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP")
library(dplyr)


# Ouvrir PoissonsClusters.RData, BenthosClusters.RData et J2Datras.RData das l'environnement

# Mise en forme 3 tabs
J2<- J2 %>% dplyr::select(Year, moyLat, moyLong, ScientificName_WoRMS, Poids, Nombre, DensityWgt, DensityNb)
names(J2)[4]<- "Species"

benthos<- benthos[, c(1, 3, 9,  2, 4, 5, 6, 7, 8)]
benthos$Cluster<- as.factor(benthos$Cluster)

fishtraitnew<- fishtraitnew[, c(1, 5, 10, 2, 3, 4, 6, 7, 8, 9)]
fishtraitnew$Cluster<- as.factor(fishtraitnew$Cluster)
names(fishtraitnew)[2]<- "MaturityAge"


# Jointure Benthos-Poissons
essai<- fishtraitnew %>% bind_rows(fishtraitnew, benthos)
#length(which(unique(benthos$Species) %in% unique(fishtraitnew$Species)))
essai<- unique(essai)


# Jointure Datras-traits
Joint<- J2 %>% left_join(essai, by= ("Species"))

save(Joint, file="C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/Jointure Datras-Traits.Rdata")
