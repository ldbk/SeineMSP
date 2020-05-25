library(worms)

#Test worms

#Datras sp
Espdatras<- read.csv("data/TabespecesDATRAS.csv")
names(Espdatras)[2]<- "Species"
Espdatras$Species<- as.character(Espdatras$Species)

#Check taxa update
Aphia_datras <- wormsbynames(Espdatras$Species)

table(Aphia_datras$status) #7 unaccepted
Aphia_datras[Aphia_datras$status=="alternate representation",]



#Beukhof sp
Beukhof<- readxl::read_excel("data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")
Beukhof <- Beukhof[Beukhof$LME %in% c(22,24),]
Beukhof_f <- Beukhof$family[Beukhof$taxonomic.rank=="F"]
Beukhof_g <- Beukhof$genus[Beukhof$taxonomic.rank=="G"]
Beukhof_s <- Beukhof[Beukhof$taxonomic.rank=="S",c(2,3)]
Beukhof_s <- paste(Beukhof_s$genus,Beukhof_s$species)

Beukhof_Taxa <- c(unique(Beukhof_f), unique(Beukhof_g),unique(Beukhof_s))

#Check taxa update
Aphia_beukhof <- wormsbynames(Beukhof_Taxa)
table(Aphia_beukhof$status) #7 unaccepted
Aphia_beukhof[Aphia_beukhof$status=="unaccepted",]

Aphia_beukhof22_24 <- wormsbynames(Beukhof_Taxa)
table(Aphia_beukhof22_24$status) #7 unaccepted
Aphia_beukhof22_24[Aphia_beukhof22_24$status=="unaccepted",]

#Check correspondance between 
length(which(Aphia_datras$valid_name %in% Aphia_beukhof22_24$valid_name))

#Remove sub species
Sp_commun <-  Aphia_datras$valid_name[which(Aphia_datras$valid_name %in% Aphia_beukhof22_24$valid_name)]

#Selection Beukhof pour taxon dans datras
Beukhof_22_24 <- Beukhof[Beukhof$taxon %in% Sp_commun,]

#Selection des lignes en enlevant les doublons
Beukhof_22_24_final <- Beukhof_22_24[!duplicated(Beukhof_22_24$taxon),]
