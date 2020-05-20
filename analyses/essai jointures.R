setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(googlesheets4)
library(fpc)
#library(mice)


# Read the species list
Espdatras<- read.csv("TabespecesDATRAS.csv")
names(Espdatras)[2]<- "Species"
Espdatras$Species<- as.character(Espdatras$Species)


# Especes LME=22 North Sea
Beukhof<- readxl::read_excel("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")
Beukhof1<- Beukhof %>% dplyr::select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max) %>%
  filter(LME==22)


Beukhofspe0<- Beukhof1 %>% select(genus, species) %>%  na.omit %>%
  mutate(Taxon=paste(genus, species))
Beukhofspe1<- Beukhof1 %>% left_join(Beukhofspe0, by=c("genus", "species"))
Beukhofspe1<- unique(Beukhofspe1)
Beukhofspe<- Beukhofspe1 %>% filter(!is.na(Taxon))
Beukhofspe<- Beukhofspe[,-c(1, 2, 3, 4)]
Beukhofspe<- Beukhofspe[, c(8,1, 2, 3, 4, 5, 6, 7 )]

Beukhofgen<- Beukhof1 %>% select(genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
Beukhofgen<- Beukhofgen %>% filter(is.na(Beukhofgen$species),!is.na(Beukhofgen$genus))
names(Beukhofgen)[1]<- "Taxon"
Beukhofgen<- Beukhofgen[,-c(2, 3)]

Beukhoffam<- Beukhof1 %>% select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
Beukhoffam<- Beukhoffam %>% filter((is.na(Beukhoffam$species)), (is.na(Beukhoffam$genus)), (!is.na(Beukhoffam$family)))
names(Beukhoffam)[1]<- "Taxon"
Beukhoffam<- Beukhoffam[,-c(2, 3, 4)]


TotBenkhof<- Beukhofspe %>% bind_rows(Beukhofspe, Beukhofgen, Beukhoffam)
TotBenkhof<- unique(TotBenkhof)
TotBenkhof$Taxon<- as.character(TotBenkhof$Taxon)


traits<- TotBenkhof %>% filter(Taxon %in% Espdatras$Species) %>%
  mutate(Reconnue= "Yes")




# Especes LME=24 Celtic-Biscay shelf

Beukhof2<- Beukhof %>% dplyr::select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max) %>%
  filter(LME==24)

Beukhofspe0Z<- Beukhof2 %>% select(genus, species) %>%  na.omit %>%
  mutate(Taxon=paste(genus, species))
Beukhofspe1Z<- Beukhof2 %>% left_join(Beukhofspe0Z, by=c("genus", "species"))
Beukhofspe1Z<- unique(Beukhofspe1Z)
BeukhofspeZ<- Beukhofspe1Z %>% filter(!is.na(Taxon))
BeukhofspeZ<- BeukhofspeZ[,-c(1, 2, 3, 4)]
BeukhofspeZ<- BeukhofspeZ[, c(8,1, 2, 3, 4, 5, 6, 7 )]

BeukhofgenZ<- Beukhof2 %>% select(genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
BeukhofgenZ<- BeukhofgenZ %>% filter(is.na(BeukhofgenZ$species),!is.na(BeukhofgenZ$genus))
names(BeukhofgenZ)[1]<- "Taxon"
BeukhofgenZ<- BeukhofgenZ[,-c(2, 3)]

BeukhoffamZ<- Beukhof2 %>% select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
BeukhoffamZ<- BeukhoffamZ %>% filter((is.na(BeukhoffamZ$species)), (is.na(BeukhoffamZ$genus)), (!is.na(BeukhoffamZ$family)))
names(BeukhoffamZ)[1]<- "Taxon"
BeukhoffamZ<- BeukhoffamZ[,-c(2, 3, 4)]


TotBenkhofZ<- BeukhofspeZ %>% bind_rows(BeukhofspeZ, BeukhofgenZ, BeukhoffamZ)
TotBenkhofZ<- unique(TotBenkhofZ)
TotBenkhofZ$Taxon<- as.character(TotBenkhofZ$Taxon)


traitsZ<- TotBenkhofZ %>% filter(Taxon %in% Espdatras$Species) %>%
  mutate(Reconnue= "Yes")

traitsZ<- traitsZ %>% filter(!Taxon %in% traits$Taxon)



# Jointure des deux

Joint<- traits %>% bind_rows(traits, traitsZ)
Joint<- unique(Joint)

missing<- Espdatras %>% filter(!Species %in% Joint$Taxon)

write.csv(missing, file="C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/missingspecies.csv")











