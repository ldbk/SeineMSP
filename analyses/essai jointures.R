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

# Pangea traits
Beukhof<- readxl::read_excel("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")
Beukhof<- Beukhof %>% dplyr::select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max) %>%
  filter(LME==22)


Beukhofspe0<- Beukhof %>% select(genus, species) %>%  na.omit %>%
  mutate(Taxon=paste(genus, species))
Beukhofspe1<- Beukhof %>% left_join(Beukhofspe0, by=c("genus", "species"))
Beukhofspe1<- unique(Beukhofspe1)
Beukhofspe<- Beukhofspe1 %>% filter(!is.na(Taxon))
Beukhofspe<- Beukhofspe[,-c(1, 2, 3, 4)]
Beukhofspe<- Beukhofspe[, c(8,1, 2, 3, 4, 5, 6, 7 )]

Beukhofgen<- Beukhof %>% select(genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
Beukhofgen<- Beukhofgen %>% filter(is.na(Beukhofgen$species),!is.na(Beukhofgen$genus))
names(Beukhofgen)[1]<- "Taxon"
Beukhofgen<- Beukhofgen[,-c(2, 3)]

Beukhoffam<- Beukhof %>% select(family, genus, species, LME,habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
Beukhoffam<- Beukhoffam %>% filter((is.na(Beukhoffam$species)), (is.na(Beukhoffam$genus)), (!is.na(Beukhoffam$family)))
names(Beukhoffam)[1]<- "Taxon"
Beukhoffam<- Beukhoffam[,-c(2, 3, 4)]


TotBenkhof<- Beukhofspe %>% bind_rows(Beukhofspe, Beukhofgen, Beukhoffam)
TotBenkhof<- unique(TotBenkhof)
TotBenkhof$Taxon<- as.character(TotBenkhof$Taxon)


traits<- TotBenkhof %>% filter(Taxon %in% Espdatras$Species) %>%
  mutate(Reconnue= "Yes")


missing<- Espdatras %>% filter(!Species %in% TotBenkhof$Taxon) %>%
  mutate(Reconnue= "No")

missing<- Espdatras %>% filter(Species %in% TotBenkhof$Taxon) %>%
  mutate(Reconnue= "No")





missing<- Espdatras$Species[!which(Espdatras$Species %in% TotBenkhof$Taxon)]
missing<- unique(missing)










# Essais / brouillons / gros bazar

fishtrait0<- Beukhof %>% select(genus, species)
fishtrait1<- fishtrait0 %>% na.omit %>%
  mutate(Species=paste(genus, species)) 
fishtrait3<- Beukhof %>% left_join(fishtrait1, by=c("genus", "species"))
fishtrait3<- fishtrait3 %>% 
  filter(Species %in% Espdatras$Species) %>%
  mutate(Reconnue= "Yes")



fishtrait10<- Beukhof %>%
  filter(family %in% Espdatras$Species & is.na(species) & is.na(genus)) %>%
  filter(LME==22) %>%
  mutate(Reconnue= "Yes")

fishtrait11<- fishtrait10 %>%
  select(genus, species) 
fishtrait12<- fishtrait11 %>%
  na.omit %>%
  mutate(Species=paste(genus, species))
fishtrait12<- 


fishtrait20<- Beukhof %>% na.omit %>%
  mutate(Species=paste(genus, species)) 

#%>%  filter(genus %in% Espdatras$Species & is.na(species)) %>%filter(LME==22) %>%mutate(Reconnue= "Yes")

fishtrait<- fishtrait1 %>% bind_rows(fishtrait1, fishtrait20) %>% select(family, Species, Reconnue, habitat,feeding.mode,tl, age.maturity, growth.coefficient, length.max, age.max)
fishtrait<- fishtrait %>% bind_rows(fishtrait, fishtrait10) %>% select(family, Species, Reconnue, habitat,feeding.mode,tl, age.maturity, growth.coefficient, length.max, age.max)
fishtrait<- unique(fishtrait)


# Species missing 
missingtrait1<- Espdatras %>%
  filter(!Species %in% fishtrait$Species) #%>% dplyr::select(Species)

missingtrait10<- Espdatras %>%
  filter(!Species %in% fishtrait$family ) #%>% dplyr::select(Species)





write.csv(missingtrait1, file="C:/Users/jrivet/Documents/Stage M2/Data/CGFS/missingtrait1.csv")
missingtrait2<- read.csv("C:/Users/jrivet/Documents/Stage M2/Data/CGFS/missingtrait1_Worms.csv", sep=";")
{
  missingtrait2==""
  which(missingtrait2=="", arr.ind = T)
  missingtrait2[which(missingtrait2=="", arr.ind = T)]
  missingtrait2[which(missingtrait2=="", arr.ind = T)]<- NA
}

missingtrait2<- missingtrait2 %>% mutate(Reconnue= "No")


# Traitement Species missing

{
  Beukhof==""
  which(Beukhof=="", arr.ind = T)
  Beukhof[which(Beukhof=="", arr.ind = T)]
  Beukhof[which(Beukhof=="", arr.ind = T)]<- NA
}
names(Beukhof)[1]<- "Family"
names(Beukhof)[2]<- "Genus"

{
  Beukhofgenus<- Beukhof %>% filter(is.na(Beukhof$species))
  Beukhofgenus<- Beukhofgenus %>% filter (!is.na(Genus))
  Beukhofgenus<- Beukhofgenus %>% filter (Beukhofgenus$LME==22)
}

{
  Beukhoffamily<- Beukhof %>% filter(is.na(Beukhof$species))
  Beukhoffamily<- Beukhoffamily %>% filter(is.na(Beukhoffamily$Genus))
  Beukhoffamily<- Beukhoffamily %>% filter (Beukhoffamily$LME==22)
}

{
  missingtrait2genus<- missingtrait2 %>% filter(is.na(missingtrait2$Species))
  missingtrait2genus<- missingtrait2genus %>% filter (!is.na(Genus))
}

{
  missingtrait2family<- missingtrait2 %>% filter(is.na(missingtrait2$Species))
  missingtrait2family<- missingtrait2family %>% filter(is.na(missingtrait2family$Genus))
  missingtrait2family<- missingtrait2family %>% filter (!is.na(Family))
}


jointgenus<- missingtrait2genus %>% left_join(Beukhofgenus, by=c("Genus", "Family")) %>%
  dplyr::select(Family, Genus, species, Reconnue, habitat, feeding.mode, tl, age.maturity,growth.coefficient, length.max, age.max)
jointfamily<- missingtrait2family %>% left_join(Beukhoffamily, by="Family") %>%
  dplyr::select(Family, Genus.x, species, habitat, feeding.mode, tl, age.maturity,growth.coefficient, length.max, age.max )
names(jointfamily)[2]<- "Genus"
jointgf<- jointgenus %>% bind_rows(jointgenus, jointfamily)
jointgf<- unique(jointgf)







