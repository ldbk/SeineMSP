#setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)


# DATRAS
    # Original file
Datras0<- read.csv("../data/J2Datras.csv")

Datrastax<- Datras0 %>% dplyr::select(ScientificName_WoRMS)
Datrastax<- unique(Datrastax)
write.csv(Datrastax, file="../data/DATRAS_taxons_original.csv")

        # Verif sous marinespecies.org ou wormsbynames [worms]

    # WoRMS approved taxons
Datras<- read.csv("../data/DATRAS_taxons_verified.csv", sep=";")
{
  Datras==""
  which(Datras=="", arr.ind = T)
  Datras[which(Datras=="", arr.ind = T)]
  Datras[which(Datras=="", arr.ind = T)]<- NA
}
Datras<- Datras %>% dplyr::select(-c(ScientificName, X, AphiaID, Match.type, Taxon.status, ScientificName.1, AphiaID_accepted, Kingdom, Phylum, Class, Order, Subgenus, Subspecies, isMarine, isBrackish, isFresh, isTerrestrial))
Datras<- unique(Datras)
Datras<- Datras %>% filter(!is.na(ScientificName_accepted))

Datrasspp<- Datras %>% dplyr::select(-c(ScientificName_accepted, Family))
Datrasspp<- Datrasspp %>% filter(!is.na(Genus), !is.na(Species))
Datrasspp<- unique(Datrasspp)                                                                         # Parmis tous les taxons valides de DATRAS (184), 157 sont identifiés par un genre ET une espèce


# BEUKHOF
    # Original file
Beukhof0<- readxl::read_excel("../data/BEUKHOF_tab_original.xlsx")

Beukhoftax<- Beukhof0 %>% dplyr::select(taxon, LME, habitat,feeding.mode,tl,age.maturity,growth.coefficient,length.max,age.max)
Beukhoftax<- Beukhoftax %>% filter(LME==22 | LME==24)
Beukhoftax<- Beukhoftax[!duplicated(Beukhoftax$taxon),]
Beukhoftax<- Beukhoftax %>% dplyr::select(-LME)
Beukhoftax<- unique(Beukhoftax)
write.csv(Beukhoftax, file="../data/BEUKHOF_taxons_original.csv")

        # Verif sous marinespecies.org ou wormsbynames [worms]

    # WoRMS approved taxons
Beukhof<- read.csv("../data/BEUKHOF_taxons_verified.csv", sep=";")
{
  Beukhof==""
  which(Beukhof=="", arr.ind = T)
  Beukhof[which(Beukhof=="", arr.ind = T)]
  Beukhof[which(Beukhof=="", arr.ind = T)]<- NA
}
Beukhof<- Beukhof %>% dplyr::select(-c(ScientificName, X, AphiaID, Match.type, Taxon.status, AphiaID_accepted, Kingdom, Phylum, Class, Order, Subgenus, Subspecies, isMarine, isBrackish, isFresh, isTerrestrial))
Beukhof<- unique(Beukhof)
Beukhof<- Beukhof %>% filter(!is.na(ScientificName_accepted))

Beukhofspp<- Beukhof %>% dplyr::select(-c(ScientificName_accepted, Family))
Beukhofspp<- Beukhofspp %>% filter(!is.na(Genus), !is.na(Species))                                  # Parmi tous les taxons acceptés de Beukhof (361), 305 sont identifiés par un genre ET une espèce
Beukhofspp<- unique(Beukhofspp)

Beukhofgen<- Beukhof %>% filter(is.na(Species), !is.na(Genus)) %>% 
  dplyr::select(-c(ScientificName_accepted, Family, Species))
Beukhofgen<- unique(Beukhofgen)
Beukhoffam<- Beukhof %>% filter(is.na(Genus)) %>% 
  dplyr::select(-c(ScientificName_accepted, Genus, Species))
Beukhoffam<- unique(Beukhoffam)



# CROISEMENT DATRAS-BEUKHOF
spp<- inner_join(Datrasspp, Beukhofspp, by= c("Genus","Species"))
spp<- unique(spp)                                                                                   # 84 taxons identifiés par Genre + espèce sont à la fois dans DATRAS et dans BEUKHOF
missing<- anti_join(Datras, spp)                                                                    # 101 taxons de DATRAS qui manquent encore

gen<- inner_join(missing, Beukhofgen, by="Genus") %>%
  filter(is.na(Species), !is.na(Genus))
gen<- unique(gen)                                                                                   # 5 taxons identifiés par Genre uniquement sont à la fois dans DATRAS et dans BEUKHOF
missing<- anti_join(missing, gen)                                                                   # 96 taxons de DATRAS qui manquent encore 

fam<- inner_join(missing, Beukhoffam, by="Family") %>%
  filter(is.na(Species), is.na(Genus), !is.na(Family))
fam<- unique(fam)                                                                                   # 1 taxon identifié par la Famille uniquement est à la fois dans DATRAS et dans BEUKHOF
missing<- anti_join(missing, fam)                                                                   # 95 taxons de DATRAS qui manquent encore 



# Cephalopodes
Datrasceph<- read.csv("../data/DATRAS_taxons_verified.csv", sep=";")
{
  Datrasceph==""
  which(Datrasceph=="", arr.ind = T)
  Datrasceph[which(Datrasceph=="", arr.ind = T)]
  Datrasceph[which(Datrasceph=="", arr.ind = T)]<- NA
}

Datrasceph<- Datrasceph %>% dplyr::select(Class, ScientificName_accepted) %>% filter(Class=="Cephalopoda")
Datrasceph<- unique(Datrasceph)                                                                     # 7 taxons céphalopodes sont retirés des "missing" et sont regroupés dans un groupe à part de céphalo
missing<- anti_join(missing, Datrasceph)                                                            # 88 taxons de DATRAS qui manquent encore dans Beukhof



# BIOTIC
missingbio<- missing %>% dplyr::select(ScientificName_accepted)
write.csv(missingbio, file="../data/missingbio.csv")

          # Extraction sous http://www.marlin.ac.uk/biotic/upload.php

missingverified<- read.csv("../data/missingbio_traits_biotic.csv", sep=";")                                 # 36 taxons de DATRAS sont identifiés dans BIOTIC
missing<- anti_join(missing, missingverified, by= c("ScientificName_accepted"="SpeciesName"))       # 52 taxons de DATRAS qui manquent encore



# RESUME
sumrez<- data.frame(nbDatras=nrow(Datras),
                    nbspp=nrow(spp),
                    nbgen=nrow(gen),
                    nbfam=nrow(fam),
                    nbcephalo=nrow(Datrasceph),
                    nbBenthos=nrow(missingverified))

sumrez$remain<- sumrez$nbDatras-sum(sumrez[2:6])



# FINAL
traitgen<- gen %>% dplyr::select(-c(Family, Genus, Species, taxon))
traitfam<- fam %>% dplyr::select(-c(Family, Genus, Species, taxon))
traitspp<- spp %>% mutate(ScientificName_accepted="")
traitspp$ScientificName_accepted<- paste(traitspp$Genus, traitspp$Species, sep=" ")
traitspp<- traitspp %>% dplyr::select(-c(Genus, Species, taxon))

traitfish<- rbind(traitspp,traitgen,traitfam)
traitbenthos<- missingverified %>% dplyr::select(SpeciesName, LifeSpan, Maturity,Habit, Sociability, FertilizationType, ReprodFreq, Migratory)
traitceph<- Datrasceph



# Nettoyage
{
  traitbenthos==""
  which(traitbenthos=="", arr.ind = T)
  traitbenthos[which(traitbenthos=="", arr.ind = T)]
  traitbenthos[which(traitbenthos=="", arr.ind = T)]<- NA
}
{
  traitbenthos=="Insufficient information"
  which(traitbenthos=="Insufficient information", arr.ind = T)
  traitbenthos[which(traitbenthos=="Insufficient information", arr.ind = T)]
  traitbenthos[which(traitbenthos=="Insufficient information", arr.ind = T)]<- NA
}
{
  traitbenthos=="Insufficient info."
  which(traitbenthos=="Insufficient info.", arr.ind = T)
  traitbenthos[which(traitbenthos=="Insufficient info.", arr.ind = T)]
  traitbenthos[which(traitbenthos=="Insufficient info.", arr.ind = T)]<- NA
}
{
  traitbenthos=="Not Researched"
  which(traitbenthos=="Not Researched", arr.ind = T)
  traitbenthos[which(traitbenthos=="Not Researched", arr.ind = T)]
  traitbenthos[which(traitbenthos=="Not Researched", arr.ind = T)]<- NA
}
{
  traitbenthos=="Not researched"
  which(traitbenthos=="Not researched", arr.ind = T)
  traitbenthos[which(traitbenthos=="Not researched", arr.ind = T)]
  traitbenthos[which(traitbenthos=="Not researched", arr.ind = T)]<- NA
}
{
  traitbenthos=="Insufficient information (13)"
  which(traitbenthos=="Insufficient information (13)", arr.ind = T)
  traitbenthos[which(traitbenthos=="Insufficient information (13)", arr.ind = T)]
  traitbenthos[which(traitbenthos=="Insufficient information (13)", arr.ind = T)]<- NA
}
{
  traitbenthos=="No text entered"
  which(traitbenthos=="No text entered", arr.ind = T)
  traitbenthos[which(traitbenthos=="No text entered", arr.ind = T)]
  traitbenthos[which(traitbenthos=="No text entered", arr.ind = T)]<- NA
}

{
  traitbenthos=="See additional information"
  which(traitbenthos=="See additional information", arr.ind = T)
  traitbenthos[which(traitbenthos=="See additional information", arr.ind = T)]
  traitbenthos[which(traitbenthos=="See additional information", arr.ind = T)]<- NA
}

traitbenthos<- traitbenthos[, c(1, 3, 2, 4, 5, 6, 7, 8)]
names(traitbenthos)[2]<- "MaturityAge"
names(traitbenthos)[1]<- "Taxons"

traitfish <- traitfish[, c(8, 1, 2, 3, 4, 5, 6, 7)]
names(traitfish)[1]<- "Taxons"
traitfish <- traitfish[, c(1, 5, 2, 3, 4, 6, 7, 8)]
names(traitfish)[2]<- "MaturityAge"

traitceph<- traitceph[, c(2, 1)]
names(traitceph)[1]<- "Taxons" 


# Ajout IUCN infos

IUCN<- read.csv("../data/IUCN.csv")
IUCN<- IUCN %>% dplyr::select(Species, IUCN.status)

traitfish<- left_join(traitfish, IUCN, by=c("Taxons"="Species")) 




save(traitfish, file="Traitfish.Rdata")
save(traitbenthos, file="Traitbenthos.Rdata")
save(traitceph, file="Traitceph.Rdata")





