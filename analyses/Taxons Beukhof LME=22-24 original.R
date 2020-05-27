setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/fishtrait")
library(dplyr)

Beukhof<- readxl::read_excel("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/fishtrait/TraitCollectionFishNAtlanticNEPacificContShelf.xlsx")

# Juste les espèces
Beukhoftax<- Beukhof %>% dplyr::select(family, genus, species, taxonomic.rank, taxon, LME) 
Beukhoftax<- Beukhoftax %>%
  filter(LME==22 | LME==24)

Beukhoftax<- Beukhoftax %>% select(taxon)
Beukhoftax<- unique(Beukhoftax)
names(Beukhoftax)[1]<- "Taxons.Beukhof"

write.csv(Beukhoftax, file="C:/Users/jrivet/Documents/Stage M2/SeineMSP/data/BEUKHOF_taxons_original.csv")
