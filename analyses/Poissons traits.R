remotes::install_github("ropensci/rfishbase")
setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library("rfishbase")
library(dplyr)
library(stats)

esp<- read.csv(file="TabespecesDATRAS.csv")
names(esp)[2]<- "SpeciesName"

Doc <- docs()

species<- species(esp$`SpeciesName`)
species<- species %>% select(Species, LongevityWild, DemersPelag, DepthRangeComShallow, DepthRangeComDeep, AnaCat)
species<- species %>% filter(!is.na(Species))

Eco<- ecology(esp$`SpeciesName`)
Eco<- Eco %>% select(Species, FoodTroph)
Eco<- Eco %>% filter(!is.na(Species))
Eco<- Eco %>% filter(!is.na(FoodTroph))

spawning<- spawning(esp$`SpeciesName`)
spawning<- spawning %>% select(Species, SpawningGround, Spawningarea)
spawning<- spawning %>% filter(!is.na(Species))
spawning<- spawning %>% filter(grepl("North Sea", Spawningarea)|grepl("Channel", Spawningarea)|grepl("Eastern Atlantic", Spawningarea)|grepl("Eastern North Atlantic", Spawningarea)|grepl("NE Atlantic", Spawningarea)|grepl("Northeast Atlantic", Spawningarea))
spawning<- spawning %>% group_by(Species) %>% summarize(SpawningGround=paste(unique(na.omit(SpawningGround)), collapse=";"), Spawningarea=paste(unique(na.omit(Spawningarea)), collapse=";"))

maturity<- maturity(esp$`SpeciesName`)
maturity<- maturity %>% select(Species, tm)
maturity<- maturity %>% filter(!is.na(Species))
maturity<- maturity %>% filter(!is.na(tm))
maturity<- maturity %>% group_by(Species) %>% summarize(Age= mean(round(tm, digits = 2)))

reprod<- reproduction(esp$`SpeciesName`)
reprod<- reprod %>% select(Species, Fertilization, Spawning)
reprod<- reprod %>% filter(!is.na(Species))

distrib<- distribution(esp$`SpeciesName`)
distrib<- distrib %>% select(Species, FAO)
distrib<- distrib %>% filter(!is.na(Species))
distrib<- distrib %>% group_by(Species) %>% summarize(Distrib=paste(unique(FAO), collapse=";"))


J1<- species %>% left_join(distrib, by = "Species")
J2<- J1 %>% left_join(reprod, by = "Species")
J3<- J2 %>% left_join(Eco, by = "Species")
J4<- J3 %>% left_join(maturity, by = "Species")
J5<- J4 %>% left_join(spawning, by ="Species")

{
names(J5)[2]<- "LifeSpan"
names(J5)[4]<- "DepthShallow"
names(J5)[5]<- "DepthDeep"
names(J5)[6]<- "Migratory"
names(J5)[8]<- "FertilizationType"
names(J5)[9]<- "SpawningFreq"
names(J5)[11]<- "MaturityAge"
}


