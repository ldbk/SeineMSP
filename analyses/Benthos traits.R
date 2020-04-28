setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos (BIOTIC).csv", sep=',')

benthos<- benthos %>% dplyr::select(SpeciesName, LifeSpan, BiogeographicRange, Maturity, DepthRange, envpos, biozone, Habit, feedingmethod, Sociability, FertilizationType, ReprodFreq, Migratory)
names(benthos)[4]<- "MaturityAge"

{
  benthos==""
  which(benthos=="", arr.ind = T)
  benthos[which(benthos=="", arr.ind = T)]
  benthos[which(benthos=="", arr.ind = T)]<- NA
}
{
  benthos=="Insufficient information"
  which(benthos=="Insufficient information", arr.ind = T)
  benthos[which(benthos=="Insufficient information", arr.ind = T)]
  benthos[which(benthos=="Insufficient information", arr.ind = T)]<- NA
}
{
  benthos=="Insufficient info."
  which(benthos=="Insufficient info.", arr.ind = T)
  benthos[which(benthos=="Insufficient info.", arr.ind = T)]
  benthos[which(benthos=="Insufficient info.", arr.ind = T)]<- NA
}
{
  benthos=="Not Researched"
  which(benthos=="Not Researched", arr.ind = T)
  benthos[which(benthos=="Not Researched", arr.ind = T)]
  benthos[which(benthos=="Not Researched", arr.ind = T)]<- NA
}
{
  benthos=="Not researched"
  which(benthos=="Not researched", arr.ind = T)
  benthos[which(benthos=="Not researched", arr.ind = T)]
  benthos[which(benthos=="Not researched", arr.ind = T)]<- NA
}
{
  benthos=="Insufficient information (13)"
  which(benthos=="Insufficient information (13)", arr.ind = T)
  benthos[which(benthos=="Insufficient information (13)", arr.ind = T)]
  benthos[which(benthos=="Insufficient information (13)", arr.ind = T)]<- NA
}
{
  benthos=="No text entered"
  which(benthos=="No text entered", arr.ind = T)
  benthos[which(benthos=="No text entered", arr.ind = T)]
  benthos[which(benthos=="No text entered", arr.ind = T)]<- NA
}

{
  benthos=="See additional information"
  which(benthos=="See additional information", arr.ind = T)
  benthos[which(benthos=="See additional information", arr.ind = T)]
  benthos[which(benthos=="See additional information", arr.ind = T)]<- NA
}


benthos<- benthos[,-5]









