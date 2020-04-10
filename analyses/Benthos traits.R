setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)

benthos<- read.csv("Traits benthos (BIOTIC).csv", sep=";")

benthos<- benthos %>% select(SpeciesName, LifeSpan, Maturity, DepthRange, envpos, biozone, Habit, feedingmethod, Sociability, ReprodLocation, Migratory)

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
  benthos=="No text entered"
  which(benthos=="No text entered", arr.ind = T)
  benthos[which(benthos=="No text entered", arr.ind = T)]
  benthos[which(benthos=="No text entered", arr.ind = T)]<- NA
}
