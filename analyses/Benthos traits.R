setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos (BIOTIC).csv", sep=";")

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

{
  benthos$MaturityAge=="<1 year"
  which(benthos$MaturityAge=="<1 year", arr.ind = T)
  benthos$MaturityAge[which(benthos$MaturityAge=="<1 year", arr.ind = T)]
  benthos$MaturityAge[which(benthos$MaturityAge=="<1 year", arr.ind = T)]<- "< 1 year"
}






# ACM

{
  nbid<- benthos %>%
    summarise_all(n_distinct) %>%
    t()
  nbNA<- function(a){a[a==""]<-NA;sum(is.na(a))}
  nbNA<- benthos %>%
    summarise_all(nbNA) %>%
    t()
  
  summary<- data.frame(nbid=nbid,nbNA=nbNA)
}

essai<- benthos %>%
  select(SpeciesName, LifeSpan, MaturityAge, Habit, ReprodFreq) 

  # Imputation
essai<- benthos %>% 
  mutate_all(as.factor) %>% 
  data.frame()

row.names(essai)<- essai[,1] 
essai<- essai[,-1]
nb<- estim_ncpMCA(essai) # Error : The algorithm fails to converge. Choose a number of components (ncp) less or equal than 2 or a number of iterations (maxiter) less or equal than 999

rez<- imputeMCA(essai, ncp=4)
rez<- MCA(essai, tab.disj=rez$tab.disj.comp)
HCPC(rez)




# essai
benthos<- benthos %>%
  mutate_all(as.factor)%>%
  data.frame()

row.names(benthos)<- benthos[,1] 
essai<- benthos[,-1]
nb<- estim_ncpMCA(benthos)


rez<- imputeMCA(benthos, ncp=4)
rez<- MCA(benthos, tab.disj=rez$tab.disj.comp)
HCPC(rez)





