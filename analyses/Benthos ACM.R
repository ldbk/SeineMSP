setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
library(FactoMineR)
library(missMDA)

benthos<- read.csv("Traits benthos f - Traits benthos f.csv", sep=',')

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

benthos<- benthos %>%
  mutate_all(as.factor)%>%
  data.frame()

row.names(benthos)<- benthos[,1] 
benthos<- benthos[,-1]
nb<- estim_ncpMCA(benthos)

rez<- imputeMCA(benthos, ncp=4)
rez<- MCA(benthos, tab.disj=rez$tab.disj.comp)
HCPC(rez)
