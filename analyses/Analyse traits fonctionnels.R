setwd("C:/Users/jrivet/Documents/Stage M2/Data/CGFS")

esp<- read.csv("Traits especes (BIOTIC).csv", sep=";") # UNIQUEMENT 46 ESPECES TROUVEES / 190

# Nettoyage tableau
esp<- esp[, -c(3, 4, 5, 45, 47, 48, 49, 50)] # ResearchedBy, DataSuppliedBy, RefereedBy, physpref (all NA), salinity (all NA), substratum (NA),waterflow (all NA), waveexp (all NA)
{
  esp==""
  which(esp=="", arr.ind = T)
  esp[which(esp=="", arr.ind = T)]
  esp[which(esp=="", arr.ind = T)]<- NA
}
{
  esp=="Insufficient information"
  which(esp=="Insufficient information", arr.ind = T)
  esp[which(esp=="Insufficient information", arr.ind = T)]
  esp[which(esp=="Insufficient information", arr.ind = T)]<- NA
}
{
  esp=="Insufficient info."
  which(esp=="Insufficient info.", arr.ind = T)
  esp[which(esp=="Insufficient info.", arr.ind = T)]
  esp[which(esp=="Insufficient info.", arr.ind = T)]<- NA
}
{
  esp=="Not Researched"
  which(esp=="Not Researched", arr.ind = T)
  esp[which(esp=="Not Researched", arr.ind = T)]
  esp[which(esp=="Not Researched", arr.ind = T)]<- NA
}
{
  esp=="Not researched"
  which(esp=="Not researched", arr.ind = T)
  esp[which(esp=="Not researched", arr.ind = T)]
  esp[which(esp=="Not researched", arr.ind = T)]<- NA
}
{
  esp=="No text entered"
  which(esp=="No text entered", arr.ind = T)
  esp[which(esp=="No text entered", arr.ind = T)]
  esp[which(esp=="No text entered", arr.ind = T)]<- NA
}
