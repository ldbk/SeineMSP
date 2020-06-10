library(ggplot2)
library(dplyr)

Debm<- read.csv("data/Seine flow/Debits mensuels 1974 - 2006.csv", sep=";")

head(Debm)
names(Debm)[1]<- "year"
names(Debm)[2]<- "month"

Debm$Débit..m3.s.<- as.numeric(Debm$Débit..m3.s.)
Debm$month<- as.numeric(Debm$month)
Debm$year<- as.numeric(Debm$year)

# Infos
mean(Debm$Débit..m3.s., na.rm= TRUE)
min(Debm$Débit..m3.s., na.rm= TRUE)
max(Debm$Débit..m3.s., na.rm= TRUE)
sd(Debm$Débit..m3.s., na.rm= TRUE)
var(Debm$Débit..m3.s., na.rm= TRUE)


Tab<- Debm %>% group_by(year) %>% summarize(moy= mean(Débit..m3.s., na.rm= TRUE))

ggplot(Tab)+
  geom_line(aes(x= year, y= moy))+
  ggtitle("Annual flow Seine 1974-2006")+
  ylab("m3/s")+
  theme_minimal()
