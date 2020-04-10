remotes::install_github("ropensci/rfishbase")
library("rfishbase")
library(dplyr)

esp<- read.csv(file="C:/Users/jrivet/Documents/Stage M2/Data/CGFS/TabespecesDATRAS.csv")
names(esp)[2]<- "Species name"

Doc <- docs()

species<- species(esp$`Species name`)
species2<- species %>% select(Species, LongevityWild, DemersPelag, DepthRangeShallow, DepthRangeComShallow, DepthRangeDeep, DepthRangeComDeep, AnaCat)
species2<- species2 %>% filter(!is.na(Species))

Eco<- ecology(esp$`Species name`)
Eco2<- Eco %>% select(Species, DietTroph, DietSeTroph, DietRemark, FoodTroph, FoodSeTroph, FoodRemark)
Eco2<- Eco2 %>% filter(!is.na(Species))



diet<- diet(esp$`Species name`)
dietitems<- diet_items(esp$`Species name`)
Ecosyst<- ecosystem(esp$`Species name`) # Pas de guide
estimate<- estimate(esp$`Species name`)
fecundity<- fecundity(esp$`Species name`)
fooditems<- fooditems(esp$`Species name`)
intro<- introductions(esp$`Species name`)
length<- length_length(esp$`Species name`)
maturity<- maturity(esp$`Species name`)
popgrowth<- popgrowth(esp$`Species name`)
reproduction<- reproduction(esp$`Species name`)
swimming<- swimming(esp$`Species name`)
distribution<- distribution(esp$`Species name`)
spawning<- spawning(esp$`Species name`)
