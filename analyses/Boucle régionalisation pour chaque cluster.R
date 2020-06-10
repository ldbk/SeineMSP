library(dplyr)
library(tidyr)
library(ggplot2)

load("data/krigeage.Rdata")
names(Kriege.logdens)[6]<- "Community"
Kriege.logdens$Community<- as.numeric(Kriege.logdens$Community)


Longitude<- numeric()
Latitude<- numeric()
Clust<- numeric()
Community<- numeric()


for (j in unique(data.frame(Kriege.logdens)[,"Community"])){
  
  Tab<- Kriege.logdens[Kriege.logdens$Community==j,] %>% select(-c(Variance, Community))
  Tab<- pivot_wider(Tab, names_from = Year, values_from = Prediction)
  metaTab<- Tab %>% select(Longitude, Latitude)
  Tab<- Tab %>% select(-c(Longitude, Latitude))

  
  
  # Classification
  
  distance<- dist(Tab)
  distance[1:5]
  
  tree<- hclust(distance)
  plot(tree, hang=-1)
  
  rect.hclust(tree, 5)
  zones<- cutree(tree, 5)
  
  toto<- cbind(metaTab, Clust=factor(zones))
  toto<- left_join(toto, Kriege.logdens[Kriege.logdens$Community==j,], by=c("Longitude", "Latitude"))
  toto<- toto %>% select(Longitude, Latitude, Clust, Community)
  
  Longitude<- c(Longitude, toto$Longitude)
  Latitude<- c(Latitude, toto$Latitude)
  Clust<- c(Clust, toto$Clust)
  Community<- c(Community, toto$Community)

  tata <- left_join(toto, cbind(metaTab, Tab))
  tata <- pivot_longer(tata, cols=c(5:36), names_to="Year", values_to = "Prediction")
  tata <- tata %>% group_by(Year, Clust) %>% summarise(Prediction=mean(Prediction))
  
  
ggtata<-  ggplot(tata)+
    geom_point(aes(x=Year,y=Prediction,col=Clust))+
    geom_line(aes(x=Year,y=Prediction,col=Clust, group=Clust))+
    theme_minimal()+
    facet_wrap(.~Clust)

print(ggtata)
    
}



toto<- data.frame(Longitude=Longitude, Latitude=Latitude, Clust=as.factor(Clust), Community=Community)

ggplot(toto)+
  geom_tile(aes(x=Longitude, y=Latitude, fill=Clust)) +
  theme_minimal() +
  coord_fixed()+
  facet_wrap(.~ Community)+
  scale_fill_manual(values=c("cyan3","red", "chartreuse4", "darkgoldenrod1", "burlywood4"))












