library(dplyr)
library(tidyr)
library(FactoMineR) # pour ACM
library(cluster) # pour agnes
library(factoextra) # pour fviz_mca_ind
library(fpc) # pour comparaison des méthodes d'agrégation
library(ggplot2)
library(openxlsx)
library(googlesheets4)
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)
library(captioner)



# 1 BENTHOS loading and tab preparation

load("data/Traitbenthos.Rdata")

    # Imputation

traitbenthos1<- mice::mice(traitbenthos, m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
traitbenthos<-mice::complete(traitbenthos1)

    # Categorize numerical values
{
  traitbenthos$Maturity<- as.character(traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("< 1 year", "[0;2[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("<1 year", "[0;2[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("1 year", "[0;2[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("1-2 years", "[0;2[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("2-3 years", "[2;6[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("3-5 years", "[2;6[", traitbenthos$Maturity)
  traitbenthos$Maturity<- sub("6-10 years", "[6;10[", traitbenthos$Maturity)
}
{
  traitbenthos$LifeSpan<- as.character(traitbenthos$LifeSpan)
  traitbenthos$LifeSpan<- sub("1-2 years", "[0;6[",traitbenthos$LifeSpan)
  traitbenthos$LifeSpan<- sub("3-5 years", "[0;6[",traitbenthos$LifeSpan)
  traitbenthos$LifeSpan<- sub("6-10 years", "[6;21[",traitbenthos$LifeSpan)
  traitbenthos$LifeSpan<- sub("11-20 years", "[6;21[",traitbenthos$LifeSpan)
  traitbenthos$LifeSpan<- sub("21-50 years", "[21;50[",traitbenthos$LifeSpan)
}

traitbenthos$Habit<- sub("Erect", "Attached", traitbenthos$Habit)


# 2 BENTHOS ACM on Burt table
traitbenthos1<- data.frame(traitbenthos[,-1])
row.names(traitbenthos1)<- traitbenthos[,1]

rez<- MCA(traitbenthos1, ncp=999, method="Burt", graph=F)
plt1<-plotellipses(rez, axes=c(1,2))
plt2<-plotellipses(rez, axes=c(1,3))

print(plt1)
print(plt2)



# 3 BENTHOS classification methods comparison

#then Hennig, C. (2017) Cluster validation by measurement of clustering
#characteristics relevant to the user. In C. H. Skiadas (ed.) Proceedings of
#ASMDA 2017, 501-520, https://arxiv.org/abs/1703.09282
# using clusterbenchmark from fpc
if(!file.exists("cbs.rds")){
  set.seed(666) #the number of the beaaasttt
  options(digits=3)
  clustermethod=c("kmeansCBI","hclustCBI","hclustCBI","hclustCBI","hclustCBI","hclustCBI","claraCBI")
  clustermethodpars <- list()
  clustermethodpars[[2]] <- clustermethodpars[[3]] <- list()
  clustermethodpars[[4]] <-  clustermethodpars[[5]] <-list()
  clustermethodpars[[6]]<-   clustermethodpars[[7]] <-list()
  clustermethodpars[[2]]$method <- "ward.D2"
  clustermethodpars[[3]]$method <- "single"
  clustermethodpars[[4]]$method <- "complete"
  clustermethodpars[[5]]$method <- "average"
  clustermethodpars[[6]]$method <- "mcquitty"
  #clustermethodpars[[7]]$method <- ""
  methodname <- c("kmeans","ward","single","complete","average","mcquitty","clara")
  cbs <-  clusterbenchstats(rez$ind$coord,G=2:20,
                            clustermethod=clustermethod,scaling=FALSE,
                            methodname=methodname,
                            distmethod=rep(FALSE,length(clustermethod)),
                            clustermethodpars=clustermethodpars,nnruns=100,kmruns=100,
                            fnruns=100,avenruns=100,multicore=FALSE,trace=F)
  saveRDS(cbs,file="cbs.rds")
}else{
  cbs<-readRDS(cbs,file="cbs.rds")
}

#redesign cbs outputs to ease ggplot2 use
cbslong<-data.frame()
#stupid classifier
for(i in 1:4){
  tmp<-bind_rows(cbs$sim[[i]][cbs$stat$minG:cbs$stat$maxG])
  tmp$nbclus<-cbs$stat$minG:cbs$stat$maxG
  tmp$method<-names(cbs$sim)[i]
  cbslong<-rbind(cbslong,tmp)
}
#main methods
for(i in 1:7){
  tmp<-bind_rows(cbs$stat[[i]][cbs$stat$minG:cbs$stat$maxG])
  tmp$nbclus<-cbs$stat$minG:cbs$stat$maxG
  tmp$method<-cbs$stat$name[i]
  cbslong<-rbind(cbslong,tmp)
}
cbslong<-tidyr::pivot_longer(cbslong,avewithin:pamc)
#add info
cbslong<-cbslong%>%mutate(info=ifelse(name%in%c("avewithin","pearsongamma"),"Cluster homogeneity",""),
                          info=ifelse(name%in%c("sindex","widestgap"),"Cluster separation",info))
#test ggplot
stupid<-names(cbs$sim[1:4])
#all
ggplot(cbslong%>%filter(!method%in%stupid),aes(x=nbclus,y=value,color=method))+
  geom_point()+
  geom_point(data=cbslong%>%filter(method%in%stupid),aes(x=nbclus,y=value),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(~name,scale="free")
#selected metric
tmp1<-cbslong%>%filter(!method%in%stupid&info!=""&nbclus<=15)
tmp2<-cbslong%>%filter(method%in%stupid&info!=""&nbclus<=15)
comparbenthos<- ggplot(tmp1,aes(x=nbclus,y=value,color=method),alpha=..5)+
  geom_point()+
  geom_path()+
  geom_violin(data=tmp2,aes(x=nbclus,y=value,group=nbclus),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(info~name,scale="free",ncol=2)+
  theme_minimal()
ggsave(plot= comparbenthos, filename="Aggregation criterion.jpeg", path="results/Communautes bio", width = 13, height = 8)



# 4 BENTHOS Classification with Ward criterion

arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
plot(arbre, which=2, hang=-1, main= "Dendrogramme des taxons benthiques", xlab="")
rect.hclust(arbre, k=4)

group4<- cutree(arbre, k=4) #4 clusters

#how clusters are presented in 2D in the MCA subspace
fviz_mca_ind(rez,repel=T,habillage=as.factor(group4),addEllipses=F,axes=c(1,2))


traitbenthos<- traitbenthos %>% mutate(Cluster= group4)
{
  traitbenthos$Cluster<- sub("1", "5", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("2", "6", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("3", "7", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("4", "8", traitbenthos$Cluster)
}

{
Cluster5<- traitbenthos %>% filter(Cluster==5)
save(Cluster5, file="results/Communautes bio/Communautés/Cluster5.Rdata")
Cluster5<- Cluster5 %>% dplyr::select(-SpeciesName, -Cluster)
Cluster5bis<- pivot_longer(Cluster5, cols=1:7, names_to = "Trait", values_to = "Modalité")
Freq<- as.data.frame(table(Cluster5bis$Modalité))
names(Freq)[1]<- "Modalité"
names(Freq)[2]<- "Fréquence"
Cluster5bis<- Cluster5bis %>% left_join(Freq, by="Modalité")
Cluster5bis<- unique(Cluster5bis)

C5bar<- ggplot(Cluster5bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
  geom_bar(stat="identity")+
  #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
  ggtitle("Caractéristiques de la communauté 5")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 0))+
  theme(axis.text.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.text = element_text(size = 15))
ggsave(plot= C5bar, filename="Cluster5.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)


Cluster6<- traitbenthos %>% filter(Cluster==6)
save(Cluster6, file="results/Communautes bio/Communautés/Cluster6.Rdata")
Cluster6<- Cluster6 %>% dplyr::select(-SpeciesName, -Cluster)
Cluster6bis<- pivot_longer(Cluster6, cols=1:7, names_to = "Trait", values_to = "Modalité")
Freq<- as.data.frame(table(Cluster6bis$Modalité))
names(Freq)[1]<- "Modalité"
names(Freq)[2]<- "Fréquence"
Cluster6bis<- Cluster6bis %>% left_join(Freq, by="Modalité")
Cluster6bis<- unique(Cluster6bis)

C6bar<- ggplot(Cluster6bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
  geom_bar(stat="identity")+
  #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
  ggtitle("Caractéristiques de la communauté 6")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 0))+
  theme(axis.text.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.text = element_text(size = 15))
ggsave(plot= C6bar, filename="Cluster6.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)


Cluster7<- traitbenthos %>% filter(Cluster==7)
save(Cluster7, file="results/Communautes bio/Communautés/Cluster7.Rdata")
Cluster7<- Cluster7 %>% dplyr::select(-SpeciesName, -Cluster)
Cluster7bis<- pivot_longer(Cluster7, cols=1:7, names_to = "Trait", values_to = "Modalité")
Freq<- as.data.frame(table(Cluster7bis$Modalité))
names(Freq)[1]<- "Modalité"
names(Freq)[2]<- "Fréquence"
Cluster7bis<- Cluster7bis %>% left_join(Freq, by="Modalité")
Cluster7bis<- unique(Cluster7bis)

C7bar<- ggplot(Cluster7bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
  geom_bar(stat="identity")+
  #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
  ggtitle("Caractéristiques de la communauté 7")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 0))+
  theme(axis.text.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.text = element_text(size = 15))
ggsave(plot= C7bar, filename="Cluster7.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)


Cluster8<- traitbenthos %>% filter(Cluster==8) 
save(Cluster8, file="results/Communautes bio/Communautés/Cluster8.Rdata")
Cluster8<- Cluster8 %>% dplyr::select(-SpeciesName, -Cluster)
Cluster8bis<- pivot_longer(Cluster8, cols=1:7, names_to = "Trait", values_to = "Modalité")
Freq<- as.data.frame(table(Cluster8bis$Modalité))
names(Freq)[1]<- "Modalité"
names(Freq)[2]<- "Fréquence"
Cluster8bis<- Cluster8bis %>% left_join(Freq, by="Modalité")
Cluster8bis<- unique(Cluster8bis)

C8bar<- ggplot(Cluster8bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
  geom_bar(stat="identity")+
  #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
  ggtitle("Caractéristiques de la communauté 8")+
  theme_minimal()+
  theme(plot.title = element_text(size = 20))+
  theme(axis.title.x = element_text(size = 0))+
  theme(axis.text.x = element_text(size = 20))+
  theme(axis.title.y = element_text(size = 10))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.text = element_text(size = 15))
ggsave(plot= C8bar, filename="Cluster8.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)
}

fviz_mca_biplot(rez, axes = c(1, 2), geom = "text",
                jitter = list(what = "label", width = 20, height = 20),
                habillage=as.factor(group4), col.var="black")




# 5 BENTHOS Cluster densities

    # J2 preparation
J2<- read.csv("data/J2Datras.csv")
J2<- J2 %>% dplyr::select(-X)
Verified<- read.csv("data/DATRAS_taxons_verified.csv", sep=";")
{
  Verified==""
  which(Verified=="", arr.ind = T)
  Verified[which(Verified=="", arr.ind = T)]
  Verified[which(Verified=="", arr.ind = T)]<- NA
}
Verified<- Verified %>% dplyr::select(ScientificName, ScientificName_accepted)
Verified$ScientificName<- as.character(Verified$ScientificName)
Verified$ScientificName_accepted<- as.character(Verified$ScientificName_accepted)
Verified$ScientificName_accepted[is.na(Verified$ScientificName_accepted)]<-	Verified$ScientificName[is.na(Verified$ScientificName_accepted)]

J2<- J2 %>% left_join(Verified, by=c("ScientificName_WoRMS"="ScientificName")) %>% dplyr::select(-ScientificName_WoRMS)

    # Jointure
Lala<- J2 %>% left_join(traitbenthos, by=c("ScientificName_accepted"="SpeciesName")) %>% filter(!is.na(Cluster))

Dens<- Lala %>% dplyr::select(Year, moyLong, moyLat, Cluster, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= sum(Nombre), Sup = unique(Superficie), DensNb= Nb/Sup) %>% 
  ungroup() # Densities Nb/km2

Dens<- Dens %>% dplyr::select(-c(Nb, Sup))
Dens<- unique(Dens)

{
  Cluster5<- Dens %>% filter(Cluster==5)
  Cluster6<- Dens %>% filter(Cluster==6)
  Cluster7<- Dens %>% filter(Cluster==7)
  Cluster8<- Dens %>% filter(Cluster==8)
  }


save(Dens, file="data/Densbenthos.Rdata")





# 1 FISH loading and tab preparation

load("data/Traitfish.Rdata")

    # Imputation

traitfish1<- mice::mice(traitfish, m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
traitfish<- mice::complete(traitfish1)

    # Categorize numerical values
catvar<- function(a,value){
  value<- c(0,as.vector(value))
  print(value)
  lval<- length(value)
  interval<- paste0("[",round(value[-lval],3),",",round(value[-1],3),"[")
  a1<- interval[findInterval(a,value,all.inside=T)]
  return(a1)
}

traitfish$tl<-                 catvar(traitfish$tl, c(1:5))
traitfish$MaturityAge<-        catvar(traitfish$MaturityAge, c(2,3,4,5,14))
traitfish$growth.coefficient<- catvar(traitfish$growth.coefficient, c(.1,.2,.3,.4,.5,2))
traitfish$length.max<-         catvar(traitfish$length.max, c(50,100,200))
traitfish$age.max<-            catvar(traitfish$age.max, c(5,10,20,60))

traitfish<- unique(traitfish)
traitfish<- traitfish[!duplicated(traitfish$Taxons),]


# 2 FISH ACM on Burt table
traitfish1<- data.frame(traitfish[,-1])
row.names(traitfish1)<- traitfish[,1]

rez1<- MCA(traitfish1, ncp=999, method="Burt", graph=F)
plt3<-plotellipses(rez1, axes=c(1,2))
plt4<-plotellipses(rez1, axes=c(1,3))

print(plt3)
print(plt4)



# 3 FISH classification methods comparison

#then Hennig, C. (2017) Cluster validation by measurement of clustering
#characteristics relevant to the user. In C. H. Skiadas (ed.) Proceedings of
#ASMDA 2017, 501-520, https://arxiv.org/abs/1703.09282
# using clusterbenchmark from fpc
if(!file.exists("cbsfish.rds")){
  set.seed(666) #the number of the beaaasttt
  options(digits=3)
  clustermethod=c("kmeansCBI","hclustCBI","hclustCBI","hclustCBI","hclustCBI","hclustCBI","claraCBI")
  clustermethodpars <- list()
  clustermethodpars[[2]] <- clustermethodpars[[3]] <- list()
  clustermethodpars[[4]] <-  clustermethodpars[[5]] <-list()
  clustermethodpars[[6]]<-   clustermethodpars[[7]] <-list()
  clustermethodpars[[2]]$method <- "ward.D2"
  clustermethodpars[[3]]$method <- "single"
  clustermethodpars[[4]]$method <- "complete"
  clustermethodpars[[5]]$method <- "average"
  clustermethodpars[[6]]$method <- "mcquitty"
  #clustermethodpars[[7]]$method <- ""
  methodname <- c("kmeans","ward","single","complete","average","mcquitty","clara")
  cbs <-  clusterbenchstats(rez1$ind$coord,G=2:20,
                            clustermethod=clustermethod,scaling=FALSE,
                            methodname=methodname,
                            distmethod=rep(FALSE,length(clustermethod)),
                            clustermethodpars=clustermethodpars,nnruns=100,kmruns=100,
                            fnruns=100,avenruns=100,multicore=FALSE,trace=F)
  saveRDS(cbs,file="cbsfish.rds")
}else{
  cbs<-readRDS(cbs,file="cbsfish.rds")
}

#redesign cbs outputs to ease ggplot2 use
cbslong<-data.frame()
#stupid classifier
for(i in 1:4){
  tmp<-bind_rows(cbs$sim[[i]][cbs$stat$minG:cbs$stat$maxG])
  tmp$nbclus<-cbs$stat$minG:cbs$stat$maxG
  tmp$method<-names(cbs$sim)[i]
  cbslong<-rbind(cbslong,tmp)
}
#main methods
for(i in 1:7){
  tmp<-bind_rows(cbs$stat[[i]][cbs$stat$minG:cbs$stat$maxG])
  tmp$nbclus<-cbs$stat$minG:cbs$stat$maxG
  tmp$method<-cbs$stat$name[i]
  cbslong<-rbind(cbslong,tmp)
}
cbslong<-tidyr::pivot_longer(cbslong,avewithin:pamc)
#add info
cbslong<-cbslong%>%mutate(info=ifelse(name%in%c("avewithin","pearsongamma"),"Cluster homogeneity",""),
                          info=ifelse(name%in%c("sindex","widestgap"),"Cluster separation",info))
#test ggplot
stupid<-names(cbs$sim[1:4])
#all
ggplot(cbslong%>%filter(!method%in%stupid),aes(x=nbclus,y=value,color=method))+
  geom_point()+
  geom_point(data=cbslong%>%filter(method%in%stupid),aes(x=nbclus,y=value),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(~name,scale="free")
#selected metric
tmp1<-cbslong%>%filter(!method%in%stupid&info!=""&nbclus<=15)
tmp2<-cbslong%>%filter(method%in%stupid&info!=""&nbclus<=15)
comparfish<- ggplot(tmp1,aes(x=nbclus,y=value,color=method),alpha=..5)+
  geom_point()+
  geom_path()+
  geom_violin(data=tmp2,aes(x=nbclus,y=value,group=nbclus),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(info~name,scale="free",ncol=2)+
  theme_minimal()
ggsave(plot= comparfish, filename="Aggregation criterion fish.jpeg", path="results/Communautes bio", width = 13, height = 8)




# 4 FISH Classification with Ward criterion

arbre1<- agnes(rez1$ind$coord, method="ward", par.method=1)
plot(arbre1, which=2, hang=-1)
rect.hclust(arbre1, k=4)

group41<- cutree(arbre1,k=4) #4 clusters

#how clusters are presented in 2D in the MCA subspace
fviz_mca_ind(rez1,repel=T, habillage=as.factor(group41), addEllipses=F, axes=c(1,2))

traitfish<- traitfish %>% mutate(Cluster= group41)


{
  Cluster1<- traitfish %>% filter(Cluster==1)
  save(Cluster1, file="results/Communautes bio/Communautés/Cluster1.Rdata")
  Cluster1<- Cluster1 %>% dplyr::select(-Taxons, -Cluster)
  Cluster1bis<- pivot_longer(Cluster1, cols=1:8, names_to = "Trait", values_to = "Modalité")
  Freq<- as.data.frame(table(Cluster1bis$Modalité))
  names(Freq)[1]<- "Modalité"
  names(Freq)[2]<- "Fréquence"
  Cluster1bis<- Cluster1bis %>% left_join(Freq, by="Modalité")
  Cluster1bis<- unique(Cluster1bis)
  
  C1bar<- ggplot(Cluster1bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
    geom_bar(stat="identity")+
    #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
    ggtitle("Caractéristiques de la communauté 1")+
    theme_minimal()+
    theme(plot.title = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 0))+
    theme(axis.text.x = element_text(size = 20))+
    theme(axis.title.y = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.text = element_text(size = 15))
  ggsave(plot= C1bar, filename="Cluster1.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)
  
  
  Cluster2<- traitfish %>% filter(Cluster==2) 
  save(Cluster2, file="results/Communautes bio/Communautés/Cluster2.Rdata")
  Cluster2<- Cluster2 %>% dplyr::select(-Taxons, -Cluster)
  Cluster2bis<- pivot_longer(Cluster2, cols=1:8, names_to = "Trait", values_to = "Modalité")
  Freq<- as.data.frame(table(Cluster2bis$Modalité))
  names(Freq)[1]<- "Modalité"
  names(Freq)[2]<- "Fréquence"
  Cluster2bis<- Cluster2bis %>% left_join(Freq, by="Modalité")
  Cluster2bis<- unique(Cluster2bis)
  
  C2bar<- ggplot(Cluster2bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
    geom_bar(stat="identity")+
    #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
    ggtitle("Caractéristiques de la communauté 2")+
    theme_minimal()+
    theme(plot.title = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 0))+
    theme(axis.text.x = element_text(size = 20))+
    theme(axis.title.y = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.text = element_text(size = 15))
  ggsave(plot= C2bar, filename="Cluster2.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)
  
  
  Cluster3<- traitfish %>% filter(Cluster==3)
  save(Cluster3, file="results/Communautes bio/Communautés/Cluster3.Rdata")
  Cluster3<- Cluster3 %>% dplyr::select(-Taxons, -Cluster)
  Cluster3bis<- pivot_longer(Cluster3, cols=1:8, names_to = "Trait", values_to = "Modalité")
  Freq<- as.data.frame(table(Cluster3bis$Modalité))
  names(Freq)[1]<- "Modalité"
  names(Freq)[2]<- "Fréquence"
  Cluster3bis<- Cluster3bis %>% left_join(Freq, by="Modalité")
  Cluster3bis<- unique(Cluster3bis)
  
  C3bar<- ggplot(Cluster3bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
    geom_bar(stat="identity")+
    #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
    ggtitle("Caractéristiques de la communauté 3")+
    theme_minimal()+
    theme(plot.title = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 0))+
    theme(axis.text.x = element_text(size = 20))+
    theme(axis.title.y = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.text = element_text(size = 15))
  ggsave(plot= C3bar, filename="Cluster3.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)
  
  
  Cluster4<- traitfish %>% filter(Cluster==4)
  save(Cluster4, file="results/Communautes bio/Communautés/Cluster4.Rdata")
  Cluster4<- Cluster4 %>% dplyr::select(-Taxons, -Cluster)
  Cluster4bis<- pivot_longer(Cluster4, cols=1:8, names_to = "Trait", values_to = "Modalité")
  Freq<- as.data.frame(table(Cluster4bis$Modalité))
  names(Freq)[1]<- "Modalité"
  names(Freq)[2]<- "Fréquence"
  Cluster4bis<- Cluster4bis %>% left_join(Freq, by="Modalité")
  Cluster4bis<- unique(Cluster4bis)
  
  C4bar<- ggplot(Cluster4bis, aes(x=Trait, y=Fréquence, fill=Modalité)) +
    geom_bar(stat="identity")+
    #geom_text(aes(label=Modalité), color="white", size=5, position=position_identity())+
    ggtitle("Caractéristiques de la communauté 4")+
    theme_minimal()+
    theme(plot.title = element_text(size = 20))+
    theme(axis.title.x = element_text(size = 0))+
    theme(axis.text.x = element_text(size = 20))+
    theme(axis.title.y = element_text(size = 10))+
    theme(axis.text.y = element_text(size = 10))+
    theme(axis.text.x = element_text(angle=90))+
    theme(legend.text = element_text(size = 15))
  ggsave(plot= C4bar, filename="Cluster4.jpeg", path="results/Communautes bio/Communautés", width = 13, height = 8)
}

fviz_mca_biplot(rez1, axes = c(1, 2), geom.ind = "point", geom.var = "text",
                jitter = list(what = "label", width = 20, height = 20),
                habillage=as.factor(group41), col.var="black", addEllipses = TRUE)





# 5 FISH Cluster densities

    # Jointure
Lele<- J2 %>% left_join(traitfish, by=c("ScientificName_accepted"="Taxons")) %>% filter(!is.na(Cluster))

Dens1<- Lele %>% dplyr::select(Year, moyLong, moyLat, Cluster, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= sum(Nombre), Sup= unique(Superficie), DensNb= Nb/Sup) %>% 
  ungroup() # Densities Nb/km2

Dens1<- Dens1 %>% dplyr::select(-c(Nb, Sup))
Dens1<- unique(Dens1)

{
  Cluster1<- Dens %>% filter(Cluster==1)
  Cluster2<- Dens %>% filter(Cluster==2)
  Cluster3<- Dens %>% filter(Cluster==3)
  Cluster4<- Dens %>% filter(Cluster==4)
}
save(Dens1, file="data/Densfish.Rdata")




# Cephalopodes

load("Traitceph.Rdata")

  # Densities
Lili<- J2 %>% left_join(traitceph, by=c("ScientificName_accepted"="Taxons"))

Densceph<- Lili %>% dplyr::select(Year, moyLong, moyLat, Nombre, Superficie) %>% 
  group_by(Year, moyLat, moyLong) %>% 
  summarize(Nb= sum(Nombre), Sup= sum(Superficie), DensNb= Nb/Sup) %>% 
  ungroup() # Densities Nb/km2

Densceph<- Densceph %>% mutate(Cluster=9)
Densceph<- Densceph %>% dplyr::select(-c(Nb, Sup))
Densceph<- unique(Densceph)

save(Densceph, file="data/Densceph.Rdata")




# Tableau densites tous clusters

Dens1$Cluster<- as.character(Dens1$Cluster)
Densceph$Cluster<- as.character(Densceph$Cluster)
Denstot<- bind_rows(Dens1, Dens, Densceph)

save(Denstot, file="data/Denstot.Rdata")




# Visualisation des clusters avec leurs traits

{
load("results/Communautes bio/Communautés/Cluster1.Rdata")
load("results/Communautes bio/Communautés/Cluster2.Rdata")
load("results/Communautes bio/Communautés/Cluster3.Rdata")
load("results/Communautes bio/Communautés/Cluster4.Rdata")
load("results/Communautes bio/Communautés/Cluster5.Rdata")
load("results/Communautes bio/Communautés/Cluster6.Rdata")
load("results/Communautes bio/Communautés/Cluster7.Rdata")
load("results/Communautes bio/Communautés/Cluster8.Rdata")
}







