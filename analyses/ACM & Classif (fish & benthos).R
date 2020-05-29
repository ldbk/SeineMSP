setwd("C:/Users/jrivet/Documents/Stage M2/SeineMSP/data")
library(dplyr)
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

load("Traitbenthos.Rdata")

    # Imputation

traitbenthos1<- mice::mice(traitbenthos, m=1)#,defaultMethod=c("pmm","rf","rf","rf"))
traitbenthos<-mice::complete(traitbenthos1)

    # Categorize numerical values
{
  traitbenthos$MaturityAge<- as.character(traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("< 1 year", "[0;2[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("<1 year", "[0;2[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("1 year", "[0;2[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("1-2 years", "[0;2[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("2-3 years", "[2;6[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("3-5 years", "[2;6[", traitbenthos$MaturityAge)
  traitbenthos$MaturityAge<- sub("6-10 years", "[6;10[", traitbenthos$MaturityAge)
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
row.names(traitbenthos1)<- traitbenthos$Taxons

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
                            fnruns=100,avenruns=100,multicore=TRUE,trace=F)
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
ggplot(tmp1,aes(x=nbclus,y=value,color=method),alpha=..5)+
  geom_point()+
  geom_path()+
  geom_violin(data=tmp2,aes(x=nbclus,y=value,group=nbclus),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(info~name,scale="free",ncol=2)



# 4 BENTHOS Classification with Ward criterion

arbre<- agnes(rez$ind$coord, method="ward", par.method=1)
plot(arbre, which=2, hang=-1)
rect.hclust(arbre, k=4)

group4<- cutree(arbre, k=4) #4 clusters
group7<- cutree(arbre, k=7) #7 clusters

#how clusters are presented in 2D in the MCA subspace
fviz_mca_ind(rez,repel=T,habillage=as.factor(group4),addEllipses=F,axes=c(1,2))
fviz_mca_ind(rez,repel=T,habillage=as.factor(group7),addEllipses=F,axes=c(1,2))


traitbenthos<- traitbenthos %>% mutate(Cluster= group4)
{
  traitbenthos$Cluster<- sub("1", "5", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("2", "6", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("3", "7", traitbenthos$Cluster)
  traitbenthos$Cluster<- sub("4", "8", traitbenthos$Cluster)
}



# 5 BENTHOS Cluster densities

    # J2 preparation
J2<- read.csv("J2Datras.csv")
J2<- J2 %>% dplyr::select(Year, ScientificName_WoRMS, Poids, Nombre, Superficie, moyLat, moyLong)
Verified<- read.csv("DATRAS_taxons_verified.csv", sep=";")
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
Lala<- J2 %>% left_join(traitbenthos, by=c("ScientificName_accepted"="Taxons")) %>% filter(!is.na(Cluster))

Dens<- Lala %>% dplyr::select(Year, moyLong, moyLat, Cluster, Poids, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= sum(Nombre), Wgt= sum(Poids), Sup= sum(Superficie), DensNb= Nb/Sup, DensWgt= Wgt/Sup) %>% 
  ungroup() # Densities Nb/km2 & kg/km2


{
  Cluster5<- Dens %>% filter(Cluster==5)
  Cluster6<- Dens %>% filter(Cluster==6)
  Cluster7<- Dens %>% filter(Cluster==7)
  Cluster8<- Dens %>% filter(Cluster==8)
  }








# 1 FISH loading and tab preparation

load("Traitfish.Rdata")

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



# 2 FISH ACM on Burt table
traitfish1<- data.frame(traitfish[,-1])
row.names(traitfish1)<- traitfish$Taxons

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
  cbs <-  clusterbenchstats(rez1$ind$coord,G=2:20,
                            clustermethod=clustermethod,scaling=FALSE,
                            methodname=methodname,
                            distmethod=rep(FALSE,length(clustermethod)),
                            clustermethodpars=clustermethodpars,nnruns=100,kmruns=100,
                            fnruns=100,avenruns=100,multicore=TRUE,trace=F)
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
ggplot(tmp1,aes(x=nbclus,y=value,color=method),alpha=..5)+
  geom_point()+
  geom_path()+
  geom_violin(data=tmp2,aes(x=nbclus,y=value,group=nbclus),color="grey",alpha=.2)+
  geom_point()+
  facet_wrap(info~name,scale="free",ncol=2)



# 4 FISH Classification with Ward criterion

arbre1<- agnes(rez1$ind$coord, method="ward", par.method=1)
plot(arbre1, which=2, hang=-1)
rect.hclust(arbre1, k=4)

group41<- cutree(arbre1,k=4) #4 clusters
group71<- cutree(arbre1,k=7) #7 clusters

#how clusters are presented in 2D in the MCA subspace
fviz_mca_ind(rez1,repel=T, habillage=as.factor(group41), addEllipses=F, axes=c(1,2))
fviz_mca_ind(rez1,repel=T, habillage=as.factor(group71), addEllipses=F, axes=c(1,2))

traitfish<- traitfish %>% mutate(Cluster= group41)



# 5 FISH Cluster densities

    # Jointure
Lele<- J2 %>% left_join(traitfish, by=c("ScientificName_accepted"="Taxons")) %>% filter(!is.na(Cluster))

Dens1<- Lele %>% dplyr::select(Year, moyLong, moyLat, Cluster, Poids, Nombre, Superficie) %>% 
  group_by(Cluster, Year, moyLat, moyLong) %>% 
  summarize(Nb= sum(Nombre), Wgt= sum(Poids), Sup= sum(Superficie), DensNb= Nb/Sup, DensWgt= Wgt/Sup) %>% 
  ungroup() # Densities Nb/km2 & kg/km2


{
  Cluster1<- Dens %>% filter(Cluster==1)
  Cluster2<- Dens %>% filter(Cluster==2)
  Cluster3<- Dens %>% filter(Cluster==3)
  Cluster4<- Dens %>% filter(Cluster==4)
}






