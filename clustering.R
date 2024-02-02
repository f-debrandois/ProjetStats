Data<-read.csv('Data-projetmodIA-2324.csv')

library(ggfortify)
library(Matrix)
library(mclust)
library(cluster)
library(factoextra)
library(FactoMineR)
library(ppclust)
library(reticulate)
library(ggplot2)
library(reshape)
library(corrplot)
library(gridExtra)
library(circlize)
library(viridis)
library(reshape2)
library(klaR)
library(mclust)
library(factoextra)
library(FactoMineR)
library(seriation)
library(clusterSim)

enlever_donnee_aber <- function(data_frame, columns) {
  # Définir le facteur d'échelle interquartile (IQR)
  iqr_factor <- 1.5
  
  # Appliquer la règle des quantiles pour chaque colonne
  for (k in seq(1, 11)) {
    # Calculer les quantiles
    q1 <- quantile(data_frame[,k], 0.15)
    q3 <- quantile(data_frame[,k], 0.85)
    # Calculer l'IQR
    iqr <- q3 - q1
    # Calculer les limites
    lower_limit <- q1 - iqr_factor * iqr
    upper_limit <- q3 + iqr_factor * iqr
    # Supprimer les outliers
    data_frame <- data_frame[data_frame[,k] >= lower_limit & data_frame[,k] <= upper_limit, ]
  }
  return(data_frame)
}


##k-means

# On détermine combien de centres choisir 

Data_kmeans <- scale(log(Data[4:14]))  # Nos données
Data_kmeans <- enlever_donnee_aber(Data_kmeans, colnames(Data_kmeans))


Kmax<-15
reskmeanscl<-matrix(0,nrow=nrow(Data_kmeans),ncol=Kmax-1)
Iintra<-NULL
for (k in 2:Kmax){
  resaux<-kmeans(Data_kmeans, centers=k)
  reskmeanscl[,k-1]<-resaux$cluster
  Iintra<-c(Iintra,resaux$tot.withinss)
}

df<-data.frame(K=2:15,Iintra=Iintra)
ggplot(df,aes(x=K,y=Iintra))+geom_line()+geom_point()+xlab("Nombre de classes")+ylab("Inertie intraclasse")

# On choisit K = 5

reskmeans <- kmeans(Data_kmeans,centers=5,nstart =20)

table(reskmeans$cluster)
fviz_cluster(reskmeans,data=Data_kmeans,ellipse.type = "norm",labelsize=8,geom=c("point"))+ggtitle("")

pca_res <- prcomp(Data_kmeans, scale. = TRUE)
#fviz_pca_ind(pca_res)

# Avec le critère de sélection silouhette 

Silhou<-NULL
for (k in 2:Kmax){
  aux<-silhouette(reskmeanscl[,k-1],daisy(Data_kmeans))
  Silhou<-c(Silhou,mean(aux[,3]))
}
df<-data.frame(K=2:Kmax,Silhouette=Silhou)
ggplot(df,aes(x=K,y=Silhouette))+
  geom_point()+
  geom_line()+theme(legend.position = "bottom")

aux<-silhouette(reskmeanscl[,1],daisy(Data_kmeans))
fviz_silhouette(aux)+theme(plot.title = element_text(size =9))
rm(df,Silhou,aux)

#Classification HIérarchique CAH

Data_CAH <- scale(log(Data[4:14]))
Data_CAH <- enlever_donnee_aber(Data_CAH, colnames(Data_CAH))

d=dist(x = Data_CAH ,method = "euclidian")
#hclustsingle<-hclust(d,method = "single")
#hclustcomplete<-hclust(d,method = "complete")
#hclustaverage<-hclust(d,method = "average")

#fviz_dend(hclustsingle,show_labels=FALSE)
#fviz_dend(hclustcomplete,show_labels=FALSE)
#fviz_dend(hclustaverage,show_labels=FALSE)

#single = dendogramme en escalier, c'est mauvais -> tendance à l'agrégation 
#complete = dendogramme équilibré 
#average = dendogramme plutôt équilibré mais à tendance 

hward<-hclust(d,method = "ward.D2")
fviz_dend(hward,show_labels=FALSE)

#hward forme des ecaliers ici et tend à l'agrégation

#critère de sélection
CH<-NULL
Kmax<-20
for (k in 2:Kmax){
  CH<-c(CH,index.G1 (Data_CAH,cutree(hward,k=k)))
}
daux<-data.frame(NbClust=2:Kmax,CH=CH)
ggplot(daux,aes(x=NbClust,y=CH))+geom_line()+geom_point()

ClustCH<-cutree(hward,k=2)
fviz_dend(hward,k=which.max(CH)+1,show_labels=FALSE,rect = TRUE, rect_fill = TRUE,palette = "npg",rect_border = "npg",
          labels_track_height = 0.8)

#silhouette

S<-NULL
Kmax<-20
for (k in 2:Kmax){
  S<-c(S,index.S (d,cutree(hward,k=k)))
}
daux<-data.frame(NbClust=2:Kmax,silhou=S)
ggplot(daux,aes(x=NbClust,y=S))+geom_line()+geom_point()

ClustS<-cutree(hward,k=which.max(S)+1)
fviz_dend(hward,k=which.max(S)+1,show_labels=FALSE,rect = TRUE, rect_fill = TRUE,palette = "npg",rect_border = "npg",
          labels_track_height = 0.8)

# mélange (en janvier)


Data_melG <- scale(log(Data[4:14]))
Data_melG <- enlever_donnee_aber(Data_melG, colnames(Data_melG))

resBIC<-Mclust(Data_melG,G=2:50)
fviz_mclust_bic(resBIC,what=c("classification","uncertainty","BIC"))
summary(resBIC)

# On peut écarter tous les modèles sphériques et diagonaux, et donc les écarter du modèle.
# VEV est toujours en haut, on se concentre donc sur celui-ci :

resBIC_VEV<-Mclust(Data_melG,G=2:50, modelNames = c("EEE", "VEE", "EVE", "VVE", "EEV", "VEV", "EVV", "VVV"))
fviz_mclust_bic(resBIC_VEV,what=c("classification","uncertainty","BIC"))
summary(resBIC_VEV)

pca_res <- prcomp(Data_melG, scale. = TRUE)
pca_coordinates <- pca_res$x

#resBIC_onlyVEV<-Mclust(pca_coordinates,G=2:150, modelNames = "VEV")
#fviz_mclust_bic(resBIC_onlyVEV,what=c("classification","uncertainty","BIC"))
#summary(resBIC_VEV)

pca_res <- prcomp(Data_melG, scale. = TRUE)
pca_coordinates <- pca_res$x

resICL<-mclustICL(pca_coordinates,G=2:100, modelNames="VEV")
summary(resICL)
k=2:100
data_test = data.frame(VEV=resICL[,1])
ggplot(data_test, aes(x=k,y=resICL[,1]))+geom_line()



