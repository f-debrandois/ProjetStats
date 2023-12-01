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
library(dbscan)
library(seriation)


##k-means

# On détermine combien de centres choisir 

Data_kmeans <- scale(log(Data[4:14]))  # Nos données

Kmax<-15
reskmeanscl<-matrix(0,nrow=nrow(Data),ncol=Kmax-1)
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
fviz_pca_ind(pca_res)

# Avec le critère de sélection silouhette 

Silhou<-NULL
for (k in 2:Kmax){
  aux<-silhouette(reskmeanscl[,k-1], daisy(Data[,4:14]))
  Silhou<-c(Silhou,mean(aux[,3]))
}

df<-data.frame(K=2:Kmax,Silhouette=Silhou)
ggplot(df,aes(x=K,y=Silhouette))+
  geom_point()+
  geom_line()+theme(legend.position = "bottom")

aux<-silhouette(reskmeanscl[,k-1], daisy(Data[,4:14]))
fviz_silhouette(aux)+theme(plot.title = element_text(size =9))
rm(df,Silhou,aux)


##PAM

Data_PAM <- scale(log(Data[4:14]))

Kmax<-15
resPAMcl<-matrix(0,nrow=nrow(Data),ncol=Kmax-1)
Silhou<-NULL
for (k in 2:Kmax){
  resaux<-pam(Data_PAM,k,metric="euclidean")
  resPAMcl[,k-1]<-resaux$clustering
  aux<-silhouette(resPAMcl[,k-1], daisy(Data[,4:14]))
  Silhou<-c(Silhou,mean(aux[,3]))
}

df<-data.frame(K=2:Kmax,Silhouette=Silhou)
ggplot(df,aes(x=K,y=Silhouette))+
  geom_point()+
  geom_line()+theme(legend.position = "bottom")


aux<-silhouette(resPAMcl[,1], daisy(Data[,4:14]))
fviz_silhouette(aux)+theme(plot.title = element_text(size =9))

adjustedRandIndex(resPAMcl[,1], reskmeanscl[,3])
table(resPAMcl[,1], reskmeanscl[,3])

## fuzzy c-means ?


# mélange (en janvier)

