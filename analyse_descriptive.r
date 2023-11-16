
#recherche préliminair Alexandre Demarquet 29/09 1ere séance


install.packages("corrplot")
library(corrplot)
install.packages("factoextra")  # Si vous ne l'avez pas encore installé
library(factoextra)

install.packages("FactoMineR")  # Installer le package FactoMineR
library(FactoMineR)  # Charger le package FactoMineR



Data<-read.csv('Data-projetmodIA-2324.csv')
head(Data)

summary(Data)

data_quant <- Data[,3:14]
data_quant_scaled <- scale(log(data_quant))

#head(data_quant)

#head(data_quant_scaled)
#boxplot(data_quant_scaled)
#qqnorm(data_quant_scaled)
#qqline(data_quant_scaled)
hist(data_quant_scaled)
table(data_quant_scaled)
cor(data_quant_scaled)
variable.names(data_quant_scaled)
str(data_quant_scaled)
data_scaled_df <- as.data.frame(data_quant_scaled)
boxplot(data_scaled_df)
pairs(data_scaled_df)
pairs(data_scaled_df[,1:2])
mat_cor <- cor(data_scaled_df)
corrplot(mat_cor,method="ellipse")
corrplot(mat_cor,method="shade")


# La transformation logarithmique peut faciliter l'interprétation des valeurs, en particulier lorsque les données présentent des variations importantes et que la standardisation ne suffit pas à rendre la distribution plus normale.

#dans rapport expliquier l'interet de la transformation en log des données et montré un exemple classique puis mettre un chunk de R pas compile pour montrer l'interet sur les autres variabables



acp_result <- prcomp(data_scaled_df, scale. = TRUE)

# Extraction des composantes principales
acp_data <- as.data.frame(acp_result$x)
fviz_pca_ind(acp_result, col.ind = "cos2", col.var = "contrib", pointsize = 2, repel = TRUE) #qualité de la représentation des http://127.0.0.1:38011/graphics/plot_zoom_png?width=1536&height=792variables sur les composantes principales.


hist(data_scaled_df$nox_kg)
# La transformation logarithmique peut faciliter l'interprétation des valeurs, en particulier lorsque les données présentent des variations importantes et que la standardisation ne suffit pas à rendre la distribution plus normale.

#dans rapport expliquier l'interet de la transformation en log des données et montré un exemple classique puis mettre un chunk de R pas compile pour montrer l'interet sur les autres variabables


#res.acp <- PCA(data_scaled_df,scale.unit = FALSE, graph = FALSE)
# Supposons que votre colonne d'année s'appelle "Year" dans votre jeu de données
#plot(res.acp, col.hab  = data_scaled_df$annee_inv)#res.acp$ind$coord)#, col = data_scaled_df$annee_inv, pch = 19, main = "ACP avec Couleurs par Année")
#legend("topright", legend = unique(data_scaled_df$annee_inv), col = unique(data_scaled_df$annee_inv), pch = 19)



############################ ACP des individus avec affichage avec les années et le type d'EPCI
library(ggfortify)
df=data_quant_scaled[,2:12]
pca_res <- prcomp(df)
annee=Data[,3]
df_bis=cbind(annee,df)
col_type=Data[,2]
for (i in 1:length(col_type)){
  col_type[i]=substr(col_type[i], 1, 2)
}
df_bis=cbind(col_type,df_bis)
autoplot(pca_res, data =df_bis, colour = 'col_type') #avec couleur pour chaque type
autoplot(pca_res, data =df_bis, colour = 'annee') #couleur pour chaque année



