
#recherche préliminair Alexandre Demarquet 29/09 1ere séance
Data<-read.csv('Data-projetmodIA-2324.csv')
head(Data)

summary(Data)
data_quant <- Data[,3:14]
head(data_quant)
data_quant_scaled <- scale(log(data_quant))
head(data_quant_scaled)
boxplot(data_quant_scaled)
qqnorm(data_quant_scaled)
qqline(data_quant_scaled)
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
hist(data_scaled_df$nox_kg)
# La transformation logarithmique peut faciliter l'interprétation des valeurs, en particulier lorsque les données présentent des variations importantes et que la standardisation ne suffit pas à rendre la distribution plus normale.

#dans rapport expliquier l'interet de la transformation en log des données et montré un exemple classique puis mettre un chunk de R pas compile pour montrer l'interet sur les autres variabables