Data <- read.csv("Data-projetmodIA-2324.csv")

polluants2019 <- Data[1:164, 4:14]


#boxplot(polluants2019)              #Boxplot des émisssions sans transformation en 2019

boxplot(log(scale(polluants2019)))           #Boxplot des émisssions sans transformation en 2019
title("2019")

polluants2014 <- Data[822:984, 4:14]

boxplot(log(scale(polluants2014)))
title("2014")
















