Data <- read.csv("Data-projetmodIA-2324.csv")

summary(Data)

polluants2019 <- Data[1:164, 4:14]


#boxplot(polluants2019)

boxplot(scale(log(polluants2019)))
title("2019")


polluants2014 <- Data[822:984, 4:14]

boxplot(scale(log(polluants2014)))
title("2014")
















