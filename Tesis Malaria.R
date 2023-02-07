library(readr)
Datos_Malaria_Pacifico1 <- read_csv("C:/Users/dafef/Dropbox/data_malaria_mining/data/Datos_Malaria_Pacifico1.csv")

unique(Datos_Malaria_Pacifico1$departamento)
unique(Datos_Malaria_Pacifico1$ano)
any(is.na(Datos_Malaria_Pacifico1))
which(is.na(Datos_Malaria_Pacifico1))
Datos_Malaria_Pacifico1 = Datos_Malaria_Pacifico1[-27]


summary(Datos_Malaria_Pacifico1)

hist(Datos_Malaria_Pacifico1$areaprilegal_sqkm)
hist(Datos_Malaria_Pacifico1$casos)
hist(Datos_Malaria_Pacifico1$total_population_DANE) 

library(ggplot2)
library(dplyr)
#Plot de casos para Choco
choco <- Datos_Malaria_Pacifico1%>%filter(cod_departamento==27)
ggplot(choco, aes(x=ano, y = casos))+ geom_point() +
  labs(x = "Año", 
       y = "Casos", 
       title = "Casos de malaria en el Choco")

