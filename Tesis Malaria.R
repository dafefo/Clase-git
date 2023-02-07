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

