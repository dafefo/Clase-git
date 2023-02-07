#INLA Models
install.packages("raster")
install.packages("sf")
install.packages("sp")
install.packages("dplyr")
install.packages("rgdal")
install.packages("MASS")
install.packages("Metrics")
install.packages("hydroGOF")
install.packages("INLAutils")
install.packages("spdep")
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#Packages
library(raster)
library("INLA")
library(INLA)
library(sf)
library(sp)
library(dplyr)
library(rgdal)
library(MASS)
library(Metrics)
library(hydroGOF)
#library("INLAutils")
library(spdep)
library(readr)

#Loading the data
DataMalaria <- read_csv("C:/Users/dafef/Dropbox/data_malaria_mining/data/Datos_Malaria_Pacifico1.csv")
DataMalaria <- DataMalaria[-1]
DataMalaria <- DataMalaria[-1]
DataMalaria <- DataMalaria[-28]
DataMalaria <- DataMalaria[-27]

summary(DataMalaria)
DataMalaria$nan <- 0
#Remove municipio with na population
for (i in 1:nrow(DataMalaria)){
  if (any(is.na(DataMalaria[i,]))){
    DataMalaria$nan[i] <- 1
  }
}
DataMalaria <- DataMalaria%>%filter(cod_municipio!=27086)

### Define neighbourhood matrix
Colombia <- rgdal::readOGR("C:/Users/dafef/Dropbox/data_malaria_mining/codes/Shapefile Municipios/01_shp_colombia_municipio_full.shp")


Pacifico <- subset(Colombia, Colombia$DPTO_CCDGO == "19"|Colombia$DPTO_CCDGO == "27"|Colombia$DPTO_CCDGO== "52" | Colombia$DPTO_CCDGO=="76")

plot(Pacifico)
nb.map <- poly2nb(Pacifico)
nb2INLA("map.graph", nb.map)

#Malarian Cases
MalarianCases <- DataMalaria$casos

# Add API so modify offset to include it 
e  <- DataMalaria$total_population_DANE

# Climate variables
t_mean <- DataMalaria$t_mean 
precip <- DataMalaria$total_precip 

# Land use variables
mining <- DataMalaria$areaprilegal_sqkm
#forest_decrease <- scale(data$deforestation_lag0)[,1]
#urbanization    <- scale(data$urban_increase_cum)[,1]

# Spatial random effects
s1 <- rep(1:178, 108)
Departamentos <- as.factor(DataMalaria$cod_departamento)
Municipios <- as.factor(DataMalaria$cod_municipio)

# Monthly/seasonal random effects
t1 <- rep(rep(1:12, each = 178),9)
# Yearly random effects
t2 <- rep(2006:2014, each = 2136)

# Cut data with median mines 
DataMalaria$mining_cat[DataMalaria$areaprilegal_sqkm <= mean(DataMalaria$areaprilegal_sqkm)] <- 1 # low mining
DataMalaria$mining_cat[DataMalaria$areaprilegal_sqkm > mean(DataMalaria$areaprilegal_sqkm)] <- 1 # high mining

mining_cat <- DataMalaria$mining_cat

df_inla <- data.frame(MalarianCases,mining, e,
                      Departamentos, Municipios,
                      s1, t1, t2,
                      t_mean, precip, mining_cat)

INLAmodel = function(formula, data = df_inla, family = "nbinomial", config = FALSE)
  
{
  model = inla(formula = formula, data = data, family = family, offset = log(e),
               control.inla = list(strategy = 'adaptive'),
               control.compute = list(dic = TRUE, config = config,
                                      cpo = TRUE, return.marginals = FALSE),
               control.predictor = list(link = 1, compute = TRUE),
               verbose = TRUE)
  model = inla.rerun(model)
  return(model)
}
#Base formula. Only Spatiotemporal model with mean temperature, precipitation and mining
baseformula = MalarianCases ~ 1 + f(t1, model = "rw1", cyclic = TRUE, constr = TRUE,
                        scale.model = TRUE) +
  f(s1, model = "bym2", graph = "map.graph", scale.model = TRUE)

baseformula = MalarianCases ~ 1 + f(s1, model = "bym2", graph = "map.graph") +
                              f(t1, model = "rw1") +
                              f(t2, model = "iid") +
                              f(inla.group(t_mean), model = "rw1", replicate = mining_cat) +
                              f(inla.group(precip), model = "rw1", replicate = mining_cat) +
                              mining 

mod <- inla(baseformula, data = df_inla, family = "nbinomial",
            offset = log(e), verbose = TRUE,
            control.inla = list(strategy = 'adaptive'),
            control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE,
                                   config = TRUE,
                                   return.marginals = TRUE),
            control.predictor = list(link = 1, compute = TRUE),
            control.family = list(link = "log"))

#test baseline model
model = INLAmodel(baseformula, family = "nbinomial")  


  
  
  
  