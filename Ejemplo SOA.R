###########################################################
#             UNIVERSIDAD DE BUENOS AIRES                 #
#           FACULTAD DE CIENCIAS ECON�MICAS               #
###########################################################

## Asignatura: AN�LISIS NUM�RICO
## A�o Lectivo: 2020
## Docente: Mauro Speranza - Mart�n Masci

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

directorio <- "/ruta"  ## colocar la ruta entre comillas

setwd(directorio)

getwd() ## verifico si me modific� la ruta

## permite cambiar el directorio donde estamos trabajando (working directory)
## Hay que tener en cuenta que los datos se guardar�n en ese directorio

#########################
####### LIBRERIAS #######
#########################

library(ggplot2)
library(faraway)
library(FactoMineR)
library(cluster) 
library(gridExtra)
library(dplyr)
library(factoextra)

###################
####### PCA #######
###################

rm(list=ls())

# Ejemplo sacado de la web de SOA: https://sections.soa.org/publication/?i=662070&article_id=3687343&view=articleBrowser&ver=html5?homepagecard
# Dataset disponible en: https://sci2s.ugr.es/keel/dataset.php?cod=1296

pollution <- read.delim("pollution.dat", header = FALSE, skip = 19, sep = ",")

colnames(pollution) <- c("PRECReal","JANTReal","JULTReal","OVR65Real",
                         "POPNReal","EDUCReal","HOUSReal","DENSReal",
                         "NONWReal","WWDRKReal","POORReal","HCReal",
                         "NOXReal","SO@Real","HUMIDReal","MORTReal") 

#Se agrega una columna con la clasificaci�n de mortalidad
pullution <- mutate(pollution, MORTReal_Type = case_when(
  pollution$MORTReal < 900.0 ~ "Low Mortality",
  pollution$MORTReal > 900.0 & MORTReal < 1000.0 ~ "Medium Mortality",
  pollution$MORTReal > 1000.0 ~ "Hight Mortality",)
  )

#Planteo de PCA
pollution.PCA <- PCA(pollution[c(-17)], scale.unit = T, graph = F)
pollution.PCA #visualizamos resultados de la Lista

pollution.PCA$eig #visualizamos los autovalores. Clasificadores/dimensiones iniciales.

eig.val <- get_eigenvalue(pollution.PCA) 
eig.val #visualizamos lo mismo con el paquete "factoextra"

#Graficamos con el paquete "factoextra"
fviz_eig(pollution.PCA, addlabels = T, hjust = -0.3, ylim = c(0,35), 
         title = "Primeras 10 dimensiones")

#Ploteamos c�rculo de correlaciones
var_pollution <- get_pca_var(pollution.PCA)
var_pollution

head(var_pollution$contrib) #Visualizamos algunos valores de correlaci�n

fviz_pca_var(pollution.PCA, col.var = "black")  #plot

#veamos la calidad de la representaci�n anterior usando el mapeado cos2
head(var_pollution$cos2)

fviz_pca_var(pollution.PCA, col.var = "cos2", gradient.cols = c("green", "blue", "red"),
             repel = T)

#Ahora veamos la contribuci�n de las variables al PC1
fviz_contrib(pollution.PCA, choice = "var", axes = 1, top = 10)

#Ahora veamos la contribuci�n de las variables al PC2
fviz_contrib(pollution.PCA, choice = "var", axes = 2, top = 10)


fviz_pca_var(pollution.PCA, col.var = "contrib", gradient.cols = c("yellow", "blue", "red"),
             repel = T, title = "Representaci�n de autovectores en contribuci�n")

#Mapeo biplot en 2 dimensiones del efecto de la poluci�n en la tasa de mortalidad

fviz_pca_biplot (pollution.PCA, col.ind = pollution$MORTReal, pallete = "jco",
                 addEllipses = F, label = "var", col.var = "blue", gradient.cols = c("green", "yellow", "red"),
                 repel = T, legend.title = "Mortality_Range")

#RESETEO DE SESI�N Y BORRADO DE MEMORIA
rm(list=ls())
.rs.restartR()
