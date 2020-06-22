###########################################################
#             UNIVERSIDAD DE BUENOS AIRES                 #
#           FACULTAD DE CIENCIAS ECONÓMICAS               #
###########################################################

## Asignatura: ANÁLISIS NUMÉRICO
## Año Lectivo: 2020
## Docente: Mauro Speranza - Martín Masci - Rodrigo Del Rosso

################################
####### SETEO DE CARPETA #######
################################

getwd()

dir()

directorio <- "/ruta"  ## colocar la ruta entre comillas

setwd(directorio)

getwd() ## verifico si me modificó la ruta

## permite cambiar el directorio donde estamos trabajando (working directory)
## Hay que tener en cuenta que los datos se guardarán en ese directorio

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

## % de asaltos (Assault), asesinatos (Murder) y secuestros (Rape) 
## por cada 100,000 habitantes para cada uno de los 50 estados de USA (1973). 
## Además, también incluye el porcentaje de la población de cada estado 
## que vive en zonas rurales (UrbanPoP).

data("USArrests")
head(USArrests)

View(USArrests)

## la función prcomp es una de las múltiples funciones en R que realizan PCA
## por defecto centra las variables para que tengan media cero, pero si se quiere además
## que su desviación estándar sea de uno, hay que indicar scale = TRUE

pca <- prcomp(USArrests, scale = TRUE)
names(pca)

summary(pca)

## Los elementos center y scale y	almacenados en el objeto pca
## contienen la media y desviación típica de las variables 
## previa estandarización (en la escala original).

pca$center
apply(X = USArrests, MARGIN = 2, FUN = mean) #acá armamos la media para comparar

pca$scale
apply(X = USArrests, MARGIN = 2, FUN = sd) #acá armamos la varianza para comparar

## rotation contiene el valor de los loadings para cada componente (eigenvector).
## El número máximo de componentes principales se corresponde con el mínimo(n-1,p), 
## que en este caso es min(49,4)=4

pca$rotation

## Analizar con detalle el vector de loadings que forma cada componente
## puede ayudar a interpretar que tipo de información recoge cada una de ellas
## La función	prcomp() calcula automáticamente el valor de las componentes principales
## para cada observación (principal component scores) 
## multiplicando los datos por los vectores de loadings. 
## El resultado se almacena en la matriz x.

head(pca$x)

dim(pca$x)

## Mediante la función biplot()	se puede obtener una 
## representación bidimensional de las dos primeras componentes

biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

## La imagen especular, cuya interpretación es equivalente, se puede obtener 
## invirtiendo el signo de los loadings y de los principal component scores.

pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

## Una vez calculadas las componentes principales, 
## se puede conocer la varianza explicada por cada una de ellas, 
## la proporción respecto al total y la proporción de varianza acumulada.

pca$sdev
pca$sdev^2

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

summary(pca)

VE <- ggplot(data = data.frame(prop_varianza, pc = 1:4),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente Principal",
       y = "En %") +
  ggtitle("Varianza Explicada", subtitle = "Por Factor")

#dev.off()  #cerrar el gráfico

prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum

VAE <- ggplot(data = data.frame(prop_varianza_acum, pc = 1:4),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente Principal",
       y = "En %") +
  ggtitle("Varianza Acumulada Explicada", subtitle = "Por Factor")

grid.arrange(VE, VAE, ncol=2)

## En  este  caso,  la  primera  componente  explica  el  62%  de  la  varianza  
## observada  en  los datos y la segunda el 24.7%. 

## Las dos últimas componentes no superan por separado el 1% de varianza explicada. 
## Si se empleasen únicamente las dos primeras componentes se 
## conseguiría explicar el 86.75% de la varianza observada.

####################################### OTRO EJEMPLO ##########################################

# Carga de datos inicial, tipos de flores con diferentes caracteristicas 
data(iris)

# Nos quedamos todas las variables excepto la variable dependiente
datos <- iris[-5] 

# Ejecutar el análisis de componentes principales PCA
modelo <- prcomp(datos) 

# Mostrar la desviación estandar y la relación de cada variable con cada componente
modelo

# Mostrar un resumen de cada uno de los componentes
# Standard deviation = valores propios de la varianza explicada en cada componente
# Proportion of Variance = porcentaje de varianza explicada por cada factor
# Cumulative Proportion = porcentaje de varianza acumulada explicada por cada factor
summary(modelo)

# Preparación del modelo para mostrar la distinción por colores
colores <- as.character(iris$Specie)
colores[colores=="setosa"] <- "red"
colores[colores=="virginica"] <- "black"
colores[colores=="versicolor"] <- "blue"

# Dibujar los pares de componentes
pairs(modelo$x,col=colores) 

# Grafico de PC1 y PC2
plot(modelo$rotation,pch='')
abline(h = 0, v = 0, col = "gray60")
text(modelo$rotation,labels=rownames(modelo$rotation))

# Grafico de PC1 y PC3 
plot(modelo$rotation[,1],modelo$rotation[,3],pch='.')
abline(h = 0, v = 0, col = "gray60")
text(modelo$rotation[,1],modelo$rotation[,3],labels=rownames(modelo$rotation))

# Dibujar la varianza que explica cada factor
plot(modelo)

# Se guardan los dos primeros factores del PCA en un nuevo data.frame para usarlo en un GLM
valoresDeFactores <- modelo$x[,1:2]
str(valoresDeFactores)
View(valoresDeFactores)


####################################### OTRO EJEMPLO ##########################################

#Dataset del desempleo en cada Estado de USA

states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID", "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO", "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA", "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
states

raw <- read.csv("http://www.biz.uiowa.edu/faculty/jledolter/DataMining/unempstates.csv")
dim(raw)

raw[1:3,1:5] # Y las filas continuan hasta la dimensión 416

## transponemos la matriz 50 filas (cada uno de los Estados) 
## y 416 columnas (cada Tasa de Desempleo)
rawt <- matrix(nrow=50,ncol=416)
rawt <- t(raw)
rawt[1:3,1:5] # Y las columnas continuan hasta la dimensión 416

pcaunemp <- prcomp(rawt,scale=FALSE) # computamos las componentes

head(pcaunemp$sdev) # Aqui vemos las varianzas de las componentes

plot(pcaunemp, main="Varianza de las Componentes")
mtext(side=1,"Desempleo: 50 Estados",line=1,font=2)

pcaunemp$rotation[1:10,1]
## Cargas de la primera componentes. 
## Solo se visualizan las primeras 10 variables 
## Acá tendremos una ecuación con 416 cargas para cada una de las 
## variables en columna de la matriz de datos original

# Calculamos la Tasa Promedio de Desempleo para todos los Estados 
## para cada uno de los meses en el análisis

ave <- dim(416)
for(j in 1:416){
  ave[j] <- mean(rawt[,j])
}


par(mfrow = c(1,2))
## Gráfica de los valores (negativos) de las cargas de la 1era Componente
plot(-pcaunemp$rotation[,1], 
     main ='Cargas de la primera componente', 
     type = "l")

## Gráfica de los valores medios de paro para los estados
plot(ave,
     type ="l",
     ylim = c(3,10),
     xlab = "Mes",
     ylab= "Evolución de la Tasa Promedio de Desempleo")

# Calculamos la correlación entre los factores de la primera componente 
## y las Tasas Promedios de Desempleo
abs(cor(ave,pcaunemp$rotation[,1]))

unemppc <- predict(pcaunemp)

## A continuación construímos una gráfica de los estados 
## utilizando solo la información de las primeras 2 componentes principales. 
## Llevamos a cabo un análisis de clusters y pintamos los estados que 
## pertenecen a cada cluster de diferentes colores,

set.seed(123)
grpunemp3 <- kmeans(rawt,centers=3,nstart=10)
par(mfrow=c(1,1))
plot(unemppc[,1:2],type="n")
text(x=unemppc[,1],y=unemppc[,2],labels=states,col=rainbow(7)
     [grpunemp3$cluster])

## OTRO EJEMPLO ## 

data(decathlon)
View(decathlon)

head(decathlon, 3)

res <- PCA(decathlon,quanti.sup=11:12,quali.sup=13)

plot(res,invisible="quali")
plot(res,choix="var",invisible="quanti.sup")
plot(res,habillage=13)

aa <- cbind.data.frame(decathlon[,13],res$ind$coord)
bb <- coord.ellipse(aa,bary=TRUE)

plot.PCA(res,habillage=13,ellipse=bb)

#RESETEO DE SESIÓN Y BORRADO DE MEMORIA
rm(list=ls())
.rs.restartR()
gc()
