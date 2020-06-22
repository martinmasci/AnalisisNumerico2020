
rm(list = ls())

vector <- c(0.00, 0.21, 0.10, 0.15, 0.30,
            0.21, 0.00, 0.05, 0.21, 0.32,
            0.10, 0.05, 0.00, 0.20, 0.31,
            0.15, 0.21, 0.20, 0.00, 0.23,
            0.30, 0.32, 0.31, 0.23, 0.00)

dist_m <- matrix(data = vector, ncol = 5)

print(dist_m)

## ELEMENTOS DE LA DIAGONAL
as.dist(dist_m)

## MATRIZ DE DISTANCIAS
d = as.dist(dist_m)

## TOMANDO MÁXIMA DISTANCIA
plot(hclust(d = d,method = "complete"))

## TOMANDO MÍNIMA DISTANCIA
plot(hclust(d = d,method = "single"))

## TOMANDO DISTANCIA PROMEDIO
plot(hclust(d = d,method = "average"))

## EJEMPLO FINAL RECUPERATORIO 2019 ##

vector <- c(0.00, 0.30, 0.20, 0.40, 0.05 ,
            0.30, 0.00, 0.15, 0.70, 0.25,
            0.20, 0.15, 0.00, 0.95, 0.20,
            0.40, 0.70, 0.95, 0.00, 0.80,
            0.05, 0.25, 0.20, 0.80, 0.00)

dist_m <- matrix(data = vector, ncol = 5)

## MATRIZ DE DISTANCIAS
d = as.dist(dist_m)

## TOMANDO MÁXIMA DISTANCIA
plot(hclust(d = d,method = "complete"))

## FINAL 2020 ##

vector <- c(0.00, 0.30, 0.20, 0.40, 0.05 ,
            0.30, 0.00, 0.15, 0.70, 0.25,
            0.20, 0.15, 0.00, 0.95, 0.20,
            0.40, 0.70, 0.95, 0.00, 0.80,
            0.05, 0.25, 0.20, 0.80, 0.00)

dist_m <- matrix(data = vector, ncol = 5)
dist_m2 <- 2 * dist_m

## MATRIZ DE DISTANCIAS
l = as.dist(dist_m)
l2 = as.dist(dist_m2)

## TOMANDO MÁXIMA DISTANCIA
par(mar = c(1,1,1,1))
par(mfrow = c(3,2))

par(mfrow = c(1,2))
plot(hclust(d = l,method = "complete"))
plot(hclust(d = l2,method = "complete"))

plot(hclust(d = l,method = "single"))
plot(hclust(d = l2,method = "single"))

plot(hclust(d = l,method = "average"))
plot(hclust(d = l2,method = "average"))

par(mfrow = c(1,3))
plot(hclust(d = l,method = "single"),main = "Single")
plot(hclust(d = l,method = "average"),main = "Average")
plot(hclust(d = l,method = "complete"), main = "Complete")

