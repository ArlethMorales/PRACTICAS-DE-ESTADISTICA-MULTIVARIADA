# DISTANCIA DE MAHALANOBIS

# Cargar los datos
ventas= c( 1054, 1057, 1058, 1060, 1061, 1060, 1061, 1062, 1062, 1064, 1062, 1062, 1064, 1056, 1066, 1070)
clientes= c(63, 66, 68, 69, 68, 71, 70, 70, 71, 72, 72, 73, 73, 75, 76, 78)

# Utilizamos la funci√≥n data.frame() para crear un juego de datos en R
datos <- data.frame(ventas ,clientes)

#ExploraciÛn
dim(datos)

# 16 observaiones y 2 variables
str(datos)
#Las variables son n˙mericas
summary(datos)
# En la venta el valor m·ximo es de 1070 y el valor mÌnimo es de 1054

# ---------------- Calculo de la distancia ---------------

# El mÈtodo de distancia Mahalanobis mejora el mÈtodo cl·sico 
# de distancia de Gauss eliminando el efecto que pueden 
# producir la correlaciÛn entre las variables a analizar.

# Determinar el n√∫mero de outlier que queremos encontrar.
num.outliers <- 2

# Ordenar los datos de mayor a menor distancia, seg√∫n la m√©trica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(datos , colMeans( datos), cov(datos)), decreasing=TRUE)
mah.ordenacion

# Los datos 14, 16 y 1 tienen una mayor distancia de 
# Mahalanobis y los datos 7, 9 y 11 tienen una menor distancia

# Generar un vector boleano los dos valores m√°s alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(datos))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 * 16

# Visualizar el gr√°fico con los datos destacando sus outlier.
plot(datos , pch=0)
points(datos , pch=colorear.outlier)

## 2do Ejemplo Mahalanobis

require(graphics)

ma <- cbind(1:6, 1:3)
(S <-  var(ma))
mahalanobis(c(0, 0), 1:2, S)

x <- matrix(rnorm(100*3), ncol = 3)
stopifnot(mahalanobis(x, 0, diag(ncol(x))) == rowSums(x*x))
##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)
plot(density(D2, bw = 0.5),
     main="Gr·fico de densidad de la distancia cuadrada de Mahalanobis, n=100, p=3") ; rug(D2)

qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Distancia de Mahalanobis al cuadrado contra 
       los cuartiles de una distribuciÛn Chi cuadrada"))
abline(0, 1, col = 'blue')

# ------------------------------------------------------------------

## CARGAR LOS DATOS
arboles<-datasets::trees

#EXPLORACI”N
str(arboles)
# Todas las variables son n˙mericas
dim(arboles)
# La matriz cuenta con 31 observaciones y 3 variables, para esto necesitamos
# solo 2 variables.

arboles1<- arboles[,2:3]

colnames(arboles1)

## Height = Altura
## Volume =  Volumen

# SeleccionÈ las variables de Altura y Volumen para ver que tanta distancia hay
# entre las medidas de los ·rboles registrados.


summary(arboles1)

# La altura maxima de un ·rbol es de 87 y la mÌnima de 63.

# ---------------- Calculo de la distancia ---------------

#El mÈtodo de distancia Mahalanobis mejora el mÈtodo cl·sico 
#de distancia de Gauss eliminando el efecto que pueden 
#producir la correlaciÛn entre las variables a analizar.

# Determinar el n˙mero de outlier que queremos encontrar.
num.outliers <- 3

# Ordenar los datos de mayor a menor distancia, seg√∫n la m√©trica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(arboles1 , colMeans( arboles1), cov(arboles1)), decreasing=TRUE)
mah.ordenacion

# Los datos 31,20 y 18 tienen una mayor distancia de Mahalanobis 
# y los datos 13, 16 y 21 tienen una menor distancia de Mahalanobis

# Generar un vector boleano los dos valores m·s alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(arboles1))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 * 16

# Visualizar el gr√°fico con los datos destacando sus outlier.
plot(arboles1, pch=0)
points(arboles1 , pch=colorear.outlier)

