
#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos iris
Z<-as.data.frame(iris)
colnames(Z)

# Se define la matriz de datos y la variable
# respesta con las clasificaciones.
x<-Z[,1:4]
y<-Z[,5]

# Definir como n y p el n煤mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An谩lisis discriminante lineal (LDA)
# Cross validation (cv): clasificaci贸n optima
lda.iris<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.iris$class

# Creacion de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.iris$class)
table.lda

## Podemos obersvar 3 mal clasificados

# Proporci贸n de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n


# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificaci贸n (negro), mala clasificaci贸n (rojo)",
      pch=19,col=col.lda.iris)

# Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior

# Grafico de probabilidades

plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N煤mero de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="green")
points(1:n,lda.iris$posterior[,3],
       pch=20, col="orange")

#________ ANALISIS DISCRIMINANTE LINEAL_____

library(MASS)

# Se cargan los datos 
input <- ("
especie pata abdomen organo_sexual 
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46
")
datos <- read.table(textConnection(input), header = TRUE)
datos$especie <- as.factor(datos$especie)
str(datos)

library(ggplot2)
library(ggpubr)

# Se define la matriz de datos y la variable
# respesta con las clasificaciones.
x<-datos[,2:4]
y<-datos[,1]

# Definir como n y p el n煤mero de flores y
# variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el An谩lisis discriminante lineal (LDA)
# Cross validation (cv): clasificaci贸n optima
lda.iris<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas
# por CV usando LDA.
lda.iris$class

# Creacion de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.iris$class)
table.lda

## Podemos obersvar 2 mal clasificados

# Proporci贸n de errores
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n

# Se obtiene un 10% de error de clasificaci髇

# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificaci贸n (negro), mala clasificaci贸n (rojo)",
      pch=19,col=col.lda.iris)

# Probabilidad de pertenencia a uno de los tres grupos
lda.iris$posterior

# Grafico de probabilidades

plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="blue",
     xlab="N煤mero de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="green")

# Est醤 bien clasificados a excepci髇 de