
#__________ K-MEANS____________

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (3 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.

Kmeans.3<-kmeans(X.s, 3, nstart=25)

# visualizar centroides
Kmeans.3$centers

# cluster de pertenencia
Kmeans.3$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.3$withinss)
SCDG
# Lo minimo que puede minimizar el algoritmo es
## 203.2068

# 5.- Clusters
cl.kmeans<-Kmeans.3$cluster
cl.kmeans
#Posición de los clusters

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("blue", "red", "green")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-medias", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")
## Podemos observar que Alaska pertenece al cluster 3
## California cluster 1
## Texas cluster 2
#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)


#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
col="blue")
# Silhouette va de 0 a 1, si está en 0 no está
# clasificando bien
# En el grupo uno está muy bajita la clasificación con 0.16
# y es preferible meter mas clusters
# El cluster 2 "presenta" mejor clasificación
## Con estos resultados nos demuestra que no fue tan bueno
## tomar 3 clusters, es mejor más clusters

# --------------------------------------------
#     Ejercicio 
#----------------------------------------------
# 1. Replicar el script pero vas a sugerir un nuevo número
# de clusters diferente a 3
# 2. Incluir interpretaciones

#__________ K-MEANS____________

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo k-means
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Algoritmo k-medias (5 grupos)
# nstart: cantidad de subconjuntos aleatorios que se escogen
# para realizar los calculos de algoritmo.

Kmeans.5<-kmeans(X.s, 5, nstart=30)

# visualizar centroides
Kmeans.5$centers

# cluster de pertenencia
Kmeans.5$cluster


# 4.- SCDG
SCDG<-sum(Kmeans.5$withinss)
SCDG
# Lo minimo que puede minimizar el algoritmo es
## 136.8587

# 5.- Clusters
cl.kmeans<-Kmeans.5$cluster
cl.kmeans
#Posición de los clusters

# 6.- Scatter plot con la division de grupos
# obtenidos (se utiliza la matriz de datos centrados).
col.cluster<-c("purple2", "orangered", "darkgoldenrod1")[cl.kmeans]
pairs(X.s, col=col.cluster, main="k-medias", pch=19)

#-----------------------------------------
#  Visualizacion con las dos componentes principales
#-----------------------------------------

library(cluster) 

clusplot(X.s, cl.kmeans, 
         main="Dos primeras componentes principales")

text(princomp(X.s)$score[,1:2],
     labels=rownames(X.s), pos=1, col="blue")

## Podemos observar que Alaska que pertenece al cluster 5, se sale del cluster
## Los demás si se ven bien agrupados
#-------------------------------------
#  Silhouette
#--------------------------------------
# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.kmeans<-silhouette(cl.kmeans, dist.Euc)


#2.- Generacion del grafico
plot(Sil.kmeans, main="Silhouette for k-means", 
     col="blue")

## En el gráfico Silhouette se observa que el cluster 3 tiene una
## clasificación moderada en comparación con los demás cluster.
## El cluster 1, 2, 5 tienen una clasificación muy baja.

## Cómo conclusión, no fue una buena idea tomar 5 clusters, ya que se dispersan mucho
## y no se clasifican bien, hasta se salen y la clasificación es muy baja.
## Es mejor tener menos clusters.

