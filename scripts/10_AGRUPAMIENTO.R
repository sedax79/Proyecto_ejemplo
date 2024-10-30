library(cluster)
library(stats)
library(caret)
library(pheatmap)
library(gplots)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra)
library(dendextend)# clustering algorithms & visualization

###DOCUMENTACION
###HEATMAPS:  https://github.com/uqlibrary/technology-training/blob/master/R/heatmaps/heatmaps_intermediate.md
###Agrupamiento; https://uc-r.github.io/kmeans_clustering
###  dendrogramas_metodos: https://uc-r.github.io/hc_clustering
  ### ckustering_github : https://github.com/kavana-r/RLadies_Hierarchical_Clustering


load("datos/pisos_filtrado.RData")
str(PISOS_filtrado)

#procesamos los datos para que esten en rango 0-1- normalización de datos
normalizados<-preProcess(PISOS_filtrado, method=c("range"))
hc_set<-predict(normalizados, PISOS_filtrado)
str(hc_set)

variables_numericas <- hc_set[,c(2,3,4,8)]

modelo_hc<-hclust(dist(variables_numericas), method = "ward.D2")

plot(modelo_hc)

rect.hclust(modelo_hc, k= 3, border = "magenta")

orden_dendrograma <- modelo_hc$order

clusters <-cutree(modelo_hc, k = 3)

### agregar columna a dataframe

variables_numericas_clust<-cbind(variables_numericas, clusters)

## reordenar
variables_numericas_clust_ordenado <- variables_numericas_clust[orden_dendrograma, ]

#######################################################################
#### realizar agrupamiento de variables mixtas
#######################################################################

hc_set$Exterior.Interior<-factor(hc_set$Exterior.Interior)
str(hc_set)


##### calcular distancias
dist_matrix <- daisy(hc_set, metric = "gower")

#realizar agrupamiento
hc <- hclust(dist_matrix, method = "complete")  # Puedes usar "average" o "single" también

# Visualiza el dendrograma
pdf("plots/dendrograma.pdf")
plot(hc, main = "Dendrograma del Clustering Jerárquico con Distancia de Gower", labels = FALSE)
rect.hclust(hc, k= 5, border = "magenta")
dev.off()
orden_dendrograma <- hc$order

clusters <-cutree(hc, k = 5)

### agregar columna a dataframe

hc_set_clust<-cbind(hc_set, clusters)

## reordenar
hc_set_clust_ordenado <- hc_set_clust[orden_dendrograma, ]


### crear matriz con las distancias de gowr para generar un hetmap
g_matriz<-as.matrix(dist_matrix)


### heatmap
pdf("plots/heatmap.pdf")
heatmap.2(g_matriz,
          Rowv = as.dendrogram(hc),
          Colv = as.dendrogram(hc),
          dendrogram = "both",         # Muestra dendrogramas de filas y columnas
          trace = "none",              # Quita las trazas en la matriz
          col = colorRampPalette(c("blue", "white", "red"))(100), # Ajusta la paleta de colores
          key = TRUE,                  # Activa la barra de colores
          key.title = "Distancia",     # Título de la barra de colores
          key.xlab = "Distancia Gower",# Etiqueta de la barra de colores
          margins = c(5, 5))           # Ajusta márgenes si es necesario

dev.off()


#### otros ejemplos de visualización con heatmaps
### rbase


mtcars_matrix <- data.matrix(mtcars) # convert a DF to a numeric matrix
class(mtcars_matrix)

mi_paleta_colores <- rev(heat.colors(10))
heatmap(mtcars_matrix)
heatmap.2(mtcars_matrix, 
          trace = "none",
          col = mi_paleta_colores, 
          main = "heatmap_clase")


### https://uc-r.github.io/kmeans_clustering

datos_para_hc<- USArrests
datos_para_hc <- na.omit(datos_para_hc)
datos_para_hc <- scale(datos_para_hc)
head(datos_para_hc)

#### VER MATRIZ DE DISTANCIAS
distance <- get_dist(datos_para_hc)
pdf("plots/distancias_matriz.pdf")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()

k2 <- kmeans(datos_para_hc, centers = 2, nstart = 25)
str(k2)

pdf("plots/grupos.pdf")
fviz_cluster(k2, data = datos_para_hc)
dev.off()



k3 <- kmeans(datos_para_hc, centers = 3, nstart = 25)
k4 <- kmeans(datos_para_hc, 4, nstart = 25)
k5 <- kmeans(datos_para_hc, centers = 5, nstart = 25)


p1 <- fviz_cluster(k2, geom = "point", data = datos_para_hc) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = datos_para_hc) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = datos_para_hc) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = datos_para_hc) + ggtitle("k = 5")


library(gridExtra)
pdf("plots/grupos_difK.pdf")
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()

#### METODOS DE AGRUPAMIENTO

##Dissimilarity matrix
d <- dist(datos_para_hc, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method =  "ward.D2")

# Plot the obtained dendrogram
plot(hc1)
rect.hclust(hc1, k = 4, border = 2:5)


### podemos comparar dos dendrogrmas

res.dist <- dist(datos_para_hc, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)


### ejercicio recomendado para casa
##https://github.com/Thomas-George-T/Social-Media-Analytics-in-R

