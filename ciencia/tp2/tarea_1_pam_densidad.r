# Para cada corrida guardar
# nombreAlgoritmo, K, silhuette, sse, tiempo (segundos), nombre medida de distancia.
library(cluster)
library(fpc)
library(ggplot2)
library(rgl)
library(kohonen)
library(RColorBrewer)

df <- read.csv("../tp1/data_tp1.csv",row.names = 1)
df <- scale(df)

# Detalles del dataset

summary(df)
rownames(df)
nrow(df)

# Distancias y k params

k.values <- 2:20
df.dist <- dist(df)

# Cluster jerárquico para visualizar estructuras
df.hclust <- hclust(df.dist)
plot(as.dendrogram(df.hclust))

# Agrupamiento por SOM

################################################################################################
# Implementación de PAM
################################################################################################

# Definición de función
# Retorna los medoides, sse y sil
pam.runner <- function(di,da,k) {
  results <- list()
  print(paste("Running PAM with k = ",k,sep=""))
  t0 = Sys.time()
  p <- pam(di, k, diss = T)
  results$p.meds <- p$medoids[p$clustering]
  results$p.sse <- sum(as.matrix(di)[cbind(row.names(da),results$p.meds)]^2)
  t1 = Sys.time()
  results$p.sil <- p$silinfo$avg.width
  results$p.sec <- as.numeric(t1 - t0, units = "secs")
  results$p <- p
  return(results)
}

# Genero headers de la salida
cat("dataset","k","silhuette_avg.width","sse","time", file = "pam_results.csv", sep = ",", append = F, fill = T)

# Defino arrays para guardar resultados por cada k
df.pam.sse <- array()
df.pam.sil <- array()
df.pam.sec <- array()

# Para cada valor de k, ejecuto pam.runner y guardo resultados
for(k in k.values){
  results <- pam.runner(df.dist,df,k)
  df.pam.sse[k-1] <- results$p.sse
  df.pam.sil[k-1] <- results$p.sil
  df.pam.sec[k-1] <- results$p.sec
  # Imprimo resultados en archivo de salida
  cat("tp1", k, results$p.sil, results$p.sse, results$p.sec, file = "pam_results.csv", sep = ",", append = T, fill = T)
}

# Gráficos para verificar resultados de PAM

par(mfrow=c(2,1))
plot(k.values, df.pam.sil, type="b", xlab="k")
plot(k.values, df.pam.sse, type="b", xlab="k")

# Defino el K óptimo con PAM
# Lo seteo en 5, luego ajusar de acuerdo a lo que corresponda

k.optimal <- 5

df.pam.optimal <- pam.runner(df.dist,df,k.optimal)
par(mfrow=c(1,1))
plot(df.pam.optimal$p, main="Silhouette, k = 5")

# Muestro los prototipos + isolation

data.frame(df[df.pam.optimal$p$medoids,], tamaño=df.pam.optimal$p$clusinfo[,1])
df.pam.optimal$p$isolation




################################################################################################
# Implementación de Clustering por Densidad
################################################################################################

# Hago búsqueda de parámetros en función de eps, grafico distancias, no hay búsqueda exhaustiva

# Genero headers de la salida

df.dbscan.search <- apply(as.matrix(df.dist),1,function(x) sort(x)[10])
plot(sort(df.dbscan.search),type="l")

df.dbscan.1 <- dbscan(df.dist,eps=0.5,method="dist",MinPts = 10)
df.dbscan.1

df.dbscan.2 <- dbscan(df.dist,eps=0.6,method="dist",MinPts = 10)
df.dbscan.2

# La segunda búsqueda tiene menos ruido y genera algunos grupos adicionales
# Con un eps más grande el radio comienza a agrupar mucho los clusters
# Con un eps más chico se genera muchos grupos 

# PCA para graficar
df.pca <- prcomp(df)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]
df.pca.pc3 <- df.pca$x[,3]

qplot(df.pca.pc1,df.pca.pc2,colour=factor(df.dbscan.2$cluster))

plot3d(df.pca.pc1, df.pca.pc2, df.pca.pc3,col=df.dbscan.2$cluster+1, size=5)

# TODO !!!
# Como computamos silhouette y sse, si hay observaciones que no quedan clusterizadas


################################################################################################
# Implementación de SOM
################################################################################################
par(mfrow=c(1,1))

df.som <- som(as.matrix(df), somgrid(4,4,"hexagonal"))
plot(df.som, type="codes")

# Relación entre SOM y PAM
pal <- colorRampPalette(brewer.pal(11,"Spectral"))(6)


plot(df.som,type="mapping",col=pal[df.pam.optimal$p$clustering],pch=19, main="Clusters según PAM")
leg <- sort(unique(df.pam.optimal$p$clustering))
legend("left", legend = leg, col=pal[leg], pch=19, ncol =1, cex=0.8)

# Relación entre SOM y Densidad

plot(df.som,type="mapping",col=pal[df.dbscan.2$cluster+1],pch=19, main="Clusters según DBSCAN")
leg <- sort(unique(df.dbscan.2$cluster))
legend("left", legend = leg, col=pal[leg], pch=19, ncol =1, cex=0.8)

# Exploro algunas variables
par(mfrow=c(2,4))
for(x in 1:8){
  plot(df.som, type="property", property = df.som$codes[,x], main=colnames(df)[x])
}
