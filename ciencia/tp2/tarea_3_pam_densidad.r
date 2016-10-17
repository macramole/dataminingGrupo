# Para cada corrida guardar
# nombreAlgoritmo, K, silhuette, sse, tiempo (segundos), nombre medida de distancia.

library(cluster)
library(fpc)
library(ggplot2)
library(rgl)
library(kohonen)
library(RColorBrewer)
library(gridExtra)

#Inicializo pdf
pdf("plots/p3/pam_densidad.pdf")

dfcomp <- read.csv("data_clean.csv",row.names = 1)
df <- dfcomp[-c(1,2)]

# Detalles del dataset

summary(df)
rownames(df)
nrow(df)

# Distancias y k params

#k.values <- 2:20
df.dist <- dist(df)

# Utilizando el conjunto de datos completos los tiempos de ejecución se incremenal considerablemente
# Definimos función para seleccionar registros aleatorios
# Vamos a construir n modelos con PAM incrementando el tamaño de observaciones para obtener tiempos de ejecución

randomRows <- function(df,n){
  return(df[sample(nrow(df),n),])
}

randomColumns <- function(df,p){
  return(df[,sample(col(df),p)])
}

# Cluster jerárquico para visualizar estructuras
#df.hclust <- hclust(df.dist)
#plot(as.dendrogram(df.hclust))

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
  print("Termine de calcular pam")
  results$p.meds <- p$medoids[p$clustering]
  results$p.sse <- sum(as.matrix(di)[cbind(row.names(da),results$p.meds)]^2)
  t1 = Sys.time()
  results$p.sil <- p$silinfo$avg.width
  results$p.sec <- as.numeric(t1 - t0, units = "secs")
  results$p <- p
  return(results)
}

# Genero headers de la salida
#cat("dataset","k","silhuette_avg.width","sse","time", file = "pam_results_3.csv", sep = ",", append = F, fill = T)

# Defino arrays para guardar resultados por cada k
#df.pam.sse <- array()
#df.pam.sil <- array()
#df.pam.sec <- array()

# Para cada valor de k, ejecuto pam.runner y guardo resultados
# for(k in k.values){
#   results <- pam.runner(df.dist,df,k)
#   df.pam.sse[k-1] <- results$p.sse
#   df.pam.sil[k-1] <- results$p.sil
#   df.pam.sec[k-1] <- results$p.sec
#   # Imprimo resultados en archivo de salida
#   cat("tp1", k, results$p.sil, results$p.sse, results$p.sec, file = "pam_results_3.csv", sep = ",", append = T, fill = T)
# }

# Gráficos para verificar resultados de PAM

# par(mfrow=c(2,1))
# plot(k.values, df.pam.sil, type="b", xlab="k")
# plot(k.values, df.pam.sse, type="b", xlab="k")

# Defino el K óptimo con PAM
# Lo seteo en 5, luego ajusar de acuerdo a lo que corresponda

k.optimal <- 3

df.pam.optimal <- pam.runner(df.dist,df,k.optimal)
par(mfrow=c(1,1))
plot(df.pam.optimal$p)

# Muestro los prototipos + isolation

write.csv(data.frame(df[df.pam.optimal$p$medoids,], tamaño=df.pam.optimal$p$clusinfo[,1]),"pam_medoides_3.csv")
df.pam.optimal$p$isolation

# PCA para graficar
df.pca <- prcomp(df)
df.pca.s <- summary(df.pca)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]
df.pca.pc3 <- df.pca$x[,3]

col <- c("#766272","#4BDBC0","#F5AF3B")

qplot(df.pca.pc1,df.pca.pc2,colour=factor(df.pam.optimal$p$clustering))  + scale_colour_manual(values=col) + labs(title="PAM sobre PCA",x="PC1",y="PC2",colour="Cluster") 



# Pruebas de tiempos de PAM
# Genero headers de la salida
# Comento la linea para no pisar los resultados ya procesado
# cat("n","k","time", file = "pam_performance.csv", sep = ",", append = F, fill = T)
# 
# for(n in seq(from=500,to=14000,by=500)){
#   ddata <- randomRows(df,n)
#   ddist <- dist(ddata)
#   
#   r <- pam.runner(ddist,ddata,k.optimal)
#   print(paste(r$p.sec,n,sep="----"))
#   cat(n,k.optimal,r$p.sec, file = "pam_performance.csv", sep = ",", append = T, fill = T)
# }

# Grafico los tiempos de PAM en función de la cantidad de registros

df.pam.performance <- read.csv("pam_performance.csv")
qplot(df.pam.performance$n,df.pam.performance$time) + stat_smooth() + labs(title="PAM: Tiempo de ejecución",x="Observaciones",y="Tiempo (secs)")


################################################################################################
# Implementación de Clustering por Densidad
################################################################################################

# Hago búsqueda de parámetros en función de eps, grafico distancias, no hay búsqueda exhaustiva

# Genero headers de la salida

# df.dbscan.search <- apply(as.matrix(df.dist),1,function(x) sort(x)[20])
# par(mfrow=c(1,1))
# plot(sort(df.dbscan.search),type="l")
# 
# df.dbscan.1 <- dbscan(df.dist,eps=0.5,method="dist",MinPts = 20)
# df.dbscan.1

df.dbscan.2 <- dbscan(df.dist,eps=0.7,method="dist",MinPts = 20)
df.dbscan.2

# La segunda búsqueda tiene menos ruido y genera algunos grupos adicionales
# Con un eps más grande el radio comienza a agrupar mucho los clusters
# Con un eps más chico se genera muchos grupos 

# PCA para graficar
df.pca <- prcomp(df)
df.pca.s <- summary(df.pca)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]
df.pca.pc3 <- df.pca$x[,3]

col <- c(rgb(0,0,0,alpha=0.1) ,"#4BDBC0","#F5AF3B","#D44C8B")

qplot(df.pca.pc1,df.pca.pc2,colour=factor(df.dbscan.2$cluster)) + geom_density_2d() + scale_colour_manual(values=col) + labs(title="DBSCAN sobre PCA",x="PC1",y="PC2",colour="Cluster") 

# Pruebas de tiempos de DBSCAN
# Genero headers de la salida
# Comento la linea para no pisar los resultados ya procesado
# cat("n","minpts","time", file = "dbscan_performance.csv", sep = ",", append = F, fill = T)
# 
# for(n in seq(from=500,to=14000,by=500)){
#  ddata <- randomRows(df,n)
#  ddist <- dist(ddata)
#  
#  t0 <- Sys.time()
#  r <- dbscan(ddist,eps=0.6,method="dist",MinPts = 10)
#  t1 <- Sys.time()
#  tdelta <- as.numeric(t1 - t0, units = "secs")
#  print(paste(tdelta,n,sep="----"))
#  
#  cat(n,10,tdelta, file = "dbscan_performance.csv", sep = ",", append = T, fill = T)
# }

# Grafico los tiempos de DBSCAN en función de la cantidad de registros
df.dbscan.performance <- read.csv("dbscan_performance.csv")
qplot(df.dbscan.performance$n,df.dbscan.performance$time) + stat_smooth() + labs(title="DBSCAN: Tiempo de ejecución",x="Observaciones",y="Tiempo (secs)") 



################################################################################################
# Implementación de SOM
################################################################################################
# par(mfrow=c(1,1))
# 
# df.som <- som(as.matrix(df), somgrid(4,4,"hexagonal"))
# plot(df.som, type="codes")
# 
# # Relación entre SOM y PAM
# pal <- c(rgb(0,0,0,alpha=0.1) ,"#4BDBC0","#F5AF3B","#D44C8B")
# 
# 
# plot(df.som,type="mapping",col=pal[df.pam.optimal$p$clustering],pch=19, main="Clusters según PAM")
# leg <- sort(unique(df.pam.optimal$p$clustering))
# legend("left", legend = leg, col=pal[leg], pch=19, ncol =1, cex=0.8)
# 
# # Relación entre SOM y Densidad||
# 
# plot(df.som,type="mapping",col=pal[df.dbscan.2$cluster+1],pch=19, main="Clusters según DBSCAN")
# leg <- sort(unique(df.dbscan.2$cluster))
# legend("left", legend = leg, col=pal[leg], pch=19, ncol =1, cex=0.8)
# 
# # Exploro algunas variables
# par(mfrow=c(2,4))
# for(x in 1:8){
#   plot(df.som, type="property", property = df.som$codes[,x], main=colnames(df)[x])
# }
# 
# # Pruebas de tiempos de SOM
# # Genero headers de la salida
# # Comento la linea para no pisar los resultados ya procesado
# # cat("n","xgrid","ygrid","time", file = "som_performance.csv", sep = ",", append = F, fill = T)
# # 
# # 
# # for(n in seq(from=500,to=14000,by=500)) {
# #   ddata <- randomRows(df,n)
# #   
# #   t0 <- Sys.time()
# #   r <- som(as.matrix(ddata), somgrid(4,4,"hexagonal"))
# #   t1 <- Sys.time()
# #   tdelta <- as.numeric(t1 - t0, units = "secs")
# #   print(paste(tdelta,n,sep="----"))
# # 
# #   cat(n,4,4,tdelta, file = "som_performance.csv", sep = ",", append = T, fill = T)
# # }
# 
# df.som.performance <- read.csv("som_performance.csv")
# qplot(df.som.performance$n,df.som.performance$time) + stat_smooth() + labs(title="SOM: Tiempo de ejecución",x="Observaciones",y="Tiempo (secs)")

dev.off()
