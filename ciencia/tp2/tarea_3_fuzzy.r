library(cluster)
library(MASS)
library(fpc)
library(ggplot2)

df = read.csv("data_clean.csv",row.names = 1)
df <- df[-c(1,2)]
df.dist = dist(df, method = "euclidean")

randomRows <- function(df,n){
  return(df[sample(nrow(df),n),])
}

# Pruebas de tiempos de PAM
# Genero headers de la salida
# Comento la linea para no pisar los resultados ya procesado
#cat("n","k","time", file = "fuzzy_performance.csv", sep = ",", append = F, fill = T)
# 
# for(n in seq(from=500,to=14000,by=500)){
#   ddata <- randomRows(df,n)
#   ddist <- dist(ddata)
# 
#   t0 =  Sys.time()
#   r <- fanny( ddist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F) #pam.runner(ddist,ddata,k.optimal)
#   t1 =  Sys.time()
#   
#   tiempo = as.numeric(  t1 - t0, units = "secs" )
#   print(paste(tiempo,n,sep="----"))
#   cat(n,3,tiempo, file = "fuzzy_performance.csv", sep = ",", append = T, fill = T)
# }
# 
# # Grafico los tiempos de PAM en función de la cantidad de registros
# 
# df.performance <- read.csv("fuzzy_performance.csv")
# qplot(df.performance$n,df.performance$time) + stat_smooth() + labs(title="Agrupamiento difuso: Tiempo de ejecución",x="Observaciones",y="Tiempo (secs)")


df.fuzzy = fanny( df.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

df.fuzzy.clusters = apply(df.fuzzy$membership, 1, which.max)
df.fuzzy.clusters = cbind(df.fuzzy.clusters, apply(df.fuzzy$membership, 1, max) )
head(df.fuzzy.clusters)

df.fuzzy.clusters.0 = df.fuzzy.clusters
df.fuzzy.clusters.0[df.fuzzy.clusters.0[,2] < 0.6, 1 ] = 0
head(df.fuzzy.clusters.0)

df.fuzzy.negSil = df.fuzzy$silinfo$widths[ df.fuzzy$silinfo$widths[,3] < 0, ]
df.fuzzy.negSil = cbind ( df.fuzzy$membership[ rownames(df.fuzzy.negSil), ], df.fuzzy.negSil[,3] )

df.fuzzy.clusters.negSil = df.fuzzy.clusters
df.fuzzy.clusters.negSil[ rownames(df.fuzzy.negSil), 1 ] = 0
head(df.fuzzy.clusters.negSil, n = 10)

a = rownames(df.fuzzy.clusters.negSil[ df.fuzzy.clusters.negSil[,1] == 0,  ])
b = rownames(df.fuzzy.clusters.0[ df.fuzzy.clusters.0[,1] == 0,  ])
length(a) / nrow(df)
length(b) / nrow(df)
length( intersect(a,b) ) / length(b)

df.fuzzy.clusters.0yNegSil = df.fuzzy.clusters.0
df.fuzzy.clusters.0yNegSil[ intersect(a,b), 1 ] = "Membresía < 0.6\n y Silhouette < 0"
df.fuzzy.clusters.0yNegSil[ df.fuzzy.clusters.0yNegSil[, 1] == "0",  ] = "Membresía < 0.6"

df.pca <- prcomp(df)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]

col <- c("#766272","#4BDBC0","#F5AF3B","#000000","#FF0000")
qplot(df.pca.pc1,df.pca.pc2, color = df.fuzzy.clusters.0yNegSil[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre PCA",x="PC1",y="PC2",colour="Grupo")

df.xy = read.csv("data_clean.csv",row.names = 1)
df.xy <- df.xy[,c(1,2)]

qplot(df.xy$x,df.xy$y, color = df.fuzzy.clusters.0yNegSil[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre X e Y",x="X",y="Y",colour="Grupo")

df.fuzzy.negSil = df.fuzzy$silinfo$widths[ df.fuzzy$silinfo$widths[,3] < 0, ]
df.fuzzy.negSil = cbind ( df.fuzzy$membership[ rownames(df.fuzzy.negSil), ], df.fuzzy.negSil[,3] )
df.fuzzy.clusters.negSil = df.fuzzy.clusters
df.fuzzy.clusters.negSil[ rownames(df.fuzzy.3.negSil), 1 ] = 0



pdf("silhouette_fuzzy_2.pdf")
plot(silhouette(df.fuzzy))
dev.off()





df.no0 = df[ c, ]
df.no0.dist = dist(df.no0, method = "euclidean")
df.no0.fuzzy = fanny( df.no0.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

pdf("silhouette_fuzzy_2.pdf")
plot(silhouette(df.no0.fuzzy))
dev.off()

col <- c("#766272","#4BDBC0","#F5AF3B")
df.no0.clusters = apply(df.no0.fuzzy$membership, 1, which.max)
df.no0.clusters = cbind(df.no0.clusters, apply(df.no0.fuzzy$membership, 1, max) )
qplot(df.pca.pc1,df.pca.pc2, color = df.no0.clusters[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre PCA",x="PC1",y="PC2",colour="Grupo")



