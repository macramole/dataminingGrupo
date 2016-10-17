library(cluster)
library(MASS)
library(fpc)
library(doMC)

registerDoMC(3)

df = read.csv("../tp1/data_tp1.csv",row.names = 1)
df = scale(df)

summary(df)
rownames(df)
nrow(df)

df.dist = dist(df, method = "euclidean")

varK = 3
varMemb.exp = 1.7

cat("dataset","k","memb.exp","silhuette_avg.width","coeff","time", file = "fuzzy_results.csv", sep = ",", append = F, fill = T)

#barrer k, memb.exp
foreach(varK = 2:11) %dopar% {
  for ( varMemb.exp in seq(1.2,1.7,0.1) ) {
    t0 = Sys.time()
    df.fuzz = fanny( df.dist, k = varK, diss = T,  memb.exp = varMemb.exp, keep.diss = F, keep.data = F)
    t1 = Sys.time()
    # cat("Tiempo: ", as.numeric(t1 - t0))
    cat("tp1", varK, varMemb.exp, df.fuzz$silinfo$avg.width, df.fuzz$coeff[2], as.numeric(t1 - t0, units = "secs"),
        file = "fuzzy_results.csv", sep = ",", append = T, fill = T)
  }
}

#Analizo resultados

fuzzyResults =read.csv("fuzzy_results.csv")
fuzzyResults = fuzzyResults[ fuzzyResults$coeff != -1, ]
fuzzyResults = arrange(fuzzyResults, k, memb.exp)

#head(f)
table(fuzzyResults$k) 



fuzzyResults.max = c()
for ( i in min(fuzzyResults$k):max(fuzzyResults$k) ) {
  kFiltered = fuzzyResults[ fuzzyResults$k == i , ]
  maxSilhuetteIndex = which.max(kFiltered[,c("silhuette_avg.width")])
  
  fuzzyResults.max = rbind(fuzzyResults.max, kFiltered[maxSilhuetteIndex,] )
}


library("plotly")
library(dplyr)

plot_ly(fuzzyResults.max, x = k, y= silhuette_avg.width, name="silhouette") %>%
  add_trace(x = k, y = coeff, name="coeficiente de Dunn") %>%
  layout( yaxis = list( title = "" ), title = "Elección del K" )

plot_ly(fuzzyResults[fuzzyResults$k == 2,], x = memb.exp, y = silhuette_avg.width, name = "k = 2")
add_trace(fuzzyResults[fuzzyResults$k == 3,], x = memb.exp, y = silhuette_avg.width, name = "k = 3")
add_trace(fuzzyResults[fuzzyResults$k == 4,], x = memb.exp, y = silhuette_avg.width, name = "k = 4")

plot_ly(fuzzyResults[fuzzyResults$k == 2,], x = memb.exp, y = coeff, name = "k = 2")
add_trace(fuzzyResults[fuzzyResults$k == 3,], x = memb.exp, y = coeff, name = "k = 3")
add_trace(fuzzyResults[fuzzyResults$k == 4,], x = memb.exp, y = coeff, name = "k = 4")
add_trace( x = c(1,2), y=c(1,0) )


fuzzyResults.max

df.fuzzy.2 = fanny( df.dist, k = 2, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)
df.fuzzy.3 = fanny( df.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)
df.fuzzy.3.alt = fanny( df.dist, k = 3, diss = T,  memb.exp = 1.4, keep.diss = F, keep.data = F)
df.fuzzy.4 = fanny( df.dist, k = 4, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

c = setdiff(rownames(df),intersect(a,b))
length(rownames(df)) - length(intersect(a,b))
length(c)

df.no0 = df[ c, ]
df.no0.dist = dist(df.no0, method = "euclidean")
df.no0.fuzzy = fanny( df.no0.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

df.fuzzy.3.no0

df.dist

df.fuzzy.2.negSil = df.fuzzy.2$silinfo$widths[ df.fuzzy.2$silinfo$widths[,3] < 0, ]
cbind ( df.fuzzy.2$membership[ rownames(df.fuzzy.2.negSil), ], df.fuzzy.2.negSil[,3] )
df.fuzzy.2.highSil = df.fuzzy.2$silinfo$widths[ df.fuzzy.2$silinfo$widths[,3] > 0.4, ]
cbind ( df.fuzzy.2$membership[ rownames(df.fuzzy.2.highSil), ], df.fuzzy.2.highSil[,3] )
df.fuzzy.2$objective
df.fuzzy.3.negSil = df.fuzzy.3$silinfo$widths[ df.fuzzy.3$silinfo$widths[,3] < 0, ]
cbind ( df.fuzzy.3$membership[ rownames(df.fuzzy.3.negSil), ], df.fuzzy.3.negSil[,3] )


pdf("silhouette_fuzzy.pdf")
# plot(silhouette(df.fuzzy.2))
plot(silhouette(df.fuzzy.3))
# plot(silhouette(df.fuzzy.3.asd))
plot(silhouette(df.no0.fuzzy))

# plot(silhouette(df.fuzzy.3.alt))
# plot(silhouette(df.fuzzy.4))
dev.off()


#intento mejorar el coeff 
varK = 2

foreach(varMemb.exp = seq(1,1.2,0.02)) %dopar% {
  t0 = Sys.time()
  df.fuzz = fanny( df.dist, k = varK, diss = T,  memb.exp = varMemb.exp, keep.diss = F, keep.data = F)
  t1 = Sys.time()
  # cat("Tiempo: ", as.numeric(t1 - t0))
  cat("tp1", varK, varMemb.exp, df.fuzz$silinfo$avg.width, df.fuzz$coeff[2], as.numeric(t1 - t0, units = "secs"),
      file = "fuzzy_results.csv", sep = ",", append = T, fill = T)
}

c(c(1,1),c(2,0))

table(fuzzyResults.max)

fuzzyResults %>%
  filter( k == 3, coeff > 0 ) %>%
  arrange(k, memb.exp) %>%
  plot_ly(x = memb.exp, y = coeff  )

add_trace( x = c(1,2), y=c(1,0) )

df.pca <- prcomp(df)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]

df.pca$

df.fuzzy.3.clusters = apply(df.fuzzy.3$membership, 1, which.max)
df.fuzzy.3.clusters = cbind(df.fuzzy.3.clusters, apply(df.fuzzy.3$membership, 1, max) )

df.no0.clusters = apply(df.no0.fuzzy$membership, 1, which.max)
df.no0.clusters = cbind(df.no0.clusters, apply(df.no0.fuzzy$membership, 1, max) )
head(df.no0.clusters)
nrow(df.no0.clusters[df.no0.clusters[,2] < 0.6, ])

df.fuzzy.3.clusters.alt = apply(df.fuzzy.3.alt$membership, 1, which.max)
df.fuzzy.3.clusters.alt = cbind(df.fuzzy.3.clusters.alt, apply(df.fuzzy.3.alt$membership, 1, max) )
head(df.fuzzy.3.clusters.alt)

df.fuzzy.3.clusters.alt.0 = df.fuzzy.3.clusters.alt
head(df.fuzzy.3.clusters.alt.0)
df.fuzzy.3.clusters.alt.0[df.fuzzy.3.clusters.alt.0[,2] < 0.6, 1 ] = 0

df.fuzzy.3.clusters.0 = df.fuzzy.3.clusters
head(df.fuzzy.3.clusters.0)
df.fuzzy.3.clusters.0[df.fuzzy.3.clusters.0[,2] < 0.6, 1 ] = "0 (Membresía < 0.6)"
df.fuzzy.3.clusters.0[,1] = as.factor(df.fuzzy.3.clusters.0[,1])df.fuzzy.3.clusters.0 = df.fuzzy.3.clusters


df.fuzzy.3.clusters.0[df.fuzzy.3.clusters.0[,2] < 0.6, 1 ] = "0 (Membresía < 0.6)"
df.fuzzy.3.clusters.0[,1] = as.factor(df.fuzzy.3.clusters.0[,1])


library(ggplot2)

col <- c("#766272","#4BDBC0","#F5AF3B")
col <- c("#000000","#766272","#4BDBC0","#F5AF3B")


qplot(df.pca.pc1,df.pca.pc2, color = factor(df.fuzzy.3.clusters.alt[,1]), alpha = df.fuzzy.3.clusters.alt[,2]) + scale_colour_manual(values=col) + labs(title="Clustering difuso sobre PCA",x="PC1",y="PC2",colour="Cluster")
qplot(df.pca.pc1,df.pca.pc2, color = factor(df.fuzzy.3.clusters.alt.0[,1])) + scale_colour_manual(values=col) + labs(title="Clustering difuso sobre PCA",x="PC1",y="PC2",colour="Cluster")




df.fuzzy.3.clusters.negSil = df.fuzzy.3.clusters
df.fuzzy.3.clusters.negSil[ rownames(df.fuzzy.3.negSil), 1 ] = "0 (Silhuette < 0)"
  
qplot(df.pca.pc1,df.pca.pc2, color = df.fuzzy.3.clusters.0[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre PCA",x="PC1",y="PC2",colour="Grupo")
qplot(df.pca.pc1,df.pca.pc2, color = factor(df.fuzzy.3.clusters.negSil[,1])) + scale_colour_manual(values=col) + labs(title="Clustering difuso sobre PCA",x="PC1",y="PC2",colour="Cluster")

a = rownames(df.fuzzy.3.clusters.negSil[ df.fuzzy.3.clusters.negSil[,1] == "0 (Silhuette < 0)",  ])
b = rownames(df.fuzzy.3.clusters.0[ df.fuzzy.3.clusters.0[,1] == "0 (Membresía < 0.6)",  ])
length(a)
length(b)

length( intersect(a,b) )

length( intersect(a,b) ) / length(b)

df.fuzzy.3.clusters.0yNegSil = df.fuzzy.3.clusters.0
df.fuzzy.3.clusters.0yNegSil[ intersect(a,b), 1 ] = "Membresía < 0.6 y Silhuette < 0"
df.fuzzy.3.clusters.0yNegSil[ df.fuzzy.3.clusters.0yNegSil[, 1] == "Membresía < 0.6 y Silhuette < 0",  ]
df.fuzzy.3.clusters.0yNegSil[ df.fuzzy.3.clusters.0yNegSil[, 1] == "0 (Membresía < 0.6)",  ] = "Membresía < 0.6"

col <- c("#766272","#4BDBC0","#F5AF3B","#000000","#FF0000")

qplot(df.pca.pc1,df.pca.pc2, color = df.fuzzy.3.clusters.0yNegSil[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre PCA",x="PC1",y="PC2",colour="Grupo")

head(df.fuzzy.3.clusters.0, n=10)

head(df.fuzzy.3.clusters[,2])

head(df.fuzzy.3.negSil)

#reflexion
#
# creo que la posta es elegir un K con el menor "ruido" (membership > 0.6), en este caso
# parecería ser el 2
#
#
#
#