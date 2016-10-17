library(cluster)
library(MASS)
library(fpc)
library(doMC)

registerDoMC(3)

df = read.csv("data_clean.csv",row.names = 1) #mirar si esta escalado
df.dist = dist(df, method = "euclidean")

df.fuzzy = fanny( df.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

df.fuzzy.clusters = apply(df.fuzzy$membership, 1, which.max)
df.fuzzy.clusters = cbind(df.fuzzy.clusters, apply(df.fuzzy$membership, 1, max) )

df.fuzzy.clusters.0 = df.fuzzy.clusters
df.fuzzy.clusters.0[df.fuzzy.clusters.0[,2] < 0.6, 1 ] = 0

df.pca <- prcomp(df)
df.pca.pc1 <- df.pca$x[,1]
df.pca.pc2 <- df.pca$x[,2]

col <- c("#000000","#766272","#4BDBC0","#F5AF3B")
qplot(df.pca.pc1,df.pca.pc2, color = df.fuzzy.clusters.0[,1] ) + scale_colour_manual(values=col) + labs(title="Agrupamiento difuso sobre PCA",x="PC1",y="PC2",colour="Grupo")

df.fuzzy.negSil = df.fuzzy$silinfo$widths[ df.fuzzy$silinfo$widths[,3] < 0, ]
df.fuzzy.negSil = cbind ( df.fuzzy$membership[ rownames(df.fuzzy.negSil), ], df.fuzzy.negSil[,3] )
df.fuzzy.clusters.negSil = df.fuzzy.clusters
df.fuzzy.clusters.negSil[ rownames(df.fuzzy.3.negSil), 1 ] = 0

a = rownames(df.fuzzy.clusters.negSil[ df.fuzzy.clusters.negSil[,1] == 0,  ])
b = rownames(df.fuzzy.clusters.0[ df.fuzzy.clusters.0[,1] == 0,  ])
length(a)
length(b)

length( intersect(a,b) )

c = setdiff(rownames(df),intersect(a,b))
length(rownames(df)) - length(intersect(a,b))
length(c)

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



