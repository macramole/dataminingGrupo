library(cluster)
library(MASS)
library(fpc)
library(doMC)

registerDoMC(3)

df = read.csv("../tp1/data_tp1.csv",row.names = 1)

summary(df)
rownames(df)
nrow(df)

df.dist = dist(df, method = "euclidean")

#barrer k, memb.exp
df.fuzzes = foreach(varK = 2:10) %dopar% {
  fanny( df.dist, k = varK, diss = T,  memb.exp = 1.5, keep.diss = F, keep.data = F)
}
df.fuzz = fanny( df.dist, k = 4, diss = T,  memb.exp = 1.5, keep.diss = F, keep.data = F)

summary(df.fuzz)
head(df.fuzz$membership)
#df.fuzzy =

