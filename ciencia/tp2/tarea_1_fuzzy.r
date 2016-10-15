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

plot_ly(fuzzyResults.max, x = k, y= silhuette_avg.width, name="silhuette") %>%
  add_trace(x = k, y = coeff, name="coeff")

plot_ly(fuzzyResults[fuzzyResults$k == 2,], x = memb.exp, y = silhuette_avg.width, name = "k = 2")
add_trace(fuzzyResults[fuzzyResults$k == 3,], x = memb.exp, y = silhuette_avg.width, name = "k = 3")
add_trace(fuzzyResults[fuzzyResults$k == 4,], x = memb.exp, y = silhuette_avg.width, name = "k = 4")

plot_ly(fuzzyResults[fuzzyResults$k == 2,], x = memb.exp, y = coeff, name = "k = 2")
add_trace(fuzzyResults[fuzzyResults$k == 3,], x = memb.exp, y = coeff, name = "k = 3")
add_trace(fuzzyResults[fuzzyResults$k == 4,], x = memb.exp, y = coeff, name = "k = 4")



fuzzyResults.max

df.fuzzy.2 = fanny( df.dist, k = 2, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)
df.fuzzy.3 = fanny( df.dist, k = 3, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)
df.fuzzy.4 = fanny( df.dist, k = 4, diss = T,  memb.exp = 1.2, keep.diss = F, keep.data = F)

df.fuzzy.2.negSil = df.fuzzy.2$silinfo$widths[ df.fuzzy.2$silinfo$widths[,3] < 0, ]
cbind ( df.fuzzy.2$membership[ rownames(df.fuzzy.2.negSil), ], df.fuzzy.2.negSil[,3] )
df.fuzzy.2.highSil = df.fuzzy.2$silinfo$widths[ df.fuzzy.2$silinfo$widths[,3] > 0.4, ]
cbind ( df.fuzzy.2$membership[ rownames(df.fuzzy.2.highSil), ], df.fuzzy.2.highSil[,3] )

df.fuzzy.3.negSil = df.fuzzy.3$silinfo$widths[ df.fuzzy.3$silinfo$widths[,3] < 0, ]
cbind ( df.fuzzy.3$membership[ rownames(df.fuzzy.3.negSil), ], df.fuzzy.3.negSil[,3] )

pdf("asd.pdf")
plot(silhouette(df.fuzzy.2))
plot(silhouette(df.fuzzy.3))
plot(silhouette(df.fuzzy.4))
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

fuzzyResults %>%
  filter( k == 2, coeff > 0 ) %>%
  arrange(memb.exp) %>%
  plot_ly(x = memb.exp, y = coeff  )

add_trace( x = c(1,2), y=c(1,0) )


#reflexion
#
# creo que la posta es elegir un K con el menor "ruido" (membership > 0.6), en este caso
# parecería ser el 2
#
#
#
#