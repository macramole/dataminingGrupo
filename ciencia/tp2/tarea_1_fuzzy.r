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

f=read.csv("fuzzy_results.csv")
#head(f)
table(f$k) 

a = c()
for ( i in min(f$k):max(f$k) ) {
  kFiltered = f[ f$k == i , ]
  maxSilhuetteIndex = which.max(kFiltered[,c("silhuette_avg.width")])
  
  a = rbind(a, kFiltered[maxSilhuetteIndex,] )
}


library("plotly")
library(dplyr)

plot_ly(a, x = k, y= silhuette_avg.width) %>%
  add_trace(x = k, y = coeff)

