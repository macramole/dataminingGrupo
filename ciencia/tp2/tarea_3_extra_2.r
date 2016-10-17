library(arules)

df = read.csv("data_clean.csv",row.names = 1)
df.xy <- df[,c(1,2)]
df <- df[-c(1,2)]

summary(df$MC_z)
hist(df$MC_z)
df.MC_z.discret = discretize(df$MC_z,method = "interval",categories = 3)

qplot(df.xy$x,df.xy$y, color = df.MC_z.discret ) + labs(title="Corrimiento al rojo sobre X e Y",x="X",y="Y", colour= "MC_z estandarizado")
qplot(df.xy$x,df.xy$y, color = df$MC_z ) + labs(title="Corrimiento al rojo sobre X e Y",x="X",y="Y", colour= "MC_z estandarizado")
