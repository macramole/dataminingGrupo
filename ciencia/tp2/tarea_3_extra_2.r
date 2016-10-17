library(arules)

df = read.csv("data_clean.csv",row.names = 1)

df.xy <- df[,c(2,3)]
df <- df[-c(1,2)]

summary(df$MC_z)
hist(df$MC_z)
df.MC_z.discret = discretize(df$MC_z,method = "interval",categories = 4)

df.xy = scale(df.xy)

col = c("#000000", "#848484", "#dbdbdb")
col = c("#f93e3e", "#f98f3e", "#ffff00")
col = c("#ff2b2b", "#ff872b", "#2b78ff", "#522bff")

qplot(df.xy$x,df.xy$y, color = df.MC_z.discret ) + scale_colour_manual(values=col) + scale_size( range = c(0.1) ) + theme(panel.background = element_rect(fill = 'white')) + labs(title="Corrimiento al rojo sobre X e Y",x="X",y="Y", colour= "MC_z")
qplot(df.xy$x,df.xy$y, alpha = df$MC_z ) +  labs(title="Corrimiento al rojo sobre X e Y",x="X",y="Y", colour= "MC_z")

str(base)
base = read.csv("data_clean.csv",row.names = 1)
base.xy = base[,c(1,2)]
base = base[,-c(1,2)]

K1<- kmeans(x = base, 3 , iter.max = 50)
col <- c("#766272","#4BDBC0","#F5AF3B")
qplot(base.xy$x,base.xy$y, color = as.factor(K1$cluster) )  + scale_colour_manual(values=col) + theme(panel.background = element_rect(fill = 'white')) + labs(title="Corrimiento al rojo sobre X e Y",x="X",y="Y", colour= "MC_z")

