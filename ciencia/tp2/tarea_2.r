####################
####  DATA PREP ####
####################

df = read.csv("data.csv")
str(df)
table(df$MC_class)
df = df[ df$MC_class == "Galaxy", ]
table(df$MC_class)

summary(df$Rmag) 
hist(df$Rmag)
nrow( df[ df$Rmag < 24, ] )
df = df[ df$Rmag < 24,]

summary(df$phot_flag)
hist(df$phot_flag)
boxplot(df$phot_flag)
nrow( df[ df$phot_flag < 8, ] )
df = df[ df$phot_flag < 8,]

summary(df$ApD_Rmag)
hist(df$ApD_Rmag)
boxplot(df$ApD_Rmag)
nrow( df[ df$ApD_Rmag < 0, ] )
df[ df$ApD_Rmag < 0, "ApD_Rmag"] = 0

rownames(df)<-df[,1]
df<-df[,-1]

df<-df[,c(1,2,3,4,8,9,10,11)] #columnas con las que vamos a trabajar
df$BjMag.comp<-df[,6]
df[,c(5,6,7)]<-df[,c(5,6,7)]-df[,8] #normalizamos segun S280
df<-as.data.frame(scale(df))     #estandarizamos

dim(df)
head(df)
str(df)
summary(df)

df <- df[ -which(is.na(df[,8])), ]
df <- df[ -which(is.na(df[,5])), ]

write.csv(df, file = "data_clean.csv", row.names = T)

