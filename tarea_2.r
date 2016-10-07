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


