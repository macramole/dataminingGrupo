### PAra trabajar sobre el dataframe que tiene a las 8 Vbles: ID, 3 Johnson normalizadas, BvMag, RMag, APDRMag y Mcz
# base0<- read.csv(file="https://raw.githubusercontent.com/macramole/dataminingGrupo/fcf6cbe2a149ac91fa782cf3eb981b07464777f5/ciencia/tp1/data_tp1.csv")
summary(base0)
rownames(base0)<-base0[,1]
base0<-base0[,-1]
####### descriptivas para chequear

dim(base0)
head(base0)
str(base0)
summary(base0)
colnames(base0)

base<-base0[,c(1,3,5,11,27,9,11,13)]
str(base)
base[,c(6,7,8)]<-base[,c(6,7,8)]-base[,5] 
# base<-base[,-5]
base<-base[-which(is.na(base[,6])),]
summary(base)

colnames(base)[c(6:8)] = c("UjMAG.norm", "BjMAG.norm", "VjMAG.norm")
write.csv(base,"../tp1/data_tp1.csv")

head(base)
