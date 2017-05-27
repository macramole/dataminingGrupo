library(forecast)


setwd("C:\\Users\\Gonzalo\\Desktop\\Gonzalo\\Datamining\\Series de tiempo\\TP")
base <- read.table(file = "bitcoin_solo_monedas_interpolado.csv", 
                   sep = "\t", dec = ".", stringsAsFactors = F,
                   header = T)

str(base)
summary(base)

tail(base$Fecha) # min 24/7/2010
head(base$Fecha) #max 23/4/2017
nrow(base)/365 # 6.78 años

#ver la forma de Bitcoin
max(base$BITCOIN_COINDESK)

plot(base$BITCOIN_COINDESK[1100:1400], x = rownames(base)[1100:1400], type = "l") #maximo esta entre 1200-1250
plot(base$BITCOIN_COINDESK[1200:1250], x = rownames(base)[1200:1250], type = "l") #maximo esta entre 1220-1240
plot(base$BITCOIN_COINDESK[1220:1240], x = rownames(base)[1220:1240], type = "l") 

base[1220:1240, 1:2] # se puede cortar en 2013
base[1230:1270, 1:2] # se puede cortar en 2013

plot(base$BITCOIN_COINDESK, type = "l") #maximo esta entre 1200-1250
abline(v = 1262, col = "pink" ) 


#sacar los datos antes de 2013

base <- base[1263:nrow(base),]
tail(base$Fecha) # max 23/4/2017
head(base$Fecha) # min 1/1/2014
nrow(base)/365 # 3.32 años


#transformar en una matriz de serie de tiempo
base <- base [-which(base$Fecha == "29/2/2016"),] #sacar el 29 de febrero
base$day <- 0
base$month <- 0
base$year <- 0
for (i in 1:nrow(base)) { #loop para tranformar las fechas a numerico
  base [ i, 9:11] <- as.vector(strsplit(x = base$Fecha[i], split = "/" )[[1]])
}
base$Fecha <- seq(from = 2014,to = (nrow(base)/365 + 2013.999), length.out = nrow(base))

#loop para invertir todas las monedas
for (i in 3:8){
  base[,i] <- base[,i]^(-1)
  
}



#identificar la tendencia
par(mfrow = c(2,4))

for (i in 2:8){
  plot( x = base$Fecha, base[,i], main = names(base)[i], type = "l")
}

  #hay un cero en bitcoin
  tail(base[,1:2], n = 100)
  which(base$BITCOIN_COINDESK == 0)
  base$BITCOIN_COINDESK[1190] <- mean(base$BITCOIN_COINDESK[c(1189,1191)])#reemplazmos por la media de los dos des puntos alrededor

  #loop por las 7 series
  #probando tendencia: lineal, log, cuadratica, inversa
  R2_tend <- matrix(ncol = 7, nrow = 4)
  colnames(R2_tend) <- colnames(base)[2:8]
  rownames(R2_tend) <- c("lin", "log", "inversa", "cuadratica")
  
  for (i in 2:8){
    R2_tend[1,i-1]<- summary(lm(base[,i]~ base$Fecha))$adj.r.squared
    R2_tend[2,i-1]<- summary( lm( base[,i]~ I(log(base$Fecha))))$adj.r.squared
    R2_tend[3,i-1]<- summary( lm( base[,i]~ I( 1/base$Fecha)))$adj.r.squared
    R2_tend[4,i-1]<- summary( lm( base[,i]~ I(base$Fecha^2)+base$Fecha))$adj.r.squared
  }
 
  R2_tend # todos cuadraticos exceptio MEX que es lineal
  
  windows()
  par(mfrow = c(2,4))
  for (i in 2:8){
    plot( x = base$Fecha, base[,i], main = names(base)[i], type = "l")
    if( i == 6){
      lines( x = base$Fecha, predict(lm(base[,i]~ base$Fecha)), type = "l", col ="pink")
    } else {
      lines( x = base$Fecha, predict(lm(base[,i]~ I(base$Fecha^2)+base$Fecha)), type = "l", col ="pink")
    }
  }  
  
#Agregaar columnas con la tendencia  

  for (i in 2:8){
    nomb <- colnames(base)
  if( i == 6){
      base$V <- base[,i] - predict(lm(base[,i]~ base$Fecha))
    } else {
      base$V <- base[,i] - predict(lm(base[,i]~ I(base$Fecha^2)+base$Fecha))
    }
  colnames(base) <- c(nomb, paste(names(base)[i], "_sintend", sep ="") )
}  
  
  
  
  
  
  
  
  
  
