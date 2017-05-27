library(plotly)

df = read.csv("base_sin_tend.txt", sep = "\t")

range = 118:length(df$BITCOIN_COINDESK_sintend)


camposSinTend = c("BITCOIN_COINDESK_sintend", "USD_EUR_sintend", "USD_BZR_sintend", "USD_INR_sintend", "USD_MEX_sintend", "USD_JPY_sintend", "USD_SWF_sintend")

df.fft.original = matrix(ncol = length(camposSinTend), nrow = length(range) )
df.fft = matrix(ncol = length(camposSinTend), nrow = length(range)/2 )

colnames(df.fft.original) = camposSinTend
colnames(df.fft) = camposSinTend

for ( campo in camposSinTend ) {
  currentOriginal = fft( df[,campo][range] )
  df.fft.original[,campo] = currentOriginal
  
  currentFFT = Mod( currentOriginal )
  currentFFT = currentFFT[1:(length(df.fft.bitcoin)/2)]
  df.fft[,campo] = currentFFT
}

# par(mfrow=c(1,1))
par(mfrow=c(2,4))
for ( campo in camposSinTend ) {
  plot(0:12, df.fft[,campo][1:13], type="h", main = campo)
}


df.fft.bitcoin.original = fft( df$BITCOIN_COINDESK_sintend[range] )
df.fft.bitcoin = Mod( df.fft.bitcoin.original )
df.fft.bitcoin = df.fft.bitcoin[1:(length(df.fft.bitcoin)/2)]
plot(df.fft.bitcoin[1:50], type="h")

plot_ly( y = df.fft.bitcoin[1:50] , x = 0:49, type = "bar"  )





# frecuenciasDejo = c(2)
frecuenciasDejo = c(2,5,6,8,9)
filtro = rep(0, length(df.fft.bitcoin.original))
filtro[frecuenciasDejo+1] = 1

df.fft.bitcoin.filtrado = df.fft.bitcoin.original * filtro
df.bitcoin.filtrado = fft(df.fft.bitcoin.filtrado, inverse = T)/length(range)

plot(df$Fecha[range], df$BITCOIN_COINDESK_sintend[range], type="l")
lines(df$Fecha[range], Re(df.bitcoin.filtrado), type="l", col="red")

