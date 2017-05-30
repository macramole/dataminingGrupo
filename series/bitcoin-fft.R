library(plotly)

df = read.csv("base_sin_tend.txt", sep = "\t")

range = 118:length(df$BITCOIN_COINDESK_sintend)
N = length(range)

camposSinTend = c("BITCOIN_COINDESK_sintend", "USD_EUR_sintend", "USD_BZR_sintend", "USD_INR_sintend", "USD_MEX_sintend", "USD_JPY_sintend", "USD_SWF_sintend")

par(mfrow=c(1,1))
plot( df$BITCOIN_COINDESK_sintend, type = "l" )

df.fft.original = matrix(ncol = length(camposSinTend), nrow = length(range) )
df.fft = matrix(ncol = length(camposSinTend), nrow = length(range)/2 )

colnames(df.fft.original) = camposSinTend
colnames(df.fft) = camposSinTend

for ( campo in camposSinTend ) {
  serie = df[,campo][range]
  # serie = serie - mean(serie)
  # serie = serie / sd(serie)
  # NORMALIZO !!
  serie = scale(serie)
  
  currentOriginal = fft( serie )
  df.fft.original[,campo] = currentOriginal
  
  currentFFT = Mod( currentOriginal )
  currentFFT = currentFFT[1:(length(currentFFT)/2)]
  # Calibro los datos del FFT
  currentFFT = currentFFT / N
  df.fft[,campo] = currentFFT
}

# par(mfrow=c(1,1))
par(mfrow=c(2,4))
for ( campo in camposSinTend ) {
  plot(1:12, df.fft[,campo][2:13], type="h", main = campo)
}

library(dplyr)

#BAR
as.data.frame(df.fft) %>%
  slice(2:13) %>%
  plot_ly( y = ~BITCOIN_COINDESK_sintend, x = 1:12, type = "bar", name="Bitcoin"  ) %>%
  add_trace( y = ~USD_EUR_sintend, x = 1:12, type = "bar", name="Euro"  ) %>%
  add_trace( y = ~USD_JPY_sintend, x = 1:12, type = "bar", name="Japon"  ) %>%
  add_trace( y = ~USD_SWF_sintend, x = 1:12, type = "bar", name="Suecia"  ) %>%
  add_trace( y = ~USD_BZR_sintend, x = 1:12, type = "bar", name="Brazil"  ) %>%
  add_trace( y = ~USD_INR_sintend, x = 1:12, type = "bar", name="India"  ) %>%
  add_trace( y = ~USD_MEX_sintend, x = 1:12, type = "bar", name="Mexico" ) 
  

#LINE
as.data.frame(df.fft) %>%
  slice(2:13) %>%
  plot_ly( y = ~BITCOIN_COINDESK_sintend, x = 1:12, type = "scatter", mode="lines+markers", name="Bitcoin"  ) %>%
  add_trace( y = ~USD_EUR_sintend, x = 1:12, type = "scatter", name="Euro"  ) %>%
  add_trace( y = ~USD_JPY_sintend, x = 1:12, type = "scatter", name="Japon"  ) %>%
  add_trace( y = ~USD_SWF_sintend, x = 1:12, type = "scatter", name="Suecia"  ) %>%
  add_trace( y = ~USD_BZR_sintend, x = 1:12, type = "scatter", name="Brazil"  ) %>%
  add_trace( y = ~USD_INR_sintend, x = 1:12, type = "scatter", name="India"  ) %>%
  add_trace( y = ~USD_MEX_sintend, x = 1:12, type = "scatter", name="Mexico" )
  

df.fft.bitcoin.original = fft( df$BITCOIN_COINDESK_sintend[range] )
df.fft.bitcoin = Mod( df.fft.bitcoin.original )
df.fft.bitcoin = df.fft.bitcoin[1:(length(df.fft.bitcoin)/2)]
plot(df.fft.bitcoin[1:50], type="h")


plot_ly( y = df.fft.bitcoin[1:50] , x = 0:49, type = "bar"  )


#layout( xaxis = list( title = "" ), yaxis = list( title = "Promedios" ) )


# frecuenciasDejo = c(2)
frecuenciasDejo = c(2,5,6,8,9)
filtro = rep(0, length(df.fft.bitcoin.original))
filtro[frecuenciasDejo+1] = 1

df.fft.bitcoin.filtrado = df.fft.bitcoin.original * filtro
df.bitcoin.filtrado = fft(df.fft.bitcoin.filtrado, inverse = T)/length(range)

plot(df$Fecha[range], df$BITCOIN_COINDESK_sintend[range], type="l")
lines(df$Fecha[range], Re(df.bitcoin.filtrado), type="l", col="red")

