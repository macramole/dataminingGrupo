library(lmtest)
library(plotly)

# Autocorrelación

df = read.csv("base_sin_tend.txt", sep = "\t")
head(df)

par(mfrow=c(2,4))
plot( df$Fecha, df$BITCOIN_COINDESK_sintend, type="l" )
plot( df$Fecha, df$USD_EUR_sintend, type="l" )
plot( df$Fecha, df$USD_EUR_sintend, type="l" )

lagMax = 365 * 1

camposSinTend = c("BITCOIN_COINDESK_sintend", "USD_EUR_sintend", "USD_BZR_sintend", "USD_INR_sintend", "USD_MEX_sintend", "USD_JPY_sintend", "USD_SWF_sintend")
autocorrelaciones = c()
threshold_menores = 0.05

for ( campo in camposSinTend ) {
  autocorrelacion = acf( df[,campo], lag.max = lagMax  )
  autocorrelacionesMenores = length ( autocorrelacion$acf[ abs(autocorrelacion$acf) < threshold_menores ] )
  autocorrelaciones = c(autocorrelaciones, autocorrelacionesMenores )
}
names(autocorrelaciones) = camposSinTend

plot_ly( y = names(autocorrelaciones), x = autocorrelaciones )
plot_ly( y = names(autocorrelaciones), x = autocorrelaciones/lagMax)

# Gráficos de autocorrelación junto con la prueba Ljung-Box

par( mfrow = c(2,4) )
for(moneda in camposSinTend){
  print(moneda)
  print(Box.test(df[moneda], lag= lagMax, type = "Ljung-Box"))
  acf(df[moneda], lag.max = lagMax)
}

# Gráficos de correlación cruzada

par( mfrow = c(3,4) )
for(moneda1 in camposSinTend){
  for(moneda2 in camposSinTend){
    ccf(df[moneda1],df[moneda2], lag.max = lagMax,main=paste(moneda1,"&",moneda2))
  }
}
