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

# Mismo gráfico que el anterior, pero expresado como % de puntos no significativos

plot_ly( y = names(autocorrelaciones), x = autocorrelaciones/lagMax)

# Gráficos de autocorrelación junto con la prueba Ljung-Box

par( mfrow = c(2,4) )
for(moneda in camposSinTend){
  print(moneda)
  print(Box.test(df[moneda], lag= lagMax, type = "Ljung-Box"))
  acf(df[moneda], lag.max = lagMax, main = moneda)
}


# Gráficos de correlación cruzada

pnl <- function(x, y = x) { par(new = TRUE); ccf(x, y, lag.max = lagMax) }
pairs(df[,12:18], lower.panel = pnl, diag.panel = NULL, cex.labels = 1)
