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
autocorrelaciones

plot_ly( y = names(autocorrelaciones), x = autocorrelaciones )

par( mfrow = c(2,4) )
# par( mfrow = c(1,1) )
acf( df$BITCOIN_COINDESK_sintend, lag.max = lagMax  )
acf( df$USD_EUR_sintend, lag.max = lagMax )
acf( df$USD_BZR_sintend, lag.max = lagMax )
acf( df$USD_INR_sintend, lag.max = lagMax )
acf( df$USD_MEX_sintend, lag.max = lagMax )
acf( df$USD_JPY_sintend, lag.max = lagMax )
acf( df$USD_SWF_sintend, lag.max = lagMax )
