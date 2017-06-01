library(lmtest)
library(plotly)
library(gridExtra)
library(grid)

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
ljung.tests <- data.frame()
for(moneda in camposSinTend){
  ljung.test <- Box.test(df[moneda], lag= lagMax, type = "Ljung-Box")
  ljung.tests <- rbind(ljung.tests,data.frame(moneda,ljung.test$statistic,"2.2e-16"))
  acf(df[moneda], lag.max = lagMax, main = moneda)
}

names(ljung.tests) <- c("moneda","ljung.test.statistic","p.value")
dev.off()
grid.table(ljung.tests,rows = NULL)

# Gráficos de correlación cruzada

pnl <- function(x, y = x,xlim,ylim) { par(new = TRUE); ccf(x, y, lag.max = lagMax,yaxt="n",xaxt="n",xlim=c(-350,350),ylim=c(-0.8,0.8)); abline(v=0,col="red",lty=2) }
pairs(df[,12:18], lower.panel = pnl, diag.panel = NULL, upper.panel =NULL,cex.labels=1,xlim=c(-350,350),ylim=c(-0.8,0.8))
