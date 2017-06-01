library(lmtest)
library(plotly)
library(gridExtra)
library(grid)

# Autocorrelación

?acf

df = read.csv("base_sin_tend.txt", sep = "\t")
head(df)

par(mfrow=c(2,4))
plot( df$Fecha, df$BITCOIN_COINDESK_sintend, type="l" )
plot( df$Fecha, df$USD_EUR_sintend, type="l" )
plot( df$Fecha, df$USD_EUR_sintend, type="l" )

lagMax = 365 * 2

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

png('plots/auto-correlacion.png',width=1000,height=600)
par( mfrow = c(2,4) )
ljung.tests <- data.frame()
for(moneda in camposSinTend){
  ljung.test <- Box.test(df[moneda], lag= lagMax, type = "Ljung-Box")
  ljung.tests <- rbind(ljung.tests,data.frame(moneda,ljung.test$statistic,"2.2e-16"))
  acf(df[moneda], lag.max = lagMax,cex.lab=1.5,cex.axis=1.5,ylim=c(-1,1),main="")
  title(main=gsub("_sintend","",moneda),cex.main=1.5)
}
dev.off()

names(ljung.tests) <- c("moneda","ljung.test.statistic","p.value")
dev.off()
png('plots/ljung-box.png')
grid.table(ljung.tests,rows = NULL)
dev.off()

# Gráficos de correlación cruzada

df.matrix <- df[,12:18]
head(df.matrix)
names(df.matrix) <- c("BITCOIN_COINDESK","USD_EUR","USD_BZR","USD_INR","USD_MEX","USD_JPY","USD_SWF")
png('plots/correlacion-cruzada.png',width=800,height=600)
pnl <- function(x, y = x,xlim,ylim,cex.axis) { par(new = TRUE); ccf(x, y, lag.max = lagMax,yaxt="n",xaxt="n",xlim=c(-700,700),ylim=c(-0.75,0.75)); abline(v=0,col="red",lty=2) }
pairs(df.matrix, lower.panel = pnl, diag.panel = NULL, upper.panel =NULL,cex.axis=1.7,cex.labels=1.2,xlim=c(-700,700),ylim=c(-0.75,0.75))
dev.off()