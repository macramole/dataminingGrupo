setwd("C:\\Users\\Gonzalo\\Desktop\\Gonzalo\\Datamining\\Series de tiempo\\TP")
base <- read.table(file = "base_sin_tend.txt", sep = "\t", dec = ".", stringsAsFactors = F, header = T)

par(mfrow = c(2,1))





acf(base$USD_EUR, lag.max = 365)
acf(diff(base$USD_EUR), lag.max = 365)

acf(base$BITCOIN_COINDESK, lag.max = 365)
acf(diff(BITCOIN_COINDESK), lag.max = 365)

acf(base$USD_MEX, lag.max = 365)
acf(diff(base$USD_MEX), lag.max = 365)

acf(base$USD_BZR, lag.max = 365)
acf(diff(base$USD_BZR), lag.max = 365)

acf(base$USD_INR, lag.max = 365)
acf(diff(base$USD_INR), lag.max = 365)

acf(base$USD_SWF, lag.max = 365)
acf(diff(base$USD_SWF), lag.max = 365)

acf(base$USD_JPY, lag.max = 365)
acf(diff(base$USD_JPY), lag.max = 365)



par(mfrow = c(1,1))

plot(c(0:39) ,Mod(fft(base$BITCOIN_COINDESK[(1213-365*3):1213]))[1:(40)], type = "h", xlab = "bitcoin")
lines(Mod(fft(base$BITCOIN_COINDESK_sintend[(1213-365*3):1213]))[1:(40)], type = "l", col ="red")




rango <- c()
max_min <- c()

for( i in 2:8){
    rango <- c(rango, max(base[,i])- min(base[,i]) )
  max_min <- c(max_min, max(base[,i])/min(base[,i]) )
  }

rangos <- rbind(rango, max_min)
colnames(rangos) <- colnames(base)[2:8]


library(tseries)
library(forecast)

adf.test(base$BITCOIN_COINDESK, alternative = "e", k = 1)
kpss.test(diff(((base$BITCOIN_COINDESK))), null ="Level")

par(mfrow = c(2,2))
plot((base$BITCOIN_COINDESK), type ="l")
plot(log(base$BITCOIN_COINDESK), type ="l", col ="red")
plot(diff(base$BITCOIN_COINDESK), type ="l")
plot(diff(log(base$BITCOIN_COINDESK)), type ="l")



# esto cuenta como el test de estacionariedad y el autoarima
## Salen dos conclusiones:
# Bitcoion es el unico integrado de orden 2, lo que habla de una serie mas volatil, y con procesos de aceleracion
#Bitocoin es el modelo mas complejo, en Arima, que haya un 3 en alguno de los elementos ya es muy complejo,
#el bitcoin es un (5,2,0), lo que lo hace ultra complejo.


auto.arima(base$BITCOIN_COINDESK) #(5,2,0)

auto.arima(base$USD_EUR)#(0,1,0)
auto.arima(base$USD_JPY)#(0,1,0) Drift
auto.arima(base$USD_SWF)#(1,1,2)

auto.arima(base$USD_BZR)#(2,1,2)
auto.arima(base$USD_INR)#(0,1,0)
auto.arima(base$USD_MEX)#(0,1,0)





