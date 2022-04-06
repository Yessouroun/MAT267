
library(ggplot2)
library(gridExtra)
library(forecast)
library(tseries)
library(ggfortify)
library(MASS)
library(Metrics)

#Definición
path = "/Users/Laura/Desktop/datos P2 series.txt"
datos = scan(path)
vec = seq(1900, 1970.5, by = 0.5)
df <- data.frame("tiempo" = vec, "xt" = datos)
serie = ts(datos, frequency = 2, start = c(1900,1))

#Primer análisis
autoplot(serie, ts.colour = "red", title ="Gráfico serie globtemp.dat")

#suavizamiento exponencial simple
SES = ses(serie, h = 2)

plot(SES, main = "Predicción a partir de Suaviamiento Exponencial Simple")
summary(SES)

#sin tendencia
dif1 = diff(serie)
plot(dif1)

dif2 = diff(dif1)
plot(dif2)

fit = decompose(dif2, type = "additive")
autoplot(fit)

#Regresión lineal
lineal = lm(xt ~ tiempo, data = df)
lineal
plot(df$tiempo, df$xt)
summary(lineal)


#Descomposición
fit = decompose(serie, type = "additive")
autoplot(fit)

