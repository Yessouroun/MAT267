library(ggplot2)
library(gridExtra)
library(forecast)
library(tseries)
library(ggfortify)
library(MASS)

#Definición
path = "/Users/Laura/Desktop/datos P1 series.txt"
datos = scan(path)
serie = ts(datos, frequency = 12, start = c(1968,1))
vec = seq(1968, 1978 + 11/12, by = 1/12)
df <- data.frame("tiempo" = vec, "xt" = datos)
plot(df)
#132 mediciones

#Primer análisis
autoplot(serie, ts.colour = "red")+ ggtitle("Serie flu.dat") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Año") + ylab(expression("Muertes por 10"^4* " personas"))
cat("la media de los datos es: ")
mean(datos)
cat("la varianza de los datos es: ")
var(datos)

#BoxCox
lambda = BoxCox.lambda(serie, method = "loglik", lower = -5, upper = 5)
lambda
datos_n <- BoxCox(serie,lambda)

#Análisis post-BoxCox
autoplot(datos_n, ts.colour = "red") + ggtitle("Serie flu.dat transformada") + theme(plot.title = element_text(hjust = 0.5))
cat("la media de los nuevos datos es: ")
mean(datos_n)
cat("la varianza de los nuevos datos es: ")
var(datos_n)

#Regresión
error = c()

df <- data.frame("tiempo" = vec-1900, "xt" = datos)
for(i in 1:131) {
  regre = lm(xt ~ poly(tiempo, i, raw = TRUE), data = df)
  error[i] = sum(regre$residuals^2)
}

error
pestimado = which.min(error)
pestimado
error[pestimado]

#Gráfico reg + datos
ggplot(df, aes(x=tiempo, y=xt)) + geom_point() + stat_smooth(method = 'lm', formula = y ~ poly(x, pestimado, raw = TRUE), size = 1) + xlab("Tiempo-1900") + ylab(expression("Muertes por 10"^4* " personas")) + ggtitle("Ajuste lineal multivariado")+theme(plot.title = element_text(hjust = 0.5))

regre = lm(xt ~ poly(tiempo, 117, raw = TRUE), data = df)
summary(regre)

#Descomposición
fit = decompose(serie, type = "additive")
autoplot(fit)

error2 = sum(((fit$rand)[7:126])^2)

#HoltWinters
HW = HoltWinters(serie, seasonal = "additive")
plot(HW)
error3 = HW$SSE
error3

plot(fitted(HW))

