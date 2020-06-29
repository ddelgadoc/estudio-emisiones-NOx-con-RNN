#Cargar datos con Import Dataset from excel
library(readxl)
data_ok <- read_excel("C:/ALMACEN/Documentos/Máster Ing de Organizaón UPM/TFM/Códigos/Análisis estadistico en R/Final_Todo_Sin_valores_erroneos.xlsx") #la ruta cambia según donde tengas el fichero
head(data_ok)
str(data_ok) #estructura de variable en R
#Analisis estadístico unidim
summary(data_ok$`NOx mass`)
var(data_ok$`NOx mass`)
sd(data_ok$`NOx mass`)
boxplot(data_ok$`NOx mass`,main="Boxplot de Caudal Másico de NOx",ylab="NOx(g/s)")
hist(data_ok$`NOx mass`,main="Histograma del Caudal Másico de NOx",xlab="Emisiones NOx(g/s)",col = "blue")
#correlacion
data <- data.frame(data_ok[,4:8],data_ok[,11:12])
co <- round(cor(data),3)
#mapa de calor de correlaciones #activar paquete corrplot
corrplot(co,method = "color",type="lower" ,order = "FPC",addCoef.col = T,  tl.col = "black",tl.srt = 45)
#scatterplot matrix de las variables de correlaciones más significativas
pairs(data[c("NOx.mass","Aceleracion","Velocity")])
plot(x=data$Aceleracion,y=data$NOx.mass,main="Scatterplot de NOx vs Aceleración",xlab = "Acelearación (m/s2)",ylab = "Caudal de NOx (g/s)")
#ampliacion del scatterplot
pairs.panels(data[c("NOx.mass","Aceleracion","Velocity")])
#Estandarización train
data_train <- data.frame(train[,5:9],train[,12:13])
mean <- apply(data_train,2,mean)
std <- apply(data_train,2,sd)
data_train <- as.data.frame(scale(data_train, center = mean,scale = std))
data_test <- data.frame(Final_Test_Sin_valores_erroneos[,5:9],Final_Test_Sin_valores_erroneos[,12:13])
data_test <- as.data.frame(scale(data_test,center = mean,scale = std))
#Analisis exploratorio train con variables estandarizadas
summary(data_train$NOx.mass)
var(data_train$NOx.mass)
sd(data_train$NOx.mass)
boxplot(data_train$NOx.mass,main="Boxplot de Caudal Másico de NOx",ylab="NOx(g/s)")
hist(data_train$NOx.mass,main="Histograma del Caudal Másico de NOx",xlab="Emisiones NOx(g/s)",col = "blue")
x = vector() #determinar cantidad de observaciones nulas en NOx despues de la estandarizacion
for (n in data_train$NOx.mass) {
 if(n == 0) append(x,n)
}
L = length(x)
L
#Estadarización específica para acel y sobreacel
nor <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}
dt<- as.data.frame(lapply(train[,12:13],nor))
data_train_norm <- data.frame(data_train[,1:5],dt,train[,11])
#Unir con la variable carga
data_train_ok <- data.frame(data_train,Data_train[,10])
data_test_ok <- data.frame(data_test,Final_Test_Sin_valores_erroneos[,11])
write.csv(data_train_norm,file = "Train_estandarizadoV02.csv",row.names = T)
write.csv(data_test_norm,file = "Test_estandarizadoV02.csv",row.names = T)
#Autocorrelograma
acf(data_s$NOx.mass,lag.max = 50,plot = T,main="Correlaciones entre la variable NOx y sus retardos")
acf(data_s$NOx.mass,lag.max = 10,plot = F) 
acf(data_s$NOx.mass,lag.max = 100,plot = F)
#Ver tendencia en la serie
library(ggplot2)
ggplot(data_s, aes(x = 1:nrow(data_s), y = 'NOx.mass')) + geom_line()
plot(data_s$NOx.mass,xlab = "Tiempo(s)",ylab = "NOx",xlim = c(1,500)) #g´rafico de puntos
data.ts <- ts(data_s$NOx.mass,start = 0) #convertir a una gráfico de serie temporal
plot(data.ts,xlab = "Tiempo(s)",ylab = "NOx",xlim = c(0,500)) #gráfico de serie temporal sin ggplot
#Gráfico de dispersión VS retardos
lag.plot(data.ts,lags = 5,diag = T,diag.col = "blue",do.lines = F,main = "Gráfico de dispersión de NOx vs Retardos")
#Curva de velocidad vs NOx en el tiempo
data_v <- ts(data_s$Velocity,start = 0)
data_NO <- ts(data_s$NOx.mass,start = 0)
plot(data_NO,xlab = "Tiempo(s)",xlim = c(0,500),col="red",main="Curva de Velocidad y caudal de NOx",cex.main=0.9) #gráfico velc vs NOx V01
lines(data_v,col="green",type="l")
legend(x=200,y=6.5,legend = c("Caudal de NOx","Velocidad"),fill = c("red","green"))
#Curva de velocidad vs NOx en el tiempo otra forma
M <- ts(data.frame(data_NO,data_v))
plot(M,plot.type = "single",lty=1:3,col=c("red","green"),xlim=c(0,500),xlab = "Tiempo(s)", main="Curva de Velocidad y caudal de NOx",cex.main=0.9)
legend(x=200,y=6.5,legend = c("Caudal de NOx","Velocidad"),fill = c("red","green"),cex=0.9)
#Curva de aceleración vs NOx en el tiempo
data_a <- ts(data_s$Aceleracion,start = 0)
plot(data_NO,xlab = "Tiempo(s)",xlim = c(0,500),ylim=c(-5,7),col="red",main="Curva de aceleración y caudal de NOx",cex.main=0.9) 
lines(data_a,col="blue",type="l")
legend(x=200,y=7.5,legend = c("Caudal de NOx","Aceleración"),fill = c("red","blue"))
M <- ts(data.frame(ts(data$NOx.mass,start = 0),ts(data$Aceleracion,start = 0)))
plot(M,plot.type = "single",lty=1:3,col=c("red","blue"),xlim=c(0,500),ylim=c(-5,7),xlab = "Tiempo(s)", main="Curva de Aceleración y caudal de NOx",cex.main=0.9)
legend(x=200,y=7,legend = c("Caudal de NOx","Aceleración"),fill = c("red","blue"),cex=0.9)
#Curva de sobreaceleración vs NOx en el tiempo
data_sa <- ts(data$Sobreace,start = 0)
plot(data_NO,xlab = "Tiempo(s)",xlim = c(0,500),ylim=c(-5,7),col="red",main="Curva de sobreaceleración y caudal de NOx",cex.main=0.9) 
lines(data_sa,col="darkmagenta",type="l")
legend(x=200,y=7.5,legend = c("Caudal de NOx","Sobreaceleración"),fill = c("red","darkmagenta"))
M <- ts(data.frame(ts(data$NOx.mass,start = 0),ts(data$Sobreace,start = 0)))
plot(M,plot.type = "single",lty=1:3,col=c("red","darkmagenta"),xlim=c(0,500),xlab = "Tiempo(s)", main="Curva de Aceleración y caudal de NOx",cex.main=0.9)
#Detalles de la curva de velocidad
plot(data_NO,xlab = "Tiempo(s)",xlim = c(0,50),col="red",main="Curva de Velocidad y caudal de NOx",cex.main=0.9) 
lines(data_v,col="green",type="l")
legend(x=35,y=6.5,legend = c("Caudal de NOx","Velocidad"),fill = c("red","green"))
#Detalles de la curva de aceleracion
plot(data_NO,xlab = "Tiempo(s)",xlim = c(0,50),col="red",main="Curva de aceleración y caudal de NOx",cex.main=0.9) 
lines(data_a,col="blue",type="l")
legend(x=35,y=6.5,legend = c("Caudal de NOx","Aceleración"),fill = c("red","blue"))


