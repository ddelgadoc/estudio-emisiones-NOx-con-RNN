#ESTUDIO DEL MAPE
##Cargar datos de entrada y objetivo
load("matrix_datos_optimización.RData") #abrir directorio de trabajo donde estén los datos
##Callback genera resultados del entrenamiento en un CSV
csv_callback <- callback_csv_logger('training_registro_model_final.log')
##Callback guardar los pesos del mejor modelo
checkpoint_dir <- "checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE) #crea una carpeta en directorio de trabajo
filepath_b <- file.path(checkpoint_dir, "model_final_pesos_best.hdf5")
checkpoint_best <- callback_model_checkpoint(filepath = filepath_b,save_weights_only = TRUE,save_best_only = TRUE,monitor = 'val_mape') 
##Definir modelo
build_model <- function(unit_1,unit_2,unit_3) {   
  model <- keras_model_sequential() %>%
    layer_simple_rnn(units = unit_1,return_sequences = TRUE,dropout = 0.1,recurrent_dropout = 0.2, activation = "tanh",input_shape=list(NULL,7)) %>% 
    layer_simple_rnn(units = unit_2,return_sequences = TRUE,dropout = 0.1,recurrent_dropout = 0.2, activation = "tanh") %>% 
    layer_simple_rnn(units = unit_3,dropout = 0.1,recurrent_dropout = 0.2, activation = "tanh") %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mse",
    metrics = c("mape","mae")
  )
}

##Entrenamiento
model_1 <- build_model(30,35,40)
model_1 %>% summary()
history_1 <- model_1 %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33,callbacks =list(csv_callback,checkpoint_best)
)

##Crear datos de validación
NoVar <- 7
subset <- seq(nrow(samples)-(ceiling(0.33*nrow(samples))-1),nrow(samples),by=1)
val_data_x <- array(0, dim = c(length(subset), 
                               dim(samples)[[2]],
                               NoVar))
val_data_x[] <- samples[subset,,]
val_data_y <- targets[subset]
##Cargar modelo guardado
model <- build_model(30,35,40)
model %>% summary()
model %>% load_model_weights_hdf5("model_final_pesos_best.hdf5")
metric_val <- model %>% evaluate(val_data_x, val_data_y,batch_size=150,verbose = 0)
metric_val
prediction <- model %>% predict(val_data_x)
colnames(prediction) <- "predicciones"
prediction <- cbind(prediction,val_data_y)
prediction_df <- data.frame(prediction)
##Calculo MAPE de cada observación
mape <- 100*(abs(((prediction_df$val_data_y-prediction_df$predicciones)/prediction_df$val_data_y))) #100%
prediction_df <- cbind(prediction_df,mape)
##Guardar en CSV
write.csv(prediction_df,file = "predicciones_modelo_11_condropout.csv",row.names = T)
##Exploracion de la distribución del mape
summary(prediction_df$mape)
var(prediction_df$mape)
sd(prediction_df$mape)
boxplot(prediction_df$mape,main="Boxplot del MAPE",ylab="MAPE(%)")
boxplot(prediction_df$mape,main="Boxplot del MAPE",ylab="MAPE(%)",outline = FALSE) #sin puntos atipicos
hist(prediction_df$mape,main="Histograma de la distribución del MAPE",xlab="Valores del MAPE(%)",ylab = "Frecuencia",
     col = "blue",freq = TRUE)
hist(prediction_df$mape,main="Histograma de la distribución del MAPE",xlab="Valores del MAPE(%)",ylab = "Frecuencia",
     col = "blue",freq = TRUE,breaks = 5000,xlim = c(0,500))
##calculo de la cantidad de puntos atipicos
x = 0 
for (n in prediction$mape) {
  if(n > 140) x <<- x + 1
}
print(x)
##Estudio de los puntos atipicos
punto_at <- prediction[prediction$mape > 140,]
dim(punto_at)
summary(punto_at$predicciones)
summary(punto_at$val_data_y)

compare_pre_at <- data.frame(Predicciones=punto_at$predicciones,Valor_real = punto_at$val_data_y)
compare_pre_at <- data.frame(compare_pre_at[1:100,]
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr
  gather(key = "type", value = "value", -rowname) #libreria tidyr

ggplot(compare_pre_at, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("Observaciones de puntos atípicos") +
  ylab("Emisiones de NOx estandarizadas") 

sin_punto_ati <- pre[pre$mape <= 140,]
compare_pre <- data.frame(Predicciones=sin_punto_ati$predicciones,Valor_real = sin_punto_ati$val_data_y)
compare_pre <- data.frame(compare_pre[1:100,]
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr
  gather(key = "type", value = "value", -rowname) #libreria tidyr

ggplot(compare_pre, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("Observaciones sin puntos atípicos") +
  ylab("Emisiones de NOx estandarizadas") 

summary(sin_punto_ati$predicciones)
par(mfrow=c(2,1)) #dibuja los siguientes gráfico en la misma figura 
hist(sin_punto_ati$predicciones,main="Histograma de las predcciones para observaciones sin puntos atípicos",xlab="Valores de las predicciones",ylab = "Frecuencia",
     col = "blue",freq = TRUE)
hist(punto_at$predicciones,main="Histograma de las predcciones para observaciones de puntos atípicos",xlab="Valores de las predicciones",ylab = "Frecuencia",
     col = "blue",freq = TRUE)
par(mfrow=c(2,1)) #dibuja los siguientes gráfico en la misma figura 
hist(sin_punto_ati$val_data_y,main="Histograma del valor real para observaciones sin puntos atípicos",xlab="Valores reales de NOx",ylab = "Frecuencia",
     col = "14",freq = TRUE)
hist(punto_at$val_data_y,main="Histograma del valor real  para observaciones de puntos atípicos",xlab="Valores reales de NOx",ylab = "Frecuencia",
     col = "14",freq = TRUE)
##Estudio de los puntos atipicos y las variables explicativas
indi <- c(punto_at$X1)
val_data_x_atipico <- val_data_x[indi,4,] 
dim(val_data_x_atipico)
colnames(val_data_x_atipico) <- c("NOx","Veloc","Acel","Sobreacel","Temp","Hum","Carga")
write.csv(val_data_x_atipico,file = "Valores_dato_entrada_val_observaciones_atipicas",row.names = T)
val_data_x_NO_atipico <- val_data_x[-indi,4,] 
colnames(val_data_x_NO_atipico) <- c("NOx","Veloc","Acel","Sobreacel","Temp","Hum","Carga")
write.csv(val_data_x_NO_atipico,file = "Valores_dato_entrada_val_observaciones_NO_atipicas",row.names = T)
##Desecalado
###Cargar muestras_atipicas y muestras_NO_atipicas
unscale <- function(x,mean,sd) {
  (x*sd)+mean
}
NOx_atipico_desscalado <- unscale(muestra_atipicas$NOx,mean,std)
summary(NOx_atipico_desscalado)
hist(NOx_atipico_desscalado,main="Histograma del NOx sin estandarizar para las observaciones atípicas",xlab="Valores del NOx",ylab = "Frecuencia",
     col = "14",freq = TRUE)
NOx_NO_atipica_des <- unscale(muestra_NO_atipica$NOx,mean,std)
summary(NOx_NO_atipica_des)
hist(NOx_NO_atipica_des,main="Histograma del NOx sin estandarizar sin observaciones atípicas",xlab="Valores del NOx",ylab = "Frecuencia",
     col = "14",freq = TRUE)
##cantidad de puntos atipicos en la muestra de puntos atipicos del MAPE
x = 0 
for (n in NOx_NO_atipica_des) {
  if(n > 0.2) x <<- x + 1
}
print(x)
##gráfica conjunta MAPE, valor real NOx y predicciones
pre_descal <- unscale(pre[,2:3],mean,std)
result <- cbind(as.ts(pre_descal[1:100,]),as.ts(pre$mape[1:100]))
colnames(result) <- c("Predicciones","Valor real de NOx","Mape")
plot(result,main="Comparativa MAPE,Valores reales de NOx y Predicciones",col="blue")
##Gráfica MAPE y variable retardos NOx(para el retardo 1 o ultimo valor de la secuencia)
val_data_x_R1 <- val_data_x[,4,] 
colnames(val_data_x_R1) <- c("NOx","Veloc","Acel","Sobreacel","Temp","Hum","Carga")
val_data_x_R1_des <- unscale(val_data_x_R1[,1],mean,std)
val_data_x_R1_des <- data.frame(val_data_x_R1_des)
resultado <- cbind(as.ts(val_data_x_R1_des[1:100,]),as.ts(pre$mape[1:100]))
colnames(resultado ) <- c("Retardo 1 de NOx","Mape")
plot(resultado,main="Comparativa MAPE y el retardo 1 del NOx",col="blue")

#Predicciones finales
##Entrenamiento todos los datos de entrenamiento(val+train)
model <- build_model(30,35,40)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150
)
##Muestra test normalización, importar la muestra test 
data_test <- data.frame(Test_data[,5:9],Test_data[,11])
data_test <- as.data.frame(scale(data_test,center = mean,scale = std)) #mean y std son los calculados para la muestra train
##Muestra test normalización de acel y sobreacl
dt<- as.data.frame(scale(Test_data[,12:13], center = min,scale = den)) #normalizar a rango 0-1, pero no dará este rango porque se usa los datos de train #min y den son los calculados para la muestra train
test <- data.frame(data_test,dt)
##Crear test_x y test_y
test_1 <- data.frame(NOx.mass=test[,5],Velocity=test[,4],Aceleracion=test[,7],Sobreacel=test[,8],Temp=test[,1],Humd=test[,3],Carga=test[,6])#escoger solo las columnas que interesan y convertir a matrix
test_1 <- data.matrix(test_1)
NoVar <- 7
Noensayos <- 6 
lookback <- 4
step <- 1
desf <- 5 #desfase de las variables cinméticas
test_x <- array(0, dim = c(nrow(test_1)-(Noensayos*(lookback+desf-1)), #correcto la longitud
                            lookback / step,
                            NoVar))
test_y <- array(0, dim = c(nrow(test_1)-(Noensayos*(lookback+desf-1))))

j <- 1
for (n in 1:length(index)) { #index para la muestra test obtenido en el escalado
  r <- c((index[[n]][[1]]+lookback):(index[[n]][[2]]-desf+1)) # se suma 1 por el (r[[i]]-1)
  for (i in 1:length(r)) {
    indices <- seq(r[[i]] - lookback / step, (r[[i]]-1),   #para hacer consecutivos los valores
                   length.out = dim(test_x)[[2]])
    indices_des <- seq(r[[i]] - lookback / step + desf, (r[[i]]-1)+ desf,   #para hacer consecutivos los valores
                       length.out = dim(test_x)[[2]])
    indices_sin <- seq(r[[i]] - lookback +1, r[[i]],   #para hacer consecutivos los valores
                       length.out = dim(test_x)[[2]])
    test_x[j,,1] <- test_1[indices,1] #la columna que no tiene desfase
    test_x[j,,2:4] <- test_1[indices_des,2:4] #2:3 columnas que si tienen desfase
    test_x[j,,5:7] <- test_1[indices_sin,5:7]
    test_y[[j]] <- test_1[r[[i]],1] # 1 es la columna de la variable de salida
    j <<- j + 1
  }  
}
save(test_x,test_y,file = "muestras_test.RData")
load("muestras_test.RData")
##Prediccion
metric_test <- model %>% evaluate(test_x, test_y,batch_size=150,verbose = 0)
metric_test
prediction_test <- model %>% predict(test_x)
colnames(prediction_test) <- "predicciones"
prediction <- cbind(prediction_test,test_y)
prediction_df <- data.frame(prediction)
colnames(prediction_df) <- c("predicciones","test_y")
##Calculo MAPE de cada observación
mape <- 100*(abs(((prediction_df$test_y-prediction_df$predicciones)/prediction_df$test_y))) #100%
prediction_df <- cbind(prediction_df,mape)
##Guardar en CSV
write.csv(prediction_df,file = "predicciones_finales_model01.csv",row.names = T)
##Distribucion mape
summary(prediction_df$mape)
var(prediction_df$mape)
sd(prediction_df$mape)
cor(prediction_df$test_y,prediction_df$predicciones)
VE <- 1- (mse(prediction_df$test_y,prediction_df$predicciones)/var(prediction_df$test_y))
VE #variabilidad explicada
plotRegressionError(prediction_df$test_y,prediction_df$predicciones, main= "Error de regresión para la muestra test")
boxplot(prediction_df$mape,main="Boxplot del MAPE",ylab="MAPE(%)")
boxplot(prediction_df$mape,main="Boxplot del MAPE",ylab="MAPE(%)",outline = FALSE) #sin puntos atipicos
hist(prediction_df$mape,main="Histograma de la distribución del MAPE",xlab="Valores del MAPE(%)",ylab = "Frecuencia",
     col = "blue",freq = TRUE,breaks = 500000,xlim = c(0,500))
hist(prediction_df$mape,main="Histograma de la distribución del MAPE",xlab="Valores del MAPE(%)",ylab = "Frecuencia",
     col = "blue",freq = TRUE)
##Curvas de valor real y las predicciones
compare_pre <- data.frame(Predicciones=prediction_df$predicciones,Valor_real = prediction_df$test_y)
compare_pre <- data.frame(compare_pre[1:100,]
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr
  gather(key = "type", value = "value", -rowname) #libreria tidyr

ggplot(compare_pre, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("Observaciones de la muestra test") +
  ylab("Emisiones de NOx") 
##Puntos atípicos del MAPE
punto_at <- pre_finales[pre_finales$mape > 140,]
obser_buen_MAPE <- pre_finales[pre_finales$mape <= 22,]
dim(obser_buen_MAPE )

summary(punto_at$mape)
hist(punto_at$mape,main="Histograma de la distribución del MAPE para los puntos atípicos",xlab="Valores del MAPE(%)",ylab = "Frecuencia",
     col = "blue",freq = TRUE,breaks = 500000,xlim = c(100,500))
##Destandarización de la muestra test para todas la variables del retardo 1
test_x_R1 <- test_x[,4,] 
colnames(test_x_R1) <- c("NOx","Veloc","Acel","Sobreacel","Temp","Hum","Carga")
test_x_R1 <- data.frame(test_x_R1)
unscale <- function(x,mean,sd) {
  (x*sd)+mean
}
unscalemin <- function(x,max,min){
  (x*(max-min))+min
}
mean_1 <- mean(Train_data$`NOx mass`) #desecalar solo NOx
std_1 <- sd(Train_data$`NOx mass`) #desecalar solo NOx
mean_2 <- mean(Train_data$`Amb. Temp.`) 
std_2 <- sd(Train_data$`Amb. Temp.`) 
mean_3 <- mean(Train_data$`Amb. Humid.`) 
std_3 <- sd(Train_data$`Amb. Humid.`) 
mean_4 <- mean(Train_data$Velocity) 
std_4 <- sd(Train_data$Velocity) 
mean_5 <- mean(Train_data$Carga) 
std_5 <- sd(Train_data$Carga)
max_1 <- max(Train_data$Aceleracion)
min_1 <- min(Train_data$Aceleracion)
max_2 <- max(Train_data$Sobreace)
min_2 <- min(Train_data$Sobreace)
NOx <- unscale(test_x_R1$NOx,mean_1,std_1)
Temp <- unscale(test_x_R1$Temp,mean_2,std_2)
Hum <- unscale(test_x_R1$Hum,mean_3,std_3)
Velc <- unscale(test_x_R1$Veloc,mean_4,std_4)
carga <- unscale(test_x_R1$Carga,mean_5,std_5)
Acel <- unscalemin(test_x_R1$Acel,max_1,min_1)
Sobrea <- unscalemin(test_x_R1$Sobreacel,max_2,min_2)

test_x_R1_descale <- data.frame(cbind(NOx,Temp,Hum,Velc,Acel,Sobrea,carga))
write.csv(test_x_R1_descale,file = "Test_unscale_R1",row.names = T)
##descalar test targets 
test_y_unscale <- unscale(test_y,mean_1,std_1)
test_y_unscale <- as.matrix(test_y_unscale)
pre_descal <- unscale(pre_finales$predicciones,mean_1,std_1)
pre_descal <- as.matrix(pre_descal)
#grafico conjunto Valores reales Nox y mape 
result <- cbind(as.ts(test_y_unscale[1:100]),as.ts(pre_finales$mape[1:100]))
colnames(result) <- c("Valor real de NOx","Mape")
plot(result,main="Comparativa MAPE y Valores reales de NOx sin estandarizar",col="blue")
##Gráfica MAPE y variable ambientales sin estandarizar
resultado <- cbind(as.ts(Test_unscale$Temp[1:100]),as.ts(Test_unscale$Hum[1:100]),as.ts(pre_descal[1:100]))
colnames(resultado ) <- c("Temp","Hum","Predicciones")
plot(resultado,main="Comparativa de las predicciones de NOx y variable ambientales del modelo",col="blue")
##Gráfica MAPE y variable ambientales cinemáticas(para el retardo 1 o ultimo valor de la secuencia)
resultado <- cbind(as.ts(Test_unscale$Velc[1:100]),as.ts(Test_unscale$Acel[1:100]),as.ts(Test_unscale$Sobrea[1:100]),as.ts(pre_descal[1:100]))
colnames(resultado ) <- c("Veloc","Acel","Sobreacele","Predicciones")
plot(resultado,main="Comparativa de las predicciones de NOxy variable cinemáticas del modelo",col="14")
##Analisis del comportamiento de las variables con el MAPE
Comp <- unscale_R1[c(4493,1722,2482),]
Comp_variable <- cbind(Comp,test_y_unscale[c(4493,1722,2482)],prediction_df$mape[c(24,45,19)])
print(Comp_variable)
pre_unscale <- unscale(pre_finales$predicciones,mean_1,std_1)
pre_finales$test_unscale <- test_y_unscale
pre_finales <- cbind(pre_finales,pre_unscale)

##Comparativa observ con MAPE>140 y MAPE <22
test_y_unscale_atipico <- unscale(punto_at$test_y,mean_1,std_1)
test_y_unscale_atipico <- as.matrix(test_y_unscale_atipico)
obser_buen_MAPE_unscale <- unscale(obser_buen_MAPE$test_y,mean_1,std_1)
obser_buen_MAPE_unscale <- as.matrix(obser_buen_MAPE_unscale)
summary(test_y_unscale_atipico)
summary(obser_buen_MAPE_unscale)
Valor_real <- c(test_y_unscale_atipico[1:100],obser_buen_MAPE_unscale[1:100])
Valor_real <- as.matrix(Valor_real)
Mape <- c(punto_at$mape[1:100],obser_buen_MAPE$mape[1:100])
Mape <- as.matrix(Mape)
result <- cbind(as.ts(Valor_real),as.ts(Mape))
colnames(result) <- c("Valor real","Mape")
plot(result,main="Comparativa MAPE y el valor real",col="14")
