#Preprocesamineto de los datos, convertir a tensor 3D
##Cargar datos train
library(readxl)
Train_data <- read_excel("C:/ALMACEN/Documentos/Máster Ing de Organizaón UPM/TFM/Códigos/Construccion de las redes/Final_Train_valores erroneos_mediana.xlsx")
##Buscar los índices de cambio de ensayo
library(data.table)
Train_data$relative_1 <- shift(Train_data$relative,n=1) #crear columna relative_1
indmin <- c() #índice para el cual comienza un ensayo
for (i in 2:nrow(Train_data)){
  if (Train_data$relative[[i]]-Train_data$relative_1[[i]] != 1 & Train_data$relative[[i]]-Train_data$relative_1[[i]] != 2){
    indmin <- append(indmin, i, after = length(indmin))
  }
}

print(indmin)
index <- list()
for (m in 2:length(indmin)){
  index[[m]] <- c(indmin[[m-1]],indmin[[m]]-1)   
}        #para crear una lista de index automatica
index[[1]] <- c(1,1881)    # agregar los index del primer esnsayo c(1,1881) para train, c(1,1159) para test
index[[41]]<- c(70743,71867)  # agregar los index del último esnsayo  c(5722,6736) para test
Tam_ensayos <- c() #Determinar cantidad de observaciones de cada ensayo, que sería cada sublista en index
for (n in 1:length(index)){
  Tam_ensayos[[n]]<- index[[n]][[2]]-index[[n]][[1]]
}
index <- sample(index) #muestrear los ensayos para que la variable carga sea aleatoria
#Guardar index de train
save(index,file = "index_train.RData")
##Normalizar
data_train <- data.frame(Train_data[,4:8],Train_data[,10])
mean <- apply(data_train,2,mean)
std <- apply(data_train,2,sd)
data_train <- as.data.frame(scale(data_train, center = mean,scale = std))
##estandarizar a rango 0-1 para acel y sobreacel
min <- apply(Train_data[,11:12],2,min) #para destandarizar y para la muestra test
d <- function(x) {
  return(max(x)-min(x))
}
den <- apply(Train_data[,11:12],2,d) #para destandarizar y para la muestra test
nor <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}
dt<- as.data.frame(lapply(Train_data[,11:12],nor))
data <- data.frame(data_train[,1:5],dt,data_train[,6])
write.csv(data,file = "datos_train_estandarizados.csv",row.names = T)
##Crear datos de entrada con las secuencias y datos objetivo
data_1 <- data.matrix(data[,5])#escoger solo las columnas que interesan y convertir a matrix
data_1 <- data.matrix(data_1)
NoVar <- 1 
Noensayos <- 41 #Realmente son 38, pero en un mismo ensayo se interrumpe la medición
lookback <- 4  #esto varia para cad experimento
step <- 1
samples <- array(0, dim = c(nrow(data_1)-(Noensayos*lookback), 
                            lookback / step,
                            NoVar))
targets <- array(0, dim = c(nrow(data_1)-(Noensayos*lookback)))
#index min y max de cada ensayo(aleatorio) [(min_index,max_index)]
j <- 1
for (n in 1:length(index)) {
  r <- c((index[[n]][[1]]+lookback):index[[n]][[2]]) #si usas sample te muestrea las observaciones de cada ensayo cada vez crees los datos de entrada, pero ya fit() muestrea la muestra train.
  for (i in 1:length(r)) {
    indices <- seq(r[[i]] - lookback / step, (r[[i]]-1),   #crea cada secuencia de retardos
                   length.out = dim(samples)[[2]])
    samples[j,,] <- data_1[indices,] #si hay mas columnas en la data hay que especificar cual
    targets[[j]] <- data_1[r[[i]],1] # 1 es la columna de la variable de salida
    j <<- j + 1
  }  
  
}

#Computar el MAPE de referencia
subset <- seq(1,nrow(samples),length.out=0.33*nrow(samples))
subset <- seq(nrow(samples)-(ceiling(0.33*nrow(samples))-1),nrow(samples),by=1) #muestra de val, las últimas 0.33% de observ de la muestra train
val_data_x <- samples[subset,,]
val_data_y <- targets[subset]
pre <- val_data_x[,4]
mape <- mean(abs((val_data_y-pre)/val_data_y))
print(mape)
#Definición del modelo base
library(keras)

model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 1, activation = "tanh",input_shape=list(NULL,1)) %>% #input_sequence es false pq la proxima capa es dense y recibe un 2D array 
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse",
  metrics = c("mape","mae")
)

model %>% summary() 
history <- model %>% fit(
  samples,targets,epochs= 100,batch_size=150,validation_split=0.33
)

plot(history) #te da el gráfico de comparación de los errores de val y train
Mape <- history$metrics$val_mape #te devuelve los errores en la muestra de validación del mape de cada epoca
plot(history,metrics = "mape",smooth = FALSE) 
history_df <- as.data.frame(history)
print(history_df[history_df$metric == "mape" & history_df$data == "validation",])
plot(history,metrics = "loss",smooth = FALSE)
#Variar unidades con una capa oculta
##Contrucción función estructura una capa SimpleRNN y una capa dense
build_model <- function(unit) {   
  model <- keras_model_sequential() %>%
    layer_simple_rnn(units = unit, activation = "tanh",input_shape=list(NULL,1)) %>% #input_sequence es false pq la proxima capa es desde y recibe un 2D array 
    layer_dense(units = 1)
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mse",
    metrics = c("mape","mae")
  )
}
##Gráficos y resultados del entrenamiento
plot(history,metrics = "mape",smooth = FALSE) + coord_cartesian(ylim = c(150, 350))
plot(history,metrics = "loss",smooth = FALSE) + coord_cartesian(ylim = c(0.1, 1))
plot(history,metrics = "mae",smooth = FALSE) + coord_cartesian(ylim = c(0.2, 0.35))

history_df <- as.data.frame(history)
history_train_loss <- history_df[history_df$metric == "loss" & history_df$data == "training",]
history_val_loss <- history_df[history_df$metric == "loss" & history_df$data == "validation",]
history_val_mape <- history_df[history_df$metric == "mape" & history_df$data == "validation",]
history_train_mape <- history_df[history_df$metric == "mape" & history_df$data == "training",]

ggplot(history_train_loss,aes(x = epoch, y = value)) + geom_line() #grafico linea de la loss para train 
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(0.1, 0.5)) #grafico linea de la loss para val
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_smooth() #gráfico suavizado, srive para ver cuando el error deja de disminuir
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(150, 300))
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_smooth() 

print(history_df[history_df$epoch == 750 & history_df$data == "validation",]) #ver los resultados de una iteración en específico
print(history_df[history_df$epoch == 750 & history_df$data == "training",])  #ver los resultados de una iteración en específico

value <- history_df[history_df$metric == "mape" & history_df$data == "validation",]
value_list <- c(value$value)
median(value_list)
mean(value_list)
value[value$value == min(value$value),]
##Modelo 1(2 unidades y 4 lookback)
model_1 <- build_model(2)
history <- model_1 %>% fit(
  samples,targets,epochs= 100,batch_size=150,validation_split=0.33
)
model_1 %>% summary() 

##Modelo 2(4 unidades y 4 lookback)
model <- build_model(4)
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
 
##Modelo 3(6 unidades y 4 lookback)
model_3 <- build_model(6)
history <- model_3 %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 4(8 unidades y 4 lookback)
model <- build_model(8)
model %>% summary()
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 100) #para definir early stopping 
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33,callbacks = list(early_stop)) 

##Modelo 15 unidades
model_15 <- build_model(15)
model_15 %>% summary()
history <- model_5 %>% fit(
  samples,targets,epochs= 100,batch_size=150,validation_split=0.33
)
##Modelo 5(2 unidades,6 lookback, 1000 iteraciones)
model_5 <- build_model(2)
model_5 %>% summary()
history <- model_5 %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)

##Modelo 6(4 unidades, 6 lookback)
model <- build_model(4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 7(6 unidades, 6 lookback)
model <- build_model(6)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 8(8 unidades,6 lookback)
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 9(10 unidades,6 lookback)
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 10(12 unidades,6 lookback)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 11(1 unidad, 1 lookback)
model <- build_model(1)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 12(2 unidades,1lookback)
model <- build_model(2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
#Modelo 13(4 unidades,1lookback)
model <- build_model(4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 14(6 unidades,1lookback)
model <- build_model(6)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 15(1 unidad,2lookback) #Hay 5 parámetro en total--> 3 para capa RNN(1 de NOx,1 del estado anterior,1 término independiente)
model <- build_model(1)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 16(2 unidad,2lookback)
model <- build_model(2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 18(6 unidad,2lookback)
model <- build_model(6)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 19(8 unidad,2lookback)
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 20(2 unidad,10 lookback)
model <- build_model(2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 21(4 unidad,10 lookback)
model <- build_model(4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 22(6 unidad,10 lookback)
model <- build_model(6)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 23(8 unidad,10 lookback)
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 24(10 unidad,10 lookback)
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 25(10 unidad,4 lookback)
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 26(2 unidades,3lookback)
model <- build_model(2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 27(3 unidades,3lookback)
model <- build_model(3)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 28(6 unidades,3lookback)
model <- build_model(6)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 28(8 unidades,3lookback)
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)

#Modelo finales del escalado
list.files() #lee los nombre de los archivos de un directorio, si no indicas nada el directorio seleccionado
##Callback genera resultados de del entrenamiento en un CSV
csv_callback <- callback_csv_logger('registros_training_model_4.log') #te lo guarda donde está el código de R, cambiar nombre para cada modelo
##Callback guardar los pesos del modelo que queda al final de cada época
checkpoint_dir <- "checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE) #crea una carpeta en directorio de trabajo
filepath <- file.path(checkpoint_dir, "model_4_pesos.{epoch:02d}-{val_loss:.2f}.hdf5") #asigna el formato del nombre de los archivos, cambiar nombre para cada modelo
checkpoint_pesos <- callback_model_checkpoint(filepath = filepath,save_weights_only = TRUE) #te lo guarda donde hayas puesto el directorio
##Callback guardar los pesos del mejor modelo
filepath_b <- file.path(checkpoint_dir, "model_4_pesos_best.hdf5") #cambiar nombre para cada modelo
checkpoint_best <- callback_model_checkpoint(filepath = filepath_b,save_weights_only = TRUE,save_best_only = TRUE,monitor = 'val_mape') 
##Modelo 3 looback, modelo 28
Model_3_looback <- build_model(6)
Model_3_looback %>% summary()
history_3 <- Model_3_looback %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33,callbacks=list(checkpoint_pesos,checkpoint_best)
)
##Modelo 6 looback
Model_6_looback <- build_model(6)
Model_6_looback %>% summary()
history_6 <- Model_6_looback %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33,callbacks=list(csv_callback,checkpoint_pesos,checkpoint_best)
)
##Modelo 4 looback
Model_4_looback <- build_model(8)
Model_4_looback %>% summary()
history_4 <- Model_4_looback %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33,callbacks=list(csv_callback,checkpoint_pesos,checkpoint_best)
)
##Cargar estos modelo guardado
newmodel <- build_model(6) #contruir la misma estructura
newmodel %>% summary()
newmodel %>% load_model_weights_hdf5("model_6_pesos_best.hdf5") #"my_model_6.h5"
##Comparación de la curva loss para los modelos 
compare_loss <- data.frame(Epocas=history_3$epoch,modelo_3_val = history_3$metrics$val_loss,modelo_3_train=history_3$metrics$loss,modelo_4_val = history_4$metrics$val_loss,modelo_4_train=history_4$metrics$loss,modelo_6_val = history_6$metrics$val_loss,modelo_6_train=history_6$metrics$loss
) 
compare_loss <- data.frame(comparete_loss
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr
  gather(key = "type", value = "value", -rowname) #libreria tidyr

ggplot(compare_loss, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("Epocas") +
  ylab("Loss") 

write.csv(compare_loss,file = "Comparación_loss.csv",row.names = T) #guardar en un archivo csv


