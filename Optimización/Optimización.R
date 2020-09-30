#Crear datos de entrada #cargar el archivo csv de data
data_1 <- data.frame(NOx.mass=data[,5],Velocity=data[,4],Aceleracion=data[,6],Sobreacel=data[,7],Temp=data[,1],Humd=data[,3],Carga=data_train[,6])#escoger solo las columnas que interesan y convertir a matrix
data_1 <- data.matrix(data_1)
load("index_train.RData") #cargar el objeto index desde el directorio
NoVar <- 7
Noensayos <- 41 #esto hay que deducirlo debería ser 38
lookback <- 4
step <- 1
desf <- 5 #desfase de las variables cinméticas
samples <- array(0, dim = c(nrow(data_1)-(Noensayos*(lookback+desf-1)), #correcto la longitud
                            lookback / step,
                            NoVar))
targets <- array(0, dim = c(nrow(data_1)-(Noensayos*(lookback+desf-1))))

j <- 1
for (n in 1:length(index)) {
  r <- c((index[[n]][[1]]+lookback):(index[[n]][[2]]-desf+1)) # se suma 1 por el (r[[i]]-1),  
  for (i in 1:length(r)) {
    indices <- seq(r[[i]] - lookback / step, (r[[i]]-1),  
              length.out = dim(samples)[[2]])
    indices_des <- seq(r[[i]] - lookback / step + desf, (r[[i]]-1)+ desf,   
                       length.out = dim(samples)[[2]])
    indices_sin <- seq(r[[i]] - lookback +1, r[[i]],   
                       length.out = dim(samples)[[2]])
    samples[j,,1] <- data_1[indices,1] #retardos de NOx
    samples[j,,2:4] <- data_1[indices_des,2:4] #variables con desfase
    samples[j,,5:7] <- data_1[indices_sin,5:7]
    targets[[j]] <- data_1[r[[i]],1] # 1 es la columna de la variable de salida
    j <<- j + 1
  }  
}

#Definir la red#se modifica la estructura de build_model para la optimizacion
library(keras)
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

#Callback genera resultados de del entrenamiento en un CSV
csv_callback <- callback_csv_logger('dropout_training_model_1.log')

#Guardar samples y targets que se usan durante toda la optimización, 7 variables y 4 lookback)
save(samples,targets,file = "matrix_datos_optimización.RData")

#leer objetos
load("matrix_datos_optimización.RData")

#Gráficos y registros
plot(history,metrics = "mape") + coord_cartesian(ylim = c(120, 400))
plot(history,metrics = "loss",smooth = FALSE) + coord_cartesian(ylim = c(0.05, 0.4))
plot(history,metrics = "mae",smooth = FALSE) + coord_cartesian(ylim = c(0.1, 0.5))
history_df <- as.data.frame(history)
history_train_loss <- history_df[history_df$metric == "loss" & history_df$data == "training",]
history_val_loss <- history_df[history_df$metric == "loss" & history_df$data == "validation",]
history_val_mape <- history_df[history_df$metric == "mape" & history_df$data == "validation",]
his_train_mape <- history_df[history_df$metric == "mape" & history_df$data == "training",]
ggplot(history_train_loss,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(0, 0.3)) #grafico linea de la loss para train 
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(0.1, 0.5)) #grafico linea de la loss para val
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_smooth() #gráfico suavizado, srive para ver cuando el error deja de disminuir
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(120, 300))
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_smooth()
ggplot(his_train_mape,aes(x = epoch, y = value)) + geom_smooth()

print(history_df[history_df$epoch == 98 & history_df$data == "validation",])
print(history_df[history_df$epoch == 98 & history_df$data == "training",])

value <- history_df[history_df$metric == "mape" & history_df$data == "validation",]
value_list <- c(value$value)
median(value_list)
mean(value_list)
min(value_list)
value[value$value == min(value$value),]

#Probar Dropout
##Modelo 1(16 neuronas, dropout=0.2, recurrent_dropout=0.2)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 2(20 neuronas, dropout=0.2, recurrent_dropout=0.2)
model <- build_model(20)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 1000,batch_size=150,validation_split=0.33
)
##Modelo 3(1ra capa=20 neuronas y 2da capa= 8 neuronas, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(20,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4 o 1.9 (1ra capa=16 neuronas,2da capa= 25 neuronas y 3ra capa=25, dropout=0.2, recurrent_dropout=0.5)
model <- build_model(16,25,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 5 o 1.9 (1ra capa=16 neuronas,2da capa= 25 neuronas y 3ra capa=25, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(16,25,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 6 o 1.9 (1ra capa=16 neuronas,2da capa= 25 neuronas y 3ra capa=25, dropout=0.1, recurrent_dropout=0.1)
model <- build_model(16,25,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 7 o 2.10 (1ra capa=20 neuronas,2da capa= 20 neuronas y 3ra capa=25, dropout=0.1, recurrent_dropout=0.1)
model <- build_model(20,20,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 8 o 2.11 (1ra capa=20 neuronas,2da capa= 25 neuronas y 3ra capa=30, dropout=0.1, recurrent_dropout=0.1)
model <- build_model(20,25,30)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 9 o 2.11 (1ra capa=20 neuronas,2da capa= 25 neuronas y 3ra capa=30, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(20,25,30)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 10 (50 neuronas, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(50)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

##Modelo 11 (1ra capa=30 neuronas,2da capa= 35 neuronas y 3ra capa=40, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(30,35,40)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 12 (1ra capa=35 neuronas,2da capa= 40 neuronas, dropout=0.1, recurrent_dropout=0.2)
model <- build_model(35,40)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelos con 2 capas ocultas
##Modelo 1.1(16/2 neuronas)
model_1.1 <- build_model(16,2)
model_1.1 %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.2(16/4 neuronas)
model <- build_model(16,4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.3(16/8 neuronas)
model <- build_model(16,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.4(16/10 neuronas)
model <- build_model(16,10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)

##Modelo 2.1(20/2 neuronas)
model <- build_model(20,2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)

##Modelo 2.3(20/8 neuronas)
model <- build_model(20,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.4(20/10 neuronas)
model <- build_model(20,10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.5(20/12 neuronas)
model <- build_model(20,12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 3.1(10/2 neuronas)
model <- build_model(10,2)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 3.2(10/8 neuronas)
model <- build_model(10,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 3.3(10/10 neuronas)
model <- build_model(10,10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
#Modelos con 3 capas ocultas
##Modelo 1.1(16/4/4 neuronas)
model <- build_model(16,4,4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.2(16/8/4 neuronas)
model <- build_model(16,8,4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.3(16/12/4 neuronas)
model <- build_model(16,12,4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.4(16/8/8 neuronas)
model <- build_model(16,8,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.5(16/12/8 neuronas)
model <- build_model(16,12,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.6(16/10/10 neuronas)
model <- build_model(16,10,10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.7(16/12/10 neuronas)
model <- build_model(16,12,10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)

##Modelo 1.8(16/20/20 neuronas)
model <- build_model(16,20,20)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1.9(16/25/25 neuronas)
model <- build_model(16,25,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.3(20/12/4 neuronas)
model <- build_model(20,12,4)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.6(20/12/8 neuronas)
model <- build_model(20,12,8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.9(20/12/16 neuronas)
model <- build_model(20,12,16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 2.10(20/20/25 neuronas)
model <- build_model(20,20,25)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)

##Modelo 2.11(20/25/30 neuronas)
model <- build_model(20,25,30)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)

##Modelo 1 sola capa(30 neuronas)
model <- build_model(30)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)
##Modelo 1 sola capa(50 neuronas)
model <- build_model(50)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs=500,batch_size=150,validation_split=0.33
)