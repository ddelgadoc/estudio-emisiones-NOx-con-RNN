#Crear datos de entrada #cargar el archivo csv de data
data_1 <- data.frame(NOx.mass=data[,5],Velocity=data[,4],Aceleracion=data[,6],Sobreacel=data[,7],Temp=data[,1],Humd=data[,3],Carga=data_train[,6])#escoger solo las columnas que interesan y convertir a matrix
data_1 <- data.matrix(data_1)
load("index_train.RData") #cargar el objeto index desde el directorio
NoVar <- 7  #se varía la cantidad de variables para cada experimento
Noensayos <- 41 
lookback <- 4
step <- 1
desf <- 5 #desfase de las variables cinméticas, se varía para cada experimento
samples <- array(0, dim = c(nrow(data_1)-(Noensayos*(lookback+desf-1)), 
                            lookback / step,
                            NoVar))
targets <- array(0, dim = c(nrow(data_1)-(Noensayos*(lookback+desf-1))))
#esto se va modificando según los experimentos
j <- 1
for (n in 1:length(index)) {
  r <- c((index[[n]][[1]]+lookback):(index[[n]][[2]]-desf+1)) # se suma 1 por el (r[[i]]-1)
  for (i in 1:length(r)) {
    indices <- seq(r[[i]] - lookback / step, (r[[i]]-1),   
                   length.out = dim(samples)[[2]])
    indices_des <- seq(r[[i]] - lookback / step + desf, (r[[i]]-1)+ desf,  
                       length.out = dim(samples)[[2]])
    indices_sin <- seq(r[[i]] - lookback +1, r[[i]],   #indices de las variables que no tienen desfase, ni datos pasados
                           length.out = dim(samples)[[2]])
    samples[j,,1] <- data_1[indices,1] #variable NOx(retardos)
    samples[j,,2:4] <- data_1[indices_des,2:4] #poner las variables que interesan cinematicas
    samples[j,,5:7] <- data_1[indices_sin,5:7]
    targets[[j]] <- data_1[r[[i]],1] # 1 es la columna de la variable de salida
    j <<- j + 1
  }  
}

#Definir la red
library(keras)
build_model <- function(unit) {   
  model <- keras_model_sequential() %>%
    layer_simple_rnn(units = unit, activation = "tanh",input_shape=list(NULL,7)) %>% #input_sequence es false pq la proxima capa es dense y recibe un 2D array, input_shape--> se modifica el número por la cantidad de variables
    layer_dense(units = 1)
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mse",
    metrics = c("mape","mae")
  )
}

#Callback genera resultados de del entrenamiento en un CSV
csv_callback <- callback_csv_logger('registros_training_model_4_2.log')

#Modelos para 3 variables y 4 lookback(NOx retardos,Velocidad y aceleración)
##Modelo 1, 8 neuronas
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

#Gráficos y registros
plot(history,metrics = "mape",smooth = FALSE) + coord_cartesian(ylim = c(140, 350))
plot(history,metrics = "loss",smooth = FALSE) + coord_cartesian(ylim = c(0.05, 0.4))
plot(history,metrics = "mae",smooth = FALSE) + coord_cartesian(ylim = c(0.1, 0.5))
history_df <- as.data.frame(history)
history_train_loss <- history_df[history_df$metric == "loss" & history_df$data == "training",]
history_val_loss <- history_df[history_df$metric == "loss" & history_df$data == "validation",]
history_val_mape <- history_df[history_df$metric == "mape" & history_df$data == "validation",]

ggplot(history_train_loss,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(0.05, 0.3)) #grafico linea de la loss para train 
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(0.1, 0.5)) #grafico linea de la loss para val
ggplot(history_val_loss,aes(x = epoch, y = value)) + geom_smooth() #gráfico suavizado, srive para ver cuando el error deja de disminuir
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_line() + coord_cartesian(ylim = c(140, 300))
ggplot(history_val_mape,aes(x = epoch, y = value)) + geom_smooth()

print(history_df[history_df$epoch == 57 & history_df$data == "validation",])
print(history_df[history_df$epoch == 57 & history_df$data == "training",])

value <- history_df[history_df$metric == "mape" & history_df$data == "validation",]
value_list <- c(value$value)
median(value_list)
mean(value_list)
min(value_list)
value[value$value == min(value$value),]

##Modelo 1.1, 10 neuronas
model <- build_model(10)
model %>% summary()
history_1.1 <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 1.2, 12 neuronas
model <- build_model(12)
model %>% summary()
history_1.2 <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

##Modelo 1.3, 14 neuronas
model <- build_model(14)
model %>% summary()
history_1.3 <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2, 8 neuronas
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2.1, 10 neuronas
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2.2, 12 neuronas
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33,callbacks=csv_callback
)
##Modelo 3, 8 neuronas
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3.1, 10 neuronas
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3.2, 12 neuronas
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33,callbacks=csv_callback
)
##Modelo 3.3, 14 neuronas
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4, 8 neuronas, desfase =0
model <- build_model(8)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4.1, 10 neuronas, desfase =0
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33,callbacks=csv_callback
)
##Modelo 4.2, 12 neuronas, desfase =0
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33,callbacks=csv_callback
)
##Modelo sin datos pasados, 10 neuronas, desfase =0
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Comparación modelos de la serie 1, 1.1 ...
compare_mape <- data.frame(modelo_2_val = history$metrics$val_mape,modelo_2_train=history$metrics$mape,modelo_3_val = history_3$metrics$val_mape,modelo_3_train=history_3$metrics$mape,
                           modelo_4_val = history_4$metrics$val_mape,modelo_4_train=history_4$metrics$mape
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr
  gather(key = "type", value = "value", -rowname) #libreria tidyr

compare_loss <- data.frame(modelo_1_val = history$metrics$val_loss,modelo_1_train=history$metrics$loss,modelo_1.1_val = history_1.1$metrics$val_loss,modelo_1.1_train=history_1.1$metrics$loss,
                           modelo_1.2_val = history_1.2$metrics$val_loss,modelo_1.2_train=history_1.2$metrics$loss,modelo_1.3_val=history_1.3$metrics$val_loss,modelo_1.3_train=history_1.3$metrics$loss
) %>%
  rownames_to_column() %>% #libreria tibble
  mutate(rowname = as.integer(rowname)) %>% #libreria dplyr crea una columna rowname
  gather(key = "type", value = "value", -rowname) #libreria tidyr crea dos columnas mas type y value            


ggplot(compare_mape, aes(x = rowname, y = value, color = type)) +
  geom_line() +
  xlab("Epocas") +
  ylab("MAPE") +
  ylim(150,300)

#Modelos para 4 variables y 4 lookback(NOx retardos,Velocidad,aceleración y sobreacel)
##Modelo 1(10 neuronas)
model <- build_model(10)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3(14 neuronas)
model <- build_model(14)
model %>% summary()
history_3 <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

##Modelo 4(16 neuronas)
model <- build_model(16)
model %>% summary()
history_4 <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

#Modelos temperatura (T)
##Modelo 1 (12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 1.1 (14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 1.2 (16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 1.3 (18 neuronas)
model <- build_model(18)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Presión
##Modelo 2(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Humedad
##Modelo 3(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Temp y Presión
##Modelo 4(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4.3(18 neuronas)
model <- build_model(18)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Temp y Humedad
##Modelo 5(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 5.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 5.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Presión y Humedad
##Modelo 6(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 6.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 6.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
#Modelo Temp, Presión y Humedad
##Modelo 7(12 neuronas)
model <- build_model(12)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 7.1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 7.2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

#Modelo TH(14 neuronas con retardos)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

#Modelos Carga
##Modelo 1(14 neuronas)
model <- build_model(14)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 2(16 neuronas)
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 3(18 neuronas)
model <- build_model(18)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)
##Modelo 4(20 neuronas)
model <- build_model(20)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)

#Modelo sin la variable retardos
model <- build_model(16)
model %>% summary()
history <- model %>% fit(
  samples,targets,epochs= 500,batch_size=150,validation_split=0.33
)