#Crear inputs data(lookback=4)
data_1 <- data.matrix(data[,5])#datos ya estabdarizados cargar archivo csv
data_1 <- data.matrix(data_1)
NoVar <- 1
Noensayos <- 41 #Realmente son 38, pero en un mismo ensayo se interrumpe la medición
lookback <- 4
step <- 1
inputs <- array(0,dim = c(nrow(data_1)-(Noensayos*lookback),lookback / step))
outputs <- array(0,dim = c(nrow(data_1)-(Noensayos*lookback)))
j <- 1
for (n in 1:length(index)) {
  r <- c((index[[n]][[1]]+lookback):index[[n]][[2]]) 
  for (i in 1:length(r)) {
    indices <- seq(r[[i]] - lookback / step, (r[[i]]-1),   #para hacer consecutivos los valores
                   length.out = dim(inputs)[[2]])
    indices <- sort(indices,decreasing = TRUE) # para que te ponga X1, X2 ... en vez de Xn...X2,X1
    inputs[j,] <- data_1[indices,] #si hay mas columnas hay en la data hay que especificar cual
    outputs[[j]] <- data_1[r[[i]],1] # 2 es la columna de la variable de salida
    j <<- j + 1
  }  
}


#Dividir en train y validacion
subset <- seq(1,nrow(inputs),length.out=0.33*nrow(inputs))
subset <- seq(nrow(samples)-(ceiling(0.33*nrow(samples))-1),nrow(samples),by=1)
val_data_x <- inputs[subset,]
val_data_y <- outputs[subset]
train_data_x <- inputs[-subset,] #borrar las filas de los índices de subset 
train_data_y <- outputs[-subset]

#Construcción red elman formato matriz los datos de entrada
#activar paquete RSNNS #learning rate por defecto 0.2 y funcion de activ por defecto:  Act_Logistic 
fit <- elman(train_data_x,train_data_y,size = c(8),maxit = 1000,inputsTest = val_data_x,targetsTest = val_data_y)
fit
summary(fit)
plotIterativeError(fit) #gráfico de convergencia, rápido descenso al inicio indica que el modelo está aprendiendo
plotRegressionError(train_data_y,fit$fitted.values) 
plotRegressionError(val_data_y,fit$fittedTestValues) 
#Computar errores # activar paquete Metrics
train_data_y <- as.matrix(train_data_y)
val_data_y  <- as.matrix(val_data_y)
mae(train_data_y,fit$fitted.values)
mae(val_data_y,fit$fittedTestValues)
mape(train_data_y,fit$fitted.values)
mape(val_data_y,fit$fittedTestValues)
mse(train_data_y,fit$fitted.values)
mse(val_data_y,fit$fittedTestValues)