library(caret)

#Ajusta el modelo para Ylabel con el vector de predictores,
# utilizando el dataset data
fit.model.fn <- function(candidate_predictors, predictors, Ylabel, data) {
  
  # Crear la formula 
  formula <- paste(Ylabel, "~", paste(c(predictors, candidate_predictors), collapse = "+"), collapse = "")
  
  # Ajustar el modelo
  model <- lm(formula, data = data)
  
  # Valores ajustados
  Ypred <- predict(model, data)
  mse <- mean((Ypred - data[[Ylabel]])^2)
  return(mse)
}



# Esta función devuelve una lista con los nombres de las mejores p
# variables para explicar a la variable target, 
# utilizando datos de ajuste en 'dataset'
forward.stepwise.fn <- function(dataset, target, p) {
  # Arreglo de predictoras candidatas
  predictors <- setdiff(colnames(dataset), target)
  # Predictoras iniciales
  init_predictors <- c("1")
  
  # Variables auxiliares
  listaPredictoras <- list()
  modelosEstimados <- 0
  
  # Validación de p
  if (p > length(predictors)) {
    p = 2
  }
  
  for (i in 1:p) {
    # Obtener la lista de rss con cada variable
    ss <- sapply(predictors, fit.model.fn, 
                 predictors = init_predictors, 
                 Ylabel = "medv", data = dataset)
    
    modelosEstimados <- modelosEstimados + length(ss)
    
    # Obtener el índice de variables que minimiza el ss
    i.min <- which.min(ss)
    
    # Agregar a la lista de predictores fijos y quitar de los candidatos
    init_predictors <- union(init_predictors, predictors[i.min])
    predictors <- setdiff(predictors, predictors[i.min])
    
    # Status
    print(paste("Mejor modelo de ", i, " variables: ", collapse = ""))
    print(init_predictors)
    
    # Guardar las predictoras del modelo de i variables
    listaPredictoras[[i]] <- init_predictors
  }
  
  print(paste("Fin. Modelos estimados: ", modelosEstimados, collapse = ""))
  return(listaPredictoras)
}

# Esta función devuelve una lista con los nombres de las mejores P-p
# variables para explicar a la variable target, 
# utilizando datos de ajuste en 'dataset'
backward.stepwise.fn <- function(dataset, target, p) {
  # All predictors
  all.predictors = setdiff(colnames(dataset), target)
  P <- length(all.predictors)
  
  # Variables auxiliares
  listaPredictoras <- list()
  modelosEstimados <- 0
  
  # Validación de p
  if (p > P) {
    p = 2
  }
  
  # Iterar para encontrar los modelos
  for (i in P:p) {
    # Obtener las predictoras
    if (i == P) {
      # Predictoras iniciales, todas excepto el target
      init.predictors <- combn(all.predictors, i, simplify = FALSE)
    } else {
      # De la lista con i+1 predictoras, seleccionar i
      # Esto es equivalente a quitar una a la vez para evaluar cuál modelo
      # resulta mejor al quitar una variable
      init.predictors <- combn(listaPredictoras[[i+1]], i, simplify = FALSE)
    }
    
    # Obtener la lista de rss con las predictoras en la lista init.predictors
    ss <- sapply(init.predictors, fit.model.fn, 
                 predictors = c("1"), 
                 Ylabel = target, 
                 data = dataset)
    
    modelosEstimados <- modelosEstimados + length(ss)
    
    # Obtener el índice de variables que minimiza el ss
    i.min <- which.min(ss)
    
    # Guardar las predictoras del modelo de i variables
    listaPredictoras[[i]] <- init.predictors[[i.min]]
    
    # Status
    print(paste("Mejor modelo de ", i, " variables: ", collapse = ""))
    print(init.predictors[[i.min]])
  }
  
  # Fin del algoritmo, devolver la lista
  print(paste("Fin. Modelos estimados: ", modelosEstimados, collapse = ""))
  return(listaPredictoras[p:P])
}


# Esta función recibe una lista de arreglos con los nombres de variables para estimar 
# modelos y los valida utilizando 10-fold para escoger el mejor entre todos
cv.models.fn <- function(predictors, dataset, target, k = 10) {
  
  # Función para obtener la medida de fitness sobre uno de los folds
  get.fitness.measure <- function(foldIndex, data, formula, label) {
    # Obtener los datos de ajuste y validación del modelo
    trainData <- data[foldIndex, ]
    cvData <- data[-foldIndex, ]
    
    # Ajustar el modelo
    lm.model <- lm(formula, data = trainData)
    # Se obtiene la medida de fitness
    
    # MSE sobre el conjunto de prueba
    pred <- predict(lm.model, cvData)
    fitness.measure <- sqrt(mean((cvData[[label]] - pred)^2))
    
    return(fitness.measure)
  }
  
  
  
  # Obtener la formula del modelo
  formula <- paste(target, "~", paste(predictors, collapse = "+"), collapse = "")
  
  # Obtener los k folds para ajustar el modelo
  folds <- createFolds(dataset[[target]], k = k, returnTrain = TRUE)
  
  # Obtener las medidas de validación sobre cada fold
  fitness <- sapply(folds, get.fitness.measure, 
                    data = dataset, 
                    formula = formula, 
                    label = target)
  
  # Devolver el promedio
  return(mean(fitness))
}