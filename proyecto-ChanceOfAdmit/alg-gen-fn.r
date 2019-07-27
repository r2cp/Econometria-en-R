##  -------------------------------------------------------
##  Área de funciones
##  -------------------------------------------------------

# Obtener cromosoma
# Esta función devuelve un arreglo con 1s y 0s, en la posición de cada variable en train,
# exceptuando la variable indicada por 'label'
get_chromosome <- function(x, data, label){
  col_names <- names(data)
  inputs <- setdiff(col_names, label)
  cromosoma <- sample(c(0,1), size = length(inputs), replace = TRUE)
  # Si todos son cero, pone a 1 todas las variables
  if (sum(cromosoma) == 0) {
    cromosoma <- rep(1,length(inputs)) 
  }
  output <- cromosoma
  return(output)
}

# Función de fitness con validación cruzada (K-folds CV)
# Ajusta K modelos utilizando (K-1)/K de los datos y obteniendo el fitness sobre
# la fracción 1/K restante de los datos.
# Devuelve el promedio de las medidas de fitness
get_fitness_cv <- function(chromosome, data, label, k=10) {
  
  # Función para obtener la medida de fitness sobre uno de los folds
  get.fitness.measure <- function(foldIndex, data, formula, label, chromosome) {
    # Obtener los datos de ajuste y validación del modelo
    trainData <- data[foldIndex, ]
    cvData <- data[-foldIndex, ]
    
    # Ajustar el modelo
    lm.model <- lm(formula, data = trainData)
    # Se obtiene la medida de fitness
    
    # MSE sobre el conjunto de prueba
    pred <- predict(lm.model, cvData)
    #fitness.measure <- sqrt(mean((cvData[[label]] - pred)^2)) + 0.001*sum(chromosome)
    fitness.measure <- sqrt(mean((cvData[[label]] - pred)^2))
    
    # Criterios de información sobre el conjunto de ajuste
    #fitness.measure <- BIC(lm.model)
    #fitness.measure <- AIC(lm.model)
    return(fitness.measure)
  }
  
  # Quita la columna con nombre 'label'
  inputs <- setdiff(names(data), label)
  # Obtiene un arreglo lógico con las variables a incluir
  chromosome <- as.logical(chromosome)
  
  # Obtiene la fórmula de regresión lineal
  regresoras <- paste0(inputs[chromosome], collapse = '+')
  formula <- paste0(label, "~", regresoras)
  
  # Obtener los k folds para ajustar el modelo
  folds <- createFolds(data[[label]], k = k, returnTrain = TRUE)
  
  # Obtener las medidas de fitness
  fitness <- sapply(folds, get.fitness.measure, data, formula, label, chromosome)
  
  # Devolver la media de las medidas de fitness
  return(mean(fitness))
}


# Función de fitness sin validación cruzada 
get_fitness_nocv <- function(chromosome, data, label, k=10) {
  
  # Quita la columna con nombre 'label'
  inputs <- setdiff(names(data), label)
  # Obtiene un arreglo lógico con las variables a incluir
  chromosome <- as.logical(chromosome)
  
  # Obtiene la fórmula de regresión lineal
  regresoras <- paste0(inputs[chromosome], collapse = '+')
  formula <- paste0(label, "~", regresoras)
  
  # Ajustar el modelo
  lm.model <- lm(formula, data = data)
  
  # MSE sobre el conjunto de prueba
  pred <- predict(lm.model, data)
  #fitness.measure <- sqrt(mean((data[[label]] - pred)^2))
  #fitness.measure <- AIC(lm.model)
  fitness.measure <- BIC(lm.model)
  
  # Devolver la media de las medidas de fitness
  return(fitness.measure)
}


# Función de crossover para generar 2 hijos a partir de 2 padres
# parents - una lista de 2 cromosomas
# Devuelve una lista de 2 cromosomas hijos
crossover.fn <- function(parents){
  p1 <- parents[[1]]
  p2 <- parents[[2]]
  chromosome_len <- length(p1)
  mask1<-rep(0,ceiling(chromosome_len-chromosome_len/2))
  mask2 <- rep(1,chromosome_len/2)
  mask_last_half <-c(mask1,mask2)
  mask1<-rep(1,ceiling(chromosome_len-chromosome_len/2))
  mask2 <- rep(0,chromosome_len/2)
  mask_first_half <- c(mask1,mask2)
  child1 <- mask_first_half*p1 + mask_last_half*p2
  child2 <- mask_first_half*p2 + mask_last_half*p1
  return(list(child1,child2))
}

# Devuelve una lista de 2 padres obtenidos de la población a partir
# de una variable de roulette
select_mating_parents <- function(x, roulette, population) {
  # Toma dos índices de padres aleatoriamente, utilizando como pesos el ranking
  # de la ruleta en el dataframe ordenado
  parents <- sample(roulette$parent, size = 2, prob = roulette$rank)
  # Devuelve una lista de 2 padres obtenidos de la población
  return(population[parents])
}

# Función de mutación, aplica una mutación aleatoria al cromosoma 'child'
mutation.fn <- function(child, rate=0.01){
  mask <- sample(c(1,0), length(child), replace = TRUE, prob = c(rate, 1-rate))
  mutation.child <- xor(child, mask)*1.0
  return(mutation.child)
}

# Función para obtener una transformación del dataframe data
# Quita las etiquetas en la lista remove.labels
# Aplica la función fn al dataframe y añade el sufijo fn.label a los campos
get_transformed_df <- function(data, remove.labels, fn, fn.label) {
  # Obtener las columnas de data
  col.names <- names(data)
  # Quitar la lista de columnas especificada por remove.labels
  inputs <- setdiff(col.names, remove.labels)
  # Aplicar la transformación fn a data
  output.df <- fn(data[inputs])
  # Modificar sus nombres de columnas de acuerdo con fn.label
  names(output.df) <- paste(names(output.df), fn.label, sep=".")
  return(output.df)
}

# Función para graficar la medida fitness de la media de la generación y el top
graficarGeneraciones <- function (gen.hist) {
  # Crear un dataframe con los resultados top y promedio por generación
  tray <- tibble(generation = 1 : length(gen.hist$top.fitness.tray), 
                 top.fitness = gen.hist$top.fitness.tray, 
                 pop.mean.fitness = gen.hist$mean.fitness.tray)
  
  # Poner los resultados en formato tidy
  tidy.tray <- tray %>% 
    gather("medida", "fitness", -1)
  
  # Graficar
  ggplot(tidy.tray, aes(x = generation, y = fitness, col = medida)) + 
    geom_line() +
    theme(legend.position = "bottom")
}


##  --------------------------------------------------------------------
##  Función principal de entrenamiento y ejecución del algoritmo genético
##  --------------------------------------------------------------------

alg_gen <- function (data, label, pop_size, generations, mutation_rate, 
                     fitness.fn=get_fitness_cv, k_folds=10) {
  # Lista para guardar el top de cada generación
  gen.hist <- list()
  
  # Variables para graficar
  gen.hist$top.fitness.tray <- c(1:generations)
  gen.hist$mean.fitness.tray <- c(1:generations)
  
  # Correr las generaciones
  for (i in 1 : generations) {
    
    print(paste0("Generación = ", i, collapse=""))
    
    # Obtener la población
    if (i == 1) {
      population <- lapply(1:pop_size, get_chromosome, 
                           data=data, 
                           label=label)
    } else {
      # Se aplica la mutación aleatoria a la lista de hijos para obtener la nueva población
      population <- lapply(children, mutation.fn, 
                           rate=mutation_rate)
    }
    
    # Se obtiene el fitness para la población
    fitness <- lapply(population, fitness.fn, 
                      data=data, 
                      label=label, 
                      k = k_folds)
    
    # Se genera una lista de los padres y sus valores de fitness, (ruleta)
    roulette <-
      tibble(parent=1:pop_size, fitness= fitness %>% unlist()) %>%
      arrange(desc(fitness))
    
    # Se agrega la columna de ranking
    roulette$rank <- 2*(1:nrow(roulette))
    
    # Devuelve una lista de parejas para aparearse
    mating_parents <-
      lapply(1:pop_size, select_mating_parents,
             roulette=roulette, 
             population=population)
    
    # Se aplica un apareamiento entre cada par de padres para generar un par de hijos
    children <- lapply(mating_parents, crossover.fn)
    
    # Se unlistan los hijos y se toman solo los primeros pop_size
    children <- children %>% unlist(recursive = F) 
    children <- children[1:pop_size]
    
    # Se obtiene el padre con el fitness más alto
    top_parent <- roulette %>% tail(1) %>% pull(parent)
    top_parent_fitness <- roulette %>% tail(1) %>% pull(fitness)
    gen.hist$top.fitness.tray[i] <- top_parent_fitness
    gen.hist$top.chromosomes[[i]] <- population[[top_parent]]
    print(population[[top_parent]])
    print(paste0("Top parent fitness:", top_parent_fitness, collapse = " "))
    
    # Se obtiene la media del fitness para cada generación
    gen.hist$mean.fitness.tray[i] <- mean(roulette$fitness)
  }
  
  # Obtener la generación con el mejor fitness
  gen.hist$best.gen <- which.min(gen.hist$top.fitness.tray)
  gen.hist$best.chromosome <- gen.hist$top.chromosomes[[gen.hist$best.gen]]
  print(paste("Generación con mejor fitness (", gen.hist$best.gen, ") : ", gen.hist$top.fitness.tray[gen.hist$best.gen], sep=""))
  print(gen.hist$best.chromosome)
  
  # Obtener las predictoras finales con mejor fitness
  gen.hist$final.inputs <- setdiff(colnames(data), label)[as.logical(gen.hist$best.chromosome)]
  print("Variables finales")
  print(gen.hist$final.inputs)
  
  return(gen.hist)
}

##  --------------------------------------------------------------------
##  Funciones auxiliares
##  --------------------------------------------------------------------

# Función para obtener el fitness de un modelo a partir de sus predictoras
get_model_fitness <- function(data, predictors, label, fitness.fn = "RMSE") {

    # Obtiene la fórmula de regresión lineal
  regresoras <- paste0(predictors, collapse = '+')
  formula <- paste0(label, "~", regresoras)
  
  # Ajustar el modelo
  lm.model <- lm(formula, data = data)
  
  
  if (fitness.fn == "RMSE") {
    # MSE sobre el conjunto de datos completo
    pred <- predict(lm.model, data)
    fitness.measure <- sqrt(mean((data[[label]] - pred)^2))
  } else if (fitness.fn == "AIC") {
    fitness.measure <- AIC(lm.model)
  } else if (fitness.fn == "BIC") {
    fitness.measure <- BIC(lm.model)
  } else {
    pred <- predict(lm.model, data)
    fitness.measure <- (data[[label]] - pred)^2
  }
  
  returnList <- list()
  returnList[["model"]] <- lm.model
  returnList[["fitness.measure"]] <- fitness.measure
  
  # Devolver la media de las medidas de fitness
  return(returnList)
}

