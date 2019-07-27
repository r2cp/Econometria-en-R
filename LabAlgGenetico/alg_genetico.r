library(MASS)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)

##  -------------------------------------------------------
##  Área de funciones
##  -------------------------------------------------------

# Obtener cromosoma
# Esta función devuelve un arreglo con 1s y 0s, en la posición de cada variable en train,
# exceptuando la variable indicada por 'label'
get_chromosome <- function(x, train, label){
  col_names <- names(train)
  inputs <- setdiff(col_names, label)
  cromosoma <- sample(c(0,1), size = length(inputs), replace = TRUE)
  # Si todos son cero, pone a 1 todas las variables
  if (sum(cromosoma) == 0) {
    cromosoma <- rep(1,length(inputs)) 
  }
  output <- cromosoma
  return(output)
}

# Función de fitness
# Ajusta el modelo con el conjunto train utilizando las variables indicadas por el cromosoma
# Computa la suma de cuadrados de los residuos sobre el conjunto test
# label indica la variable dependiente 
get_fitness <- function(chromosome, train, test, label) {
  # Quita la columna con nombre 'label'
  col_names <- names(train)
  inputs <- setdiff(col_names, label)
  # Obtiene un arreglo lógico con las variables a incluir
  chromosome <- as.logical(chromosome)
  
  # Obtiene la fórmula de regresión lineal
  regresoras <- paste0(inputs[chromosome], collapse = '+')
  formula <- paste0(label, "~", regresoras)
  # Ajusta el modelo lineal sobre train
  lm.model <- lm(formula, data = train)
  # Se obtienen valores ajustados sobre el conjunto test
  pred <- predict(lm.model, test)
  # Suma de errores al cuadrado sobre test
  fitness.measure <- sum((test[[label]] - pred)^2)
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
  child1 <- mask_first_half*p1+mask_last_half*p2
  child2 <- mask_first_half*p2+mask_last_half*p1
  return(list(child1,child2))
}

# Devuelve una lista de 2 padres obtenidos de la población a partir
# de una variable de roullete
select_mating_parents <- function(x, roullete, population) {
  # Toma dos índices de padres aleatoriamente, utilizando como pesos el ranking
  # de la ruleta en el dataframe ordenado
  parents <- sample(roullete$parent, size = 2, prob = roullete$rank)
  # Devuelve una lista de 2 padres obtenidos de la población
  return(population[parents])
}

# Función de mutación, aplica una mutación aleatoria al cromosoma 'child'
mutation.fn <- function(child, rate=0.01){
  mask <- sample(c(1,0), length(child), replace = TRUE, prob = c(rate, 1-rate))
  mutation.child <- xor(child, mask)*1.0
  return(mutation.child)
}

##  --------------------------------------------------------------------
##  Código principal de entrenamiento y ejecución del algoritmo genético
##  --------------------------------------------------------------------

# Datos de entrenamiento y prueba
train_index <- createDataPartition(Boston$medv,p=0.75,list = F)
train <- Boston[train_index,]
test <- Boston[-train_index,]

# Parámetros de simulación
POP_SIZE <- 100
GENERATIONS <- 20
MUTATION_RATE <- 0.1

# Variables para graficar
top.fitness.tray <- c(1:GENERATIONS)
mean.fitness.tray <- c(1:GENERATIONS)

# Obtener la población original - 1era. generación
#population <- lapply(1:POP_SIZE, get_chromosome, train=train, label="medv")

for (i in 1 : GENERATIONS) {

  print(paste0("Generación = ", i, collapse=""))
  
  # Obtener la población
  if (i == 1) {
    population <- lapply(1:POP_SIZE, get_chromosome, train=train, label="medv")
  } else {
    # Se aplica la mutación aleatoria a la lista de hijos para obtener la nueva población
    population <- lapply(children, mutation.fn, rate=MUTATION_RATE)
  }
  
  # Se obtiene el fitness para la población
  fitness <- lapply(population, get_fitness, train=train, test=test, label="medv")
  
  # Se genera una lista de los padres y sus valores de fitness, (ruleta)
  roullete <-
    tibble(parent=1:POP_SIZE, fitness= fitness %>% unlist()) %>%
    arrange(desc(fitness))
  
  # Se agrega la columna de ranking
  roullete$rank <- 1:nrow(roullete)
  
  # Devuelve una lista de parejas para aparearse
  mating_parents <-
    lapply(1:POP_SIZE, select_mating_parents,
      roullete=roullete, 
      population=population)
  
  # Se aplica un apareamiento entre cada par de padres para generar un par de hijos
  children <- lapply(mating_parents, crossover.fn)
  
  # Se unlistan los hijos y se toman solo los primeros pop_size
  children <- children %>% unlist(recursive = F) 
  children <- children[1:POP_SIZE]
  
  # Se obtiene el padre con el fitness más alto
  top_parent <- roullete %>% tail(1) %>% pull(parent)
  top_parent_fitness <- roullete %>% tail(1) %>% pull(fitness)
  top.fitness.tray[i] <- top_parent_fitness
  print(population[[top_parent]])
  print(paste0("Top parent fitness:", top_parent_fitness, collapse = " "))
  
  # Se obtiene la media del fitness para cada generación
  mean.fitness.tray[i] <- mean(roullete$fitness)
}


##  --------------------------------------------------------------------
##  Gráficas de la ejecución del algoritmo genético
##  --------------------------------------------------------------------

# Crear un dataframe con los resultados top y promedio por generación
tray <- tibble(generation = 1 : GENERATIONS, 
  top.fitness = top.fitness.tray, 
  pop.mean.fitness = mean.fitness.tray)

# Poner los resultados en formato tidy
tidy.tray <- tray %>% 
  gather("medida", "fitness", -1)

# Graficar
ggplot(tidy.tray, aes(x = generation, y = fitness, col = medida)) + 
  geom_line()

# Graficar el fitness top
#plot(top.fitness.tray)
#plot(mean.fitness.tray)









