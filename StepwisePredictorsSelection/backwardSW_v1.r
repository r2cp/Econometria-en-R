# Implementación del algoritmo Forwart Stepwise Selection
# Página 207 de ISLR
# Rodrigo Chang

library(MASS)

# Funciones para el forward stepwise
source(file = "stepwise_fn.r")


## Algoritmo principal
# All predictors
# all.predictors = setdiff(colnames(dataset), target)
# P <- length(all.predictors)
# 
# # Variables auxiliares
# listaPredictoras <- list()
# modelosEstimados <- 0
# 
# for (i in P:1) {
#   # Obtener las predictoras
#   if (i == P) {
#     # Predictoras iniciales, todas excepto el target
#     init.predictors <- combn(all.predictors, i, simplify = FALSE)
#   } else {
#     # De la lista con i+1 predictoras, seleccionar i
#     # Esto es equivalente a quitar una a la vez para evaluar cuál modelo
#     # resulta mejor al quitar una variable
#     init.predictors <- combn(listaPredictoras[[i+1]], i, simplify = FALSE)
#   }
#   
#   # Obtener la lista de rss con las predictoras en la lista init.predictors
#   ss <- sapply(init.predictors, fit.model.fn, 
#                predictors = c("1"), 
#                Ylabel = target, 
#                data = dataset)
#   
#   modelosEstimados <- modelosEstimados + length(ss)
#   
#   # Obtener el índice de variables que minimiza el ss
#   i.min <- which.min(ss)
#   
#   # Guardar las predictoras del modelo de i variables
#   listaPredictoras[[i]] <- init.predictors[[i.min]]
#   
#   # Status
#   print(paste("Mejor modelo de ", i, " variables: ", collapse = ""))
#   print(init.predictors[[i.min]])
# }




# Código principal

dataset <- Boston
target <- "medv"
p <- 5

# Obtener la lista de mejores modelos
listaVar <- backward.stepwise.fn(dataset = Boston, target = target, p)

# Validar los modelos para obtener el mejor
cvMeasures <- sapply(listaVar, cv.models.fn, dataset = Boston, target = target)

# Obtener el mejor modelo:
i.minmdl <- which.min(cvMeasures)

# Predictoras mejor modelo
listaVar[[i.minmdl]]

modelo <- lm(formula = paste(target, "~", paste(listaVar[[i.minmdl]], collapse = "+"), collapse = ""),
             data = dataset)

summary(modelo)
