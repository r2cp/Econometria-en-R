# Implementación del algoritmo Forwart Stepwise Selection
# Página 207 de ISLR
# Rodrigo Chang

library(dplyr)
library(MASS)

# Funciones para el forward stepwise
source(file = "stepwise_fn.r")

# Código principal

dataset <- Boston
target <- "medv"
p <- 13

# Obtener la lista de mejores modelos
listaVar <- forward.stepwise.fn(dataset = Boston, target = target, p)

# Validar los modelos para obtener el mejor
cvMeasures <- sapply(listaVar, cv.models.fn, dataset = Boston, target = target)

# Obtener el mejor modelo:
i.minmdl <- which.min(cvMeasures)

# Predictoras mejor modelo 
listaVar[[i.minmdl]]

modelo <- lm(formula = paste(target, "~", paste(listaVar[[i.minmdl]], collapse = "+"), collapse = ""),
             data = dataset)

summary(modelo)
