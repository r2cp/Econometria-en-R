library(MASS)
library(dplyr)
library(caret)
library(ggplot2)
library(tidyr)

# Cargar las funciones
source(file = "alg-gen-fn.r")

##  --------------------------------------------------------------------
##  Código principal
##  --------------------------------------------------------------------

# Cargar datos
Admissions <- read.csv("econometria-ug-2019/train.csv")

# Obtener datos transformados
sq.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) x^2, "sq")
cub.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) x^3, "cube")
inv.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) 1/x, "inv")
log.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) log(x), "log")

# Interacciones
intGRE.Admissions <- Admissions$GRE.Score * get_transformed_df(Admissions, 
  c("Chance.of.Admit", "GRE.Score", "Research"), 
  function(x) x, "int.GRE")

intTOEFL.Admissions <- Admissions$TOEFL.Score * get_transformed_df(Admissions,
  c("Chance.of.Admit", "GRE.Score", "Research", "TOEFL.Score"), 
  function(x) x, "int.TOEFL")

int.Admissions <- Admissions$Research * get_transformed_df(Admissions, 
  c("Chance.of.Admit", "Research"), 
  function(x) x, "int.Research")


# Dataframes finales para entrenamiento
#final.Admissions <- cbind(Admissions, sq.Admissions, cub.Admissions, inv.Admissions, int.Admissions, log.Admissions)
#final.Admissions <- cbind(Admissions, int.Admissions)
#final.Admissions <- cbind(Admissions, sq.Admissions, cub.Admissions, inv.Admissions, int.Admissions)
final.Admissions <- cbind(Admissions, sq.Admissions, inv.Admissions, intGRE.Admissions, intTOEFL.Admissions, int.Admissions)
                         
# Parámetros de simulación
POP_SIZE <- 100
GENERATIONS <- 50
MUTATION_RATE <- 0.02
K <- 5

# Ejecutar la función de algoritmo genético
gen.hist <- alg_gen(data = final.Admissions, label = "Chance.of.Admit", 
  pop_size = POP_SIZE,
  generations = GENERATIONS,
  mutation_rate = MUTATION_RATE, 
  fitness.fn = get_fitness_cv,
  k_folds = K)

# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist)

# Obtener el RMSE
get_model_fitness(final.Admissions, predictors = gen.hist$final.inputs, label="Chance.of.Admit")

# Guardar los resultados del corrimiento
#save(gen.hist, file = paste("sub-gen-alg-mutrate0.01-gens100-difcosto-gens-", GENERATIONS, ".Rdata", sep=""))





##  --------------------------------------------------------------------
##  Clasificación de los datos por University Rating
##  --------------------------------------------------------------------

# Función para obtener la transformación de features de Admissions
# Separa por Admissions$University.Rating = university.rating
# Genera las transformaciones y devuelve el dataframe final para entrenamiento
get.Admissions.features <- function (Admissions, university.rating) {
  
  # Filtrar por University.Rating
  Admissions <- Admissions %>%
    dplyr::filter(University.Rating == university.rating) %>%
    dplyr::select(-University.Rating)
  
  # Obtener datos transformados
  sq.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) x^2, "sq")
  cub.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) x^3, "cube")
  inv.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) 1/x, "inv")
  #log.Admissions <- get_transformed_df(Admissions, c("Chance.of.Admit", "Research"), function(x) log(x), "log")
  
  # Interacciones
  intGRE.Admissions <- Admissions$GRE.Score * get_transformed_df(Admissions, 
    c("Chance.of.Admit", "GRE.Score", "Research"), 
    function(x) x, "int.GRE")
  
  intTOEFL.Admissions <- Admissions$TOEFL.Score * get_transformed_df(Admissions,
    c("Chance.of.Admit", "GRE.Score", "Research", "TOEFL.Score"), 
    function(x) x, "int.TOEFL")
  
  int.Admissions <- Admissions$Research * get_transformed_df(Admissions, 
    c("Chance.of.Admit", "Research"), 
    function(x) x, "int.Research")
  
  intCGPA.Admissions <- Admissions$GRE.Score * get_transformed_df(Admissions, 
    c("Chance.of.Admit", "CGPA", "Research"), 
    function(x) x, "int.CGPA")
  
  
  # Dataframes finales para entrenamiento

  if (university.rating == 1) {
    final.Admissions <- cbind(Admissions, sq.Admissions, intGRE.Admissions, intTOEFL.Admissions)
  } else if (university.rating == 2) {
    final.Admissions <- cbind(Admissions, sq.Admissions, cub.Admissions, intCGPA.Admissions, intTOEFL.Admissions) #%>%
      #dplyr::select(-contains("SOP")) #%>%
      #mutate(SOP = factor(Admissions$SOP))
  } else if (university.rating == 5) {
    final.Admissions <- cbind(Admissions, sq.Admissions, intGRE.Admissions, intTOEFL.Admissions, inv.Admissions)
  } else {
    final.Admissions <- cbind(Admissions, sq.Admissions, intGRE.Admissions, intTOEFL.Admissions, int.Admissions, inv.Admissions)
  }
  
  # Devolver el dataframe final
  return(final.Admissions)
}


# Parámetros de simulación
POP_SIZE <- 100
GENERATIONS <- 25
MUTATION_RATE <- 0.02
K <- 5

##  --------------------------------------------------------------------
##  Modelo para la clase university.rating = 1
##  --------------------------------------------------------------------

Admissions.1 <- get.Admissions.features(Admissions, university.rating = 1)
# Ejecutar la función de algoritmo genético
gen.hist.1 <- alg_gen(data = Admissions.1, label = "Chance.of.Admit", 
                    pop_size = POP_SIZE,
                    generations = GENERATIONS,
                    mutation_rate = MUTATION_RATE, 
                    fitness.fn = get_fitness_cv,
                    k_folds = K)
# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist.1)
# Obtener el RMSE
modelo.1 <- get_model_fitness(Admissions1, predictors = gen.hist.1$final.inputs, label="Chance.of.Admit")

get_model_results(Admissions.1, modelo.1$model)
#0.02449348

##  --------------------------------------------------------------------
##  Modelo para la clase university.rating = 2
##  --------------------------------------------------------------------

# Transformaciones para algoritmo genético
Admissions.2 <- get.Admissions.features(Admissions, university.rating = 2)

# Variables en niveles para los modelos de SVR
Admissions.2 <- Admissions %>%
  dplyr::filter(University.Rating == 2) %>%
  dplyr::select(-University.Rating)

##  -------------------------------------------
##  Pruebas con SVR
##  -------------------------------------------

# Búsqueda de SVR con kernel polinomial con validación cruzada 10 folds, 
# Admissions.2 con 7 variables
# Admissions.2 transformado con todas las variables
modelo.2$tune <- tune.svm(Chance.of.Admit~., data = Admissions.2, kernel = "polynomial", 
                           degree = c(2:4),
                           gamma = seq(0.025, 0.15, 0.025), 
                           cost = 2^(0:4))
modelo.2$tune 

modelo.2$model <- svm(Chance.of.Admit~., Admissions.2,
                      kernel = "polynomial", degree = 3, gamma = 0.025, cost = 1)

get_model_results(Admissions.2, modelo.2$model)
#0.07889196
#0.078082

# Búsqueda de SVR con kernel RBF con validación cruzada 10 folds
# Admissions.2 con 7 variables
# Admissions.2 transformado con todas las variables
modelo.2$tune <- tune.svm(Chance.of.Admit~., data = Admissions.2, 
                          gamma = seq(0.025, 0.15, 0.025), 
                          cost = 2^(0:4))
modelo.2$tune 

modelo.2$model <- svm(Chance.of.Admit~., Admissions.2,
                      gamma = 0.025, cost = 2)

get_model_results(Admissions.2, modelo.2$model)
#0.07508124
#0.06882022

##  -------------------------------------------
##  Pruebas con algoritmo genético
##  -------------------------------------------

# Ejecutar la función de algoritmo genético
gen.hist.2 <- alg_gen(data = Admissions.2, label = "Chance.of.Admit", 
                      pop_size = POP_SIZE,
                      generations = GENERATIONS,
                      mutation_rate = MUTATION_RATE, 
                      fitness.fn = get_fitness_cv,
                      k_folds = K)
# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist.2)
# Obtener el modelo
# Admissions.2 con todas las transformaciones
modelo.2 <- get_model_fitness(Admissions.2, predictors = gen.hist.2$final.inputs, label="Chance.of.Admit")

get_model_results(Admissions.2, modelo.2$model)
#0.07418765

##  --------------------------------------------------------------------
##  Modelo para la clase university.rating = 3
##  --------------------------------------------------------------------

# Transformaciones de todas las variables
Admissions.3 <- get.Admissions.features(Admissions, university.rating = 3)

# Variables en niveles para los modelos de SVR
Admissions.3 <- Admissions %>%
  dplyr::filter(University.Rating == 3) %>%
  dplyr::select(-University.Rating)

##  -------------------------------------------
##  Pruebas con algoritmo genético
##  -------------------------------------------

# Ejecutar la función de algoritmo genético
gen.hist.3 <- alg_gen(data = Admissions.3, label = "Chance.of.Admit", 
                      pop_size = POP_SIZE,
                      generations = GENERATIONS,
                      mutation_rate = MUTATION_RATE, 
                      fitness.fn = get_fitness_cv,
                      k_folds = K)
# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist.3)
# Obtener el RMSE
modelo.3 <- get_model_fitness(Admissions.3, predictors = gen.hist.3$final.inputs, label="Chance.of.Admit")

get_model_results(Admissions.3, modelo.3$model)
#0.05798834

##  -------------------------------------------
##  Pruebas con SVR
##  -------------------------------------------

# - Prueba con Admissions.3 con todas las variables
# - Prueba con Admissions.3 con 7 variables
modelo.3$tune <- tune.svm(Chance.of.Admit~., data = Admissions.3, kernel = "polynomial", 
                           degree = c(2:4),
                           gamma = seq(0.025, 0.1, 0.025), 
                           cost = 2^(0:4))
modelo.3$tune
# 1 - degree gamma cost
# 3 0.025    1

# 2 - degree gamma cost
# 3 0.075    8

modelo.3$model <- svm(Chance.of.Admit~., Admissions.3,
                         kernel = "polynomial", degree = 3, gamma = 0.075, cost = 8)

get_model_results(Admissions.3, modelo.3$model)
#0.05544331
#0.05316731

##  --------------------------------------------------------------------
##  Modelo para la clase university.rating = 4
##  --------------------------------------------------------------------

Admissions.4 <- get.Admissions.features(Admissions, university.rating = 4)
# Ejecutar la función de algoritmo genético
gen.hist.4 <- alg_gen(data = Admissions.4, label = "Chance.of.Admit", 
                      pop_size = POP_SIZE,
                      generations = GENERATIONS,
                      mutation_rate = MUTATION_RATE, 
                      fitness.fn = get_fitness_cv,
                      k_folds = K)
# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist.4)
# Obtener el RMSE
modelo.4 <- get_model_fitness(Admissions.4, predictors = gen.hist.4$final.inputs, label="Chance.of.Admit")

get_model_results(Admissions.4, modelo.4$model)
#0.02725043

##  --------------------------------------------------------------------
##  Modelo para la clase university.rating = 5
##  --------------------------------------------------------------------

Admissions.5 <- get.Admissions.features(Admissions, university.rating = 5)
# Ejecutar la función de algoritmo genético
gen.hist.5 <- alg_gen(data = Admissions.5, label = "Chance.of.Admit", 
                      pop_size = POP_SIZE,
                      generations = GENERATIONS,
                      mutation_rate = MUTATION_RATE, 
                      fitness.fn = get_fitness_cv,
                      k_folds = K)
# Graficar las trayectorias de las generaciones
graficarGeneraciones(gen.hist.5)
# Obtener el RMSE
modelo.5 <- get_model_fitness(Admissions.5, predictors = gen.hist.5$final.inputs, label="Chance.of.Admit")

get_model_results(Admissions.5, modelo.5$model)
#0.01659423

##  --------------------------------------------------------------------
##  Evaluación sobre el conjunto de entrenamiento
##  --------------------------------------------------------------------

pred.1 <- tibble(y = Admissions.1$Chance.of.Admit, yhat = predict(modelo.1$model, Admissions.1), Modelo = 1)
pred.2 <- tibble(y = Admissions.2$Chance.of.Admit, yhat = predict(modelo.2$model, Admissions.2), Modelo = 2)
pred.3 <- tibble(y = Admissions.3$Chance.of.Admit, yhat = predict(modelo.3$model, Admissions.3), Modelo = 3)
pred.4 <- tibble(y = Admissions.4$Chance.of.Admit, yhat = predict(modelo.4$model, Admissions.4), Modelo = 4)
pred.5 <- tibble(y = Admissions.5$Chance.of.Admit, yhat = predict(modelo.5$model, Admissions.5), Modelo = 5)

training.results <- bind_rows(pred.1, pred.2, pred.3, pred.4, pred.5)

get_model_results <- function(data, modelobj) {
  y <- data$Chance.of.Admit
  yhat <- predict(modelobj, data)
  rmse = sqrt(mean((y - yhat)^2))
  return(rmse)
}


##  -------------------------------------------
##  Evaluación RMSE por modelo
##  -------------------------------------------

model.results <- training.results %>%
  mutate(e2 = (y - yhat)^2) %>%
  group_by(Modelo) %>%
  summarise(RMSE = sqrt(mean(e2))) %>%
  print()


##  -------------------------------------------
##  Evaluación RMSE en conjunto de entrenamiento
##  -------------------------------------------

model.results <- training.results %>%
  mutate(e2 = (y - yhat)^2) %>%
  summarise(RMSE = sqrt(mean(e2))) %>%
  print()
  


##  --------------------------------------------------------------------
##  Generación del conjunto de prueba para submit
##  --------------------------------------------------------------------


Admissions.test <- read.csv("econometria-ug-2019/test.csv")

# Para la clase university.rating = 1
Admissions.test.1 <- get.Admissions.features(Admissions.test, university.rating = 1) 
pred.1 <- tibble(id = Admissions.test.1$id, `Chance of admit` = predict(modelo.1$model, Admissions.test.1))

# Para la clase university.rating = 2
Admissions.test.2 <- get.Admissions.features(Admissions.test, university.rating = 2) 
pred.2 <- tibble(id = Admissions.test.2$id, `Chance of admit` = predict(modelo.2$model, Admissions.test.2))

# Para la clase university.rating = 3
Admissions.test.3 <- get.Admissions.features(Admissions.test, university.rating = 3) 
pred.3 <- tibble(id = Admissions.test.3$id, `Chance of admit` = predict(modelo.3$model, Admissions.test.3))

# Para la clase university.rating = 4
Admissions.test.4 <- get.Admissions.features(Admissions.test, university.rating = 4) 
pred.4 <- tibble(id = Admissions.test.4$id, `Chance of admit` = predict(modelo.4$model, Admissions.test.4))

# Para la clase university.rating = 5
Admissions.test.5 <- get.Admissions.features(Admissions.test, university.rating = 5) 
pred.5 <- tibble(id = Admissions.test.5$id, `Chance of admit` = predict(modelo.5$model, Admissions.test.5))

# Generar el dataframe de predicciones finales
submission <- bind_rows(pred.1, pred.2, pred.3, pred.4, pred.5) %>% arrange(id)

sum(submission$`Chance of admit` < 0)
sum(submission$`Chance of admit` > 1)

write.csv(submission, "submissions/modelosUniversityRating-v3.csv", quote = FALSE, row.names = FALSE)


##  ----------------------------------------------------------------------------------------------------------
##  Pruebas anteriores, antes de la clasificación por University.Rating
##  ----------------------------------------------------------------------------------------------------------

##  --------------------------------------------------------------------
##  Generación del conjunto de prueba
##  --------------------------------------------------------------------

Admissions.test <- read.csv("econometria-ug-2019/test.csv")

sq.Admissions.test <- get_transformed_df(Admissions.test, c("Chance.of.Admit", "Research", "id"), function(x) x^2, "sq")
cub.Admissions.test <- get_transformed_df(Admissions.test, c("Chance.of.Admit", "Research", "id"), function(x) x^3, "cube")
inv.Admissions.test <- get_transformed_df(Admissions.test, c("Chance.of.Admit", "Research", "id"), function(x) 1/x, "inv")
log.Admissions.test <- get_transformed_df(Admissions.test, c("Chance.of.Admit", "Research", "id"), function(x) log(x), "log")
int.Admissions.test <- Admissions.test$Research * get_transformed_df(Admissions.test, c("Chance.of.Admit", "Research", "id"), function(x) x, "int.Research")
#final.Admissions.test <- cbind(Admissions.test, sq.Admissions.test, cub.Admissions.test, inv.Admissions.test, int.Admissions.test, log.Admissions.test)
final.Admissions.test <- cbind(Admissions.test, sq.Admissions.test, cub.Admissions.test, inv.Admissions.test, int.Admissions.test)
#final.Admissions.test <- cbind(Admissions.test, int.Admissions.test)

# Obtener la fórmula final y ajustar el modelo con final.Admissions (de entrenamiento)
formula <- paste("Chance.of.Admit~", paste(gen.hist$final.inputs, collapse = "+"), sep = "")
lm.fit <- lm(formula, data = final.Admissions)

# Obtener los valores ajustados con el conjunto de prueba
pred.lm <- predict(lm.fit, final.Admissions.test)

# Generar un dataframe con id y pronóstico
submission <- Admissions.test %>% dplyr::select(id)
submission$`Chance of Admit`<- pred.lm

# Guardar el archivo de submission
write.csv(submission, "submissions/sub-gen-alg-mutrate0.01-gens100-nopoly.csv", quote = FALSE, row.names = FALSE)


##  --------------------------------------------------------------------
##  Obtener valores ajustados y observados para ver en dónde se está fallando más
##  --------------------------------------------------------------------

# Obtener la fórmula final y ajustar el modelo con final.Admissions (de entrenamiento)
formula <- paste("Chance.of.Admit~", paste(gen.hist$final.inputs, collapse = "+"), sep = "")
lm.fit <- lm(formula, data = final.Admissions)

# Obtener los valores ajustados con el conjunto de prueba
pred.in.lm <- predict(lm.fit, final.Admissions)

revAdmissions <- Admissions
revAdmissions[["fitted.Chance.of.Admit"]] <- pred.in.lm
revAdmissions$ID <- 1 : nrow(revAdmissions)


write.csv(revAdmissions, "econometria-ug-2019/rev-Admissions.csv", quote = FALSE, row.names = FALSE)



