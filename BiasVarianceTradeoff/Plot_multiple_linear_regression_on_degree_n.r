library(MASS)
library(ISLR)
library(dplyr)
library(ggplot2)


know_data <- Boston %>% 
  dplyr::select(lstat,medv)


names(know_data)[1] <- "input"
names(know_data)[2] <- "output"


know_data.rows <- nrow(know_data)

# Función para obtener un boot sample
get_boot <- function(x) {
  index <- sample(1 : know_data.rows, 
                size = know_data.rows, 
                replace = TRUE)
  return(know_data[index, ])
}

# Función para ajustar modelo de grado 'degree'
fit_lm <- function(dataset, degree = 2) {
  formula <- paste0("I(","input^", 1:degree, ")", collapse = '+')
  formula <- paste0("output ~ ", formula)
  fit <- lm(formula, data = dataset)
  return(fit)
}

model_plot_data <- function(fit){
  xaxis <- seq(min(know_data$input), max(know_data$input), by=0.01)
  yaxis <- predict(fit, tibble(input = xaxis))
  return(tibble(input=xaxis, output=yaxis))
}


nboots<-100
boots <- lapply(1:nboots, get_boot)


all.models <- lapply(boots, fit_lm, degree = 45)

all.model.prediction <- lapply(all.models, model_plot_data)


plot(know_data, pch=20, cex=0.25)

for(i in 1:nboots){
  print(i)
  points(all.model.prediction[[i]], col='gray', type='l')
}


# Valor promedio de las hipótesis

df <- bind_rows(all.model.prediction,.id = "boot")

mean_pred <- df %>% 
  group_by(input) %>% 
  summarise(avg_pred = mean(output))

points(mean_pred,type = 'l', col='red')

fit.global <- fit_lm(know_data, 45)
plot.global <- model_plot_data(fit.global)
points(plot.global, col='darkgreen', type='l')


  










