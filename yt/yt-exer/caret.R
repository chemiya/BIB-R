# Cargar paquete
if (!require("caret")) install.packages("caret")
library(caret)

# datos
data(mtcars)

# Dividir datos
set.seed(123)  # semilla
indices <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
datos_entrenamiento <- mtcars[indices, ]
datos_prueba <- mtcars[-indices, ]

# modelo de regresion lineal
modelo <- train(mpg ~ ., data = datos_entrenamiento, method = "lm")

# resumen
print(modelo)

# Predecir valores
predicciones <- predict(modelo, newdata = datos_prueba)

# Calcular RMSE
rmse <- sqrt(mean((predicciones - datos_prueba$mpg)^2))
print(paste("RMSE:", rmse))
