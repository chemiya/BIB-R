
library(tibble)
library(dplyr)

# Crear tibble
datos <- tibble(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia")
)

# Mostrarlo
print("Tibble original:")
print(datos)

# Insertar columna con condicion
datos <- datos %>%
  mutate(tipo_cliente = if_else(ciudad == "Madrid" & edad > 27, "cliente Madrid", "cliente externo"))

# Mostrarlo
print("Tibble con nueva columna añadida:")
print(datos)
