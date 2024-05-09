
library(tibble)

# Crear tibble
datos <- tibble(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia")
)

# imprimirlo
print("Tibble original:")
print(datos)

# transformar celdas que cumplen condicion
datos <- datos %>%
  mutate(edad = ifelse(edad > 30, edad + 1, edad))

# mostrarlo
print("Tibble con transformación aplicada:")
print(datos)

# añadir fila
nueva_fila <- tibble(
  ID = 5,
  nombre = "Luis",
  edad = 29,
  ciudad = "Málaga"
)

#insertar fila
datos <- bind_rows(datos, nueva_fila)

# mostrar
print("Tibble con nueva fila añadida:")
print(datos)
