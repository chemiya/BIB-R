# dataframe
datos <- data.frame(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia"),
  stringsAsFactors = FALSE
)

# Mostrarlo
print("Dataframe original:")
print(datos)

# Crear fila
nueva_fila <- c(ID = 5, nombre = "Luis", edad = 40, ciudad = "Málaga")

# añadir la fila
datos <- rbind(datos, nueva_fila)

# reiniciar ids
rownames(datos) <- NULL

# Mostrarlo
print("Dataframe con nueva fila añadida:")
print(datos)
