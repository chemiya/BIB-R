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

# Insertar columna
datos$tipo_cliente <- ifelse(datos$ciudad == "Madrid" & datos$edad > 27, "cliente Madrid", "cliente externo")

# Mostrarlo
print("Dataframe con nueva columna añadida:")
print(datos)
