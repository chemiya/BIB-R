library(dplyr)

# datos
ventas <- data.frame(
  region = c("A", "A", "B", "B", "A", "B"),
  mes = c("enero", "febrero", "enero", "febrero", "enero", "febrero"),
  ventas = c(1000, 1500, 2000, 2500, 1200, 1800)
)

# sumar ventas por region, utiliza sumarise
resumen_ventas <- ventas %>%
  group_by(region) %>%
  summarise(ventas_totales = sum(ventas))

print(resumen_ventas)
print(class(resumen_ventas))
