library(tidyr)

# datos
temperaturas <- data.frame(
  dia_semana = c("lunes", "martes", "miércoles"),
  hora_0 = c(20, 22, 19),
  hora_1 = c(18, 21, 20),
  hora_2 = c(17, 20, 19)
)
temperaturas

# una fila por dia, hora y temperatura
temperaturas_reorganizadas <- temperaturas %>%
  pivot_longer(cols = starts_with("hora"), 
               names_to = "hora",
               values_to = "temperatura")

print(temperaturas_reorganizadas)#lunes      hora_0          20
