# tidyverse
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# cargamos conjunto de datos
data(mtcars)
mtcars
print(dim(mtcars))#dimension
print(length(mtcars))#da las columnas
print(nrow(mtcars))#da las filas
print(ncol(mtcars))#da las columnas

# filtramos solo los que tienen mas de 150 caballos
mtcars_filtrado <- mtcars %>%
  filter(hp > 150)

print(dim(mtcars_filtrado))#dimension

# resumen de los datos, agrupandolos
resumen_estadistico <- mtcars_filtrado %>%
  summarise(media_hp = mean(hp),
            mediana_mpg = median(mpg),
            maximo_disp = max(disp))

# ver relacion entre hp y mpg
grafico <- ggplot(mtcars_filtrado, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +  
  labs(x = "Caballos de Fuerza", y = "Millas por Galón", title = "Relación entre HP y MPG")

# ver resumen y grafico
print(resumen_estadistico)
print(grafico)
