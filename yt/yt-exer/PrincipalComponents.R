

library(datasets)  
?mtcars
head(mtcars)
#seleccionar variables
cars <- mtcars[, c(1:4, 6:7, 9:11)]  
head(cars)

#media a 0 y varianza 1
pc <- prcomp(cars,
        center = TRUE,  
        scale = TRUE)   

#lo mismo pero con las variables
pc <- prcomp(~ mpg + cyl + disp + hp + wt + qsec + am +
        gear + carb, 
        data = mtcars, 
        center = TRUE,
        scale = TRUE)

summary(pc)#resumen de los componentes
plot(pc)# el primero es muy importante, tiene un valor alto
pc#muestra la rotacion, similar a la correlacion entre las variables
library(dplyr)
predict(pc) %>% round(2)#para cada fila del conjunto, calcula el componente
biplot(pc)#muestra la direccion de cada variable
#caballos y cilindros van dirigidos a coches mas pesados
