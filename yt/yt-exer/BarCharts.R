
library(datasets)
#conjunto de datos de coches
head(mtcars)

cylinders <- table(mtcars$cyl) 
cylinders
barplot(cylinders)              

