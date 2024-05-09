
library(datasets)  

head(iris)
plot(iris$Species)  # variable categorica--> de barras
plot(iris$Petal.Length)  # variable cuantitativa--> de puntos
plot(iris$Species, iris$Petal.Width)  # boxplot
plot(iris$Petal.Length, iris$Petal.Width)  # de puntos
plot(iris)  # con todas las parejas de variables

#grafico de puntos con plot
plot(iris$Petal.Length, iris$Petal.Width,
  col = "#cc0000",  
  pch = 19,         
  main = "Iris: Petal Length vs. Petal Width",
  xlab = "Petal Length",
  ylab = "Petal Width")





