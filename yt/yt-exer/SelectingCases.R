
library(datasets)  


head(iris)

#descripcion de los datos
hist(iris$Petal.Length)
summary(iris$Petal.Length) #media, 1o quartil...
summary(iris$Species)  #cuenta cuantos de cada tipo

# solo con este tipo
hist(iris$Petal.Length[iris$Species == "versicolor"],
  main = "Petal Length: Versicolor")

# solo con estos petalos
hist(iris$Petal.Length[iris$Petal.Length < 2],
  main = "Petal Length < 2")

#con ambas condiciones
hist(iris$Petal.Length[iris$Species == "virginica" & 
  iris$Petal.Length < 5.5],
  main = "Petal Length: Short Virginica")


#subconjunto
i.setosa <- iris[iris$Species == "setosa", ]
#ver sus datos
head(i.setosa)
summary(i.setosa$Petal.Length)
hist(i.setosa$Petal.Length)
