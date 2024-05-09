
library(datasets)  

?mtcars
head(mtcars)

hist(mtcars$wt)
hist(mtcars$mpg)

#de puntos
plot(mtcars$wt, mtcars$mpg)

#otro tipo de puntos
plot(mtcars$wt, mtcars$mpg,
  pch = 19,         
  cex = 1.5,        
  col = "#cc0000",  
  main = "MPG as a Function of Weight of Cars",
  xlab = "Weight (in 1000 pounds)",
  ylab = "MPG")
