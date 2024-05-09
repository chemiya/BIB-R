

library(datasets)  
?mtcars
head(mtcars)
#seleccionamos variables
cars <- mtcars[, c(1:4, 6:7, 9:11)] 
head(cars)

library(dplyr)
hc <- cars   %>%  
      dist   %>%  
      hclust      
plot(hc)          

#añadir colores
rect.hclust(hc, k = 2, border = "gray")
rect.hclust(hc, k = 3, border = "blue")
rect.hclust(hc, k = 4, border = "green4")
rect.hclust(hc, k = 5, border = "darkred")


