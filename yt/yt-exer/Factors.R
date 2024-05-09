

(x1 <- 1:3)
(y  <- 1:9)

#combinamos como columnas: primera fila 1,1
(df1 <- cbind.data.frame(x1, y))






#como y es mas grande, se repite 1,2,3
x3  <- c(1:3)
df3 <- cbind.data.frame(x3, y)
(df3$x3 <- factor(df3$x3,
  levels = c(1, 2, 3)))
df3


#reemplaza numero por etiqueta
x4  <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4,
  levels = c(1, 2, 3),
  labels = c("macOS", "Windows", "Linux"))
df4



#igual pero pone las etiquetas ordenadas
x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
  levels = c(3, 1, 2),
  labels = c("No", "Maybe", "Yes")))
df5

