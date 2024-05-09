
mi_vector <- c(10, 20, 30, 40, 50)#vector
mi_lista <- list(nombre = "Juan", edad = 30, notas = c(85, 90, 75))#lista



#int
n1 <- 15  
typeof(n1)
#double
n2 <- 1.5
#caracter
c1 <- "c"
#string
c2 <- "a string of text"
#boolean
l1 <- TRUE
l2 <- F


#vector
v1 <- c(1, 2, 3, 4, 5)
is.vector(v1)
#matriz
m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m2 <- matrix(c("a", "b", 
               "c", "d"), 
               nrow = 2,
               byrow = T)
#array
# (rows, columns, tables)
a1 <- array(c( 1:24), c(4, 3, 2))

# Data frame
vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)
dfa <- cbind(vNumeric, vCharacter, vLogical)#une en columnas->1acolumna: 1,2,3
dfa1 <- rbind(vNumeric, vCharacter, vLogical)#une en filas->1afila: 1,2,3
df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))


#lista
o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(T, F, T, T, F)
list1 <- list(o1, o2, o3)
list2 <- list(o1, o2, o3, list1)  



#a int
(coerce3 <- as.integer(5))
typeof(coerce3)
#a numero
(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

#matriz a dataframe
(coerce6 <- matrix(1:9, nrow= 3))
is.matrix(coerce6)

(coerce7 <- as.data.frame(matrix(1:9, nrow= 3)))
is.data.frame(coerce7)
