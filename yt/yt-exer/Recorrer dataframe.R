#cargamos datos
data(mtcars)

#tamaño
n_row <- nrow(mtcars)
n_col <- ncol(mtcars)

# inicializar lista
valores <- list()

# recorrer filas
for (i in 1:n_row) {
  # recorrer columna
  for (j in 1:n_col) {
    # obtener valor fila y columna
    valor <- mtcars[i, j]
    # guardar valor en la lista
    valores[[paste0("fila", i, "_columna", j)]] <- valor
  }
}

# mostrar valores
valores

#resultado como: $fila32_columna6 [1] 2.78
