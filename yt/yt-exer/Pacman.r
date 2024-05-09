if (!require("pacman")) install.packages("pacman")

# cargar paquete
library(pacman)

# Instalar paquetes
pacman::p_install("ggplot2", "dplyr", "readr")

# Cargar paquetes
pacman::p_load(ggplot2, dplyr, readr)
