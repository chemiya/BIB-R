

library(datasets)  

head(lynx)
hist(lynx)
hist(lynx,
     breaks = 14,          
     freq   = FALSE,       
     col    = "thistle1", 
     main   = paste("Histogram of Annual Canadian Lynx",
                    "Trappings, 1821-1934"),
     xlab   = "Number of Lynx Trapped")

# Distribucion normal, superpuesto anterior grafico
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",  
      lwd = 2,           
      add = TRUE)        

# curva de densidad estimada
lines(density(lynx), col = "blue", lwd = 2)
lines(density(lynx, adjust = 3), col = "purple", lwd = 2)

# Agrega pelos para ver la distribucion
rug(lynx, lwd = 2, col = "gray")
