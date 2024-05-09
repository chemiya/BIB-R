
library(datasets)
install.packages("lars")
library(lars)

?USJudgeRatings
head(USJudgeRatings)
data <- USJudgeRatings

#columnas menos la 12
x <- as.matrix(data[, -12])
#columna 12
y <- data[, 12]

#regresion
reg1 <- lm(y ~ x)

#especificando variables
reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG +
           DECI + PREP + FAMI + ORAL + WRIT + PHYS,
           data = USJudgeRatings)

# resultados
reg1           
summary(reg1)  

#otros datos
anova(reg1)            
coef(reg1)             
confint(reg1)          
resid(reg1)            
hist(residuals(reg1))  

# Conventional stepwise regression
stepwise <- lars(x, y, type = "stepwise")

# Stagewise: stepwise pero con mejor generalizacion
forward <- lars(x, y, type = "forward.stagewise")

# LAR
lar <- lars(x, y, type = "lar")

# LASSO
lasso <- lars(x, y, type = "lasso")

# comparar los r2, cogiendolos y redondeando
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) %>% 
            round(2)
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp 
