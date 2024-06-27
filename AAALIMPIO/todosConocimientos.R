
# Carga de datos
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO")
nombres<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre",
           "octubre","noviembre","diciembre")
# Establecer nombres columnas
datos <- read.csv("revenues-expenses.csv",header = FALSE,col.names=nombres)
head(datos)


# Resumen
head(datos)
summary(datos)
str(datos)



# Separar datos
# Primera fila
revenues <- datos[1, ]
head(revenues)
expenses <- datos[2, ]
head(expenses)
print(class(revenues))




# Cálculos iniciales
# Calcular la media y redondear
medias_revenues <- round(rowMeans(revenues),2)
mensaje<-paste("La media de los ingresos es:",medias_revenues[1])
print(mensaje)
medias_expenses <- rowMeans(expenses)
mensaje_expenses<-paste("La media de los expenses es:",medias_expenses[1])
print(mensaje_expenses)





# Ver tipos
print(class(revenues))
print(class(expenses))


# Cálculo beneficios
profit <- revenues - expenses
print("beneficios")
profit


# Suma de todos los valores
suma_profits <- rowSums(profit)
mensaje_profits<-paste("La suma de los profits es:",suma_profits[1])
print(mensaje_profits)




# Impuestos
tax <- round(0.30 * profit, 2)
tax 



#Crear elemento nulo
profit.after.tax<-NULL



# Quitando impuestos. Recorrer los profits
for (i in seq_along(profit)) {
  # Si el beneficio es negativo, los impuestos también son negativos y se suman.
  if(profit[i]<0){
    profit.after.tax[i]=profit[i]+tax[i]
  }else{
    profit.after.tax[i]=profit[i]-tax[i]
  }
}
profit.after.tax




# Conversiones
print(class(profit.after.tax))
# Lista a Dataframe
profit.after.tax<-as.data.frame(profit.after.tax)
print(class(profit.after.tax))
# Nombres de las columnas
names(profit.after.tax)<-nombres
profit.after.tax

print(class(profit))
print(class(profit.after.tax))
print("Comparación")
# Une filas dataframes
comparacion<-rbind(profit, profit.after.tax)
print(comparacion)


# Acumular valores de la fila
suma_profits_after <- rowSums(profit.after.tax)
mensaje_profits_after<-paste("La suma de los profits_after_tax es:",suma_profits_after[1])
print(mensaje_profits_after)




# Margen calculando desde otras listas
profit.margin <- round(profit.after.tax/ revenues, 2) * 100
profit.margin




# Beneficios medios
mean_pat <- round(rowMeans(profit.after.tax),2)
mensaje_media_after<-paste("La media de los profits_after_tax es:",mean_pat[1])
print(mensaje_media_after)

print(class(profit.after.tax))


# Meses con beneficios superiores a la media
good.months <- profit.after.tax > mean_pat
# Devuelve true, false...
good.months 
cuenta_good<-sum(good.months)
mensaje_good<-paste("Meses superiores a la media:",cuenta_good[1])
print(mensaje_good)




# Meses con beneficios inferiores a la media
bad.months <- !good.months
# Opuestos a true, false...
cuenta_bad<-sum(bad.months)
mensaje_bad<-paste("Meses inferiores a la media:",cuenta_bad[1])
print(mensaje_bad)



# Mejor mes. Encontrar índice del valor máximo
mejor <- names(profit.after.tax)[which.max(profit.after.tax)]
print(paste("Mejor mes:", mejor))

# Peor mes Encontrar índice del valor mínimo
peor <- names(profit.after.tax)[which.min(profit.after.tax)]
print(paste("Peor mes:", peor))


  
  
  
  








  
  
  
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO")
# Leer csv con header y corrigiendo vacíos
companies_data <- read.csv("companies.csv",header = TRUE,na.strings = c(""))




# Primeros elementos
head(companies_data) 
# Resumen de las variables->media y std
summary(companies_data) 
# Ver valores que puede tomar
str(companies_data) 
# Nombre columnas
names(companies_data) 
colnames(companies_data)
# Número filas
nrow(companies_data) 
# Número columnas
ncol(companies_data) 


# Filas con algún atributo vacío
companies_data[!complete.cases(companies_data),]
# Contar esas filas
nrow(companies_data[!complete.cases(companies_data),])


# Convertir a factor una columna
companies_data$Industry<-factor(companies_data$Industry)
# Se muestran los valores diferentes que puede tomar
str(companies_data)
# Valores diferentes del factor
levels(companies_data$Industry)


# Valores únicos de un atributo
unicos_estados<-unique(companies_data$State)
unicos_estados
# Contarlos
cantidad_estados <- length(unicos_estados)
cantidad_estados


# Reemplazar cadenas
head(companies_data)
companies_data$Expenses<-gsub(" Dollars","",companies_data$Expenses)
head(companies_data)
companies_data$Expenses<-gsub(",","",companies_data$Expenses)
head(companies_data)
#\\ porque es especial el dolar
companies_data$Revenue<-gsub("\\$","",companies_data$Revenue)
head(companies_data)
companies_data$Revenue<-gsub(",","",companies_data$Revenue)
head(companies_data)
companies_data$Growth<-gsub("%","",companies_data$Growth)
head(companies_data)
str(companies_data)


# Ver el tipo de una columna
tipo_columna <- class(companies_data$Expenses)
tipo_columna


# Convertir a numérica una columna
companies_data$Expenses<-as.numeric(companies_data$Expenses)
tipo_columna <- class(companies_data$Expenses)
tipo_columna

companies_data$Revenue<-as.numeric(companies_data$Revenue)
companies_data$Growth<-as.numeric(companies_data$Growth)
str(companies_data)


# Filas con algún atributo vacío. Mostrar y contar
companies_data[!complete.cases(companies_data),]
nrow(companies_data[!complete.cases(companies_data),])


# Filtrar filas con determinado número de empleados
companies_data[which(companies_data$Employees==45),]


# Coger las filas que no tienen el valor vacío en esa columna
companies_data<-companies_data[!is.na(companies_data$Industry),]
nrow(companies_data[!complete.cases(companies_data),])


#Reiniciar índices
rownames(companies_data)<-NULL
companies_data

# Volver a buscar las que tienen algún nulo
nrow(companies_data[!complete.cases(companies_data),])
companies_data[!complete.cases(companies_data),]

# Contar los valores NA en la columna
num_na <- sum(is.na(companies_data$Inception))
# Mostrar el número de valores NA
print(num_na)

# Eliminar nulos reemplazando por la moda
# Función para calcular la moda
# Cuenta las veces que aparece cada elemento
calcular_moda <- function(x) {
  uniqx <- unique(x)
  # Cuenta número de veces que hay de cada elemento y coge el máximo
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Calcular la moda de la columna, ignorando los NA
moda <- calcular_moda(na.omit(companies_data$Inception))
moda

# Reemplazar los NA con la moda
companies_data$Inception[is.na(companies_data$Inception)] <- moda

# Convertir a numérica una columna
companies_data$Expenses<-as.numeric(companies_data$Inception)
tipo_columna <- class(companies_data$Inception)
tipo_columna


# Poner en el atributo state el valor que corresponde
companies_data[is.na(companies_data$State) & companies_data$City=="New York","State"]<-"NY"

# Ver esas filas
companies_data[c(11,377),]


# Poner el estado que corresponde
companies_data[is.na(companies_data$State) & companies_data$City=="San Francisco",]
companies_data[is.na(companies_data$State) & companies_data$City=="San Francisco","State"]<-"CA"
companies_data[c(82,265),]


# Filas que quedan con algún atributo vacío
companies_data[!complete.cases(companies_data),]
nrow(companies_data[!complete.cases(companies_data),])


#Calcular mediana de la columna. Los valores faltantes no los tiene en cuenta
median(companies_data[,"Employees"],na.rm=TRUE)
mean(companies_data[,"Employees"],na.rm=TRUE)
# Media de la columna de empleados
mean(companies_data$Employees)


# Mediana de la columna empleados para las filas de la industria retail
med_emp_retail<-median(companies_data[companies_data$Industry=="Retail","Employees"],na.rm=TRUE)



# Buscar filas con empleados vacíos y de la industria retail y poner el valor
companies_data[is.na(companies_data$Employees) & companies_data$Industry=="Retail",]
companies_data[is.na(companies_data$Employees) & companies_data$Industry=="Retail","Employees"]<-med_emp_retail


# Ver la fila
companies_data[3,]


# Filas de esa industria. 
companies_data[companies_data$Industry=="Financial Services",]
# Ver solo los empleados de las filas de esa industria
companies_data[companies_data$Industry=="Financial Services","Employees"]

# Calcular mediana de empleados de las filas de esa industria
med_emp_financial<-median(companies_data[companies_data$Industry==
                                           "Financial Services","Employees"],na.rm=TRUE)

# Buscar filas sin empleados y de esa industria y poner el valor de la mediana en los empleados
companies_data[is.na(companies_data$Employees) &
                 companies_data$Industry=="Financial Services",]
companies_data[is.na(companies_data$Employees) &
                 companies_data$Industry=="Financial Services","Employees"]<-med_emp_financial
companies_data[330,]

# Filas quedan con algun atributo vacío
companies_data[!complete.cases(companies_data),]





# Mediana de crecimientos para filas de la industria de construcción
med_grow_constr<-median(companies_data[companies_data$Industry==
                                         "Construction","Growth"],na.rm=TRUE)
med_grow_constr

# Buscar filas sin crecimiento y de la industria de construcción y poner el valor en crecimiento
companies_data[is.na(companies_data$Growth) & companies_data$Industry=="Construction",]
companies_data[is.na(companies_data$Growth) & companies_data$Industry==
                 "Construction","Growth"]<-med_grow_constr
companies_data[8,]





# Mediana de los ingresos para empresas de construcción
med_revenue_constr<-median(companies_data[companies_data$Industry=="Construction",
                                          "Revenue"],na.rm=TRUE)
med_revenue_constr

# Si no tienen ingresos y son de construcción, se pone ese valor
companies_data[is.na(companies_data$Revenue) & companies_data$Industry=="Construction",]
companies_data[is.na(companies_data$Revenue) &
                 companies_data$Industry=="Construction","Revenue"]<-med_revenue_constr
companies_data[c(8,42),]

# Lo mismo con los gastos
med_expenses_constr<-median(companies_data[companies_data$Industry=="Construction","Expenses"],na.rm=TRUE)
med_expenses_constr

companies_data[is.na(companies_data$Expenses) & companies_data$Industry=="Construction"& is.na(companies_data$Profit),]
companies_data[is.na(companies_data$Expenses) & companies_data$Industry=="Construction"& is.na(companies_data$Profit),"Expenses"]<-med_expenses_constr
companies_data[c(8,42),]


# El beneficio se calcula a partir de otros, filas que lo tienen vacío
# Poner el calculo de beneficios-gastos
companies_data[is.na(companies_data$Profit),]
companies_data[is.na(companies_data$Profit),"Profit"]<-companies_data[is.na(companies_data$Profit),"Revenue"]-companies_data[is.na(companies_data$Profit),"Expenses"]

companies_data[c(8,42),]


# Filas que quedan con algún atributo vacío
companies_data[!complete.cases(companies_data),]


# Similar, poner el valor calculándolo desde los otros
companies_data[is.na(companies_data$Expenses),"Expenses"]<-companies_data[is.na(companies_data$Expenses),"Revenue"]-companies_data[is.na(companies_data$Expenses),"Profit"]
companies_data[15,]



# Quedar solo los que tienen todos los atributos informados
companies_data<-companies_data[complete.cases(companies_data),]





head(companies_data)
# Nombres columnas
colnames(companies_data) 
# Valores diferentes de las columnas
str(companies_data) 


# Instalar la librería y cargarla
install.packages("ggplot2")
library(ggplot2)


# Gráfico puntos con 4 dimensiones. X,y,tamaño y color
p<-ggplot(data=companies_data)
p+geom_point(aes(x=Revenue,y=Expenses,colour=Industry,size=Profit))

# Con línea curva
d<-ggplot(data=companies_data,aes(x=Revenue,y=Expenses,colour=Industry))
d+geom_point()+geom_smooth(fill=NA,size=1.2)

# De caja y bigotes. X,y, y color
f<-ggplot(data=companies_data,aes(x=Industry,y=Growth,colour=Industry))
f+geom_boxplot(size=1)

# De caja con puntos
f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)

# Separando varios gráficos. x,y, color separando por industria
p<-ggplot(data=companies_data,aes(x=Revenue,y=Expenses,colour=State))
p+geom_point(size=3)+facet_grid(.~Industry)


#dplyr
install.packages("dplyr")
library(dplyr)

# Agrupa por industria, suma los beneficios y ordena descendente
industria_beneficios <- companies_data %>%
  group_by(Industry) %>%
  summarise(Suma_Beneficios = sum(Profit)) %>%
  arrange(desc(Suma_Beneficios))


# Convertir a dataframe y mostrar
print(class(industria_beneficios))
industria_beneficios<- as.data.frame(industria_beneficios)
industria_beneficios


# Cuenta cuantas compañíass son de cada industria
frecuencia <- table(companies_data$Industry)
frecuencia<- as.data.frame(frecuencia)
frecuencia
colnames(frecuencia)<-c("Industry","Cuenta")
frecuencia

# Juntar ambas tablas por la clave industria
merged_industry<-merge(industria_beneficios, frecuencia, by.x="Industry", by.y="Industry")
merged_industry


# Insertar columna con cálculo redondeando
merged_industry$Media<-round(merged_industry$Suma_Beneficios/merged_industry$Cuenta,2)
merged_industry


install.packages("ggplot2")
library(ggplot2)


# Beneficios por área, poniendo etiquetas
ggplot(merged_industry, aes(x = Industry, y = Cuenta)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Gráfico de Barras de proporción beneficios", x = "Industria",
       y = "Proporción")+
  theme(plot.title = element_text(hjust = 0.5))













# Carga de datos
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO")
movies_data <- read.csv("movies-data.csv",header = TRUE)





# Comprobar datos cargados
head(movies_data) 
summary(movies_data) 
str(movies_data) 
names(movies_data) 
nombres_datos<-names(movies_data)
# Tipo de variable
print(class(nombres_datos)) 
nrow(movies_data) 
ncol(movies_data) 





# Corregir nombres de las columnas eliminando puntos
nombres_datos <- gsub("\\.", "", nombres_datos)
nombres_datos
# Volver a establecerlos
names(movies_data)<-nombres_datos
str(movies_data)




# Tipo de una columna
print(class(movies_data$Profitmill))
# Convertir columna a numérica
movies_data$Profitmill<- as.numeric(movies_data$Profitmill)

# Contar nulos e imprimir en pantalla
num_na <- sum(is.na(movies_data$Profitmill))
print(paste("NA en Profitmill:", num_na))



# Media de la columna con 2 decimales
media_valor <- round(mean(movies_data$Profitmill, na.rm = TRUE),2)
print(paste("Media de la columna Profitmill si na:", media_valor))
# Si no se elimina na, sale na
media_valor <- round(mean(movies_data$Profitmill),2)
print(paste("Media de la columna Profitmill con na:", media_valor))


# Mostrar tipo de una columna
print(class(movies_data$AdjustedGrossmill))
# Convertir a numérica
movies_data$AdjustedGrossmill<- as.numeric(movies_data$AdjustedGrossmill)
# Contar nulos
num_na <- sum(is.na(movies_data$AdjustedGrossmill))
print(paste("NA en AdjustedGrossmill:", num_na))

print(class(movies_data$Grossmill))
movies_data$Grossmill<- as.numeric(movies_data$Grossmill)
num_na <- sum(is.na(movies_data$Grossmill))
print(paste("NA en Grossmill:", num_na))


print(class(movies_data$Overseasmill))
movies_data$Overseasmill<- as.numeric(movies_data$Overseasmill)
num_na <- sum(is.na(movies_data$Overseasmill))
print(paste("NA en Overseasmill:", num_na))

# Contar filas con al menos algún valor faltante
nrow(movies_data[!complete.cases(movies_data),])
# Quedarse solo con filas completas
movies_data<-movies_data[complete.cases(movies_data),]
nrow(movies_data)






# Convertir a factor una columna
movies_data$DayofWeek<-factor(movies_data$DayofWeek)
# Ver su valores
levels(movies_data$DayofWeek)
movies_data$Genre<-factor(movies_data$Genre)
levels(movies_data$Genre)
str(movies_data)





# Ver distribuciones días, cuántos hay de cada elemento
distribucion_dia <- table(movies_data$DayofWeek)
print(distribucion_dia)




# Gráfico días de estreno con etiquetas
library(ggplot2)

p<-ggplot(data=movies_data, aes(x=DayofWeek)) 
q<-p+ geom_bar()
q <- q +
  xlab("Dia de estreno") + 
  ylab("Cuenta") + 
  ggtitle("Distribución películas día estreno") 

q<- q+ theme(plot.title = element_text(hjust = 0.5))
q





# Estudios con más películas, contar y ordenar
distribucion_studio <- table(movies_data$Studio)
distribucion_studio <- sort(distribucion_studio, decreasing = TRUE)
print(distribucion_studio)



# Media de la columna de beneficios
mean(movies_data$Profit)


# Media de beneficios por estudio
library(dplyr)
movies_data_beneficios <- movies_data %>%
  group_by(Studio) %>%
  summarise(Media_Beneficios = mean(Profit)) %>%
  arrange(desc(Media_Beneficios))

# Convertir a dataframe y mostrar
print(class(movies_data_beneficios))
movies_data_beneficios<- as.data.frame(movies_data_beneficios)
movies_data_beneficios



# Estudio con mayor media
studio_mas_beneficios<-movies_data_beneficios[1,"Studio"]
print(paste("Studio con mayor media beneficios:", studio_mas_beneficios))






# Película con mayor beneficio
fila_max <- movies_data[which.max(movies_data$Profit), ]
print(fila_max$MovieTitle)


# Película con mejor puntuación
fila_max <- movies_data[which.max(movies_data$IMDbRating), ]
print(fila_max$MovieTitle)


# Nueva columna con media puntuaciones
movies_data$MediaRating<-round((movies_data$IMDbRating+movies_data$MovieLensRating)/2,2)
head(movies_data)

# Eliminar columna
movies_data$MovieLensRating<-NULL
head(movies_data)



# Añadir columna con valores según condición
movies_data <- movies_data %>%mutate(aprobada = ifelse(MediaRating >5, "Aprobada", "No Aprobada"))
head(movies_data)


# Contar aprobadas y las que no
distribucion_aprobadas <- table(movies_data$aprobada)
print(distribucion_aprobadas)

# Porcentaje de aprobadas
distribucion_aprobadas<-(distribucion_aprobadas/nrow(movies_data))*100
print(distribucion_aprobadas)


# Tabla de frecuencias con las notas
intervalos <- seq(0, 10, by = 0.5)  
movies_data$IntervaloNota <- cut(movies_data$MediaRating, breaks = intervalos, 
                                 include.lowest = TRUE)
tabla_frecuencias <- table(movies_data$IntervaloNota)
print(tabla_frecuencias)
print(class(tabla_frecuencias))
tabla_frecuencias<-as.data.frame(tabla_frecuencias)
tabla_frecuencias

# Convertir la tabla de frecuencias a un vector para barplot
frecuencias_vector <- as.vector(tabla_frecuencias)
nombres_intervalos <- names(tabla_frecuencias)

# Crear el gráfico de barras
barplot(frecuencias_vector, 
        main = "Distribución de MediaRating", 
        xlab = "Intervalo de puntuación", 
        ylab = "Frecuencia", 
        col = "lightblue", 
        border = "black", 
        las = 2, 
        cex.names = 0.8,
        names.arg = nombres_intervalos)



# Gráfico rating, profit y género
qplot(data=movies_data, x=MediaRating, y=Profit,
      colour=Genre,                               
      size=I(5), 
      alpha=I(0.6),                               
      main="MediaRating vs Profit segun genero" 
)




# Filtrar las que cumplen condición
filt_studio <- ( (movies_data$Studio== "WB") | (movies_data$Studio== "Fox") | (movies_data$Studio== "Universal") | (movies_data$Studio== "Sony") )
filt_nota<-movies_data$MediaRating>6.4

# Que cumplan ambas condiciones
filtradas <- movies_data[filt_studio & filt_nota,]
filtradas





# Gráfico de caja y bigotes con puntos
p <- ggplot(data=movies_data, aes(x=Genre, y=GrossUS))
q<-p+
  geom_jitter(aes(size=Budgetmill,color=Genre))+
  geom_boxplot(alpha=0.7)
q


# De puntos con 4 dimensiones
p<-ggplot(data=movies_data,aes(x=MediaRating,y=GrossUS))
p+geom_point(size=3)
p+geom_point(aes(color=Genre,size=Budgetmill))


# De puntos en dos gráficos
p<-ggplot(data=movies_data,aes(x=MediaRating,y=Profit,colour=Genre))
p+geom_point(size=3)+facet_grid(~aprobada)









#Leer datos
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO")
Chicago<-read.csv("Chicago-F.csv",row.names=1)
NewYork<-read.csv("NewYork-F.csv",row.names=1)
Houston<-read.csv("Houston-F.csv",row.names=1)
SanFrancisco<-read.csv("SanFrancisco-F.csv",row.names=1)


# Primeros elementos
head(Chicago)
head(NewYork)
head(Houston)
head(SanFrancisco)
# Comprobar si es dataframe
is.data.frame(Chicago)
# Nombres columnas
names(Chicago)
colnames(Chicago)
# Valores diferentes columna
str(Chicago)







# Conversión matriz
Chicago<- as.matrix(Chicago)
NewYork<- as.matrix(NewYork)
Houston<- as.matrix(Houston)
SanFrancisco<- as.matrix(SanFrancisco)
# Ver si es matriz
is.matrix(Chicago)
head(Chicago)




# Lista con matrices
weather<-list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)
weather



# Acceso elemento
weather[1]
weather[[1]]
weather$Houston





#apply: Esta función se utiliza para aplicar una función a filas o columnas de matrices o data frames. Puedes especificar si deseas aplicar la función a filas (MARGIN = 1), columnas (MARGIN = 2) o a ambos (MARGIN = c(1,2)).
#lapply: Esta función aplica una función a cada elemento de una lista y devuelve una lista con los resultados. Es útil cuando deseas aplicar la misma operación a todos los elementos de una lista.
#sapply: Similar a lapply, pero intenta simplificar el resultado si es posible. En lugar de devolver una lista, intenta convertir el resultado en un vector o una matriz si es apropiado. Es útil cuando quieres obtener un resultado simplificado en lugar de una lista.








# Valor medio en Chicago
# Obtener valores de la primera fila
Chicago[1,] 
# Calcula media sobre los valores de la primera fila
apply(Chicago,1,mean) 
# Calcula media sobre los valores de este campo
mean(Chicago["DaysWithPrecip",]) 






# Mmedia de las filas
rowMeans(Chicago)
weather
# Media de las filas sobre todos los elementos de la matriz
lapply(weather,rowMeans)








# Otro tipo de operacion
lapply(weather,function(z) round((z[1,]-z[2,])/z[2,],2))
resultados<-sapply(weather,function(z) round((z[1,]-z[2,])/z[2,],2))
resultados
class(resultados)



# Convertir a dataframe
resultados<-as.data.frame(resultados)
resultados




# Trasponer dataframe
library(dplyr)
resultados <- resultados %>%
  mutate(mes = row.names(.))
resultados


# Eliminar índices
rownames(resultados) <- NULL
resultados




# Gráfico de puntos
p<-ggplot(data=resultados,aes(x=mes,y=Chicago))
p+geom_point()+
  labs(title = "Variacion diferencia temperatura", x = "Mes", y = "Valor")





# Coger el máximo
max_ciudades<-sapply(weather,"[",1,)
max_ciudades<-as.data.frame(max_ciudades)
max_ciudades


# Trasponer matriz
library(dplyr)
max_ciudades <- max_ciudades %>%
  mutate(Mes = row.names(.))
max_ciudades

# Eliminar índices
rownames(max_ciudades) <- NULL
max_ciudades




ciudades<-colnames(max_ciudades)
ciudades
# Eliminar último elemento
ciudades <- head(ciudades, -1)
ciudades

# Coger meses únicos
meses<-unique(max_ciudades$Mes)
meses

# Crear dataframe
dataframe_analisis<-data.frame(Ciudad=character(),Mes=character(),Maximo=numeric())


for (i in 1:nrow(max_ciudades)) {
  # Coger la fila
  fila<-max_ciudades[i,]
  
  # Coger mes con máxima temperatura en cada ciudad
  for(k in seq_along(fila)){
    if(k<5){
      elemento_anadir<-data.frame(Ciudad=ciudades[k],Mes=max_ciudades[i,5],
                                  Maximo=fila[[k]])
      dataframe_analisis <- rbind(dataframe_analisis, elemento_anadir)
    }
  }
}


dataframe_analisis

# Gráfico de puntos
p<-ggplot(data=dataframe_analisis,aes(x=Mes,y=Maximo))
p+geom_point(aes(color=Ciudad))


# Gráfico de líneas
ggplot(dataframe_analisis, aes(x = Mes, y = Maximo, color = Ciudad, group = Ciudad)) +
  geom_line() +
  labs(title = "Temperaturas por ciudadd", x = "Mes", y = "Temperatura") +
  scale_color_manual(values = c("SanFrancisco" = "red", "Chicago" = "blue",
                                "NewYork" = "green","Houston"="purple"))







library(datasets)
# Conjunto de datos de coches
head(mtcars)


# Contar por cilindros
cylinders <- table(mtcars$cyl) 
cylinders
barplot(cylinders)  


# Cargar paquete
if (!require("caret")) install.packages("caret")
library(caret)

# Datos
data(mtcars)

# Dividir datos
# Semilla
set.seed(123) 
indices <- createDataPartition(mtcars$mpg, p = 0.8, list = FALSE)
datos_entrenamiento <- mtcars[indices, ]
datos_prueba <- mtcars[-indices, ]

datos_entrenamiento

# Modelo de regresión lineal
modelo <- train(mpg ~ ., data = datos_entrenamiento, method = "lm")

# Resumen
print(modelo)

# Predecir valores
predicciones <- predict(modelo, newdata = datos_prueba)

# Calcular RMSE
rmse <- sqrt(mean((predicciones - datos_prueba$mpg)^2))
print(paste("RMSE:", rmse))








# Vector
v1 <- c(1, 2, 3, 4, 5)
is.vector(v1)
# Matriz
m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m2 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T)
# Array
# (rows, columns, tables)
a1 <- array(c( 1:24), c(4, 3, 2))

# Data frame
vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)
# Une en columnas->1acolumna: 1,2,3
dfa <- cbind(vNumeric, vCharacter, vLogical)
# Une en filas->1afila: 1,2,3
dfa1 <- rbind(vNumeric, vCharacter, vLogical)
df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df

# Lista
o1 <- c(1, 2, 3)
o2 <- c("a", "b", "c", "d")
o3 <- c(T, F, T, T, F)
list1 <- list(o1, o2, o3)
list2 <- list(o1, o2, o3, list1)  
list1


# Convertir a int
(coerce3 <- as.integer(5))
typeof(coerce3)
# Convertir a número
(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

# Matriz a dataframe
(coerce6 <- matrix(1:9, nrow= 3))
is.matrix(coerce6)

(coerce7 <- as.data.frame(matrix(1:9, nrow= 3)))
is.data.frame(coerce7)



# Operador dos puntos
x1 <- 0:10
x2 <- 10:0
(x1 <- 1:3)
(y  <- 1:9)

# Seq
(x3 <- seq(10))
(x4 <- seq(30, 0, by = -3))

# Scan
x6 <- scan()  
x6

# Rep
x7 <- rep(TRUE, 5)
x8 <- rep(c(TRUE, FALSE), 5)






# Combinar como columnas: primera fila 1,1
(df1 <- cbind.data.frame(x1, y))






# Como y es mas grande, se repite 1,2,3
x3  <- c(1:3)
df3 <- cbind.data.frame(x3, y)
(df3$x3 <- factor(df3$x3,
                  levels = c(1, 2, 3)))
df3


# Reemplaza número por etiqueta
x4  <- c(1:3)
df4 <- cbind.data.frame(x4, y)
df4$x4 <- factor(df4$x4,
                 levels = c(1, 2, 3),
                 labels = c("macOS", "Windows", "Linux"))
df4



# Igual pero pone las etiquetas ordenadas
x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
                   levels = c(3, 1, 2),
                   labels = c("No", "Maybe", "Yes")))
df5




library(datasets)  
?mtcars
head(mtcars)
# Seleccionar variables
cars <- mtcars[, c(1:4, 6:7, 9:11)] 
head(cars)

library(dplyr)
hc <- cars   %>%  
  dist   %>%  
  hclust      
plot(hc)          

# Añadir colores
rect.hclust(hc, k = 2, border = "gray")
rect.hclust(hc, k = 3, border = "blue")
rect.hclust(hc, k = 4, border = "green4")
rect.hclust(hc, k = 5, border = "darkred")




library(datasets)
# Cargar iris
?iris
head(iris)
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

# Poner gráficos en 3 filas y una columna
par(mfrow = c(3, 1))

hist(iris$Petal.Width [iris$Species == "setosa"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Setosa",
     xlab = "",
     col = "red")

hist(iris$Petal.Width [iris$Species == "versicolor"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Versicolor",
     xlab = "",
     col = "purple")

hist(iris$Petal.Width [iris$Species == "virginica"],
     xlim = c(0, 3),
     breaks = 9,
     main = "Petal Width for Virginica",
     xlab = "",
     col = "blue")

par(mfrow = c(1, 1))




library(datasets) 

# CSV
rio_csv <- read.csv("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\mbb.csv")
head(rio_csv)
# TXT
rio_txt <- read.csv("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\mbb.txt", sep = "\t")
head(rio_txt)
# Excel XLSX
library(readxl)
rio_xlsx <- read_excel("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\mbb.xlsx")
head(rio_xlsx)
# Ver datos cargados
View(rio_csv)
#tabla
r_txt1 <- read.table("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\mbb.txt", header = TRUE, 
                     sep = "\t")
r_txt1


# Dataframe
datos <- data.frame(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia"),
  stringsAsFactors = FALSE
)

# Mostrarlo
print("Dataframe original:")
print(datos)

# Insertar columna
datos$tipo_cliente <- ifelse(datos$ciudad == "Madrid" & datos$edad > 27, "cliente Madrid", "cliente externo")

# Mostrarlo
print("Dataframe con nueva columna añadida:")
print(datos)



library(tibble)
library(dplyr)

# Crear tibble
datos <- tibble(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia")
)

# Mostrarlo
print("Tibble original:")
print(datos)

# Insertar columna con condición
datos <- datos %>%
  mutate(tipo_cliente = if_else(ciudad == "Madrid" & edad > 27, "cliente Madrid", "cliente externo"))

# Mostrarlo
print("Tibble con nueva columna añadida:")
print(datos)



# Dataframe
datos <- data.frame(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia"),
  stringsAsFactors = FALSE
)

# Mostrarlo
print("Dataframe original:")
print(datos)

# Crear fila
nueva_fila <- c(ID = 5, nombre = "Luis", edad = 40, ciudad = "Málaga")

# Añadir la fila
datos <- rbind(datos, nueva_fila)

# Reiniciar ids
rownames(datos) <- NULL

# Mostrarlo
print("Dataframe con nueva fila añadida:")
print(datos)




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

# Distribucion normal, superpuesto anterior gráfico
curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)),
      col = "thistle4",  
      lwd = 2,           
      add = TRUE)        

# Curva de densidad estimada
lines(density(lynx), col = "blue", lwd = 2)
lines(density(lynx, adjust = 3), col = "purple", lwd = 2)

# Agrega pelos para ver la distribucion
rug(lynx, lwd = 2, col = "gray")


if (!require("pacman")) install.packages("pacman")

# Cargar paquete
library(pacman)

# Instalar paquetes
pacman::p_install("ggplot2", "dplyr", "readr")

# Cargar paquetes
pacman::p_load(ggplot2, dplyr, readr)



library(datasets)  

head(iris)
# Variable categórica--> de barras
plot(iris$Species) 
# Variable cuantitativa--> de puntos
plot(iris$Petal.Length)  
# Boxplot
plot(iris$Species, iris$Petal.Width) 
# De puntos
plot(iris$Petal.Length, iris$Petal.Width)  
# Con todas las parejas de variables
plot(iris)  

# Gráfico de puntos con plot
plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  
     pch = 19,         
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Length",
     ylab = "Petal Width")






install.packages("gapminder")
library(gapminder)
# Cargar datos
data("gapminder")

library(ggplot2)
library(plotly)

# Ggplot ejemplo con puntos
pp <-ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + 
  geom_jitter() + scale_x_log10()  
pp

# Grabar el gráfico
pp1 <- ggplotly(pp)
htmlwidgets::saveWidget(as_widget(pp1), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\mi_grafico.html") 

# Gráfico de puntos cambiando forma
pp2 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, shape = continent,  col = as.factor(year))) + 
  geom_jitter() + scale_x_log10()
pp2



# Crear gráfico con plotly
g <- plot_ly(gapminder, x = ~gdpPercap, y = ~lifeExp) 
g <- add_markers(g)
# Cambiar escala eje x
g <- layout(g, xaxis = list(type = "log"))
htmlwidgets::saveWidget(as_widget(g), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\plotly.html") 


# Crear gráfico con plotly con color por continente
g <- plot_ly(gapminder, x = ~gdpPercap, y = ~lifeExp, color = ~continent) 
g <- add_markers(g)
# Cambiar escala eje x
g <- layout(g, xaxis = list(type = "log"))
htmlwidgets::saveWidget(as_widget(g), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO\\plotly.html") 



library(datasets)  
?mtcars
head(mtcars)
# Seleccionar variables
cars <- mtcars[, c(1:4, 6:7, 9:11)]  
head(cars)

# Media a 0 y varianza 1
pc <- prcomp(cars,
             center = TRUE,  
             scale = TRUE)   

# Lo mismo pero con las variables
pc <- prcomp(~ mpg + cyl + disp + hp + wt + qsec + am +
               gear + carb, 
             data = mtcars, 
             center = TRUE,
             scale = TRUE)
# Resumen de los componentes
summary(pc)
# El primero es muy importante, tiene un valor alto
plot(pc)
# Muestra la rotación, similar a la correlación entre las variables
pc

library(dplyr)
# Para cada fila del conjunto, calcula el componente
predict(pc) %>% round(2)
# Muestra la dirección de cada variable
biplot(pc)
# Caballos y cilindros van dirigidos a coches más pesados



# Cargar datos
data(mtcars)

# Tamaño
n_row <- nrow(mtcars)
n_col <- ncol(mtcars)

# Inicializar lista
valores <- list()

# Recorrer filas
for (i in 1:n_row) {
  # Recorrer columna
  for (j in 1:n_col) {
    # Obtener valor fila y columna
    valor <- mtcars[i, j]
    # Guardar valor en la lista
    valores[[paste0("fila", i, "_columna", j)]] <- valor
  }
}

# mostrar valores
valores

#resultado como: $fila32_columna6 [1] 2.78



library(datasets)
install.packages("lars")
library(lars)

?USJudgeRatings
head(USJudgeRatings)
data <- USJudgeRatings

# Columnas menos la 12
x <- as.matrix(data[, -12])
# Columna 12
y <- data[, 12]

# Regresión
reg1 <- lm(y ~ x)

# Especificando variables
reg1 <- lm(RTEN ~ CONT + INTG + DMNR + DILG + CFMG +
             DECI + PREP + FAMI + ORAL + WRIT + PHYS,
           data = USJudgeRatings)

# Resultados
reg1           
summary(reg1)  

# Otros datos
anova(reg1)            
coef(reg1)             
confint(reg1)          
resid(reg1)            
hist(residuals(reg1))  

# Conventional stepwise regression
stepwise <- lars(x, y, type = "stepwise")

# Stagewise: stepwise pero con mejor generalización
forward <- lars(x, y, type = "forward.stagewise")

# LAR
lar <- lars(x, y, type = "lar")

# LASSO
lasso <- lars(x, y, type = "lasso")

# Comparar los r2, cogiéndolos y redondeando
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) %>% 
  round(2)
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp 



library(datasets)  

?mtcars
head(mtcars)

hist(mtcars$wt)
hist(mtcars$mpg)

# De puntos
plot(mtcars$wt, mtcars$mpg)

# Otro tipo de puntos
plot(mtcars$wt, mtcars$mpg,
     pch = 19,         
     cex = 1.5,        
     col = "#cc0000",  
     main = "MPG as a Function of Weight of Cars",
     xlab = "Weight (in 1000 pounds)",
     ylab = "MPG")



library(datasets)  

head(iris)

# Descripción de los datos
hist(iris$Petal.Length)
# Media, 1o quartil...
summary(iris$Petal.Length) 
# Cuenta cuántos de cada tipo
summary(iris$Species)  

# Solo con este tipo
hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal Length: Versicolor")

# Solo con estos pétalos
hist(iris$Petal.Length[iris$Petal.Length < 2],
     main = "Petal Length < 2")

# Con ambas condiciones
hist(iris$Petal.Length[iris$Species == "virginica" & 
                         iris$Petal.Length < 5.5],
     main = "Petal Length: Short Virginica")


# Subconjunto
i.setosa <- iris[iris$Species == "setosa", ]
# Ver sus datos
head(i.setosa)
summary(i.setosa$Petal.Length)
hist(i.setosa$Petal.Length)


library(dplyr)

# Datos
ventas <- data.frame(
  region = c("A", "A", "B", "B", "A", "B"),
  mes = c("enero", "febrero", "enero", "febrero", "enero", "febrero"),
  ventas = c(1000, 1500, 2000, 2500, 1200, 1800)
)

# Sumar ventas por región, utiliza sumarise
resumen_ventas <- ventas %>%
  group_by(region) %>%
  summarise(ventas_totales = sum(ventas))

print(resumen_ventas)
print(class(resumen_ventas))




library(tibble)

# Crear tibble
datos <- tibble(
  ID = c(1, 2, 3, 4),
  nombre = c("Juan", "María", "Pedro", "Ana"),
  edad = c(30, 25, 35, 28),
  ciudad = c("Madrid", "Barcelona", "Sevilla", "Valencia")
)

# Imprimirlo
print("Tibble original:")
print(datos)

# Transformar celdas que cumplen condición
datos <- datos %>%
  mutate(edad = ifelse(edad > 30, edad + 1, edad))

# Mostrarlo
print("Tibble con transformación aplicada:")
print(datos)

# Añadir fila
nueva_fila <- tibble(
  ID = 5,
  nombre = "Luis",
  edad = 29,
  ciudad = "Málaga"
)

# Insertar fila
datos <- bind_rows(datos, nueva_fila)

# Mostrar
print("Tibble con nueva fila añadida:")
print(datos)



library(tidyr)

# Datos
temperaturas <- data.frame(
  dia_semana = c("lunes", "martes", "miércoles"),
  hora_0 = c(20, 22, 19),
  hora_1 = c(18, 21, 20),
  hora_2 = c(17, 20, 19)
)
temperaturas

# Una fila por dia, hora y temperatura
temperaturas_reorganizadas <- temperaturas %>%
  pivot_longer(cols = starts_with("hora"), 
               names_to = "hora",
               values_to = "temperatura")

print(temperaturas_reorganizadas)#lunes      hora_0          20




# tidyverse
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Cargar conjunto de datos
data(mtcars)
mtcars
# Dimensión
print(dim(mtcars))
# Columnas
print(length(mtcars))
# Filas
print(nrow(mtcars))
# Columnas
print(ncol(mtcars))

# Filtrar solo los que tienen más de 150 caballos
mtcars_filtrado <- mtcars %>%
  filter(hp > 150)

# Dimensión
print(dim(mtcars_filtrado))

# Resumen de los datos, agrupándolos
resumen_estadistico <- mtcars_filtrado %>%
  summarise(media_hp = mean(hp),
            mediana_mpg = median(mpg),
            maximo_disp = max(disp))

# Ver relación entre hp y mpg
grafico <- ggplot(mtcars_filtrado, aes(x = hp, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm") +  
  labs(x = "Caballos de Fuerza", y = "Millas por Galón", title = "Relación entre HP y MPG")

# Ver resumen y gráfico
print(resumen_estadistico)
print(grafico)







#El conjunto de datos Telco-Customer-Churn contiene información sobre clientes de una empresa de telecomunicaciones. Cada fila representa un cliente y cada columna contiene un atributo sobre el cliente. A continuación, se describen brevemente los atributos:

#customerID: Identificación única del cliente.
#gender: Género del cliente (Male/Female).
#SeniorCitizen: Indica si el cliente es una persona mayor (1: Sí, 0: No).
#Partner: Indica si el cliente tiene pareja (Yes/No).
#Dependents: Indica si el cliente tiene dependientes (Yes/No).
#tenure: Número de meses que el cliente ha estado con la empresa.
#PhoneService: Indica si el cliente tiene servicio telefónico (Yes/No).
#MultipleLines: Indica si el cliente tiene múltiples líneas telefónicas (Yes/No/No phone service).
#InternetService: Tipo de servicio de Internet del cliente (DSL/Fiber optic/No).
#OnlineSecurity: Indica si el cliente tiene seguridad en línea (Yes/No/No internet service).
#OnlineBackup: Indica si el cliente tiene respaldo en línea (Yes/No/No internet service).
#DeviceProtection: Indica si el cliente tiene protección de dispositivo (Yes/No/No internet service).
#TechSupport: Indica si el cliente tiene soporte técnico (Yes/No/No internet service).
#StreamingTV: Indica si el cliente tiene streaming de TV (Yes/No/No internet service).
#StreamingMovies: Indica si el cliente tiene streaming de películas (Yes/No/No internet service).
#Contract: Tipo de contrato del cliente (Month-to-month/One year/Two year).
#PaperlessBilling: Indica si el cliente tiene facturación sin papel (Yes/No).
#PaymentMethod: Método de pago del cliente (Electronic check/Mailed check/Bank transfer (automatic)/Credit card (automatic)).
#MonthlyCharges: Cargos mensuales que paga el cliente.
#TotalCharges: Cargos totales acumulados que ha pagado el cliente.
#Churn: Indica si el cliente ha dejado la empresa (Yes/No).







# Telco análisis
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\AAALIMPIO")


# Cargar librerías necesarias
library(tidyverse)
library(readr)

# Cargar los datos
telco_data <- read_csv("telco-customer-churn.csv")

# Resumen y descripción de los datos
summary(telco_data)
str(telco_data)


# Contar el número de filas con valores nulos
total_na_rows <- sum(!complete.cases(telco_data))
cat("Número total de filas con valores nulos:", total_na_rows, "\n")

# Contar el número de valores nulos por atributo
na_count <- colSums(is.na(telco_data))
cat("Número de valores nulos por atributo:\n")
print(na_count)

# Manejar valores nulos
# Reemplazar nulos en 'gender' con la moda
mode_gender <- as.character(names(sort(table(telco_data$gender), decreasing = TRUE)[1]))
telco_data$gender[is.na(telco_data$gender)] <- mode_gender

# Reemplazar nulos en 'MonthlyCharges' con la media
mean_monthly_charges <- mean(telco_data$MonthlyCharges, na.rm = TRUE)
telco_data$MonthlyCharges[is.na(telco_data$MonthlyCharges)] <- mean_monthly_charges

# Eliminar filas con nulos en 'Contract'
telco_data <- telco_data[!is.na(telco_data$Contract), ]

# Eliminar filas con nulos en 'TotalCharges'
telco_data <- telco_data[!is.na(telco_data$TotalCharges), ]

# Contar el número de filas con valores nulos
total_na_rows <- sum(!complete.cases(telco_data))
cat("Número total de filas con valores nulos:", total_na_rows, "\n")


# Gráfico de caja y bigotes de TotalCharges separado por Contract
ggplot(telco_data, aes(x = Contract, y = TotalCharges)) +
  geom_boxplot() +
  labs(title = "Boxplot of TotalCharges by Contract",
       x = "Contract",
       y = "TotalCharges")

# Gráfico de caja y bigotes de TotalCharges con facet grid por Contract
ggplot(telco_data, aes(x = "", y = TotalCharges)) +
  geom_boxplot() +
  facet_grid(. ~ Contract) +
  labs(title = "Boxplot of TotalCharges Faceted by Contract",
       x = "",
       y = "TotalCharges")


library(tidyverse)



# Crear un gráfico de barras de InternetService y el acumulado de MonthlyCharges
ggplot(telco_data, aes(x = InternetService, y = MonthlyCharges, fill = InternetService)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Monthly Charges by Internet Service",
       x = "Internet Service",
       y = "Total Monthly Charges") +
  theme_minimal()


# Calcular el valor medio de MonthlyCharges por Contract
mean_monthly_charges_by_contract <- telco_data %>%
  group_by(Contract) %>%
  summarise(mean_monthly_charges = mean(MonthlyCharges, na.rm = TRUE))

# Mostrar la tabla
print(mean_monthly_charges_by_contract)


# Contar clientes por InternetService y OnlineSecurity
count_by_internet_and_security <- telco_data %>%
  group_by(InternetService, OnlineSecurity) %>%
  summarise(count = n())

# Mostrar la tabla
print(count_by_internet_and_security)

# Histograma de ternure
ggplot(telco_data, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Tenure",
       x = "Tenure (months)",
       y = "Number of Customers") +
  theme_minimal()


# Gráfico de barras de payment method
ggplot(telco_data, aes(x = PaymentMethod, fill = PaymentMethod)) +
  geom_bar() +
  labs(title = "Payment Method Distribution",
       x = "Payment Method",
       y = "Number of Customers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de Densidad de MonthlyCharges por Churn
ggplot(telco_data, aes(x = MonthlyCharges, fill = Churn)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Monthly Charges by Churn",
       x = "Monthly Charges",
       y = "Density") +
  theme_minimal()


# Gráfico de Barras Apiladas de Contract y Churn
ggplot(telco_data, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Contract Type",
       x = "Contract Type",
       y = "Proportion of Customers") +
  theme_minimal()


# Scatter Plot de TotalCharges vs tenure coloreado por Churn
ggplot(telco_data, aes(x = tenure, y = TotalCharges, color = Churn)) +
  geom_point(alpha = 0.6) +
  labs(title = "Total Charges vs Tenure by Churn",
       x = "Tenure (months)",
       y = "Total Charges") +
  theme_minimal()



library(reshape2)
install.packages("ggcorrplot")
library(ggcorrplot)
library(dplyr)

# Seleccionar solo las columnas numéricas
numeric_data <- telco_data %>% select(tenure, MonthlyCharges, TotalCharges)

# Calcular la matriz de correlación
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Crear el heatmap
ggcorrplot(cor_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3,
           title = "Correlation Matrix of Numeric Variables", 
           ggtheme = theme_minimal())


# Gráfico de Barras de SeniorCitizen y Churn
ggplot(telco_data, aes(x = factor(SeniorCitizen), fill = Churn)) +
  geom_bar(position = "fill") +
  labs(title = "Churn Rate by Senior Citizen Status",
       x = "Senior Citizen",
       y = "Proportion of Customers",
       fill = "Churn") +
  scale_x_discrete(labels = c("No", "Yes")) +
  theme_minimal()

install.packages("GGally")
library(GGally)
library(dplyr)

# Seleccionar las variables numéricas y la variable Churn
numeric_data_with_churn <- telco_data %>%
  select(tenure, MonthlyCharges, TotalCharges, Churn)

# Crear el pairplot
ggpairs(numeric_data_with_churn, aes(color = Churn, alpha = 0.5),
        title = "Pairplot of Numeric Variables by Churn",
        columns = 1:3) +
  theme_minimal()


# Calcular el porcentaje de clientes por cada tipo de cliente, método de pago y servicio de Internet
percentage_by_group <- telco_data %>%
  group_by(SeniorCitizen, PaymentMethod, InternetService) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Mostrar la tabla con los porcentajes
print(percentage_by_group)


# Calcular el porcentaje de clientes que Churned (dejaron la empresa)
churn_percentage <- telco_data %>%
  summarise(churn_rate = mean(Churn == "Yes") * 100)

# Mostrar el resultado
print(churn_percentage)


# Comparar cargos mensuales promedio por tipo de contrato
monthly_charges_summary <- telco_data %>%
  group_by(Contract) %>%
  summarise(mean_monthly_charges = mean(MonthlyCharges, na.rm = TRUE))

# Mostrar el resultado
print(monthly_charges_summary)


# Contar clientes por tipo de InternetService y OnlineSecurity
count_by_internet_security <- telco_data %>%
  filter(!is.na(InternetService) & !is.na(OnlineSecurity)) %>%
  count(InternetService, OnlineSecurity)

# Mostrar el resultado
print(count_by_internet_security)



# Calcular el número total de clientes
total_clients <- nrow(telco_data)
total_clients

# Calcular el porcentaje de clientes por género y contrato
percentage_by_gender_contract <- telco_data %>%
  group_by(gender, Contract) %>%
  summarise(count = n()) %>%
  mutate(percentage = (count / total_clients) * 100)

# Mostrar el resultado
print(percentage_by_gender_contract)





# Crear los intervalos (bins) para TotalCharges
breaks <- seq(0, max(telco_data$TotalCharges, na.rm = TRUE), by = 500)
totalcharges_bins <- cut(telco_data$TotalCharges, breaks = breaks, include.lowest = TRUE)

# Crear la tabla de frecuencia
totalcharges_freq_table <- table(totalcharges_bins)

# Convertir la tabla de frecuencia a un data frame para una visualización más fácil
totalcharges_freq_df <- as.data.frame(totalcharges_freq_table)
names(totalcharges_freq_df) <- c("Interval", "Frequency")

# Imprimir la tabla de frecuencia
print(totalcharges_freq_df)

# Crear un histograma de TotalCharges
ggplot(telco_data, aes(x = TotalCharges)) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de TotalCharges", x = "TotalCharges", y = "Frecuencia") +
  theme_minimal()


# Filtrar por contrato "Month-to-month" y servicio de Internet "Fiber optic"
filtered_data <- telco_data %>%
  filter(Contract == "Month-to-month", InternetService == "Fiber optic")

filtered_data

# Encontrar el customerID con el mayor TotalCharges
customer_max_totalcharges <- filtered_data %>%
  top_n(1, TotalCharges) %>%
  select(customerID)

# Imprimir el customerID
print(customer_max_totalcharges)











# Instalar las bibliotecas necesarias
install.packages(c("tidyverse", "caret", "e1071", "randomForest", "class", "rpart", "rpart.plot"))

# Cargar las bibliotecas
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)

# Cargar los datos
telco_data <- read_csv("telco-customer-churn.csv")

# Convertir las variables categóricas en factores
telco_data <- telco_data %>%
  mutate(
    gender = as.factor(gender),
    SeniorCitizen = as.factor(SeniorCitizen),
    Partner = as.factor(Partner),
    Dependents = as.factor(Dependents),
    PhoneService = as.factor(PhoneService),
    MultipleLines = as.factor(MultipleLines),
    InternetService = as.factor(InternetService),
    OnlineSecurity = as.factor(OnlineSecurity),
    OnlineBackup = as.factor(OnlineBackup),
    DeviceProtection = as.factor(DeviceProtection),
    TechSupport = as.factor(TechSupport),
    StreamingTV = as.factor(StreamingTV),
    StreamingMovies = as.factor(StreamingMovies),
    Contract = as.factor(Contract),
    PaperlessBilling = as.factor(PaperlessBilling),
    PaymentMethod = as.factor(PaymentMethod),
    Churn = as.factor(Churn)
  )













# Manejar valores nulos
# Eliminar filas con valores nulos en Contract
telco_data <- telco_data %>%
  filter(!is.na(Contract))

# Completar valores nulos en gender con la moda
telco_data$gender[is.na(telco_data$gender)] <- telco_data %>%
  count(gender) %>%
  arrange(desc(n)) %>%
  pull(gender) %>%
  .[1]

# Completar valores nulos en MonthlyCharges con la media
telco_data$MonthlyCharges[is.na(telco_data$MonthlyCharges)] <- mean(telco_data$MonthlyCharges, na.rm = TRUE) 


# Completar valores nulos en TotalCharges con la media
telco_data$TotalCharges[is.na(telco_data$TotalCharges)] <- mean(telco_data$TotalCharges, na.rm = TRUE) 

# Contar el número de filas con valores nulos
total_na_rows <- sum(!complete.cases(telco_data))
cat("Número total de filas con valores nulos:", total_na_rows, "\n")

head(telco_data)
# Eliminar columna
telco_data <- telco_data %>% select(-customerID)
names(telco_data)












# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad
train_index <- createDataPartition(telco_data$Churn, p = 0.7, list = FALSE)
train_data <- telco_data[train_index, ]
test_data <- telco_data[-train_index, ]












# Escalar los datos de entrenamiento
standard_scaler_train <- preProcess(train_data[, c("MonthlyCharges", "TotalCharges")], method = c("center", "scale"))
train_data_standard_scaled <- train_data
train_data_standard_scaled[, c("MonthlyCharges", "TotalCharges")] <- predict(standard_scaler_train, train_data[, c("MonthlyCharges", "TotalCharges")])
train_data_standard_scaled$MonthlyCharges

# Aplicar el escalador al conjunto de prueba utilizando los parámetros del conjunto de entrenamiento
test_data_standard_scaled <- test_data
test_data_standard_scaled[, c("MonthlyCharges", "TotalCharges")] <- predict(standard_scaler_train, test_data[, c("MonthlyCharges", "TotalCharges")])
test_data_standard_scaled$MonthlyCharges

# Escalar los datos de entrenamiento con MinMaxScaler
minmax_scaler_train <- preProcess(train_data[, c("MonthlyCharges", "TotalCharges")], method = c("range"))
train_data_minmax_scaled <- train_data
train_data_minmax_scaled[, c("MonthlyCharges", "TotalCharges")] <- predict(minmax_scaler_train, train_data[, c("MonthlyCharges", "TotalCharges")])
train_data_minmax_scaled$MonthlyCharges

# Aplicar el escalador min-max al conjunto de prueba
test_data_minmax_scaled <- test_data
test_data_minmax_scaled[, c("MonthlyCharges", "TotalCharges")] <- predict(minmax_scaler_train, test_data[, c("MonthlyCharges", "TotalCharges")])
test_data_minmax_scaled$MonthlyCharges









# Entrenar el árbol de decisión
decision_tree_model <- rpart(Churn ~ ., data = train_data_standard_scaled, method = "class")

# Predecir en el conjunto de prueba
dt_predictions <- predict(decision_tree_model, test_data_standard_scaled, type = "class")

# Evaluar el modelo
conf_matrix_tree <- confusionMatrix(dt_predictions, test_data$Churn)
conf_matrix_tree

accuracy_tree <- conf_matrix_tree$overall['Accuracy']
accuracy_tree
precision_tree <- conf_matrix_tree$byClass['Pos Pred Value']
recall_tree <- conf_matrix_tree$byClass['Sensitivity']
f1_score_tree <- 2 * (precision_tree * recall_tree) / (precision_tree + recall_tree)











# Entrenar el random forest
random_forest_model <- randomForest(Churn ~ ., data = train_data_minmax_scaled, ntree = 100)

# Predecir en el conjunto de prueba
rf_predictions <- predict(random_forest_model, test_data_minmax_scaled)

# Evaluar el modelo
conf_matrix_rf<-confusionMatrix(rf_predictions, test_data$Churn)

# Obtener Accuracy, Precision, Recall, F1 Score para Random Forest
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
precision_rf <- conf_matrix_rf$byClass['Pos Pred Value']
recall_rf <- conf_matrix_rf$byClass['Sensitivity']
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)












# Entrenar la regresión logística
logistic_model <- glm(Churn ~ ., data = train_data_minmax_scaled, family = binomial)

# Predecir en el conjunto de prueba
logistic_predictions <- predict(logistic_model, test_data_minmax_scaled, type = "response")
logistic_predictions_class <- ifelse(logistic_predictions > 0.5, "Yes", "No")

# Evaluar el modelo
conf_matrix_log<-confusionMatrix(as.factor(logistic_predictions_class), test_data$Churn)


# Obtener Accuracy, Precision, Recall, F1 Score para Regresión Logística
accuracy_log <- conf_matrix_log$overall['Accuracy']
precision_log <- conf_matrix_log$byClass['Pos Pred Value']
recall_log <- conf_matrix_log$byClass['Sensitivity']
f1_score_log <- 2 * (precision_log * recall_log) / (precision_log + recall_log)








# Entrenar la SVM
svm_model <- svm(Churn ~ ., data = train_data_standard_scaled, probability = TRUE)

# Predecir en el conjunto de prueba
svm_predictions <- predict(svm_model, test_data_standard_scaled, probability = TRUE)

# Evaluar el modelo
conf_matrix_svm<-confusionMatrix(svm_predictions, test_data$Churn)

# Obtener Accuracy, Precision, Recall, F1 Score para SVM
accuracy_svm <- conf_matrix_svm$overall['Accuracy']
precision_svm <- conf_matrix_svm$byClass['Pos Pred Value']
recall_svm <- conf_matrix_svm$byClass['Sensitivity']
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)

















# Convertir variables categóricas a numéricas usando model.matrix
train_data_numeric <- model.matrix(Churn ~ . - 1, data = train_data_standard_scaled)
test_data_numeric <- model.matrix(Churn ~ . - 1, data = test_data_standard_scaled)

# Extraer la variable objetivo
train_labels <- train_data_standard_scaled$Churn
test_labels <- test_data_standard_scaled$Churn

# Ejecutar KNN
knn_predictions_standard <- knn(
  train = train_data_numeric,
  test = test_data_numeric,
  cl = train_labels,
  k = 5
)

# Evaluar el modelo
conf_matrix_knn<-confusionMatrix(knn_predictions_standard, test_labels)

# Obtener Accuracy, Precision, Recall, F1 Score para KNN
accuracy_knn <- conf_matrix_knn$overall['Accuracy']
precision_knn <- conf_matrix_knn$byClass['Pos Pred Value']
recall_knn <- conf_matrix_knn$byClass['Sensitivity']
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)


# Crear una tabla con las métricas
metrics <- tibble(
  Model = c("Decision Tree", "Random Forest", "Logistic Regression", "SVM", "KNN"),
  Accuracy = c(accuracy_tree, accuracy_rf, accuracy_log, accuracy_svm, accuracy_knn),
  Precision = c(precision_tree, precision_rf, precision_log, precision_svm, precision_knn),
  Recall = c(recall_tree, recall_rf, recall_log, recall_svm, recall_knn),
  F1_Score = c(f1_score_tree, f1_score_rf, f1_score_log, f1_score_svm, f1_score_knn)
)

print(metrics)


# Mostrar la matriz de confusión para cada modelo
print("Matriz de Confusión Árbol de Decisión")
print(conf_matrix_tree$table)

print("Matriz de Confusión Random Forest")
print(conf_matrix_rf$table)

print("Matriz de Confusión Regresión Logística")
print(conf_matrix_log$table)

print("Matriz de Confusión SVM")
print(conf_matrix_svm$table)

print("Matriz de Confusión KNN")
print(conf_matrix_knn$table)












# Configuración de control de entrenamiento con validación cruzada de 10 pliegues
control <- trainControl(method = "cv", number = 10)

# Definir la rejilla de hiperparámetros para la búsqueda
tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8, 10),
  splitrule = "gini",
  min.node.size = c(1, 5, 10)
)

# Entrenar el modelo Random Forest con validación cruzada y búsqueda de hiperparámetros
set.seed(123)
rf_model <- train(
  Churn ~ ., 
  data = train_data_standard_scaled,
  method = "ranger",
  trControl = control,
  tuneGrid = tune_grid,
  metric = "Accuracy"
)


# Ver los mejores hiperparámetros
best_params <- rf_model$bestTune
print(best_params)

# Predecir en el conjunto de prueba
rf_predictions <- predict(rf_model, test_data)

# Evaluación
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$Churn)
print(conf_matrix_rf)









# Cargar el paquete MASS para acceder al conjunto de datos
library(MASS)

# Cargar el conjunto de datos de Diabetes de Pima Indians
data(Pima.tr)

# Ver la estructura del conjunto de datos
str(Pima.tr)

# Entrenar una regresión lineal múltiple para predecir Glucose
lm_model <- lm(glu ~ ., data = Pima.tr)

# Ver el resumen del modelo
summary(lm_model)

# Graficar los residuos
plot(lm_model, which = 1)









# Cargar el paquete para árboles de decisión
library(rpart)

# Entrenar un árbol de decisión de regresión
tree_model <- rpart(glu ~ ., data = Pima.tr)

# Ver el árbol creado
print(tree_model)

# Graficar el árbol
plot(tree_model)
text(tree_model, pretty = 0.5)









# Cargar el paquete para bosques aleatorios
library(randomForest)

# Entrenar un bosque aleatorio de regresión
rf_model <- randomForest(glu ~ ., data = Pima.tr)

# Ver el resumen del modelo
print(rf_model)

# Graficar la importancia de las variables
varImpPlot(rf_model)







install.packages(c("glmnet", "Metrics"))
library(glmnet)
library(Metrics)


# Ajustar un modelo Ridge
ridge_model <- glmnet(as.matrix(Pima.tr[, -ncol(Pima.tr)]), Pima.tr$glu, alpha = 0)

# Calcular predicciones del modelo
ridge_pred <- predict(ridge_model, newx = as.matrix(Pima.tr[, -ncol(Pima.tr)]))

# Calcular MSE y R²
ridge_mse <- mse(Pima.tr$glu, ridge_pred)
# Calcular R² para Ridge
ridge_r2 <- 1 - sum((Pima.tr$glu - ridge_pred)^2) / sum((Pima.tr$glu - mean(Pima.tr$glu))^2)

# Mostrar resultados
cat("Ridge Regression:\n")
cat("MSE:", ridge_mse, "\n")
cat("R²:", ridge_r2, "\n")







# Ajustar un modelo Lasso
lasso_model <- glmnet(as.matrix(Pima.tr[, -ncol(Pima.tr)]), Pima.tr$glu, alpha = 1)

# Calcular predicciones del modelo
lasso_pred <- predict(lasso_model, newx = as.matrix(Pima.tr[, -ncol(Pima.tr)]))

# Calcular MSE y R²
lasso_mse <- mse(Pima.tr$glu, lasso_pred)
lasso_r2 <- 1 - sum((Pima.tr$glu - lasso_pred)^2) / sum((Pima.tr$glu - mean(Pima.tr$glu))^2)

# Mostrar resultados
cat("\nLasso Regression:\n")
cat("MSE:", lasso_mse, "\n")
cat("R²:", lasso_r2, "\n")

















