



#carga de datos
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R")
movies_data <- read.csv("movies-data.csv",header = TRUE)





#comprobar datos cargados
head(movies_data)#primeros elementos 
summary(movies_data) #resumen vairables
str(movies_data) #valores diferentes de las variables
names(movies_data) #nombres columnas
nombres_datos<-names(movies_data)
print(class(nombres_datos)) #imprimir tipo variable
nrow(movies_data) #numero filas
ncol(movies_data) #numero columnas





#corregimos nombres de las columnas eliminando puntos
nombres_datos <- gsub("\\.", "", nombres_datos)
nombres_datos
names(movies_data)<-nombres_datos
str(movies_data)



#convertimos tipo de datos de columnas erroneas y revisamos si hay na
#tipo de una columna
print(class(movies_data$Profitmill))
#convertimos columna a numerica
movies_data$Profitmill<- as.numeric(movies_data$Profitmill)

#contamos nulos e imprimirmos en pantalla
num_na <- sum(is.na(movies_data$Profitmill))
print(paste("NA en Profitmill:", num_na))



#media de la columna con 2 decimales
media_valor <- round(mean(movies_data$Profitmill, na.rm = TRUE),2)
print(paste("Media de la columna Profitmill si na:", media_valor))
#si no eliminamos na, sale na
media_valor <- round(mean(movies_data$Profitmill),2)
print(paste("Media de la columna Profitmill con na:", media_valor))


#mostrar tipo de una columna
print(class(movies_data$AdjustedGrossmill))
#convertir a numerica
movies_data$AdjustedGrossmill<- as.numeric(movies_data$AdjustedGrossmill)
#contamos nulos
num_na <- sum(is.na(movies_data$AdjustedGrossmill))
print(paste("NA en AdjustedGrossmill:", num_na))

#lo mismo
print(class(movies_data$Grossmill))
movies_data$Grossmill<- as.numeric(movies_data$Grossmill)
num_na <- sum(is.na(movies_data$Grossmill))
print(paste("NA en Grossmill:", num_na))

# lo mismo
print(class(movies_data$Overseasmill))
movies_data$Overseasmill<- as.numeric(movies_data$Overseasmill)
num_na <- sum(is.na(movies_data$Overseasmill))
print(paste("NA en Overseasmill:", num_na))

#contamos filas con al menos algun valor faltante
nrow(movies_data[!complete.cases(movies_data),])
#nos quedamos solo con filas completas
movies_data<-movies_data[complete.cases(movies_data),]
nrow(movies_data)






#convertimos a factor una columna
movies_data$DayofWeek<-factor(movies_data$DayofWeek)
#vemos su valores
levels(movies_data$DayofWeek)
movies_data$Genre<-factor(movies_data$Genre)
levels(movies_data$Genre)
str(movies_data)





#vemos distribuciones dias, cuantos hay de cada elemento
distribucion_dia <- table(movies_data$DayofWeek)
print(distribucion_dia)




#grafico dias de estreno con etiquetas
library(ggplot2)

p<-ggplot(data=movies_data, aes(x=DayofWeek)) 
q<-p+ geom_bar()
q <- q +
  xlab("Dia de estreno") + 
  ylab("Cuenta") + 
  ggtitle("Distribución peliculas dia estreno") 

q<- q+ theme(plot.title = element_text(hjust = 0.5))
q





#estudios con mas peliculas, contamos y ordenamos
distribucion_studio <- table(movies_data$Studio)
distribucion_studio <- sort(distribucion_studio, decreasing = TRUE)
print(distribucion_studio)



#media de la columna de beneficios
mean(movies_data$Profit)


#media de beneficios por studio
library(dplyr)
movies_data_beneficios <- movies_data %>%
  group_by(Studio) %>%
  summarise(Media_Beneficios = mean(Profit)) %>%
  arrange(desc(Media_Beneficios))

#convertimos a dataframe y mostramos
print(class(movies_data_beneficios))
movies_data_beneficios<- as.data.frame(movies_data_beneficios)
movies_data_beneficios



#studio con mayor media
studio_mas_beneficios<-movies_data_beneficios[1,"Studio"]
print(paste("Studio con mayor media beneficios:", studio_mas_beneficios))






#pelicula con mayor beneficio
fila_max <- movies_data[which.max(movies_data$Profit), ]
print(fila_max$MovieTitle)


#pelicula con mejor puntuacion
fila_max <- movies_data[which.max(movies_data$IMDbRating), ]
print(fila_max$MovieTitle)


#nueva columna con media puntuaciones
movies_data$MediaRating<-round((movies_data$IMDbRating+movies_data$MovieLensRating)/2,2)
head(movies_data)

#eliminamos columna
movies_data$MovieLensRating<-NULL
head(movies_data)



#añadir columna con valores segun condicion
movies_data <- movies_data %>%mutate(aprobada = ifelse(MediaRating >5, "Aprobada", "No Aprobada"))
head(movies_data)


#contamos aprobadas y las que no
distribucion_aprobadas <- table(movies_data$aprobada)
print(distribucion_aprobadas)

#porcentaje de aprobadas
distribucion_aprobadas<-(distribucion_aprobadas/nrow(movies_data))*100
print(distribucion_aprobadas)


#tabla de frecuencias con las notas
intervalos <- seq(0, 10, by = 0.5)  
movies_data$IntervaloNota <- cut(movies_data$MediaRating, breaks = intervalos, 
                                 include.lowest = TRUE)
tabla_frecuencias <- table(movies_data$IntervaloNota)
print(tabla_frecuencias)
print(class(tabla_frecuencias))
tabla_frecuencias<-as.data.frame(tabla_frecuencias)
tabla_frecuencias




#grafico rating, profit y genero
qplot(data=movies_data, x=MediaRating, y=Profit,
      colour=Genre,                               
      size=I(5), 
      alpha=I(0.6),                               
      main="MediaRating vs Profit segun genero" 
)




#filtramos las que cumplen condicion
filt_studio <- ( (movies_data$Studio== "WB") | (movies_data$Studio== "Fox") | (movies_data$Studio== "Universal") | (movies_data$Studio== "Sony") )
filt_nota<-movies_data$MediaRating>6.4

#que cumplan ambas condiciones
filtradas <- movies_data[filt_studio & filt_nota,]
filtradas





#grafico de caja y bigotes con puntos
p <- ggplot(data=movies_data, aes(x=Genre, y=GrossUS))
q<-p+
  geom_jitter(aes(size=Budgetmill,color=Genre))+
  geom_boxplot(alpha=0.7)
q


# de puntos con 4 dimensiones
p<-ggplot(data=movies_data,aes(x=MediaRating,y=GrossUS))
p+geom_point(size=3)
p+geom_point(aes(color=Genre,size=Budgetmill))


# de puntos en dos graficos
p<-ggplot(data=movies_data,aes(x=MediaRating,y=Profit,colour=Genre))
p+geom_point(size=3)+facet_grid(~aprobada)



