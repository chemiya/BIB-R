

#leemos datos
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\Weather_data")
Chicago<-read.csv("Chicago-F.csv",row.names=1)
NewYork<-read.csv("NewYork-F.csv",row.names=1)
Houston<-read.csv("Houston-F.csv",row.names=1)
SanFrancisco<-read.csv("SanFrancisco-F.csv",row.names=1)


#mostramos primeros elementos
head(Chicago)
head(NewYork)
head(Houston)
head(SanFrancisco)
#comprobar si es dataframe
is.data.frame(Chicago)
#nombres columnas
names(Chicago)
colnames(Chicago)
#valores diferentes columna
str(Chicago)







#conversion matriz
Chicago<- as.matrix(Chicago)
NewYork<- as.matrix(NewYork)
Houston<- as.matrix(Houston)
SanFrancisco<- as.matrix(SanFrancisco)
#ver si es matriz
is.matrix(Chicago)
head(Chicago)




#lista con matrices
weather<-list(Chicago=Chicago,NewYork=NewYork,Houston=Houston,SanFrancisco=SanFrancisco)
weather



#acceso elemento
weather[1]
weather[[1]]
weather$Houston









#valor medio en Chicago
Chicago[1,] #obtener valores de la primera fila
apply(Chicago,1,mean) #calcula media sobre los valores de la primera fila
mean(Chicago["DaysWithPrecip",]) #calcula media sobre los valores de este campo






#media de las filas
rowMeans(Chicago)
weather
#media de las filas sobre todos los elementos de la matriz
lapply(weather,rowMeans)








#otro tipo de operacion
lapply(weather,function(z) round((z[1,]-z[2,])/z[2,],2))
resultados<-sapply(weather,function(z) round((z[1,]-z[2,])/z[2,],2))
resultados
class(resultados)



#convertimos a dataframe
resultados<-as.data.frame(resultados)
resultados




#trasponemos dataframe
library(dplyr)
resultados <- resultados %>%
  mutate(mes = row.names(.))
resultados


#eliminamos indices
rownames(resultados) <- NULL
resultados




#grafico de puntos
p<-ggplot(data=resultados,aes(x=mes,y=Chicago))
p+geom_point()+
  labs(title = "Variacion diferencia temperatura", x = "Mes", y = "Valor")





#cogemos el maximo
max_ciudades<-sapply(weather,"[",1,)
max_ciudades<-as.data.frame(max_ciudades)
max_ciudades


#trasponer matriz
library(dplyr)
max_ciudades <- max_ciudades %>%
  mutate(Mes = row.names(.))
max_ciudades

#eliminamos indices
rownames(max_ciudades) <- NULL
max_ciudades




ciudades<-colnames(max_ciudades)
ciudades
#eliminamos ultimo elemento
ciudades <- head(ciudades, -1)
ciudades

#cogemos meses unicos
meses<-unique(max_ciudades$Mes)
meses

#creamos dataframe
dataframe_analisis<-data.frame(Ciudad=character(),Mes=character(),Maximo=numeric())


for (i in 1:nrow(max_ciudades)) {
  fila<-max_ciudades[i,]#cogemos la fila
  
  for(k in seq_along(fila)){
    if(k<5){
      elemento_anadir<-data.frame(Ciudad=ciudades[k],Mes=max_ciudades[i,5],
                                  Maximo=fila[[k]])
      dataframe_analisis <- rbind(dataframe_analisis, elemento_anadir)
    }
  }
}


dataframe_analisis

#grafico de puntos
p<-ggplot(data=dataframe_analisis,aes(x=Mes,y=Maximo))
p+geom_point(aes(color=Ciudad))


#grafico de lineas
ggplot(dataframe_analisis, aes(x = Mes, y = Maximo, color = Ciudad, group = Ciudad)) +
  geom_line() +
  labs(title = "Temperaturas por ciudadd", x = "Mes", y = "Temperatura") +
  scale_color_manual(values = c("SanFrancisco" = "red", "Chicago" = "blue",
                                "NewYork" = "green","Houston"="purple"))
