
#cargamos datos
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\original curso")
util<-read.csv("P3-Machine-Utilization.csv")
head(util,12)
str(util)
summary(util)







#creamos campo
util$Utilization=1-util$Percent.Idle
head(util,12)







#convertimos fecha
util$PosixTime<-as.POSIXct(util$Timestamp,format="%d/%m/%Y %H:%M")
head(util,12)






#eliminaos columna
util$Timestamp<-NULL
head(util,12)





#reodenamos columnas
util<-util[,c(4,1,2,3)]
head(util,12)






#filtramos filas
RL1<-util[util$Machine=="RL1",]
RL1
nrow(RL1)
RL1$Machine<-factor(RL1$Machine)
str(RL1)






#unimos campos en una lista
util_stats_RL1<-c(min(RL1$Utilization,na.rm=T),
                  mean(RL1$Utilization,na.rm=T),
                  max(RL1$Utilization,na.rm=T))

util_stats_RL1





#contamos filas cumplen filtro
under_rl1<-length(which(RL1$Utilization<0.90))
under_rl1




#unimos en lista
list_rl1<-list("RL1",under_rl1,util_stats_RL1)
list_rl1




#ponemos nombres
names(list_rl1)<-c("Machine","Under_09","Stats")
list_rl1





#otra forma de crear una lista con campos
list_rl1<-list(Machine="RL1",Under_09=under_rl1,Stats=util_stats_RL1)







#acceso a los elementos
list_rl1[1]
list_rl1[[1]]
list_rl1$Machine
list_rl1[[3]]
typeof(list_rl1[[3]])
list_rl1$Stats
list_rl1$Stats[2]
list_rl1[[3]][2]
#añadir elemento
list_rl1[4]<-"New information"







#buscar atributo de determinadas filas
RL1[is.na(RL1$Utilization),"PosixTime"]
list_rl1$UnknownHours<-RL1[is.na(RL1$Utilization),"PosixTime"]

length(list_rl1$UnknownHours)







#eliminamos elemento
list_rl1[4]<-NULL

list_rl1[4]










#anadimos elemento
list_rl1$Data<-RL1
summary(list_rl1)

str(list_rl1)

list_rl1[[4]][1]










#accedemos elemento concreto
list_rl1$UnknownHours[1]

#accedemos dos elementos
list_rl1[c(1,4)]

#sublista y su acceso
sublist_rl1<-list_rl1[c("Machine","UnknownHours")]
sublist_rl1
sublist_rl1[[2]][2]
sublist_rl1$UnknownHours[2]









#grafico
library(ggplot2)

#grafico de lineas separando por maquina
p<-ggplot(data=util)
myplot<-p+geom_line(aes(x=PosixTime,y=Utilization,colour=Machine),size=1.2)+
  facet_grid(Machine~.)+
  geom_hline(yintercept = 0.9,colour="Gray",size=1.2,linetype=3)

list_rl1$Plot<-myplot
list_rl1

summary(list_rl1)#resumen de los elementos de la lista
