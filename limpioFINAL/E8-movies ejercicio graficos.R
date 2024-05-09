
getwd()

#leemos datos
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R")
mov <- read.csv("movies-data.csv")

head(mov) 
summary(mov) 
str(mov) 
names(mov)








#graficos
library(ggplot2)


#grafico barras
ggplot(data=mov, aes(x=Day.of.Week)) + geom_bar()

#filtros
filt <- (mov$Genre == "action") | (mov$Genre == "adventure") | (mov$Genre == "animation") | (mov$Genre == "comedy") | (mov$Genre == "drama")

filt2 <- (mov$Studio == "Buena Vista Studios") | (mov$Studio== "WB") | (mov$Studio== "Fox") | (mov$Studio== "Universal") | (mov$Studio== "Sony") | (mov$Studio== "Paramount Pictures")
filt2<- mov$Studio %in% c("Buena Vista Studios","WB","Fox","Universal","Sony","Paramount Pictures")

#seleccionamos filas filtro
mov2 <- mov[filt & filt2,]
nrow(mov2)







#grafico
p <- ggplot(data=mov2, aes(x=Genre, y=Gross...US))


p + 
  geom_point()


p + 
  geom_boxplot()


p + 
  geom_boxplot() + 
  geom_point()


p + 
  geom_boxplot() + 
  geom_jitter()



p + 
  geom_jitter()+
  geom_boxplot(alpha=0.7) 
  


p <- ggplot(data=mov2, aes(x=Genre, y=Gross...US))

#boxplot con puntos y otras dimensiones
q<-p+
  geom_jitter(aes(size=Budget...mill.,color=Studio))+
  geom_boxplot(alpha=0.7,outlier.colour = NA)
q







#etiquetas
q <- q +
  xlab("Genre") + 
  ylab("Gross % US") + 
  ggtitle("Domestic Gross % by Genre") 
q








#detalles sobre el grafico
q <- q + 
  theme(
    text = element_text(family="Comic Sans MS"),
    axis.title.x = element_text(colour="Blue", size=10),
    axis.title.y = element_text(colour="Blue", size=10),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),  
    plot.title = element_text(colour="Black",
                     size=20),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10)
  )
q




