getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\original curso")
getwd()

#cargamos datos
movies<-read.csv("P2-Movie-Ratings.csv")
head(movies)








#nombres columnas
colnames(movies)<-c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
head(movies)






#factor
movies$Year<-factor(movies$Year)
str(movies)







#distribucion
distribucion <- table(movies$Year)
print(distribucion)






#columna calculando media de dos atributos
library(dplyr)
movies <- movies %>%
  mutate(media_col = (CriticRating + AudienceRating) / 2)
head(movies)






#analisis dataframe antes graficos
colnames(movies)
print(class(movies))
str(movies)







#graficos
library(ggplot2)

#4 dimensiones con puntos
ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre,size=BudgetMillions))+geom_point()






#lineas mas puntos
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre,size=BudgetMillions))
p+geom_line()+geom_point()







#4 dimensiones con puntos
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre,size=BudgetMillions))
p+geom_point()
p+geom_point(aes(colour=BudgetMillions))
p+geom_point(aes(x=BudgetMillions))+xlab("Budget Millions Axis")


# poner lineas
p+geom_line(size=1)+geom_point()








#puntos
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating))
p+geom_point(colour="DarkGreen")
p+geom_point(size=3)
p+geom_point(aes(color=Genre,size=BudgetMillions))






#histograma con 2 dimensiones
p<-ggplot(data=movies,aes(x=BudgetMillions))
p+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")






#grafica densidad
p+geom_density(aes(fill=Genre),position="Stack")





#histograma
p<-ggplot(data=movies,aes(x=AudienceRating))
p+geom_histogram(binwidth=10,fill="White",colour="Black")





#histogramas
p<-ggplot(data=movies)
p+geom_histogram(binwidth=10,aes(x=AudienceRating),fill="White",colour="Red")
p+geom_histogram(binwidth=10,aes(x=CriticRating),fill="White",colour="Red")






#puntos con linea curva, 3 dimensiones
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre))
p+geom_point()+geom_smooth(fill=NA)


#de caja con puntos
p<-ggplot(data=movies,aes(x=Genre,y=AudienceRating,colour=Genre))
p+geom_boxplot(size=1.2)+geom_point()
p+geom_boxplot(size=1.2)+geom_jitter()
p+geom_jitter()+geom_boxplot(alpha=0.5)





#histogramas separados por atributo
p<-ggplot(data=movies,aes(x=BudgetMillions))
p+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")+facet_grid(Genre~.,scales="free")






#puntos separados por atributo
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre))
p+geom_point(size=3)+facet_grid(.~Year)





#2 separaciones
p+geom_point(size=3)+facet_grid(Genre~Year)

# con linea curva
p+geom_point(size=3)+facet_grid(Genre~Year)+geom_smooth()

#escalados
p+geom_point(aes(size=BudgetMillions))+facet_grid(Genre~Year)+geom_smooth()+coord_cartesian(ylim=c(0,50))









#puntos limitados
p<-ggplot(data=movies,aes(x=CriticRating,y=AudienceRating,colour=Genre,size=BudgetMillions))
p+geom_point()+xlim(50,100)+ylim(50,100)


#histograma limitados
p<-ggplot(data=movies,aes(x=BudgetMillions))
p+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")+coord_cartesian(ylim=c(0,50))




#etiquetas
p<-ggplot(data=movies,aes(x=BudgetMillions))
k<-p+geom_histogram(binwidth=10,aes(fill=Genre),colour="Black")
k+xlab("Money analisis")+ylab("Number of movies")+ggtitle("Distribution")+theme(axis.title.x=element_text(colour="DarkGreen",size=10),axis.title.y=element_text(colour="Red",size=10),axis.text.x=element_text(size=10),axis.text.y=element_text(size=10),legend.title=element_text(size=10),legend.text=element_text(size=10),legend.position=c(1,1),legend.justification=c(1,1),plot.title = element_text(colour="Darkblue",size=30))+theme(plot.title = element_text(hjust = 0.5))
