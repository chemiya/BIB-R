




getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R")
#leemos csv con header y corrigiendo vacios
companies_data <- read.csv("companies.csv",header = TRUE,na.strings = c(""))





head(companies_data) #primeros elementos
summary(companies_data) #resumen de las variables->media y std
str(companies_data) # ver valores que puede tomar
names(companies_data) #nombre variables
nrow(companies_data) #numero filas
ncol(companies_data) #numero columnas




#filas con algun atributo vacio
companies_data[!complete.cases(companies_data),]
#contamos esas filas
nrow(companies_data[!complete.cases(companies_data),])





#convertir a factor un atributo
companies_data$Industry<-factor(companies_data$Industry)
#se muestran los valores diferentes que puede tomar
str(companies_data)






#valores diferentes del factor
levels(companies_data$Industry)



#valores unicos de un atributo
unicos_estados<-unique(companies_data$State)
unicos_estados
#los contamos
cantidad_estados <- length(unicos_estados)
cantidad_estados
















#reemplazar cadenas
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



#ver el tipo de una columna
tipo_columna <- class(companies_data$Expenses)
tipo_columna

tipo_columna <- class(companies_data$Revenue)
tipo_columna







#convertir a numerica una columna
companies_data$Expenses<-as.numeric(companies_data$Expenses)
tipo_columna <- class(companies_data$Expenses)
tipo_columna

companies_data$Revenue<-as.numeric(companies_data$Revenue)
companies_data$Growth<-as.numeric(companies_data$Growth)


str(companies_data)






#filas vacias con algun atributo vacio
companies_data[!complete.cases(companies_data),]
nrow(companies_data[!complete.cases(companies_data),])







#filtramos filas con determinado numero de empleados
companies_data[which(companies_data$Employees==45),]











#eliminamos filas con valor nulo en este atributo
nrow(companies_data[!complete.cases(companies_data),])

#cogemos las que no tienen el valor vacio
companies_data<-companies_data[!is.na(companies_data$Industry),]
nrow(companies_data[!complete.cases(companies_data),])











#reiniciamos indices
rownames(companies_data)<-NULL
companies_data






nrow(companies_data[!complete.cases(companies_data),])
companies_data[!complete.cases(companies_data),]


#ponemos en el atributo state el valor que corresponde
companies_data[is.na(companies_data$State) & companies_data$City=="New York","State"]<-"NY"

#vemos esas filas
companies_data[c(11,377),]


#ponemos el estado que corresponde
companies_data[is.na(companies_data$State) & companies_data$City=="San Francisco",]
companies_data[is.na(companies_data$State) & companies_data$City=="San Francisco","State"]<-"CA"
companies_data[c(82,265),]


#filas que quedan con algun atributo vacio
companies_data[!complete.cases(companies_data),]
nrow(companies_data[!complete.cases(companies_data),])





#calculamos mediana de la columna. Los valores faltantes no los tiene en cuenta
median(companies_data[,"Employees"],na.rm=TRUE)
mean(companies_data[,"Employees"],na.rm=TRUE)







#mediana de la columna empleados para las filas de la industria retail
med_emp_retail<-median(companies_data[companies_data$Industry=="Retail","Employees"],na.rm=TRUE)



#buscmos filas con empleados vacios y de la industria retail y ponemos el valor
companies_data[is.na(companies_data$Employees) & companies_data$Industry=="Retail",]
companies_data[is.na(companies_data$Employees) & companies_data$Industry=="Retail","Employees"]<-med_emp_retail


#vemos la fila
companies_data[3,]


#filas de esa industria. 
companies_data[companies_data$Industry=="Financial Services",]
# ver solo los empleados de las filas de esa industria
companies_data[companies_data$Industry=="Financial Services","Employees"]

#calculamos mediana de empleados de las filas de esa industria
med_emp_financial<-median(companies_data[companies_data$Industry==
                                           "Financial Services","Employees"],na.rm=TRUE)

#buscamos filas sin empleados y de esa industria y ponemos el valor de la mediana en los empleados
companies_data[is.na(companies_data$Employees) &
                 companies_data$Industry=="Financial Services",]
companies_data[is.na(companies_data$Employees) &
                 companies_data$Industry=="Financial Services","Employees"]<-med_emp_financial
companies_data[330,]

#filas quedan con algun atributo vacio
companies_data[!complete.cases(companies_data),]





#mediana de crecimientos para filas de la industria de construccion
med_grow_constr<-median(companies_data[companies_data$Industry==
                                         "Construction","Growth"],na.rm=TRUE)
med_grow_constr

#buscamos filas sin crecimiento y de la industria de construccion y ponemos el valor en crecimiento
companies_data[is.na(companies_data$Growth) & companies_data$Industry=="Construction",]
companies_data[is.na(companies_data$Growth) & companies_data$Industry==
                 "Construction","Growth"]<-med_grow_constr
companies_data[8,]





#mediana de los ingresos para empresas de construccion
med_revenue_constr<-median(companies_data[companies_data$Industry=="Construction",
                                          "Revenue"],na.rm=TRUE)
med_revenue_constr

#si no tienen ingresos y son de construccion, ponemos ese valor
companies_data[is.na(companies_data$Revenue) & companies_data$Industry=="Construction",]
companies_data[is.na(companies_data$Revenue) &
                 companies_data$Industry=="Construction","Revenue"]<-med_revenue_constr
companies_data[c(8,42),]

#lo mismo con los gastos
med_expenses_constr<-median(companies_data[companies_data$Industry=="Construction","Expenses"],na.rm=TRUE)
med_expenses_constr

companies_data[is.na(companies_data$Expenses) & companies_data$Industry=="Construction"& is.na(companies_data$Profit),]
companies_data[is.na(companies_data$Expenses) & companies_data$Industry=="Construction"& is.na(companies_data$Profit),"Expenses"]<-med_expenses_constr
companies_data[c(8,42),]











#el beneficio lo calculamos a partir de otros, filas que lo tienen vacio
#ponemos el calculo de beneficios-gastos
companies_data[is.na(companies_data$Profit),]
companies_data[is.na(companies_data$Profit),"Profit"]<-companies_data[is.na(companies_data$Profit),"Revenue"]-companies_data[is.na(companies_data$Profit),"Expenses"]


companies_data[c(8,42),]


#filas que quedan con algun atributo vacio
companies_data[!complete.cases(companies_data),]


#similar, ponemos el valor calculandolo desde los otros
companies_data[is.na(companies_data$Expenses),"Expenses"]<-companies_data[is.na(companies_data$Expenses),"Revenue"]-companies_data[is.na(companies_data$Expenses),"Profit"]
companies_data[15,]



#nos quedamos solo con los que tienen todos los atributos informados
companies_data<-companies_data[complete.cases(companies_data),]












#dataframe analisis previo
head(companies_data)
colnames(companies_data) #nombres columnas
str(companies_data) #valores diferentes de las columnas


#instalamos la libreria y la cargamos
install.packages("ggplot2")
library(ggplot2)


#grafico puntos con 4 dimensiones. x,y,tamano y color
p<-ggplot(data=companies_data)
p+geom_point(aes(x=Revenue,y=Expenses,colour=Industry,size=Profit))

#con linea curva
d<-ggplot(data=companies_data,aes(x=Revenue,y=Expenses,colour=Industry))
d+geom_point()+geom_smooth(fill=NA,size=1.2)

#de caja y bigotes. x,y, y color
f<-ggplot(data=companies_data,aes(x=Industry,y=Growth,colour=Industry))
f+geom_boxplot(size=1)

#de caja con puntos
f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)

#separando varios graficos. x,y, color separando por industria
p<-ggplot(data=companies_data,aes(x=Revenue,y=Expenses,colour=State))
p+geom_point(size=3)+facet_grid(.~Industry)




#beneficios por industria

#dplyr
install.packages("dplyr")
library(dplyr)

#agrupa por industria, suma los beneficios y ordena descendente
industria_beneficios <- companies_data %>%
  group_by(Industry) %>%
  summarise(Suma_Beneficios = sum(Profit)) %>%
  arrange(desc(Suma_Beneficios))


#convertimos a dataframe y mostramos
print(class(industria_beneficios))
industria_beneficios<- as.data.frame(industria_beneficios)
industria_beneficios


#cuenta cuantas companias son de cada industria
frecuencia <- table(companies_data$Industry)
frecuencia<- as.data.frame(frecuencia)
frecuencia
colnames(frecuencia)<-c("Industry","Cuenta")
frecuencia

#juntamos ambas tablas por la clave industria
merged_industry<-merge(industria_beneficios, frecuencia, by.x="Industry", by.y="Industry")
merged_industry


#insertamos columna con calculo redondeando
merged_industry$Media<-round(merged_industry$Suma_Beneficios/merged_industry$Cuenta,2)
merged_industry





#beneficios por area, poniendo etiquetas
ggplot(merged_industry, aes(x = Industry, y = Cuenta)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Gráfico de Barras de proporción beneficios", x = "Industria",
       y = "Proporción")+
  theme(plot.title = element_text(hjust = 0.5))
