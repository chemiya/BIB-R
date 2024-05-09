getwd()
#"C:/Users/chema/Desktop/formacionPracticar/r-studio-curso"

setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\original curso")




#leer excel
install.packages("readxl")
library(readxl)
fin <- read_excel("P3-Future-500-Marked-Up.xlsx")
head(fin)

str(fin)
summary(fin)







#ponemos como factores
fin$ID<-factor(fin$ID)
fin$Industry<-factor(fin$Industry)
str(fin)

#valores diferentes
levels(fin$Industry)







#ver tipo de variables
a<-c("23","33","334")
typeof(a)

b<-as.numeric(a)
typeof(b)

#conversiones
z<-factor(c("23","33","334","23","334"))
z
y<-as.numeric(z)
y
a<-as.numeric(as.character(z))
a

#convertir a factor
fin$Profit<-factor(fin$Profit)
str(fin)

#convertir a numero
fin$Profit<-as.numeric(fin$Profit)
str(fin)

?sub()








#reemplazos cadenas
head(fin)
fin$Expenses<-gsub(" Dollars","",fin$Expenses)
head(fin)
fin$Expenses<-gsub(",","",fin$Expenses)
head(fin)
fin$Revenue<-gsub("\\$","",fin$Revenue)
head(fin)
fin$Revenue<-gsub(",","",fin$Revenue)
head(fin)
fin$Growth<-gsub("%","",fin$Growth)
head(fin)
str(fin)

tipo_columna <- class(fin$Expenses)
tipo_columna





#convertir a numero
fin$Expenses<-as.numeric(fin$Expenses)
fin$Revenue<-as.numeric(fin$Revenue)
fin$Growth<-as.numeric(fin$Growth)

tipo_columna <- class(fin$Expenses)
tipo_columna
str(fin)

head(fin,24)





#elementos vacios con algun atributo vacio
fin[!complete.cases(fin),]

#buscamos filas por condicion
fin[which(fin$Revenue==9746272),]

fin[which(fin$Employees==45),]

#evaluar si hay na
a<-c("112","23","45","3",NA,"33")
is.na(a)






#filas con valor na en el atributo
fin[is.na(fin$Expenses),]

fin_backup<-fin
fin[!complete.cases(fin),]
fin[is.na(fin$Industry),]








#eliminamos filas con valor nulo en este atributo
fin<-fin[!is.na(fin$Industry),]
fin[!complete.cases(fin),]








#reiniciamos indices
rownames(fin)<-1:nrow(fin)
fin
rownames(fin)<-NULL
fin








#ponemos en este atributo el valor que corresponde
fin[is.na(fin$State) & fin$City=="New York","State"]<-"NY"
fin[c(11,377),]

fin[!complete.cases(fin),]

fin[is.na(fin$State) & fin$City=="San Francisco",]
fin[is.na(fin$State) & fin$City=="San Francisco","State"]<-"CA"
fin[c(82,265),]

fin[!complete.cases(fin),]











#calculamos mediana de la columna
median(fin[,"Employees"],na.rm=TRUE)
mean(fin[,"Employees"],na.rm=TRUE)











#mediana de una columna filtrando
med_emp_retail<-median(fin[fin$Industry=="Retail","Employees"],na.rm=TRUE)

#buscmos elemento y le ponemos valor
fin[is.na(fin$Employees) & fin$Industry=="Retail",]
fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"]<-med_emp_retail
fin[3,]

med_emp_financial<-median(fin[fin$Industry=="Financial Services","Employees"],na.rm=TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Financial Services",]
fin[is.na(fin$Employees) & fin$Industry=="Financial Services","Employees"]<-med_emp_financial
fin[330,]

fin[!complete.cases(fin),]

med_grow_constr<-median(fin[fin$Industry=="Construction","Growth"],na.rm=TRUE)
med_grow_constr

fin[is.na(fin$Growth) & fin$Industry=="Construction",]
fin[is.na(fin$Growth) & fin$Industry=="Construction","Growth"]<-med_grow_constr
fin[8,]

med_revenue_constr<-median(fin[fin$Industry=="Construction","Revenue"],na.rm=TRUE)
med_revenue_constr

fin[is.na(fin$Revenue) & fin$Industry=="Construction",]
fin[is.na(fin$Revenue) & fin$Industry=="Construction","Revenue"]<-med_revenue_constr
fin[c(8,42),]

med_expenses_constr<-median(fin[fin$Industry=="Construction","Expenses"],na.rm=TRUE)
med_expenses_constr

fin[is.na(fin$Expenses) & fin$Industry=="Construction"& is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry=="Construction"& is.na(fin$Profit),"Expenses"]<-med_expenses_constr
fin[c(8,42),]








#este atributo lo calculamos a partir de otros
fin[is.na(fin$Profit),]
fin[is.na(fin$Profit),"Profit"]<-fin[is.na(fin$Profit),"Revenue"]-fin[is.na(fin$Profit),"Expenses"]
fin[c(8,42),]

fin[!complete.cases(fin),]

fin[is.na(fin$Expenses),"Expenses"]<-fin[is.na(fin$Expenses),"Revenue"]-fin[is.na(fin$Expenses),"Profit"]
fin[15,]

fin[!complete.cases(fin),]










#dataframe analisis previo
head(fin)
colnames(fin)
str(fin)

library(ggplot2)

#grafico puntos con 4 dimensiones
p<-ggplot(data=fin)
p+geom_point(aes(x=Revenue,y=Expenses,colour=Industry,size=Profit))

#con linea curva
d<-ggplot(data=fin,aes(x=Revenue,y=Expenses,colour=Industry))
d+geom_point()+geom_smooth(fill=NA,size=1.2)

#de caja
f<-ggplot(data=fin,aes(x=Industry,y=Growth,colour=Industry))
f+geom_boxplot(size=1)

#de caja con puntos
f+geom_jitter()+geom_boxplot(size=1,alpha=0.5,outlier.color=NA)
