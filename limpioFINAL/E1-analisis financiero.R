
#carga de datos
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\original curso")
nombres<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre",
           "octubre","noviembre","diciembre")
#establecer nombres columnas
datos <- read.csv("revenues-expenses.csv",header = FALSE,col.names=nombres)
head(datos)







#resumen
head(datos)
summary(datos)
str(datos)









#separar datos
revenues <- datos[1, ]#primera fila
head(revenues)
expenses <- datos[2, ]
head(expenses)
print(class(revenues))









#calculos iniciales
medias_revenues <- rowMeans(revenues)
mensaje<-paste("La media de los ingresos es:",medias_revenues[1])
print(mensaje)
medias_expenses <- rowMeans(expenses)
mensaje_expenses<-paste("La media de los expenses es:",medias_expenses[1])
print(mensaje_expenses)







#beneficios
print(class(revenues))
print(class(expenses))
#realizamos calculo
profit <- revenues - expenses
print("beneficios")
profit
#realizamos suma de todos los valores
suma_profits <- rowSums(profit)
mensaje_profits<-paste("La suma de los profits es:",suma_profits[1])
print(mensaje_profits)











#impuestos
tax <- round(0.30 * profit, 2)
tax 






profit.after.tax<-NULL
#quitando impuestos
for (i in seq_along(profit)) {
  #si el beneficio es negativo, los impuestos tambien son negativos y se suman.
  if(profit[i]<0){
    profit.after.tax[i]=profit[i]+tax[i]
  }else{
    profit.after.tax[i]=profit[i]-tax[i]
  }
}
profit.after.tax




#conversion
print(class(profit.after.tax))
profit.after.tax<-as.data.frame(profit.after.tax)
print(class(profit.after.tax))
names(profit.after.tax)<-nombres
profit.after.tax

print(class(profit))
print(class(profit.after.tax))
print("Comparacion")
comparacion<-rbind(profit, profit.after.tax)#une filas dataframes
print(comparacion)

suma_profits_after <- rowSums(profit.after.tax)
mensaje_profits_after<-paste("La suma de los profits_after_tax es:",suma_profits_after[1])
print(mensaje_profits_after)








#margen calculando desde otras listas
profit.margin <- round(profit.after.tax/ revenues, 2) * 100
profit.margin








#beneficios medios
mean_pat <- round(rowMeans(profit.after.tax),2)
mensaje_media_after<-paste("La media de los profits_after_tax es:",mean_pat[1])
print(mensaje_media_after)

print(class(profit.after.tax))


#meses con beneficios superiores a la media
good.months <- profit.after.tax > mean_pat
good.months #devuelve true, false...
cuenta_good<-sum(good.months)
mensaje_good<-paste("Meses superiores a la media:",cuenta_good[1])
print(mensaje_good)







#meses con beneficios inferiores a la media
bad.months <- !good.months#opuestos a true, false...
cuenta_bad<-sum(bad.months)
mensaje_bad<-paste("Meses inferiores a la media:",cuenta_bad[1])
print(mensaje_bad)

print(class(profit.after.tax))


#mejor mes
mejor <- names(profit.after.tax)[which.max(profit.after.tax)]
print(paste("Mejor mes:", mejor))

#peor mes
peor <- names(profit.after.tax)[which.min(profit.after.tax)]
print(paste("Peor mes:", peor))

















