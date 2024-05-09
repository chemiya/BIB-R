
#cargar fichero
stats<-read.csv(file.choose())
stats





#leer fichero
getwd()
setwd("C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\original curso")
getwd()
stats<-read.csv("P2-Demographic-Data.csv")
head(stats)







#resumen general
nrow(stats)
ncol(stats)
head(stats, n=10)
tail(stats)
str(stats)
summary(stats)

print(class(stats))
#distribucion
distribucion <- table(stats$Income.Group)
print(distribucion)

colnames(stats)
#acceder a elementos
stats[3,3]
stats[3,"Birth.rate"]
stats$Internet.users#toda la columna
stats$Internet.users[2]
stats[,"Internet.users"]#toda la columna







print(class(stats))
str(stats)
#convertimos en factor
stats$Income.Group<-factor(stats$Income.Group)
levels(stats$Income.Group)
str(stats)

print(class(stats))
stats[1:10,]#filas 1 a 10
stats[c(4,100),] #filas 4 y 10
stats[,c(2,4)]#columnas 2 y 4
stats[,3:ncol(stats)]#columna 3 hasta el final
stats[3:nrow(stats),]#filas 3 hasta final

print(class(stats))
is.data.frame(stats[1,])
is.data.frame(stats[,1])
print(class(stats[,1]))

#primera columna
stats[,1,drop=F]








print(class(stats))
#calculo entre columnas
stats$MyCalc<-stats$Birth.rate+stats$Internet.users
head(stats)

str(stats)
#eliminamos columna
stats$MyCalc<-NULL
str(stats)








#filtro
print(class(stats))
filter<-stats$Internet.users<2
filter#devuelve true o false

#para obtener las filas
stats[stats$Birth.rate>40 &stats$Internet.users<2,]
stats[stats$Country.Name=="Spain",]







#añadir columna con valores segun condicion
library(dplyr)
print(class(stats))
stats <- stats %>%mutate(cumple_condicion = ifelse(Internet.users <2, "Cumple", "No Cumple"))
head(stats)








#elementos unicos
print(class(stats))
str(stats)
diferentes<-unique(stats$Income.Group)
diferentes

head(stats, n=10)
class(stats)
typeof(stats)







#graficos
library(ggplot2)
qplot(data=stats, x=Income.Group,y=Birth.rate,geom="boxplot")
qplot(data=stats, x=Internet.users,y=Birth.rate,size=I(3),colour=Income.Group)


qplot(data=stats, x=Income.Group,y=Birth.rate,size=I(3))
qplot(data=stats, x=Income.Group,y=Birth.rate,size=I(3),colour=I("blue"))








Countries_2012_Dataset <- c("Aruba","Afghanistan","Angola","Albania","United Arab Emirates","Argentina","Armenia","Antigua and Barbuda","Australia","Austria","Azerbaijan","Burundi","Belgium","Benin","Burkina Faso","Bangladesh","Bulgaria","Bahrain","Bahamas, The","Bosnia and Herzegovina","Belarus","Belize","Bermuda","Bolivia","Brazil","Barbados","Brunei Darussalam","Bhutan","Botswana","Central African Republic","Canada","Switzerland","Chile","China","Cote d'Ivoire","Cameroon","Congo, Rep.","Colombia","Comoros","Cabo Verde","Costa Rica","Cuba","Cayman Islands","Cyprus","Czech Republic","Germany","Djibouti","Denmark","Dominican Republic","Algeria","Ecuador","Egypt, Arab Rep.","Eritrea","Spain","Estonia","Ethiopia","Finland","Fiji","France","Micronesia, Fed. Sts.","Gabon","United Kingdom","Georgia","Ghana","Guinea","Gambia, The","Guinea-Bissau","Equatorial Guinea","Greece","Grenada","Greenland","Guatemala","Guam","Guyana","Hong Kong SAR, China","Honduras","Croatia","Haiti","Hungary","Indonesia","India","Ireland","Iran, Islamic Rep.","Iraq","Iceland","Israel","Italy","Jamaica","Jordan","Japan","Kazakhstan","Kenya","Kyrgyz Republic","Cambodia","Kiribati","Korea, Rep.","Kuwait","Lao PDR","Lebanon","Liberia","Libya","St. Lucia","Liechtenstein","Sri Lanka","Lesotho","Lithuania","Luxembourg","Latvia","Macao SAR, China","Morocco","Moldova","Madagascar","Maldives","Mexico","Macedonia, FYR","Mali","Malta","Myanmar","Montenegro","Mongolia","Mozambique","Mauritania","Mauritius","Malawi","Malaysia","Namibia","New Caledonia","Niger","Nigeria","Nicaragua","Netherlands","Norway","Nepal","New Zealand","Oman","Pakistan","Panama","Peru","Philippines","Papua New Guinea","Poland","Puerto Rico","Portugal","Paraguay","French Polynesia","Qatar","Romania","Russian Federation","Rwanda","Saudi Arabia","Sudan","Senegal","Singapore","Solomon Islands","Sierra Leone","El Salvador","Somalia","Serbia","South Sudan","Sao Tome and Principe","Suriname","Slovak Republic","Slovenia","Sweden","Swaziland","Seychelles","Syrian Arab Republic","Chad","Togo","Thailand","Tajikistan","Turkmenistan","Timor-Leste","Tonga","Trinidad and Tobago","Tunisia","Turkey","Tanzania","Uganda","Ukraine","Uruguay","United States","Uzbekistan","St. Vincent and the Grenadines","Venezuela, RB","Virgin Islands (U.S.)","Vietnam","Vanuatu","West Bank and Gaza","Samoa","Yemen, Rep.","South Africa","Congo, Dem. Rep.","Zambia","Zimbabwe")
Codes_2012_Dataset <- c("ABW","AFG","AGO","ALB","ARE","ARG","ARM","ATG","AUS","AUT","AZE","BDI","BEL","BEN","BFA","BGD","BGR","BHR","BHS","BIH","BLR","BLZ","BMU","BOL","BRA","BRB","BRN","BTN","BWA","CAF","CAN","CHE","CHL","CHN","CIV","CMR","COG","COL","COM","CPV","CRI","CUB","CYM","CYP","CZE","DEU","DJI","DNK","DOM","DZA","ECU","EGY","ERI","ESP","EST","ETH","FIN","FJI","FRA","FSM","GAB","GBR","GEO","GHA","GIN","GMB","GNB","GNQ","GRC","GRD","GRL","GTM","GUM","GUY","HKG","HND","HRV","HTI","HUN","IDN","IND","IRL","IRN","IRQ","ISL","ISR","ITA","JAM","JOR","JPN","KAZ","KEN","KGZ","KHM","KIR","KOR","KWT","LAO","LBN","LBR","LBY","LCA","LIE","LKA","LSO","LTU","LUX","LVA","MAC","MAR","MDA","MDG","MDV","MEX","MKD","MLI","MLT","MMR","MNE","MNG","MOZ","MRT","MUS","MWI","MYS","NAM","NCL","NER","NGA","NIC","NLD","NOR","NPL","NZL","OMN","PAK","PAN","PER","PHL","PNG","POL","PRI","PRT","PRY","PYF","QAT","ROU","RUS","RWA","SAU","SDN","SEN","SGP","SLB","SLE","SLV","SOM","SRB","SSD","STP","SUR","SVK","SVN","SWE","SWZ","SYC","SYR","TCD","TGO","THA","TJK","TKM","TLS","TON","TTO","TUN","TUR","TZA","UGA","UKR","URY","USA","UZB","VCT","VEN","VIR","VNM","VUT","PSE","WSM","YEM","ZAF","COD","ZMB","ZWE")
Regions_2012_Dataset <- c("The Americas","Asia","Africa","Europe","Middle East","The Americas","Asia","The Americas","Oceania","Europe","Asia","Africa","Europe","Africa","Africa","Asia","Europe","Middle East","The Americas","Europe","Europe","The Americas","The Americas","The Americas","The Americas","The Americas","Asia","Asia","Africa","Africa","The Americas","Europe","The Americas","Asia","Africa","Africa","Africa","The Americas","Africa","Africa","The Americas","The Americas","The Americas","Europe","Europe","Europe","Africa","Europe","The Americas","Africa","The Americas","Africa","Africa","Europe","Europe","Africa","Europe","Oceania","Europe","Oceania","Africa","Europe","Asia","Africa","Africa","Africa","Africa","Africa","Europe","The Americas","The Americas","The Americas","Oceania","The Americas","Asia","The Americas","Europe","The Americas","Europe","Asia","Asia","Europe","Middle East","Middle East","Europe","Middle East","Europe","The Americas","Middle East","Asia","Asia","Africa","Asia","Asia","Oceania","Asia","Middle East","Asia","Middle East","Africa","Africa","The Americas","Europe","Asia","Africa","Europe","Europe","Europe","Asia","Africa","Europe","Africa","Asia","The Americas","Europe","Africa","Europe","Asia","Europe","Asia","Africa","Africa","Africa","Africa","Asia","Africa","Oceania","Africa","Africa","The Americas","Europe","Europe","Asia","Oceania","Middle East","Asia","The Americas","The Americas","Asia","Oceania","Europe","The Americas","Europe","The Americas","Oceania","Middle East","Europe","Europe","Africa","Middle East","Africa","Africa","Asia","Oceania","Africa","The Americas","Africa","Europe","Africa","Africa","The Americas","Europe","Europe","Europe","Africa","Africa","Middle East","Africa","Africa","Asia","Asia","Asia","Asia","Oceania","The Americas","Africa","Europe","Africa","Africa","Europe","The Americas","The Americas","Asia","The Americas","The Americas","The Americas","Asia","Oceania","Middle East","Oceania","Middle East","Africa","Africa","Africa","Africa")


#dataframe desde vectores del tipo c(2,3,4)
print(class(Regions_2012_Dataset))
mydf<-data.frame(Countries_2012_Dataset,Codes_2012_Dataset,Regions_2012_Dataset)
head(mydf)








#nombres columnas
colnames(mydf)<-c("Country","Code","Region")
head(mydf)

#estableciendo los nombres de las columnas
mydf<-data.frame(Country=Countries_2012_Dataset,Code=Codes_2012_Dataset,
                 Region=Regions_2012_Dataset)
head(mydf)
str(mydf)










#union dataframes
head(stats)
head(mydf)
class(stats)
class(mydf)

#columna primer dataframe y columna segundo dataframe
merged<-merge(stats,mydf,by.x="Country.Code",by.y="Code")
head(merged)

#eliminamos columna
merged$Country<-NULL
str(merged)












#grafico de puntos poniendo triangulos
class(merged)
str(merged)
qplot(data=merged,x=Internet.users,y=Birth.rate,colour=Region,size=I(5),
      shape=I(17),alpha=I(0.6),main="Birth rate vs Internet users")
