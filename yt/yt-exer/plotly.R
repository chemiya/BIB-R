
install.packages("gapminder")
library(gapminder)
#cargar datos
data("gapminder")

library(ggplot2)
library(plotly)

#ggplot ejemplo con puntos
pp <-ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) + 
  geom_jitter() + scale_x_log10()  
pp

# grabar el gráfico
pp1 <- ggplotly(pp)
htmlwidgets::saveWidget(as_widget(pp1), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\mi_grafico.html") 

#grafico de puntos cambiando forma
pp2 <- ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, shape = continent,  col = as.factor(year))) + 
  geom_jitter() + scale_x_log10()
pp2



#crear grafico con plotly
g <- plot_ly(gapminder, x = ~gdpPercap, y = ~lifeExp) 
g <- add_markers(g)
#cambiar escala eje x
g <- layout(g, xaxis = list(type = "log"))
htmlwidgets::saveWidget(as_widget(g), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\plotly.html") 


#crear grafico con plotly con color por continente
g <- plot_ly(gapminder, x = ~gdpPercap, y = ~lifeExp, color = ~continent) 
g <- add_markers(g)
#cambiar escala eje x
g <- layout(g, xaxis = list(type = "log"))
htmlwidgets::saveWidget(as_widget(g), "C:\\Users\\jmlozanoo\\Documents\\GitHub\\BIB-R\\yt\\yt-exer\\plotly.html") 

