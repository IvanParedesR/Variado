# Coronavirus
#Edición: 29/03/20
#Editor IPR
#Paquetería
library(tidyverse)
library(readxl)
library(httr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(spatstat.utils)
library(data.table)
library(plyr)
#Cargar base de datos
data <- read_excel("C:/Users/iparedes/Downloads/COVID-19-geographic-disbtribution-worldwide-2020-03-29.xlsx", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "numeric"))
View(data)
#simplificar nombre
data$Countries <- data$countriesAndTerritories
#Establecer formato de fecha
data$dateRep <- as.Date(data$dateRep, origin = "2019-12-31", format = "%Y-%m-%d")
#filtros por nación, creación de base 2
data2 <- filter(data, Countries == "Argentina" | Countries == "China" | Countries == "Colombia" | Countries == "France" | Countries == "Germany" | Countries == "Italy" | Countries == "Mexico" | Countries == "Spain" | Countries == "United_States_of_America")
#ordena la base de datos
data2 <- arrange(data2, Countries, dateRep)
#Crear suma de muertos y de enfermos total
setDT(data2)[, Cases_total := cumsum(cases), by = Countries]
setDT(data2)[, Death_total := cumsum(deaths), by = Countries]
#ordena la base de datos
arrange(data2, Countries, dateRep)
# Crea base de datos eliminando los casos en donde el total de  enfermos es cero
data3 <- subset(data2, Cases_total != 0)
# Ordena la base de datos 3
arrange(data3, Countries, dateRep)
#Crea un indice que va del día en que se da el primer caso. Lo anterior, para que sea comparable.
data3$diasdesdeprimerenfermo <- ave(data3$Cases_total, data3$Countries, FUN = seq_along)
# Graficamos Enfermos
graph <- ggplot(data3, aes(diasdesdeprimerenfermo, Cases_total)) + 
  geom_line() +
  xlim(1, 23) +
  ylim(1, 750) +
  facet_wrap(~Countries)
graph
# Crea base de datos eliminando los casos en donde el total de muertos es cero
data4 <- subset(data2, Death_total != 0)
#Ordenamos
arrange(data4, Countries, dateRep)
#Crea un indice que va del día en que se da el primer muerto. Lo anterior, para que sea comparable.
data4$diasdesdeprimermuerto <- ave(data4$Death_total, data4$Countries, FUN = seq_along)
#Graficamos
graph2 <- ggplot(data4, aes(diasdesdeprimermuerto, Death_total)) + 
  geom_line() +
  xlim(1, 11) +
  ylim(1, 50) +
  facet_wrap(~Countries)
graph2

graph3 <- ggplot(data4, aes(diasdesdeprimermuerto, Death_total, colour= Countries)) + 
  geom_line(aes(linetype = Countries)) +
  xlim(1, 10) + # se toman 10, pues es el numero de días desde que México reportó a su primer muerto
  ylim(1, 50)
graph3

graph4 <- ggplot(data3, aes(diasdesdeprimerenfermo, Cases_total, colour= Countries)) + 
  geom_line(aes(linetype = Countries)) +
  xlim(1, 23) + # se toman 21, pues es el numero de días desde que México reportó a su primer enfermo el 2020-02-29 (las observaciones no son diarias)
  ylim(1, 1000)
graph4