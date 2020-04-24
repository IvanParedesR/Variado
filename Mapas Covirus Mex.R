# Coronavirus
#Edici?n: 04/04/20
#Paqueter?a
library(tidyverse)
library(readxl)
library(httr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(spatstat.utils)
library(data.table)
library("mxmaps")

CasosCovid19 <- read_excel("C:/Users/iparedes/Downloads/Tabla_casos_positivos_COVID-19_resultado_InDRE_2020.03.31-Table-1.xlsx")
View(CasosCovid19)

CasosCovid19$Region <- as.character(NA)
CasosCovid19$Region[CasosCovid19$Estado == "AGUASCALIENTES"] <- "01"
CasosCovid19$Region[CasosCovid19$Estado == "BAJA CALIFORNIA"] <- "02"
CasosCovid19$Region[CasosCovid19$Estado == "BAJA CALIFORNIA SUR"] <- "03"
CasosCovid19$Region[CasosCovid19$Estado == "CAMPECHE"] <- "04"
CasosCovid19$Region[CasosCovid19$Estado == "COAHUILA"] <- "05"
CasosCovid19$Region[CasosCovid19$Estado == "COLIMA"] <- "06"
CasosCovid19$Region[CasosCovid19$Estado == "CHIAPAS"] <- "07"
CasosCovid19$Region[CasosCovid19$Estado == "CHIHUAHUA"] <- "08"
CasosCovid19$Region[CasosCovid19$Estado == "CIUDAD DE M????XICO"] <- "09"
CasosCovid19$Region[CasosCovid19$Estado == "DURANGO"] <- "10"
CasosCovid19$Region[CasosCovid19$Estado == "GUANAJUATO"] <- "11"
CasosCovid19$Region[CasosCovid19$Estado == "GUERRERO"] <- "12"
CasosCovid19$Region[CasosCovid19$Estado == "HIDALGO"] <- "13"
CasosCovid19$Region[CasosCovid19$Estado == "JALISCO"] <- "14"
CasosCovid19$Region[CasosCovid19$Estado == "M????XICO"] <- "15"
CasosCovid19$Region[CasosCovid19$Estado == "MICHOACÁN"] <- "16"
CasosCovid19$Region[CasosCovid19$Estado == "MORELOS"] <- "17"
CasosCovid19$Region[CasosCovid19$Estado == "NAYARIT"] <- "18"
CasosCovid19$Region[CasosCovid19$Estado == "NUEVO LEON"] <- "19"
CasosCovid19$Region[CasosCovid19$Estado == "OAXACA"] <- "20"
CasosCovid19$Region[CasosCovid19$Estado == "PUEBLA"] <- "21"
CasosCovid19$Region[CasosCovid19$Estado == "QUERETARO"] <- "22"
CasosCovid19$Region[CasosCovid19$Estado == "QUINTANA ROO"] <- "23"
CasosCovid19$Region[CasosCovid19$Estado == "SAN LUIS POTOSÍ"] <- "24"
CasosCovid19$Region[CasosCovid19$Estado == "SINALOA"] <- "25"
CasosCovid19$Region[CasosCovid19$Estado == "SONORA"] <- "26"
CasosCovid19$Region[CasosCovid19$Estado == "TABASCO"] <- "27"
CasosCovid19$Region[CasosCovid19$Estado == "TAMAULIPAS"] <- "28"
CasosCovid19$Region[CasosCovid19$Estado == "TLAXCALA"] <- "29"
CasosCovid19$Region[CasosCovid19$Estado == "VERACRUZ"] <- "30"
CasosCovid19$Region[CasosCovid19$Estado == "YUCATÁN"] <- "31"
CasosCovid19$Region[CasosCovid19$Estado == "ZACATECAS"] <- "32"
CasosCovid19$enfermo <- 1

CasosCovid191 <- subset(CasosCovid19, Fecha_inicio < "2020-03-04")

enfermosdata <- CasosCovid19 %>%
  group_by(Region) %>%
  summarise(enferm = n())

enfermosdata1 <- CasosCovid191 %>%
  group_by(Region) %>%
  summarise(enferm1 = n())

enfermosdata <- enfermosdata[-c(33), ]
enfermosdata1 <- enfermosdata1[-c(33), ]

df_mxstate <- merge(df_mxstate, enfermosdata, by.x = c("region"), by.y = c("Region"), all = TRUE)

df_mxstate$x100 <- (df_mxstate$enferm/df_mxstate$pop)
df_mxstate$x1001 <- (df_mxstate$enferm1/df_mxstate$pop)

df_mxstate$value <- df_mxstate$enferm
mxstate_choropleth(df_mxstate,
                   title = " Casos de COVID-19 por estado al 3 de abril") 

df_mxstate <- merge(df_mxstate, enfermosdata1, by.x = c("region"), by.y = c("Region"), all.x=TRUE)
df_mxstate[is.na(df_mxstate)] <- 0

df_mxstate$value <- df_mxstate$enferm1
mxstate_choropleth(df_mxstate,
                   title = " Casos de COVID-19 por estado al 4 de marzo") 

df_mxstate$x100 <- (df_mxstate$enferm*100000/df_mxstate$pop)

df_mxstate$value <- df_mxstate$x100
mxstate_choropleth(df_mxstate,
                   title = " Casos de COVID-19 por estado al 3 de abril x cada 1000hbs") 
