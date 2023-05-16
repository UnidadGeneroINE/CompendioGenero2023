# READ ME: SCRIPT PARA COMPENDIO ESTADÍSTICO CON ENFOQUE DE GÉNERO 2022
# AUTOR: Aurora Monzon - ABRIL 2023

#################################################################
##            	                                               ##
##                                                             ##
##                   CONFIGURACIÓN INICIAL                     ##
##                                                             ##
##                                                             ##
#################################################################

# Cargando las bibliotecas necesarias
library(funcionesINE)
library(foreign)
library(dplyr)
library(bannerCommenter)
library(openxlsx)
library(remotes)
library(packcircles)
library(ggplot2)
library(tidyr) # install.packages("tidyverse")

# Biblioteca de librerias del capitulo 4
#library(gt) #install.packages("gt")
#library(tables) #install.packages("tables")
library(xtable) #install.packages("Xtables")
library(kableExtra) #install.packages("kableExtra")
library(knitr) #install.packages("knitr")
library(colorspace) #install.packages("colorspace")
library(tibble) #install.packages("tibble")



# Rutas del directorio de bases y gráficas
directorioBases <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\"
BasesSalud <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\datos_administrativos\\Indicadores_de_Género\\SALUD\\"
directorioGraficas <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Graficas\\"


############################################################################
###                                                                      ###
###                              CAPÍTULO 2                              ###
###                             Salud                                    ###
###                                                                      ###
############################################################################

################################################################################
# 2.1.	Número de casos de mujeres embarazadas entre 10 y 55 años atendidas por
# el sistema de salud pública (serie histórica de 2018 a 2022)                                             #
################################################################################
Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_01 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_1")) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, 
         "Grupos de edad" = Edad)

# Enviar a Latex
Tabla2_01 <- tablaLaTeX(c2_01, ruta = paste0(directorioGraficas, "Tabla2_01.tex"))

################################################################################
# 2.2.	Población entre 15 y 49 años que han usado algún método de             #
# planificación familiar por sexo                                              #
################################################################################

################################################################################
# 2.3.	Población entre 15 y 49 años que han usado algún método de             #
# planificación familiar por tipo de método                                    #
################################################################################

################################################################################
# 2.4.	Nacimientos por edad de la madre, según grupos de edad                 #
################################################################################
Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_04 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_4")) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, 
         "Grupos de edad" = Grupos.de.edad)

# Enviar a Latex 
Tabla2_04 <- tablaLaTeX(c2_04, ruta = paste0(directorioGraficas, "Tabla2_04.tex"))
################################################################################
# 2.5.	Porcentaje de partos atendidos, por tipo de asistencia                 #
# (serie histórica de 2018 a 2022)                                             #
################################################################################

Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_05 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_5"))%>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, 
         "Tipo de Asistencia" = "Año")

# Enviar a Latex
Tabla2_05 <- tablaLaTeX(c2_05, ruta = paste0(directorioGraficas, "Tabla2_05.tex"))

################################################################################
# 2.6.	Porcentaje de personas notificadas con VIH/SIDA, por sexo, según       #
# grupos de edad (serie histórica de 2018 a 2023)                              #
################################################################################

Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_06 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_6"))

c2_06$z <- factor(c2_06$z, levels = c("Mujeres", "Hombres"))

#Grafica g2_06
g2_06 <- graficaApilada(c2_06, categoria_leyenda = "", tipo = "columna", 
                                     decimales = TRUE, leyenda = "arriba")

# Enviar a Latex
exportarLatex(nombre = paste0(directorioGraficas, "g2_06.tex"), graph = g2_06)

################################################################################
# 2.7.	Número de casos de mujeres seropositivas embarazadas entre 15 y 49 años# 
# por Pueblo (serie histórica de 2018 a 2022)                                  #
################################################################################
Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_07 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_7")) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, "2022" = X2022)

# Enviar a Latex
Tabla2_07 <- tablaLaTeX(c2_07, ruta = paste0(directorioGraficas, "Tabla2_07.tex"))


################################################################################
# 2.8.	Tasa de mortalidad materna (serie histórica de 2018 a 2022)                                              #
################################################################################
Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_08 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_8"))

#Grafica g2_08
g2_08 <- graficaLinea(c2_08, inicio = -1, ancho = 1.5, precision=2, escala = "normal", rotar = F, final = 1)

# Enviar a Latex
exportarLatex(nombre = paste0(directorioGraficas,"g2_08.tex"),
              graph = g2_08, preambulo = F)

################################################################################
# 2.9.	Número de casos mortalidad materna según causa de muerte                     #
################################################################################
Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")

c2_09 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_9")) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021) %>%
  rename("Causas de Muerte" = Causa.de.muerte)

# Enviar a Latex 
Tabla2_09 <- tablaLaTeX(c2_09, ruta = paste0(directorioGraficas, "Tabla2_09.tex"))

################################################################################
# 2.10.	Defunciones por sexo y Pueblo, según 10 principales causas de muerte   #
# (comparar 2018 y 2022)                                                       #
################################################################################

Datos_Salud <- paste0(BasesSalud, "CapSalud.xlsx")
c2_10 <- data.frame(read.xlsx(xlsxFile = Datos_Salud, sheet = "2_10")) %>%
  rename("Causa de Muerte" = Causa.de.muerte)


# Enviar a Latex 
Tabla2_10 <- tablaLaTeX(c2_10,nombre_grupos = c(" ", "Pueblo de pertenencia" = 6), 
                        opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla2_10.tex"))


