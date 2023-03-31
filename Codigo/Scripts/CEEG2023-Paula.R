# READ ME: SCRIPT PARA COMPENDIO ESTADÍSTICO CON ENFOQUE DE GÉNERO 2022
# AUTOR: PAULA GÁLVEZ MOLINA - MARZO 2023


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

# Rutas del directorio de bases y gráficas
directorioBasesCenso <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\Proyectos\\Compendio estadístico de género\\Bases\\"
directorioBases <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\GitHub\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\GitHub\\CompendioGenero2023\\Codigo\\Graficas\\"

# Lectura de bases de datos
hogaresCenso <- read.spss(paste0(directorioBasesCenso, "HOGAR_BDP.sav"), to.data.frame =T)
personasCenso <- read.spss(paste0(directorioBasesCenso, "PERSONA_BDP.sav"), 
                      to.data.frame =T)
migracionesCenso <- read.spss(paste0(directorioBases, "MIGRACION_BDP.sav"), 
                         to.data.frame =T)
viviendasCenso <- read.spss(paste0(directorioBases, "VIVIENDA_BDP.sav"), 
                       to.data.frame =T)
hogaresENEI <- read.spss(paste0(directorioBases, "BD_HOGARES.sav"),
                     use.value.labels = T,
                     to.data.frame = T)
personasENEI <- read.spss(paste0(directorioBases, "BASE_ENEI_22_PERSONAS.sav"), 
                          to.data.frame =T)


# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255)) 
# color1 es el principal, color2 es el secundario

################################################################################
# DEFINIR CONSTANTES
################################################################################

poblacion2018 <- nrow(personasCenso)


############################################################################
###                                                                      ###
###                              CAPÍTULO 1                              ###
###                        Población y demografía                        ###
###                                                                      ###
############################################################################

################################################################################
# 1. Porcentaje de población según sexo por pueblos
################################################################################

poblacion_por_pueblos <- personasCenso %>%
  group_by(PCP12, PCP6) %>%
  summarise(Porcentaje = n()/poblacion2018 * 100) %>%
  rename(Sexo = PCP6, Pueblo = PCP12)

x <- c("Maya", "Garífuna", "Xinka", "Afrodescendiente*", 
       "Ladino", "Extranjero")
Hombres <- c(as.numeric(poblacion_por_pueblos[1,3]), as.numeric(poblacion_por_pueblos[3,3]), 
             as.numeric(poblacion_por_pueblos[5,3]), as.numeric(poblacion_por_pueblos[7,3]),
             as.numeric(poblacion_por_pueblos[9,3]), as.numeric(poblacion_por_pueblos[11,3]))
Mujeres <- c(as.numeric(poblacion_por_pueblos[2,3]), as.numeric(poblacion_por_pueblos[4,3]), 
             as.numeric(poblacion_por_pueblos[6,3]), as.numeric(poblacion_por_pueblos[8,3]),
             as.numeric(poblacion_por_pueblos[10,3]), as.numeric(poblacion_por_pueblos[12,3]))

poblacion_por_pueblos <- data.frame(x, Mujeres, Hombres)

g1_01 <- graficaColCategorias(data = poblacion_por_pueblos, ruta = paste0(directorioGraficas,"g1_01.tex"), 
                              etiquetas = "h")
