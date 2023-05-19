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



# Rutas del directorio de bases y gráficas
directorioBases <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Graficas\\"
directorioCenso <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\BASE_CENSO_2018\\"

# Lectura de bases de datos
hogaresENEI <- read.spss(paste0(directorioBases, "BD_HOGARES.sav"),
                     use.value.labels = T,
                     to.data.frame = T)

personasENEI <- read.spss(paste0(directorioBases, "BASE_ENEI_22_PERSONAS.sav"), 
                          to.data.frame =T)

personasENEI_18 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_PERSONAS.sav"), 
                          to.data.frame =T)

personasCENSO <- read.spss(paste0(directorioCenso, "PERSONA_CENSO_2018.sav"), 
                          to.data.frame =T)

# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento. color1 es el principal, color2 es el secundariom del documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255))


################################################################################
# DEFINIR CONSTANTES
################################################################################

# Indica el orden en el que se deben mostrar los sexos para CENSO 2018

personasCENSO$PCP6 <- factor(personasCENSO$PCP6, levels = c("Mujer", "Hombre"))


EstadoConyugal <- c('Soltera(o)', 'Unida(o)', 'Casada(o)', 'Separada(o)', 'Divorciada(o)', 'Viuda(o)', 'Menor de 10 años')
personasCENSO <- personasCENSO %>%
  mutate( EstadoConyugal = case_when( PCP34 == "Unida(o)" ~ 'Unida(o)',
                                      PCP34 == "Casada(o)" ~ 'Casada(o)',
                                      PCP34 == "Viuda(o)" ~ 'Viuda(o)', 
                                      PCP34 == "Separada(o) de una unión libre" ~ 'Separada(o)*', 
                                      PCP34 == "Divorciada(o)" ~ 'Divorciada(o)',  
                                      PCP34 == "Separada(o) de un matrimonio" ~ 'Separada(o)*',
                                      PCP34 == "Soltera(o)" ~ 'Soltera(o)',
                                      PCP34 == "NA" ~ 'Menor de 10 años'))




############################################################################
###                                                                      ###
###                              CAPÍTULO 5                              ###
###                              Violencia                               ###
###                                                                      ###
############################################################################


################################################################################
# 5.1.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar 
# y Erradicar la Violencia Intrafamiliar por sexo, según departamento de registro 
################################################################################

################################################################################
# 5.2.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar 
# y Erradicar la Violencia Intrafamiliar por sexo, según tipo de agresión sufrida
################################################################################

################################################################################
# 5.3.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar
# y Erradicar la Violencia Intrafamiliar por sexo, según grupos de edad
################################################################################

################################################################################
# 5.4.	Víctimas de violencia intrafamiliar por sexo, según Pueblos 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.5.	Víctimas de violencia intrafamiliar por sexo, según tipo de agresión 
# sufrida (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.6.	Instituciones que prestan atención a víctimas de violencia intrafamiliar,
# según tipo de servicio brindado (período 2018 - 2022)
################################################################################

################################################################################
# 5.7.	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por departamento de registro
################################################################################

################################################################################
# 5.8.	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según grupos de edad 
################################################################################

################################################################################
# 5.9.	Denuncia por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según Pueblos
################################################################################

################################################################################
# 5.10.	Víctimas de violencia contra la mujer, según Pueblos 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.11.	Víctimas de violencia contra la mujer, según tipo de agresión sufrida 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.12.	Muertes violentas por sexo, según causa de muerte y grupos de edad 
# (comparar 2018 y 2022)
################################################################################

################################################################################
# 5.13.	Muertes violentas de mujeres relacionadas con hechos delictivos 
# (serie histórica 2018 - 2022)
################################################################################

################################################################################
# 5.14.	Índice de mortalidad femenina (serie histórica 2018 - 2022)
################################################################################

################################################################################
# 5.15.	Mujeres privadas de libertad por tipo de delito 
# (serie histórica 2018 - 2022)
################################################################################

################################################################################
# 5.16. Estado conyugal de personas de 12 a 17 años, 2018 
################################################################################

#Sebe cargar la base del CENSO PARA HACER ALGUN CAMBIO
c5_16 <- filter(personasCENSO,  EstadoConyugal != "Soltera(o)" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, EstadoConyugal) %>%
  summarise(y = n()) %>% 
  rename(x = PCP7) %>%
  rename(z = PCP6) %>%                                          
  rename(w = EstadoConyugal) %>%                                          
  select(w, x, z, y)

# Se paso las filas de "x" a columnas
c5_16 <- pivot_wider(c5_16, names_from = w, values_from = c(y))                                         

c5_16$z <- factor(c5_16$z, levels = c("Mujer", "Hombre"))

c5_16 <- c5_16 %>%    
  rename("Edad" = x, "Sexo" = z)

# ordenar segun estado conyugal
#c5_16$"Estado Conyugal" <- factor(c5_16$"Estado Conyugal", levels = c("Unida(o)", "Casada(o)", 	
                                                                      #"Separada(o) de una unión libre", "Separada(o) de un matrimonio",
                                                                     #"Divorciada(o)", 	"Viuda(o)")
Tabla5_16 <- tablaLaTeX(c5_16, nombre_grupos = c(" ", " ", "Estado conyugal" = 5),
                        ruta = paste0(directorioGraficas, "Tabla5_16.tex"))




