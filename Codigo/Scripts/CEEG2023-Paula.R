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
# documento color1 es el principal, color2 es el secundario
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255)) 


################################################################################
# DEFINIR CONSTANTES
################################################################################

poblacion2018 <- nrow(personasCenso)

personasCenso <- personasCenso %>%
  mutate(quinqueneo = case_when( PCP7 < 5 ~ '0-4',
                                 PCP7 > 4 & PCP7 < 10 ~ '5-9',
                                 PCP7 > 9 & PCP7 < 15 ~ '10-14',
                                 PCP7 > 14 & PCP7 < 20 ~ '15-19',
                                 PCP7 > 19 & PCP7 < 25 ~ '20-24',
                                 PCP7 > 24 & PCP7 < 30 ~ '25-29',
                                 PCP7 > 29 & PCP7 < 35 ~ '30-34',
                                 PCP7 > 34 & PCP7 < 40 ~ '35-39',
                                 PCP7 > 39 & PCP7 < 45 ~ '40-44',
                                 PCP7 > 44 & PCP7 < 50 ~ '45-49',
                                 PCP7 > 49 & PCP7 < 55 ~ '50-54',
                                 PCP7 > 54 & PCP7 < 60 ~ '55-59',
                                 PCP7 > 59 & PCP7 < 65 ~ '60-64',
                                 PCP7 > 64 & PCP7 < 70 ~ '65-69',
                                 PCP7 > 69 & PCP7 < 75 ~ '70-74',
                                 PCP7 > 74 & PCP7 < 80 ~ '75-79',
                                 PCP7 > 79 & PCP7 < 85 ~ '80-84',
                                 PCP7 > 84 & PCP7 < 90 ~ '85-89',
                                 PCP7 > 89 & PCP7 < 95 ~ '90-94',
                                 PCP7 > 94 & PCP7 < 100 ~ '95-99',
                                 PCP7 > 99 ~ '100+'))

############################################################################
###                                                                      ###
###                              CAPÍTULO 1                              ###
###                        Población y demografía                        ###
###                                                                      ###
############################################################################

################################################################################
# 0. Porcentaje de población según sexo por pueblos (MUESTRA)
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
################################################################################
# 1.1.	Población por sexo, según grupo de edad simple
################################################################################

poblacion_por_edad <- personasCenso %>%
  group_by(PCP6, quinqueneo) %>%
  summarize(porcentaje = n()/poblacion2018 * 100)

# load sample data
sample_data <- read.csv(paste0(directorioBases, "Population.CSV"))

# load library ggplot2 and dplyr
library(ggplot2)
library(dplyr)

# change male population to negative
sample_data %>%mutate(
  population = ifelse(gender=="M", population*(-1),
                      population*1))%>%
  ggplot(aes(x = age,y = population)) +
  geom_bar(stat = "identity") +
  coord_flip()
