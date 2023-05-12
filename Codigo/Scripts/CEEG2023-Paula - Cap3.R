# READ ME: SCRIPT PARA COMPENDIO ESTADÍSTICO CON ENFOQUE DE GÉNERO 2022
# AUTOR: PAULA GÁLVEZ MOLINA - MAYO 2023


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
# Nuevas bibliotecas añadidas en cap 1
library(knitr)
library(kableExtra) #devtools::install_github("haozhu233/kableExtra")
library(colorspace)
library(tidyr)


# Rutas del directorio de bases y gráficas
directorioBasesCenso <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\Proyectos\\Compendio estadístico de género\\Bases\\"
directorioBases <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\GitHub\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\GitHub\\CompendioGenero2023\\Codigo\\Graficas\\"

# Lectura de bases de datos
hogaresCenso <- read.spss(paste0(directorioBasesCenso, "HOGAR_BDP.sav"), to.data.frame =T)
personasCenso <- read.spss(paste0(directorioBasesCenso, "PERSONA_BDP.sav"), 
                      to.data.frame =T)
migracionesCenso <- read.spss(paste0(directorioBasesCenso, "MIGRACION_BDP.sav"), 
                         to.data.frame =T)

viviendasCenso <- read.spss(paste0(directorioBasesCenso, "VIVIENDA_BDP.sav"), 
                       to.data.frame =T)

personasENEI <- read.spss(paste0(directorioBases, "ENEI2022INE.sav"), 
                             to.data.frame =T)

hogaresENEI <- read.spss(paste0(directorioBases, "ENEI2022INE_HOGARES.sav"), 
                             to.data.frame =T)

personasENEI2018 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_PERSONAS.sav"), 
                             to.data.frame =T)

hogaresENEI2018 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_HOGARES.sav"), 
                            to.data.frame =T)

# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento. color1 es el principal, color2 es el secundariom del documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255)) 

################################################################################
# DEFINIR CONSTANTES
################################################################################

# Población total para censo 2018
poblacion2018Censo <- nrow(personasCenso)

# Población total para ENEI 2018
poblacion2018ENEI <- sum(personasENEI2018$factor)

# Población total para ENEI 2022
poblacion2022 <- sum(personasENEI$factor)

# Total jefaturas de hogar ENEI 2022
jefaturas_de_hogar_22 <- filter(personasENEI, P03A05 == "Jefe (a) del hogar")
total_jefaturas_de_hogar_22 <- sum(jefaturas_de_hogar_22$factor)

# Agregando columna grupoEdad que indica el grupo de edad al que pertenece
# dependiendo de la edad reportada en el censo o ENEI
personasCenso <- personasCenso %>%
  mutate(grupoEdad = case_when( PCP7 < 15 ~ '0-14',
                                 PCP7 > 14 & PCP7 < 30 ~ '15-29',
                                 PCP7 > 29 & PCP7 < 65 ~ '30-64',
                                 PCP7 > 64 ~ '65+'))

personasENEI2018 <- personasENEI2018 %>%
  mutate(grupoEdad = case_when( PPA03 < 15 ~ '0-14',
                                 PPA03 > 14 & PPA03 < 30 ~ '15-29',
                                 PPA03 > 29 & PPA03 < 65 ~ '30-64',
                                 PPA03 > 64 ~ '65+'))

personasENEI <- personasENEI %>%
  mutate(grupoEdad = case_when( P03A03 < 15 ~ '0-14',
                                 P03A03 > 14 & P03A03 < 30 ~ '15-29',
                                 P03A03 > 29 & P03A03 < 65 ~ '30-64',
                                 P03A03 > 64 ~ '65+'))

# Indica el orden en el que se deben mostrar los sexos
personasCenso$PCP6 <- factor(personasCenso$PCP6, levels = c("Mujer", "Hombre"))
personasENEI$P03A02 <- factor(personasENEI$P03A02, levels = c("Mujer", "Hombre"))
personasENEI2018$PPA02 <- factor(personasENEI2018$PPA02, levels = c("Mujer", "Hombre"))
personasCenso$grupoEdad <- factor(personasCenso$grupoEdad, levels = c("0-14", "15-29", "30-64", "65+"))
personasENEI$grupoEdad <- factor(personasENEI$grupoEdad, levels = c("0-14", "15-29", "30-64", "65+"))
personasENEI2018$grupoEdad <- factor(personasENEI2018$grupoEdad, levels = c("0-14", "15-29", "30-64", "65+"))

# Contando personas de 15 años o más
# a partir de haber verificado que todos los NAs en la P04A01 son 
# en personas menores de 7 años y filtrando luego la edad

genteMayorQue14_2022 <- sum(filter(personasENEI, !is.na(P04A01) & P03A03 > 14)$factor)
genteMayorQue14_2018 <- sum(filter(personasENEI2018, !is.na(P03A01) & PPA03)$FACTOR)

############################################################################
###                                                                      ###
###                              CAPÍTULO 3                              ###
###                              Educación                               ###
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

g0_00 <- graficaColCategorias(data = poblacion_por_pueblos, ruta = paste0(directorioGraficas,"g0_00.tex"), 
                              etiquetas = "h")

################################################################################
# 3.1.	Tasa de alfabetismo en la población de 15 años o más por sexo, 
# según grupos de edad
################################################################################

# Contando universo para el tema de alfabetismo (personas de 15 años o más)
# a partir de haber verificado que todos los NAs en la P04A01 son 
# en personas menores de 7 años y filtrando luego la edad

genteMayorQue14_2022 <- sum(filter(personasENEI, !is.na(P04A01) & P03A03 > 14)$factor)
genteMayorQue14_2018 <- sum(filter(personasENEI2018, !is.na(P03A01) & PPA03)$FACTOR)

# Calculando el indicador para en 2022
alfabetismo2022 <- filter(personasENEI, P04A01 == "Sí" & P03A03 > 14) %>%
  group_by(P03A02, grupoEdad) %>%
  summarize(y = sum(factor)/genteMayorQue14_2022 * 100) %>%
  cbind(x = c("2022", "2022", "2022", "2022", "2022", "2022")) %>%
  rename(z = P03A02, w = grupoEdad)

# Calculando el indicador para en 2018
alfabetismo2018 <- filter(personasENEI2018, P03A01 == "Si" & PPA03 > 14) %>%
  group_by(PPA02, grupoEdad) %>%
  summarize(y = sum(FACTOR)/genteMayorQue14_2018 * 100) %>%
  cbind(x = c("2018", "2018", "2018", "2018", "2018", "2018")) %>%
  rename(z = PPA02, w = grupoEdad)

c3_01 <- rbind(alfabetismo2018, alfabetismo2022)

g3_01 <- graficaCategoriasApiladas(data = c3_01)
exportarLatex(nombre = paste0(directorioGraficas, "g3_01.tex"), graph = g3_01)

################################################################################
# 3.2.	Tasa de alfabetismo en la población de 15 años o más por sexo, 
# según dominio de estudio
################################################################################

# Calculando el indicador para en 2022
alfabetismo2022 <- filter(personasENEI, P04A01 == "Sí" & P03A03 > 14) %>%
  group_by(P03A02, dominio) %>%
  summarize(y = sum(factor)/genteMayorQue14_2022 * 100) %>%
  cbind(x = c("2022", "2022", "2022", "2022", "2022", "2022")) %>%
  rename(z = P03A02, w = dominio)

# Calculando el indicador para en 2018
alfabetismo2018 <- filter(personasENEI2018, P03A01 == "Si" & PPA03 > 14) %>%
  group_by(PPA02, DOMINIO) %>%
  summarize(y = sum(FACTOR)/genteMayorQue14_2018 * 100) %>%
  cbind(x = c("2018", "2018", "2018", "2018", "2018", "2018")) %>%
  rename(z = PPA02, w = DOMINIO)

c3_02 <- rbind(alfabetismo2018, alfabetismo2022)

g3_02 <- graficaCategoriasApiladas(data = c3_02)
exportarLatex(nombre = paste0(directorioGraficas, "g3_02.tex"), graph = g3_02)

################################################################################
# 3.3.	Nivel educativo de la población de 15 años o más por sexo
################################################################################
# Calculando el indicador para en 2022
alfabetismo2022 <- filter(personasENEI, P04A01 == "Sí" & P03A03 > 14) %>%
  group_by(P03A02, dominio) %>%
  summarize(y = sum(factor)/genteMayorQue14_2022 * 100) %>%
  cbind(x = c("2022", "2022", "2022", "2022", "2022", "2022")) %>%
  rename(z = P03A02, w = dominio)

# Calculando el indicador para en 2018
alfabetismo2018 <- filter(personasENEI2018, P03A01 == "Si" & PPA03 > 14) %>%
  group_by(PPA02, DOMINIO) %>%
  summarize(y = sum(FACTOR)/genteMayorQue14_2018 * 100) %>%
  cbind(x = c("2018", "2018", "2018", "2018", "2018", "2018")) %>%
  rename(z = PPA02, w = DOMINIO)