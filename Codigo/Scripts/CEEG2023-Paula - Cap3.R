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
library(tidyverse) #install.packages("tidyverse")

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
genteMayorQue6_2022 <- sum(filter(personasENEI, P03A03 > 6)$factor)
genteMayorQue6_2018 <- sum(filter(personasENEI2018, PPA03 > 6)$FACTOR)

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
educativo2022 <- filter(personasENEI, P03A03 > 14) %>%
  mutate(P04A05A = case_when(P04A05A == "Maestría" ~ "Posgrado",
                             P04A05A == "Doctorado" ~ "Posgrado",
                            TRUE ~ P04A05A)) %>%
  group_by(P03A02, P04A05A) %>%
  summarize(Porcentaje = sum(factor)/genteMayorQue14_2022 * 100) %>%
  rename(Sexo = P03A02, Nivel = P04A05A) %>%
  pivot_wider(names_from = Sexo, values_from = Porcentaje)

# Calculando el indicador para en 2018
educativo2018 <- filter(personasENEI2018, PPA03 > 14) %>%
  group_by(PPA02, P03A05A) %>%
  mutate(P03A05A = case_when(P03A05A == "Maestría" ~ "Posgrado",
                             P03A05A == "Doctorado" ~ "Posgrado",
                             TRUE ~ P03A05A)) %>%
  summarize(Porcentaje = sum(FACTOR)/genteMayorQue14_2018 * 100) %>%
  rename(Sexo = PPA02, Nivel = P03A05A) %>%
  pivot_wider(names_from = Sexo, values_from = Porcentaje) %>%
  rename(Mujeres_2018 = Mujer, 
         Hombres_2018 = Hombre)

c3_03 <- cbind(educativo2018, select(educativo2022, -Nivel))

# Agregando niveles conforme a orden cronológcio  a Nivel Educativo
c3_03$Nivel <- factor(c3_03$Nivel, levels = c("Ninguno", "Preprimaria", "Primaria", "Básico", "Diversificado", "Superior", "Posgrado"))
c3_03 <- arrange(c3_03, c3_03$Nivel)

g3_03 <- tablaLaTeX(data = c3_03, 
                    nombre_columnas = c("Nivel Educativo", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2022" = 2),
                    ruta = paste0(directorioGraficas, "g3_03.tex"))

################################################################################
# 3.4.	Tasa neta de escolaridad en el nivel primario por sexo 
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_04 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.4"))
g3_04 <- graficaDobleLinea(c3_04, ruta = paste0(directorioGraficas,"g3_04.tex"), inicio = 89.2,  fin = 96.2)

################################################################################
# 3.5.	Tasa neta de escolaridad en el nivel primario por sexo, según departamento 
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_05 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.5"))
g3_05 <- tablaLaTeX(c3_05,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2, "2021" = 2),
                    ruta = paste0(directorioGraficas,"g3_05.tex"))

################################################################################
# 3.6.	Tasa neta de escolaridad en el ciclo básico por sexo
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_06 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.6"))
g3_06 <- graficaDobleLinea(c3_06, ruta = paste0(directorioGraficas,"g3_06.tex"), inicio = 42.8,  fin = 51.1)

################################################################################
# 3.7.	Tasa neta de escolaridad en el ciclo básico por sexo, según departamento 
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_07 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.7"))
g3_07 <- tablaLaTeX(c3_07,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2, "2021" = 2),
                    ruta = paste0(directorioGraficas,"g3_07.tex"))

################################################################################
# 3.8.	Tasa neta de escolaridad en el ciclo diversificado por sexo
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_08 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.8"))
g3_08 <- graficaDobleLinea(c3_08, ruta = paste0(directorioGraficas,"g3_08.tex"), inicio = 20.5,  fin = 27.5)

################################################################################
# 3.9.	Tasa neta de escolaridad en el ciclo diversificado por sexo, según departamento
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_09 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.9"))
g3_09 <- tablaLaTeX(c3_07,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2, "2021" = 2),
                    ruta = paste0(directorioGraficas,"g3_09.tex"))

################################################################################
# 3.10.	Tasa de deserción en el nivel primario por sexo (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_10 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.10")) %>%
  rename(x = Año, y = Tasa, z = Sexo)
c3_10$z <- factor(c3_10$z, levels = c("Mujer", "Hombre"))
g3_10 <- graficaAnillosMultiples(c3_10, leyenda_circulos = FALSE, leyenda = "lado")
exportarLatex(g3_10, nombre = paste0(directorioGraficas,"g3_10.tex"))

################################################################################
# 3.11.	Tasa de deserción en el nivel primario por sexo, según departamento (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_11 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.11"))
g3_11 <- tablaLaTeX(c3_11,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2),
                    ruta = paste0(directorioGraficas,"g3_11.tex"))

################################################################################
# 3.12.	Tasa de deserción en el nivel básico por sexo (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_12 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.12")) %>%
  rename(x = Año, y = Tasa, z = Sexo)
c3_12$z <- factor(c3_12$z, levels = c("Mujer", "Hombre"))
g3_12 <- graficaCategorias(c3_12, tipo = "barra")
exportarLatex(g3_12, nombre = paste0(directorioGraficas,"g3_12.tex"))

################################################################################
# 3.13.	Tasa de deserción en el nivel básico por sexo, según departamento (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_13 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.13"))
g3_13 <- tablaLaTeX(c3_13,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2),
                    ruta = paste0(directorioGraficas,"g3_13.tex"))

################################################################################
# 3.14.	Tasa de deserción en el ciclo diversificado por sexo (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_14 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.14")) %>%
  rename(x = Año, y = Tasa, z = Sexo)
c3_14$z <- factor(c3_14$z, levels = c("Mujer", "Hombre"))
g3_14 <- graficaAnillosMultiples(c3_14, leyenda_circulos = FALSE, leyenda = "lado")
exportarLatex(g3_14, nombre = paste0(directorioGraficas,"g3_14.tex"))

################################################################################
# 3.15.	Tasa de deserción en el nivel diversificado por sexo, según departamento (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_15 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.15"))
g3_15 <- tablaLaTeX(c3_15,
                    nombre_columnas = c("Departamento", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2, "2020" = 2),
                    ruta = paste0(directorioGraficas,"g3_15.tex"))

################################################################################
# 3.16.	Proporción de la población matriculada en la universidad por sexo, según tipo de universidad (pública o privada) (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_16 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.16"))
g3_16 <- tablaLaTeX(c3_16,
                    nombre_columnas = c("Tipo", "Mujeres", "Hombres", "Ignorado", "Mujeres", "Hombres", "Ignorado"),
                    nombre_grupos = c(" " = 1, "2021" = 3, "2022*" = 3),
                    ruta = paste0(directorioGraficas,"g3_16.tex"))

################################################################################
# 3.17.	Proporción de la población matriculada en la universidad pública por sexo, según nivel (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_17 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.17")) %>%
  rename(x = Nivel, w = Año, y = Porcentaje, z = Sexo)
c3_17$z <- factor(c3_17$z, levels = c("Mujer", "Hombre"))
g3_17 <- graficaCategoriasApiladas(c3_17, leyenda = "abajo", tipo = "barra")
exportarLatex(g3_17, nombre = paste0(directorioGraficas,"g3_17.tex"))

################################################################################
# 3.18.	Proporción de la población matriculada en universidades privadas por sexo, según nivel (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_18 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.18"))
g3_18 <- tablaLaTeX(c3_18,
                    nombre_columnas = c("Nivel", "Mujeres", "Hombres", "Ignorado", "Mujeres", "Hombres", "Ignorado"),
                    nombre_grupos = c(" " = 1, "2021" = 3, "2022*" = 3),
                    ruta = paste0(directorioGraficas,"g3_18.tex"))

################################################################################
# 3.19.	Proporción de la población graduada de la universidad por sexo, según tipo de universidad (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_19 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.19"))
g3_19 <- tablaLaTeX(c3_19,
                    nombre_columnas = c("Tipo", "Mujeres", "Hombres", "Ignorado", "Mujeres", "Hombres", "Ignorado"),
                    nombre_grupos = c(" " = 1, "2021" = 3, "2022*" = 3),
                    ruta = paste0(directorioGraficas,"g3_19.tex"))

################################################################################
# 3.20.	Proporción de la población graduada de la universidad pública por sexo, según nivel (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_20 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.20")) %>%
  rename(x = Año, y = Porcentaje, z = Sexo)
c3_20$z <- factor(c3_20$z, levels = c("Mujer", "Hombre"))
g3_20 <- graficaAnillosMultiples(c3_20, leyenda_circulos = FALSE, leyenda = "lado")
exportarLatex(g3_20, nombre = paste0(directorioGraficas,"g3_20.tex"))

################################################################################
# 3.21.	Proporción de la población graduada de universidades privadas por sexo, según nivel (REVISAR)
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\EDUCACIÓN\\EducaciónSinFormato.xlsx")
c3_21 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "3.21"))
g3_21 <- tablaLaTeX(c3_21,
                    nombre_columnas = c("Tipo", "Mujeres", "Hombres", "Ignorado", "Mujeres", "Hombres", "Ignorado"),
                    nombre_grupos = c(" " = 1, "2021" = 3, "2022*" = 3),
                    ruta = paste0(directorioGraficas,"g3_21.tex"))

###################################################### DATOS EXTRA #############
queerParejasCensos <- personasCenso %>%
  group_by(NUM_VIVIENDA, NUM_HOGAR) %>%
  filter(all(PCP5 %in% c("Jefe o jefa de hogar", "Esposa(o) o pareja"))) %>%
  subset(duplicated(NUM_VIVIENDA) | duplicated(NUM_VIVIENDA, fromLast = TRUE)) %>%
  filter(length(unique(PCP6)) < n()) #%>%
  #select(NUM_VIVIENDA, NUM_HOGAR, PCP5, PCP6) 


queerParejasENEI22 <- personasENEI %>%
group_by(hogar_num) %>%
filter(all(P03A05 %in% c("Jefe (a) del hogar", "Esposo (a) o compañero (a)"))) %>%
subset(duplicated(hogar_num) | duplicated(hogar_num, fromLast = TRUE)) %>%
filter(length(unique(P03A02)) < n()) %>%
select(hogar_num, P03A05, P03A02) 


matrimonioInfantil <- filter(personasCenso,  PCP5 == "Esposa(o) o pareja" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, PCP5) %>%
  summarise(casos = n())

matrimonioInfantil <- filter(personasCenso,  PCP34 != "Soltera(o)" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, PCP5, PCP34, PCP35_A, PCP37, PCP38_C, DEPARTAMENTO) %>%
  summarise(casos = n())

matrimonioInfantil <- filter(personasCenso,  PCP34 != "Soltera(o)" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, PCP34) %>%
  summarise(casos = n())

