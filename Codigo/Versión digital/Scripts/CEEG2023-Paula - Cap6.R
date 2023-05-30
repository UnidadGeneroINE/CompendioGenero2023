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
# 6.1.	Participación en los Consejos de Desarrollo por sexo, según cargo
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\PARTICIPACIÓN_SOCIOPOLÍTICA\\Consejos.xlsx")
c6_01 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "ConsejosSuma"))
g6_01 <- tablaLaTeX(c6_01, nombre_columnas = c("", "Mujeres", "Hombres", 
                                               "Mujeres", "Hombres", 
                                               "Mujeres", "Hombres", 
                                               "Mujeres", "Hombres", 
                                               "Mujeres", "Hombres"),
                    nombre_grupos = c(" " = 1, "2018" = 2, "2019" = 2,
                                      "2020" = 2, "2021" = 2, "2022" = 2),
                    ruta = paste0(directorioGraficas, "g6_01.tex"))

################################################################################
# 6.2.	Personas electas para el Organismo Legislativo por sexo
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\PARTICIPACIÓN_SOCIOPOLÍTICA\\Diputados 2023.xlsx")
c6_02 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "TOTALES")) %>%
  rename(x = Año, y = Casos, z = Sexo)
c6_02$z <- factor(c6_02$z, levels = c("Mujeres", "Hombres"))
g6_02 <- graficaAnillosMultiples(c6_02, leyenda_circulos = FALSE, leyenda = "lado")
exportarLatex(graph = g6_02, nombre = paste0(directorioGraficas, "g6_02.tex"))

################################################################################
# 6.4.	Mujeres magistradas en el Organismo Judicial
##################################################L##############################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\PARTICIPACIÓN_SOCIOPOLÍTICA\\6.4_MUJERES_MAGISTRADAS_EN_EL_OJ.xlsx")
c6_04 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "6.4Limpia"))
g6_04 <- graficaApilada(c6_04, tipo = "columna", leyenda = "abajo")
exportarLatex(graph = g6_04, nombre = paste0(directorioGraficas, "g6_04.tex"))

################################################################################
# 6.7.	Mujeres electas para alcaldías 
################################################################################

x <- c("Mujeres", "Hombres")
y <- c(8, 332)
c6_07 <- data.frame(x, y)
c6_07$x <- factor(c6_07$x, levels = c("Mujeres", "Hombres"))

# Exportar a latex
g6_07 <- graficaAnillo(data = c6_07, nombre = paste0(directorioGraficas, "g6_07.tex"), preambulo = F)

################################################################################
# ANEXOS
################################################################################

xlsxFile1 <- paste0(directorioBases, "datos_administrativos\\Indicadores_de_Género\\PARTICIPACIÓN_SOCIOPOLÍTICA\\Consejos.xlsx")
anexo6_01A <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "Consejos18-19"))
anexo6_01A <- tablaLaTeX(anexo6_01A, nombre_columnas = c("", "Mujeres", "Hombres", 
                                                       "Mujeres", "Hombres", 
                                                       "Mujeres", "Hombres", 
                                                       "Mujeres", "Hombres"),
                        nombre_grupos = c(" " = 1, "Titulares" = 2, "Suplentes" = 2,
                                          "Titulares" = 2, "Suplentes" = 2),
                        ruta = paste0(directorioGraficas, "anexo6_01A.tex"))

anexo6_01B <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "Consejos20-21"))
anexo6_01B <- tablaLaTeX(anexo6_01B, nombre_columnas = c("", "Mujeres", "Hombres", 
                                                         "Mujeres", "Hombres", 
                                                         "Mujeres", "Hombres", 
                                                         "Mujeres", "Hombres"),
                         nombre_grupos = c(" " = 1, "Titulares" = 2, "Suplentes" = 2,
                                           "Titulares" = 2, "Suplentes" = 2),
                         ruta = paste0(directorioGraficas, "anexo6_01B.tex"))

anexo6_01C <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "Consejos22"))
anexo6_01C <- tablaLaTeX(anexo6_01C, nombre_columnas = c("", "Mujeres", "Hombres", 
                                                         "Mujeres", "Hombres"),
                         nombre_grupos = c(" " = 1, "Titulares" = 2, "Suplentes" = 2),
                         ruta = paste0(directorioGraficas, "anexo6_01C.tex"))
