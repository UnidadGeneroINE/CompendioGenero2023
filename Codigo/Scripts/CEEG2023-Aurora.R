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

# Rutas del directorio de bases y gráficas
directorioBases <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Githob\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "\\C:Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Githob\\CompendioGenero2023\\Codigo\\Graficas\\"

# Lectura de bases de datos
hogaresENEI <- read.spss(paste0(directorioBases, "BD_HOGARES.sav"),
                     use.value.labels = T,
                     to.data.frame = T)
personasENEI <- read.spss(paste0(directorioBases, "BASE_ENEI_22_PERSONAS.sav"), 
                          to.data.frame =T)
personasENEI_18 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_PERSONAS.sav"), 
                          to.data.frame =T)

# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento. color1 es el principal, color2 es el secundariom del documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255)) 


################################################################################
# DEFINIR CONSTANTES
################################################################################

# Agregando columna quinqueneo que indica el grupo de edad al que pertenece
# dependiendo de la edad reportada en el censo
quinqueneos <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', 
                 '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69',
                 '70-74', '75-79', '80-84', '85-89', '90-94', '95-99', '100+')
  personasENEI <- personasENEI %>%
    #no identifico quebien que quiere decir al ser iguales, cuando llame a 
    #quinqueneo jala ambos o se debe crear quiqueneo segun los años de la 
    #encuenta
  mutate(quinqueneo = case_when( P03A03 < 5 ~ '0-4',
                                 P03A03 > 4 & P03A03 < 10 ~ '5-9',
                                 P03A03 > 9 & P03A03 < 15 ~ '10-14',
                                 P03A03 > 14 & P03A03 < 20 ~ '15-19',
                                 P03A03 > 19 & P03A03 < 25 ~ '20-24',
                                 P03A03 > 24 & P03A03 < 30 ~ '25-29',
                                 P03A03 > 29 & P03A03 < 35 ~ '30-34',
                                 P03A03 > 34 & P03A03 < 40 ~ '35-39',
                                 P03A03 > 39 & P03A03 < 45 ~ '40-44',
                                 P03A03 > 44 & P03A03 < 50 ~ '45-49',
                                 P03A03 > 49 & P03A03 < 55 ~ '50-54',
                                 P03A03 > 54 & P03A03 < 60 ~ '55-59',
                                 P03A03 > 59 & P03A03 < 65 ~ '60-64',
                                 P03A03 > 64 & P03A03 < 70 ~ '65-69',
                                 P03A03 > 69 & P03A03 < 75 ~ '70-74',
                                 P03A03 > 74 & P03A03 < 80 ~ '75-79',
                                 P03A03 > 79 & P03A03 < 85 ~ '80-84',
                                 P03A03 > 84 & P03A03 < 90 ~ '85-89',
                                 P03A03 > 89 & P03A03 < 95 ~ '90-94',
                                 P03A03 > 94 & P03A03 < 100 ~ '95-99',
                                 P03A03 > 99 ~ '100+'))

############################################################################
###                                                                      ###
###                              CAPÍTULO 4                              ###
###                             economía                                 ###
###                                                                      ###
############################################################################

################################################################################
# 4.6.	Población económicamente activa por sexo, según Pueblos y grupos de edad
#(comparar 2018 y 2022)
################################################################################

# Cálculo de PET 2022
#
# Se filtran las personas mayores a 14 años, que son las que sí se pueden considerar como PET
#
PET_22 <- filter(personasENEI, P03A03 > 14)
  
PET_18 <- filter(personasENEI_18, PPA03 > 14)
  
# Cálculo de PEA 2022
# cálculo de desocupados y ocupados 2022
# desocupados 2022 <- filter(PET_22, P05B01 == 'Sí)
desocupados_22 <- filter(PET_22, P05B01 == 'Sí')

# numero de desocupados 2022 <- sum(desocupados_22$factor))
num_desocupados_22 = sum(desocupados_22$factor)

# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados

# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
ocupados_22 <- filter(PET_22, !is.na(P05C01))
num_ocupados_22 = sum(ocupados_22$Factor)
PEA_22 <- rbind(desocupados_22, ocupados_22)
# Se crea la contante PEAtotal 2022 apartir de la sumatoria de factores del
PEATOTAL_22 <- sum(PEA_22$factor)


# Se debe crear el cuadro apartir de la base de PEA 2022 limpia se selecciona
# 
c4_06 <- PEA_22 %>%
  select(P03A02, quinqueneo, P03A06, factor) %>%
  group_by(quinqueneo,P03A02, P03A06) %>%
  summarise( PEA = sum(factor)) %>%
  mutate( Porcentaje_PEA_22 = PEA / PEATOTAL_22 * 100 ) %>% 
  rename( Sexo = P03A02) %>%
  rename( Edad = quinqueneo) %>%
  rename( Pueblo = P03A06)

x <- c("Urbano", "Rural", "Urbano Metropolitano")
Mujer <- c(as.numeric(c4_02[2,3]/PO_2022*100), as.numeric(c4_02[4,3]/PO_2022*100),
           as.numeric(c4_02[6,3]/PO_2022*100))
Hombre <- c(as.numeric(c4_02[1,3]/PO_2022*100), as.numeric(c4_02[3,3]/PO_2022*100),
            as.numeric(c4_02[5,3]/PO_2022*100))
  
  
# Cálculo de PEA 2018
# Cálculo de PEA 2018
# cálculo de desocupados y ocupados 2022
# desocupados 2022 <- filter(PET, P05B04 >=0 | P05B01 == 'Sí')
desocupados_18 <- filter(PET_18, P05B01 == 'Sí')



# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados

# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
ocupados_18 <- filter(PET_18, !is.na(P05C01))
num_ocupados_18 = sum(ocupados_18$Factor)
totales_PEA_18 <- rbind(desocupados_22, ocupados_18)


# PEA 2018 - 2022
PEATOTAL = sum(totales_PEA$Factor) # número de personas económicamente activas (PEA)
x <- c('PEA 2018', 'PEA 2022')
y <- c(PEA_2021, PEATOTAL)
c4_06 <- data.frame(x, y)
# g4_06 <- graficaLinea(data = c3_02)
# g4_06 <- graficaBar(data = c3_02, escala="millones")
g4_06 <- graficaPackedBubbles(c4_06)

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
# 4.6.	Población económicamente activa por sexo, según Pueblos y grupos de edad
#(comparar 2018 y 2022) 
################################################################################

c1_01 <- personasCenso %>%
  group_by(PCP6, quinqueneo) %>%
  summarize(y = n()) %>%
  rename(z = PCP6, x = quinqueneo) %>%
  arrange(factor(x, levels = quinqueneos))%>%
  arrange(factor(c1_01$z, levels = c("Mujer", "Hombre")))

# Indica el orden en el que se debe mostrar los grupos quinquenales
c1_01$x <- factor(c1_01$x, levels = quinqueneos)
c1_01$z <- factor(c1_01$z, levels = c("Mujer", "Hombre"))

g1_01 <- graficaPiramide(data = c1_01, escala = 1000)
g1_01 <- exportarLatex(nombre = paste0(directorioGraficas, "g1_01.tex"), graph = g1_01)

################################################################################
# 1.2.	Población por sexo, según dominio de estudio
################################################################################

c1_02 <- personasCenso %>%
  group_by(PCP6, AREA) %>%
  summarize(y = n()/poblacion2018 *100) %>%
  rename(Sexo = PCP6)

x <- c("Hombre Urbana", "Hombre Rural", "Mujer Urbana", "Mujer Rural")
y <- c(as.numeric(c1_02[1,3]), as.numeric(c1_02[2,3]), as.numeric(c1_02[3,3]), as.numeric(c1_02[4,3]))

c1_02 <- data.frame(x,y)

g1_02 <- graficaPackedBubbles(data = c1_02)
g1_02 <- exportarLatex(nombre = paste0(directorioGraficas, "g1_02.tex"), graph = g1_02)




############################################################################
###                                                                      ###
###                              CAPÍTULO 4                              ###
###                               economia                               ###
###                                                                      ###
############################################################################

################################################################################
#4.5.	Población económicamente activa por sexo, según dominio de estudio
#comparar 2018 y 2022)
################################################################################



