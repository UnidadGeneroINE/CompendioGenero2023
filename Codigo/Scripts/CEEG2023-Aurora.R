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
library(gt) #install.packages("gt")
library(tables) #install.packages("tables")
library(xtable) #install.packages("Xtables")

# Rutas del directorio de bases y gráficas
directorioBases <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Graficas\\"

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
# Numero de casos en la ENEi 2022
NumeroCasosENEI22 <- nrow(personasENEI)

#Proyeción de población ENEI 2022
poblacionENEI22 <- sum(personasENEI$factor)

# en el capitulo 4 no se puede usar la clasificación de gurpos de edad
# Agregando columna GruposEdad que indica el grupo de edad al que pertenece
# dependiendo de la edad reportada en el censo
GruposEdad <- c('0-14', '15-30', '31-65', '65+')
  personasENEI <- personasENEI %>%
    #no identifico quebien que quiere decir al ser iguales, cuando llame a 
    #GruposEdad jala ambos o se debe crear GruposEdad segun los años de la 
    #encuenta
  mutate(GruposEdad = case_when( P03A03 < 15 ~ '0-14',
                                 P03A03 > 14 & P03A03 < 30 ~ '15-29',
                                 P03A03 > 29 & P03A03 < 65 ~ '30-65',
                                 P03A03 > 64 ~ '65+'))

# Agregando columna quinqueneo que indica el grupo de edad al que pertenece
# dependiendo de la edad reportada en el ENEI
quinqueneos <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', 
                   '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69',
                   '70-74', '75-79', '80-84', '85-89', '90-94', '95-99', '100+')
personasENEI <- personasENEI %>%
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
  
# Indica el orden en el que se deben mostrar los sexos
personasENEI$P03A02 <- factor(personasENEI$P03A02, levels = c("Mujer", "Hombre"))


# codigo para verificar porcetaje 
  print(sum(c4_06_data$PEA))
  print(sum(c4_06_Pueblo_Sexo$Hombre))
  print(sum(c4_06_Pueblo_Sexo$Mujer))
  
############################################################################
###                                                                      ###
###                              CAPÍTULO 4                              ###
###                             economía                                 ###
###                                                                      ###
############################################################################

################################################################################
# 4.1.	Población de 15 años o más por sector económico, según sexo y 
# nivel educativo
################################################################################

# Flitro de base personas ENEI 2022 se selecciono solo a las de Personas en Edad 
# de Trabajar -PET-
PET_22 <- filter(personasENEI, P03A03 > 14)

# Se filtro la base segun la columna formalidas que se compone de la población ocupada
# desagregada segun sector Formal o Informal
c4_01 <- PET_22 %>%
  filter(formalidad == "Formal" | 
           formalidad == "Informal") %>%
  select(P03A02, formalidad, factor) %>%
  group_by(P03A02, formalidad) %>%
  summarise( z = sum(factor)) %>%
  rename(x = formalidad) %>%
  rename(y = P03A02) %>%
  select(x, y, z)

# Se creo la sumatoaria de personas que se encuentra en el sector formal e informal.
TotalSector <- sum(c4_01$z)

x <- c("Formal", "infromal")
Mujer <- c(as.numeric(c4_01[2,3]/TotalSector*100), as.numeric(c4_01[1,3]/TotalSector*100))
Hombre <- c(as.numeric(c4_01[4,3]/TotalSector*100), as.numeric(c4_01[3,3]/TotalSector*100))

c4_01 <- data.frame(x, Mujer, Hombre)

g4_01 <- graficaColCategorias(data = c4_01, ruta = paste0(directorioGraficas,"g4_01.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")

# Verificación de porcentajes
  print(sum(c4_01$Mujer))
  print(sum(c4_01$Hombre))

  
################################################################################
# 4.2.	Carga global de trabajo entre las personas ocupadas de 15 años y más 
# por sexo (comparar 2018 y 2022)
################################################################################  

################################################################################
# 4.3.	Carga global de trabajo de la población de 7 años o más, por sexo, 
# según Pueblos, (comparar 2018 y 2022)
################################################################################  

################################################################################
#  4.4.	Tasa de participación económica por dominio de estudio, según sexo y 
# estado conyugal (serie histórica de 2018 a 2022)
################################################################################  

################################################################################
#  4.5.	Población económicamente activa por sexo, según dominio de estudio 
# (comparar 2018 y 2022)
################################################################################ 
   
################################################################################
# 4.6.	Población económicamente activa por sexo, según Pueblos y grupos de edad
#(comparar 2018 y 2022)
################################################################################

# Cálculo de PET 2022 
# (personas en edad de trabaja = PET)
#
# Se filtran las personas mayores a 14 años, que son las que sí se pueden considerar como PET
# P03A03 = edad de la persona 
PET_22 <- filter(personasENEI, P03A03 > 14)
  
# Cálculo de PEA 2022
# cálculo de desocupados y ocupados 2022
# desocupados 2022 <- filter(PET_22, P05B01 == 'Sí)
# pP05B01 = Sí bUSCO trabajo 
desocupados_22 <- filter(PET_22, P05B01 == 'Sí')

# numero de desocupados 2022 <- sum(desocupados_22$factor))
num_desocupados_22 = sum(desocupados_22$FACTOR)

# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados

# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
# pO5C01 = cantidad de trabajos 
ocupados_22 <- filter(PET_22, !is.na(P05C01))
num_ocupados_22 = sum(ocupados_22$Factor)
# PEA_22 es la unión de los desocupados que buscaron trabajo y los ocupados
PEA_22 <- rbind(desocupados_22, ocupados_22)
# Se crea la contante PEAtotal 2022 apartir de la sumatoria de factores del
PEATOTAL_22 <- sum(PEA_22$factor)


# Se debe crear el cuadro apartir de la base de PEA 2022 limpia se selecciona
# P03A02, P03A06, factor (Sexo, Pueblo, factor)
PEA_22_data <- PEA_22 %>%
  select(P03A02, P03A06, factor) %>%
  group_by(P03A02, P03A06) %>%
  summarise( PEA22 = sum(factor)/PEATOTAL_22 * 100) %>%
  rename( Sexo = P03A02) %>%
  rename( Pueblo = P03A06)


# Se crea el cuadro con Porcentaje de PEA 2022 por pueblo y sexo  
PEA_22_Pueblo_Sexo <- PEA_22_data %>%
  pivot_wider(names_from = Sexo, values_from = PEA22)

# Cálculo de PET 2018
#
# Se filtran las personas mayores a 14 años, que son las que sí se pueden considerar como PET
# (personas en edad de trabaja = PET) 
# # PPA03 = edad de la persona 
PET_18 <- filter(personasENEI_18, PPA03 > 14)

# Cálculo de PEA 2018
# Cálculo de PEA 2018
# cálculo de desocupados y ocupados 2018
# desocupados 2022 <- filter(PET, P05B04 >=0 | P05B01 == 'Sí')
# pP05B01 = Sí bUSCO trabajo 
desocupados_18 <- filter(PET_18, P04B02 == 'Sí')

# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados

# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
# P04C01 = cantidad de trabajos 
ocupados_18 <- filter(PET_18, !is.na(P04C01))
num_ocupados_18 = sum(ocupados_18$FACTOR)
# PEA_22 es la unión de los desocupados que buscaron trabajo y los ocupados
PEA_18 <- rbind(desocupados_18, ocupados_18)

###############################################################################
#VERIFICACIÓN DEL PEA 2018
#Se verifico el PEA creado y el PEA que se encotraba en la personas ENEI 2018
# pera creado por Aurora utilizando el codigo anterios
PEA18_CREADO <- PEA_18 %>%
  select(PPA02, FACTOR) %>%
  group_by(PPA02) %>%
  summarise( PEA18_prueba = n())

#Se selecciono la columna PEA y se conto el numero de casos. 
PEA18_PEA_EXISTENTE <- PEA_18 %>%
  select(PPA02, PEA) %>%
  group_by(PPA02) %>%
  summarise( PEA18_PEA = n())
###############################################################################

# Se crea la contante PEAtotal 2022 apartir de la sumatoria de factores del
PEATOTAL_18 <- sum(PEA_18$FACTOR)


# Se debe crear el cuadro apartir de la base de PEA 2022 limpia se selecciona
# P03A02, P03A06, factor (Sexo, Pueblo, FACTOR)
PEA_18_data <- PEA_18 %>%
  select(PPA02, PPA06, FACTOR) %>%
  group_by(PPA02, PPA06) %>%
  summarise( PEA_2018 = sum(FACTOR)/PEATOTAL_18 * 100) %>%
  rename( Sexo = PPA02) %>%
  rename( Edad = PPA06)

# Cuadro con el Porcentaje de PEA por pueblo y sexo  
PEA_18_Pueblo_Sexo <- PEA_18_data %>%
  pivot_wider(names_from = Sexo, values_from = PEA_2018)
 #poner etiqueta con los datos del 2018*


compPEA <- rbind(
  data.frame(PEA_18_Pueblo_Sexo, PEA = '2018', what = factor(rownames(PEA_18_Pueblo_Sexo), levels = rownames(PEA_18_Pueblo_Sexo)), 
             row.names= NULL, check.names = FALSE), 
  data.frame(PEA_22_Pueblo_Sexo, PEA = '2022', what = factor(rownames(PEA_22_Pueblo_Sexo), levels = rownames(PEA_22_Pueblo_Sexo)), 
             row.names= NULL, check.names = FALSE), 
)

c04_06 <- mytable <- tabular(Heading()*what ~ station*("Hombre" +"Mujer")*Heading()*(identity),data=compPEA)
c04_06  


################################################################################
# 4.7.	Población económicamente activa por sexo, según dominio de estudio y
# sector económico
################################################################################

################################################################################
# 4.8.	Población ocupada por sexo, según rango de edad
################################################################################

################################################################################
# 4.9.	Población ocupada por sexo, según dominio de estudio y categoría 
# ocupacional
################################################################################

################################################################################
# 4.10.	Porcentaje de trabajadoras(es) afiliadas(os) al seguro social, según 
# rama de actividad (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.11.	Créditos otorgados a la pequeña y mediana empresa por sexo 
# (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.12.	Créditos otorgados a la pequeña y mediana empresa por sexo, según 
# rama de actividad económica (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.13.	Salario o ingresos promedio por sexo, según dominio de estudio y 
# rama de actividad económica
################################################################################

################################################################################
# 4.14.	Salarios o ingresos promedio, desagregado por sexo, según pueblo
################################################################################

################################################################################
# 4.15.	Tasa de desempleo en la población de 15 años o más por sexo, según 
# dominio de estudio (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.16.	Tasa desempleo en la población de 15 años o más por sexo, según 
# Pueblos (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.17.	Mujeres jefas de hogar por número de hijas/hijos en la PO
################################################################################

################################################################################
# 4.18.	Promedio de horas dedicadas a tareas domésticas no remuneradas 
# por sexo (ODS)
################################################################################

################################################################################
# 4.19.	Distribución de tareas no remuneradas en el hogar por sexo
################################################################################



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





