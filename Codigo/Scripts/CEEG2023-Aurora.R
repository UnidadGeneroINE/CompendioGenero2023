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
#library(gt) #install.packages("gt")
#library(tables) #install.packages("tables")
library(xtable) #install.packages("Xtables")
library(kableExtra) #install.packages("kableExtra")

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



EstadoConyugal <- c('con pareja', 'sin pareja', 'Menor de 12 años')
personasENEI <- personasENEI %>%
# se creo una nueva varible que representan que visibilice el estado conyugal, 
# con pareja = Unido (a), Casado (a), 
# sin pareja =  Viudo (a), Separado (a) de matrimonio, Separado (a) de unión, 	
# Divorciado (a), Soltero (a)
# Menor de 12 años = Menor de 12 años
  mutate( EstadoConyugal = case_when( P03A10 == "Unido (a)" ~ 'con pareja',
                                      P03A10 == "Casado (a)" ~ 'con pareja',
                                      P03A10 == "Viudo (a)" ~ 'sin pareja', 
                                      P03A10 == "Separado (a) de matrimonio" ~ 'sin pareja', 
                                      P03A10 == "Separado (a) de matrimonio" ~ 'sin pareja', 
                                      P03A10 == "Divorciado (a)" ~ 'sin pareja',  
                                      P03A10 == "Separado (a) de unión" ~ 'sin pareja',
                                      P03A10 == "Soltero (a)" ~ 'sin pareja',
                                      P03A10 == "Menor de 12 años" ~ 'Menor de 12 años'))


# Calculo del Personas en Edad de Trabajar (PET) 
# Todas las personas mayores de 14 años
PET <- filter(personasENEI, P03A03 > 14)

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
PET <- filter(personasENEI, P03A03 > 14)

# Se filtro la base segun la columna formalidas que se compone de la población ocupada
# desagregada segun sector Formal o Informal
c4_01 <- PET %>%
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
#  4.2.	Tasa de participación económica por dominio de estudio, según sexo y 
# estado conyugal
################################################################################  

# Cálculo de PEA 2022
# cálculo de desocupados y ocupados 2022
# desocupados 2022 <- filter(PET_22, P05B01 == 'Sí)
# pP05B01 = Sí bUSCO trabajo 
desocupados <- filter(PET, P05B01 == 'Sí')
  
# numero de desocupados 2022 <- sum(desocupados_22$factor))
num_desocupados = sum(desocupados$factor)
  
# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados
  
# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
# pO5C01 = cantidad de trabajos 
ocupados <- filter(PET, !is.na(P05C01))
num_ocupados = sum(ocupados$factor)
# PEA_22 es la unión de los desocupados que buscaron trabajo y los ocupados
PEA <- rbind(desocupados, ocupados)
# Se crea la contante PEAtotal 2022 a partir de la sumatoria de factores del la data PEA
PEATOTAL <- sum(PEA$factor)

conteoConyugal <- personasENEI %>%
  select(P03A10) %>%
  group_by(P03A10) %>%
  summarise(y = n())

# Se debe crear el cuadro a partir de la base de PEA 2022 limpia se selecciona
# P03A02, P03A06, factor (Sexo, Pueblo, factor)
c4_02 <- PEA %>%
  select(dominio, EstadoConyugal, P03A02, factor) %>%
  group_by(dominio, EstadoConyugal, P03A02) %>%
  summarise( y = sum(factor)/ PEATOTAL * 100) %>%
  rename(x = dominio) %>%
  rename(z1 = P03A02) %>%
  rename(w = EstadoConyugal) %>%
  select(z1, w, x, y)

#Se unifico la columna sexo = z1 con la culumna Estado contyugal = w
c4_02 <- unite(data = c4_02, col = z, sep = " ", z1, w)

# Se creo la condición para posicionar a las mujeres 
c4_02$z <- factor(c4_02$z, levels = c("Mujer con pareja", "Hombre con pareja", "Mujer sin pareja", "Hombre sin pareja"))

g4_02 <- graficaColApilada(c4_02, "Sexo y Estado conyugal")

exportarLatex(nombre = paste0(directorioGraficas, "g4_02.tex"), graph = g4_02)

# Verificación de porcentajes
print(sum(c4_01$Mujer))
print(sum(c4_01$Hombre)) 

################################################################################
#  4.3.	Población económicamente activa por sexo, según dominio de estudio 
# (comparar 2018 y 2022)
################################################################################ 

#Usar el PEA 2022 de la de ENEI 2022
PEA <- rbind(desocupados, ocupados)
# Se crea la contante PEAtotal 2022 a partir de la sumatoria de factores del la data PEA
PEATOTAL <- sum(PEA$factor)

# Se crea el cuadro de PEA agrupado por dominio de estudio y sexo
PEA2022 <- PEA %>%
  select(dominio, P03A02, factor) %>%
  group_by(dominio, P03A02) %>%
  summarise( y2022 = sum(factor)/ PEATOTAL * 100) %>%
  rename(x = dominio) %>%
  rename(z = P03A02) %>%
  select(z, x, y2022)

# Se segmenta el cuadro por dominio de estudio
PEA2022 <- unite(data = PEA2022, col = z, sep = " ", z, x)

#Verificación de porcetaje
print(sum(PEA2022$y2022))


# CALCULO DE PEA 2018
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

# Se crea la contante PEAtotal 2018 apartir de la sumatoria de factores del
PEATOTAL_18 <- sum(PEA_18$FACTOR)

# Se crea el cuadro de PEA agrupado por dominio de estudio y sexo
PEA2018 <- PEA_18 %>%
  select(DOMINIO, PPA02, FACTOR) %>%
  group_by(DOMINIO, PPA02) %>%
  summarise( y2018 = sum(FACTOR)/ PEATOTAL_18 * 100) %>%
  rename(x = DOMINIO) %>%
  rename(z = PPA02) %>%
  select(z, x, y2018)

# Se segmenta el cuadro por dominio de estudio
PEA2018 <- unite(data = PEA2018, col = x, sep = " ", z, x)

#Verificación de porcetaje
print(sum(PEA2018$y2018))

#Tabla comparativa de PEA entre  2018 y 2022
#Unón de PEA 2022 a 2018
c4_03_data <- cbind(PEA2018, PEA2022[, c("y2022")])

# Se renombro las columnas por años  
c4_03 <- c4_03_data %>%
  rename("2018" = y2018) %>%
  rename("2022" = y2022)

#Crear grafica de columnas por categoria y exportar a latex
g4_03 <- graficaColCategorias(data = c4_03, ruta = paste0(directorioGraficas,"g4_03.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")

################################################################################
# 4.4.	Población económicamente activa por sexo, según Pueblos y grupos de edad
#(comparar 2018 y 2022)
################################################################################

#Usar el PEA 2022 de la de ENEI 2022
PEA <- rbind(desocupados, ocupados)
# Se crea la contante PEAtotal 2022 a partir de la sumatoria de factores del la data PEA
PEATOTAL <- sum(PEA$factor)


# Se debe crear el cuadro apartir de la base de PEA 2022, se selecciona
# P03A02, P03A06, factor (Sexo, Pueblo, factor)
PEA22_PUEBLOS <- PEA %>%
  select(P03A02, P03A06, factor) %>%
  group_by(P03A02, P03A06) %>%
  summarise( y = sum(factor)/PEATOTAL * 100) %>%
  rename( x = P03A02) %>%
  rename( z = P03A06)

#Se segmenta la tabla por sexo
x <- c("Maya", "Garífuna", "Xinka", "Afrodescendiente*", 
       "Ladino", "Extranjero")
Mujer <- c(as.numeric(PEA22_PUEBLOS[6,3]), as.numeric(PEA22_PUEBLOS[2,3]), 
             as.numeric(PEA22_PUEBLOS[1,3]), as.numeric(PEA22_PUEBLOS[4,3]),
             as.numeric(PEA22_PUEBLOS[3,3]), as.numeric(PEA22_PUEBLOS[5,3]))
Hombre <- c(as.numeric(PEA22_PUEBLOS[12,3]), as.numeric(PEA22_PUEBLOS[8,3]), 
             as.numeric(PEA22_PUEBLOS[7,3]), as.numeric(PEA22_PUEBLOS[10,3]),
             as.numeric(PEA22_PUEBLOS[9,3]), as.numeric(PEA22_PUEBLOS[11,3]))

PEA22_PUEBLOS <- data.frame(x, Mujer, Hombre)

# Verificación de porcentajes
print(sum(PEA22_PUEBLOS$Mujer))
print(sum(PEA22_PUEBLOS$Hombre))


#Población Econiomicamente Activa 2018
# Se crea el cuadro de PEA agrupado por dominio de estudio y sexo
PEA18_PUEBLOS <- PEA_18 %>%
  select(PPA06, PPA02, FACTOR) %>%
  group_by(PPA06, PPA02) %>%
  summarise( y = sum(FACTOR)/ PEATOTAL_18 * 100) %>%
  rename(x = PPA06) %>%
  rename(z = PPA02)

#Se segmenta la tabla por sexo
x <- c("Maya", "Garífuna", "Xinka", "Ladino", "Extranjero")
Mujer <- c(as.numeric(PEA18_PUEBLOS[9,3]), as.numeric(PEA18_PUEBLOS[3,3]), 
           as.numeric(PEA18_PUEBLOS[1,3]), as.numeric(PEA18_PUEBLOS[5,3]),
           as.numeric(PEA18_PUEBLOS[7,3]))
Hombre <- c(as.numeric(PEA18_PUEBLOS[10,3]), as.numeric(PEA18_PUEBLOS[4,3]), 
            as.numeric(PEA18_PUEBLOS[2,3]), as.numeric(PEA18_PUEBLOS[6,3]),
            as.numeric(PEA18_PUEBLOS[8,3]))

PEA18_PUEBLOS <- data.frame(x, Mujer, Hombre)

#Agregar la fila afro al PEA18
# Se parte el el data frame de PEA18
Fila_123 <- slice(PEA18_PUEBLOS, 1:3)
Fila_45 <- slice(PEA18_PUEBLOS, 4:5)
#Se crea la fila Afrodesendiente 
fila_Afro <- data.frame(x = "Afrodescendiente*", Mujer = 0, Hombre = 0)

#Se usen las tres tablas de PEA por pueblo 2018
PEA18_PUEBLOS <- rbind(Fila_123, fila_Afro, Fila_45)

#Unión de PEA 2022 a 2018
c4_04 <- cbind(PEA18_PUEBLOS, PEA22_PUEBLOS[, c("Mujer", "Hombre")])

#########Falta agregar el codigo para la creación de tablas agrupadas por año y exportar
# a latex
summarise(casos = n())
################################################################################
# 4.5.	Población económicamente activa por sexo, según dominio de estudio y
# sector económico
################################################################################

#PEA por sector economico

PEA_Sector_economico <- PEA %>%
  filter(formalidad == "Formal" | 
           formalidad == "Informal")

TotalSector <- sum(PEA_Sector_economico$factor)

c4_05 <- PEA_Sector_economico %>%
  select(dominio, formalidad, P03A02, factor) %>%
  group_by(dominio, formalidad, P03A02) %>%
  summarise( z = sum(factor)/ TotalSector * 100) %>%
  rename(x = dominio) %>%
  rename(w = P03A02) %>%
  rename(a = formalidad) %>%
  select(w, a, x, z)


#Se unifico la columna y = sexo + sector economico 
c4_05 <- unite(data = c4_05, col = y, sep = " ", w, a)

# Se paso las filas de "y" a columnas
c4_05 <- pivot_wider(c4_05, names_from = y, values_from = c(z))


#Grafica para latex
g4_05 <- graficaColCategorias(data = c4_05, ruta = paste0(directorioGraficas,"g4_05.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")

# Verificación de porcentajes
print(sum(c4_05$y))

################################################################################
# 4.6.	Población ocupada por sexo, según rango de edad
################################################################################

################################################################################
# 4.7.	Población ocupada por sexo, según dominio de estudio y categoría 
# ocupacional
################################################################################

################################################################################
# 4.8.	Porcentaje de trabajadoras(es) afiliadas(os) al seguro social, según 
# rama de actividad (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.9.	Créditos otorgados a la pequeña y mediana empresa por sexo 
# (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.10.	Créditos otorgados a la pequeña y mediana empresa por sexo, según 
# rama de actividad económica (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.11.	Salario o ingresos promedio por sexo, según dominio de estudio y 
# rama de actividad económica
################################################################################

################################################################################
# 4.12.	Salarios o ingresos promedio, desagregado por sexo, según pueblo
################################################################################

################################################################################
# 4.13.	Tasa de desempleo en la población de 15 años o más por sexo, según 
# dominio de estudio (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.14.	Tasa desempleo en la población de 15 años o más por sexo, según 
# Pueblos (comparar 2018 y 2022)
################################################################################

################################################################################
# 4.15.	Mujeres jefas de hogar por número de hijas/hijos en la PO
################################################################################

################################################################################
# 4.16.	Promedio de horas dedicadas a tareas domésticas no remuneradas 
# por sexo (ODS)
################################################################################

################################################################################
# 4.17.	Distribución de tareas no remuneradas en el hogar por sexo
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





