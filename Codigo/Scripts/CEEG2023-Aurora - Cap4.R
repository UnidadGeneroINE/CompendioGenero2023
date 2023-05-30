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
  
# Indica el orden en el que se deben mostrar los sexos para ENEI 2022
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


# Creando y calculando columna ingresos (será util en cap. 8)
personasENEI$P05C26[is.na(personasENEI$P05C26)] <- 0 # Cambiando los NA por 0 para poder sumar
personasENEI$P05C46[is.na(personasENEI$P05C46)] <- 0 # Cambiando los NA por 0 para poder sumar
personasENEI$P05C47[is.na(personasENEI$P05C47)] <- 0 # Cambiando los NA por 0 para poder sumar
personasENEI <- mutate(personasENEI, ingreso = P05C26 + P05C46 + P05C47)  
  
# Calculo del Personas en Edad de Trabajar (PET) 
# Todas las personas mayores de 14 años
PET <- filter(personasENEI, P03A03 > 14)

# Creando la base de Asalariados para usar en el capitulo 4.
#Se filta la base de PEA y se filta segun la P05C20 que la Categía Ocupacional
# PET = Personas en Edad de Trabajar
asalariados <- PET %>%
  filter(P05C20 == "Empleado de gobierno"
         | P05C20 == "Empleado de empresa privada"
         | P05C20 == "Empleado jornalero o peón"
         | P05C20 == "En el servicio doméstico")
# Para calcular procentajes se saca el total de Salariados sumando el factor

# codigo para verificar porcetaje 
print(sum(c4_06_data$PEA))
print(sum(c4_06_Pueblo_Sexo$Hombre))
print(sum(c4_06_Pueblo_Sexo$Mujer))

#Datos ENEI 2018 que se utilizara para comparar los datos 2018 con 2022. 

# Indica el orden en el que se deben mostrar los sexos para ENEI 2018
personasENEI_18$PPA02 <- factor(personasENEI_18$PPA02, levels = c("Mujer", "Hombre"))
# Flitro de base personas ENEI 2018 se selecciono solo a las de Personas en Edad 
# de Trabajar -PET-
PET_18 <- filter(personasENEI_18, PPA03 > 14)
# Cálculo de PEA 2022
# cálculo de desocupados y ocupados 2022
# desocupados 2022 <- filter(PET_22, P05B01 == 'Sí)
# pP05B01 = Sí bUSCO trabajo 
desocupados_18 <- filter(PET_18, P04B01 == 'Si')

# numero de desocupados 2022 <- sum(desocupados_22$factor))
num_desocupados_18 = sum(desocupados_18$factor)

# Para encontrar los ocupados, se debe evaluar si respondieron en P05C01 que
# tienen más de un trabajo, si no respondieron nada quiere decir que son
# desocupados

# Se debe cambiar de acuerdo a la codificación de la respuesta de P05C01 al tener
# la base limpia
# pO5C01 = cantidad de trabajos 
ocupados_18 <- filter(PET_18, !is.na(P04C01))
num_ocupados_18 = sum(ocupados_18$factor)
# PEA_22 es la unión de los desocupados que buscaron trabajo y los ocupados
PEA_18 <- rbind(desocupados_18, ocupados_18)
# Se crea la contante PEAtotal 2022 a partir de la sumatoria de factores del la data PEA
PEATOTAL_18 <- sum(PEA_18$FACTOR)


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

x <- c("Formal", "informal")
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
  rename(z = P03A02) %>%
  rename(w = EstadoConyugal) %>%
  select(z, w, x, y)


g4_02 <- graficaCategoriasApiladas (c4_02, tipo = "barra", categoria_leyenda = "",
                                    leyenda = "abajo")

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
  mutate(w = "2022") %>%
  select(dominio, P03A02, factor, w) %>%
  group_by(dominio, P03A02, w) %>%
  summarise( y = sum(factor)/ PEATOTAL * 100) %>%
  rename(x = dominio) %>%
  rename(z = P03A02) %>%
  select(w, z, x, y)

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
  mutate(w = "2018") %>%
  select(DOMINIO, PPA02, FACTOR, w) %>%
  group_by(DOMINIO, PPA02, w) %>%
  summarise( y = sum(FACTOR)/ PEATOTAL_18 * 100) %>%
  rename(x = DOMINIO) %>%
  rename(z = PPA02) %>%
  select(w, z, x, y)

# Se renombro las columnas por años  
c4_03 <- rbind(PEA2018, PEA2022)

#Crear grafica de columnas por categoria y exportar a latex
g4_03 <- graficaCategoriasApiladas (c4_03, tipo = "columna", categoria_leyenda = "",
                                    leyenda = "abajo")

exportarLatex(nombre = paste0(directorioGraficas, "g4_03.tex"), graph = g4_03)
################################################################################
# 4.4.	Población económicamente activa por sexo, según Pueblos
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
  rename( Pueblos = P03A02) %>%
  rename( z = P03A06)

#Se segmenta la tabla por sexo
Pueblos <- c("Maya", "Garífuna", "Xinka", "Afrodescendiente*", 
       "Ladino", "Extranjero")
Mujer <- c(as.numeric(PEA22_PUEBLOS[6,3]), as.numeric(PEA22_PUEBLOS[2,3]), 
             as.numeric(PEA22_PUEBLOS[1,3]), as.numeric(PEA22_PUEBLOS[4,3]),
             as.numeric(PEA22_PUEBLOS[3,3]), as.numeric(PEA22_PUEBLOS[5,3]))
Hombre <- c(as.numeric(PEA22_PUEBLOS[12,3]), as.numeric(PEA22_PUEBLOS[8,3]), 
             as.numeric(PEA22_PUEBLOS[7,3]), as.numeric(PEA22_PUEBLOS[10,3]),
             as.numeric(PEA22_PUEBLOS[9,3]), as.numeric(PEA22_PUEBLOS[11,3]))

PEA22_PUEBLOS <- data.frame(Pueblos, Mujer, Hombre)

# Verificación de porcentajes
print(sum(PEA22_PUEBLOS$Mujer))
print(sum(PEA22_PUEBLOS$Hombre))


#Población Econiomicamente Activa 2018
# Se crea el cuadro de PEA agrupado por dominio de estudio y sexo
PEA18_PUEBLOS <- PEA_18 %>%
  select(PPA06, PPA02, FACTOR) %>%
  group_by(PPA06, PPA02) %>%
  summarise( y = sum(FACTOR)/ PEATOTAL_18 * 100) %>%
  rename(Pueblos = PPA06) %>%
  rename(z = PPA02)

#Se segmenta la tabla por sexo
Pueblos <- c("Maya", "Garífuna", "Xinka", "Ladino", "Extranjero")
Mujer <- c(as.numeric(PEA18_PUEBLOS[9,3]), as.numeric(PEA18_PUEBLOS[3,3]), 
           as.numeric(PEA18_PUEBLOS[1,3]), as.numeric(PEA18_PUEBLOS[5,3]),
           as.numeric(PEA18_PUEBLOS[7,3]))
Hombre <- c(as.numeric(PEA18_PUEBLOS[10,3]), as.numeric(PEA18_PUEBLOS[4,3]), 
            as.numeric(PEA18_PUEBLOS[2,3]), as.numeric(PEA18_PUEBLOS[6,3]),
            as.numeric(PEA18_PUEBLOS[8,3]))

PEA18_PUEBLOS <- data.frame(Pueblos, Mujer, Hombre)

#Agregar la fila afro al PEA18
# Se parte el el data frame de PEA18
Fila_123 <- slice(PEA18_PUEBLOS, 1:3)
Fila_45 <- slice(PEA18_PUEBLOS, 4:5)
#Se crea la fila Afrodesendiente 
fila_Afro <- data.frame(Pueblos = "Afrodescendiente*", Mujer = "N/A", Hombre = "N/A")

#Se usen las tres tablas de PEA por pueblo 2018
PEA18_PUEBLOS <- rbind(Fila_123, fila_Afro, Fila_45)

# Convertimos las columnas numéricas utilizando type.convert()
PEA18_PUEBLOS[, 2:3] <- apply(PEA18_PUEBLOS[, 2:3], 2, function(x) type.convert(as.character(x), na.strings = "N/A"))

#Unión de PEA 2022 a 2018
c4_04 <- cbind(PEA18_PUEBLOS, PEA22_PUEBLOS[, c("Mujer", "Hombre")])

#Tabla latex 
Tabla4_04 <- tablaLaTeX(c4_04, nombre_columnas = colnames(c4_04), 
                       nombre_grupos = c(" ", "2018" = 2, "2022" = 2), 
                       opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla4_04.tex"))

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
  summarise( y = sum(factor)/ TotalSector * 100) %>%
  rename(x = dominio) %>%
  rename(z = P03A02) %>%
  rename(w = formalidad) %>%
  select(w, z, x, y)


#Se unifico la columna y = sexo + sector economico 
c4_05 <- unite(data = c4_05, col = y, sep = " ", w, a)

# Se paso las filas de "y" a columnas
c4_05 <- pivot_wider(c4_05, names_from = y, values_from = c(z))


#Grafica para latex
g4_05 <- graficaColCategorias(data = c4_05, ruta = paste0(directorioGraficas,"g4_05.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")

#Crear grafica de columnas por categoria y exportar a latex
g4_05 <- graficaCategoriasApiladas (c4_05, tipo = "columna", categoria_leyenda = "",
                                    leyenda = "abajo")

exportarLatex(nombre = paste0(directorioGraficas, "g4_05.tex"), graph = g4_05)

# Verificación de porcentajes
print(sum(c4_05$y))

################################################################################
# 4.6.	Población ocupada por sexo, según rango de edad
################################################################################

#Se defino PO = POlación ocupada, se filtro con la columna Ocupados todos los 
# que tengan "Polación ocupada"
PO <- PET %>%
  filter( Ocupados == "Poblacion ocupada")

#Se calculo el total de PO para sacar porcentajes
TotalPO <- sum(PO$factor)

c4_06 <- PO %>%
  select(P03A02, GruposEdad, factor) %>%
  group_by(P03A02, GruposEdad) %>%
  summarise( y = sum(factor)/ TotalPO * 100) %>%
  rename(z = P03A02) %>%
  rename(x = GruposEdad)

  print(sum(c4_06$y))
  
x <- c("15-29", "30-65", "65+")
Mujer <- c(as.numeric(c4_06[1,3]), as.numeric(c4_06[2,3]), as.numeric(c4_06[3,3]))
Hombre <- c(as.numeric(c4_06[4,3]), as.numeric(c4_06[5,3]), as.numeric(c4_06[6,3]))

c4_06 <- data.frame(x, Mujer, Hombre)

#Exportar grafica columnas por categoria a latex
g4_06 <- graficaColCategorias(data = c4_06, ruta = paste0(directorioGraficas,"g4_06.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")


################################################################################
# 4.7.	Población ocupada por sexo, según categoría ocupacional
################################################################################

#Se filtra por la categoria de ocupación se excluye la categoaria "Ingnorado"
PO_CAT <- PO %>%
  filter( P05C20 == "Empleado de gobierno" |
          P05C20 == "Empleado de empresa privada" | 
          P05C20 == "Empleado jornalero o peón" |
          P05C20 == "En el servicio doméstico" |
          P05C20 == "Trabajador por cuenta propia NO agrícola" |
          P05C20 == "Patrón empleador (a) socio (a) NO agrícola" |
          P05C20 == "Trabajador por cuenta propia agrícola" |
          P05C20 == "Patrón empleador (a) socio (a) agrícola" |
          P05C20 == "Trabajador No remunerado (Familiar o No familiar)")

#Se calculo el total de PO para sacar porcentajes
TotalPO_CAT<- sum(PO_CAT$factor)

c4_07 <- PO_CAT %>%
  select(P03A02, P05C20, factor) %>%
    group_by(P03A02, P05C20) %>%
    summarise( y = sum(factor)/ TotalPO_CAT * 100) %>%
    rename(z = P03A02) %>%
    rename(x = P05C20)

#Acortar los valores de la columna x
c4_07 <- c4_07 %>%
  mutate(x = case_when(x == "En el servicio doméstico" ~ "Servicio doméstico",
                       x == "Empleado jornalero o peón" ~ "Jornalero o peón",
                       x == "Trabajador por cuenta propia NO agrícola" ~ "Cuenta propia no agrícola",
                       x == "Trabajador No remunerado (Familiar o No familiar)" ~ "Trabajador no remunerado",
                       x == "Trabajador por cuenta propia agrícola" ~ "Por cuenta propia agrícola",
                       x == "Patrón empleador (a) socio (a) agrícola" ~ "Patrón agrícola",
                       x == "Patrón empleador (a) socio (a) NO agrícola" ~ "Patrón no agrícola",
                       TRUE ~ x))


g4_07 <- graficaCategorias(c4_07, tipo = "barra", decimales = TRUE, 
                           categoria_leyenda = "", leyenda = "abajo")

exportarLatex(nombre = paste0(directorioGraficas, "g4_07.tex"), graph = g4_07)


print(sum(c4_07$Mujer))
print(sum(c4_07$Hombre))

################################################################################
# 4.8.  Población ocupada con acceso a seguro social por sexo, según rama de 
#       actividad económica Porcentaje 
################################################################################

#total de ocupados afiliados 
Totalocupados_22<- sum(ocupados_22$factor)

c4_08 <- ocupados_22 %>%
  select(P03A02, P05C03B_1D, factor) %>%
  group_by(P03A02, P05C03B_1D) %>%
  summarise( y = sum(factor) / Totalocupados_22 * 100, casos = n()) %>%
  rename(z = P03A02) %>%
  rename(x = P05C03B_1D) %>%
  select(x,z,y)

c4_08 <- pivot_wider(c4_08, names_from = z, values_from = c(y))

c4_08 <- c4_08 %>%
  mutate(x =case_when(x == "Comercio al por mayor y al por menor, transporte y almacenamiento, actividades de alojamiento y de servicio de comidas" ~ "Comercio",
                      x == "Agricultura, ganadería, silvicultura y pesca" ~ "Agricultura",
                      x == "Industrias manufactureras, explotación de minas y canteras y otras actividades industriales" ~ "Industrias manufactureras",
                      x == "Actividades de administración pública y defensa, enseñanza, actividades de atención de la salud y asistencia social" ~ "Administración pública",
                      x == "Actividades financieras y de seguros" ~ "Financieras y de seguros",
                      x == "Actividades profesionales, científicas, técnicas, y de servicios administrativos y de apoyo" ~ "Profesionales",
                      x == "Construcción" ~ "Construcción",
                      x == "Otras actividades de servicios" ~ "Otras de servicios",
                      x == "Actividades inmobiliarias" ~ "Inmobiliarias",
                      x == "Información y comunicación" ~ "Comunicaciones",
                      x == "9999" ~ "NS/NR", TRUE ~ x)) # Reemplazando los valores y si ninguna condición se cumple conserva el valor original de la base almacenado temporalmente en col_categoría_ocupacional

c4_08 <- c4_08 %>%
  rename("Categoría Ocupacional" = x) 
  
Tabla4_08 <- tablaLaTeX(c4_08, ruta = paste0(directorioGraficas, "Tabla4_08.tex"))

################################################################################
# 4.9.	Créditos otorgados a la pequeña y mediana empresa por sexo 
# (comparar 2018 y 2022)
################################################################################
# se acutualizo la ubicación del archivo
ECONOMÍA_Y_TRABAJO <- "C:\\Users\\pgalvez\\OneDrive - ine.gob.gt\\Documentos\\GitHub\\CompendioGenero2023\\Codigo\\Bases\\datos_administrativos\\Indicadores_de_Género\\ECONOMÍA_Y_TRABAJO\\"

Creditos <- paste0(ECONOMÍA_Y_TRABAJO, "4-11_4-12.xlsx")

c4_09 <- data.frame(read.xlsx(xlsxFile = Creditos, sheet = "creditos"))

# Se acordo usar en tabla
# g4_09 <- graficaColCategorias(data = c4_09, ruta = paste0(directorioGraficas,"g4_09.tex"),
                                       # etiquetasCategorias = "A", etiquetas = "h")
#Tabla latex 
Tabla4_09 <- tablaLaTeX(c4_09, ruta = paste0(directorioGraficas, "Tabla4_09.tex"))

################################################################################
# 4.10.	Créditos otorgados a la pequeña y mediana empresa por sexo, según 
# rama de actividad económica (comparar 2018 y 2022)
################################################################################

Creditos_2018 <- data.frame(read.xlsx(xlsxFile = Creditos, sheet = "2018")) %>%
  rename("Actividad Económica" = Actividad.Económica)

Creditos_2022 <- data.frame(read.xlsx(xlsxFile = Creditos, sheet = "2022"))  %>%
  rename("Actividad Económica" = Actividad.Económica)

c4_10 <- cbind(Creditos_2018, Creditos_2022[, c("Mujeres", "Hombres")])

#Tabla latex 
Tabla4_10 <- tablaLaTeX(c4_10, nombre_columnas = colnames(c4_10), 
                        nombre_grupos = c(" ", "2018" = 2, "2022" = 2),
                        ruta = paste0(directorioGraficas, "Tabla4_10.tex"))

################################################################################
# 4.11.	Salario o ingresos promedio mensual por sexo, según dominio de estudio 
################################################################################

total_asalariados = sum(asalariados$factor)

c4_11 <- asalariados%>%
  mutate (ingresoPonderado = ingreso*factor) %>%
  select(P03A02, dominio, factor, ingresoPonderado) %>%
  group_by(P03A02, dominio) %>%
  summarize(y = sum(ingresoPonderado)/sum(factor), casos = n()) %>%
  rename(z = P03A02) %>%
  rename(x = dominio) %>%
  select(x, z, y)

c4_11 <- pivot_wider(c4_11, names_from = z, values_from = c(y)) 

g4_11 <- graficaColCategorias(data = c4_11, ruta = paste0(directorioGraficas,"g4_11.tex"),
                              etiquetasCategorias = "A", etiquetas = "h")

#Indicador solicitado por UG Ingreso o salario promedio de madres por dominio de estudio
# Se filtro la Base de asalariados por mujeres que reportaron tener 1 o mas hijos.
# Se agrupo por Dominio de Estudio 

madres_asalariadas <- asalariados%>%
  filter(P03A02 == "Mujer") %>%
  filter(P03A12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))

madres_asalariadas_total <- sum(madres_asalariadas$factor)

Salario_MAdres <- asalariados%>%
  filter(P03A02 == "Mujer") %>%
  filter(P03A12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) %>%
  mutate (ingresoPonderado = ingreso*factor) %>%
  select (dominio, factor, ingresoPonderado) %>%
  group_by (dominio) %>%
  summarize(Ingreso_Promedio = sum(ingresoPonderado)/sum(factor), y = sum(factor)/madres_asalariadas_total*100) %>%
  rename(Dominio_Estudio = dominio) %>%
  select(Dominio_Estudio, Ingreso_Promedio, y)

### Indicador soliciado por UG Ingreso o salario promedio de madres por categoría Ocuácional
# Se filtro la Base de asalariados por mujeres que reportaron tener 1 o mas hijos.
# Se agrupo por la categoría ocupacional 
Actividad_Madres <- asalariados%>%
  filter(P03A02 == "Mujer" & P03A03 > 11) %>%
  filter(P03A12 != "No respondio" & P03A12 != 0) %>%
  mutate (ingresoPonderado = ingreso*factor) %>%
  select(P05C20, factor, ingresoPonderado) %>%
  group_by(P05C20) %>%
  summarize(Ingreso_Promedio = sum(ingresoPonderado)/sum(factor), casos = n(), y = sum(factor)/madres_asalariadas_total*100) %>%
  rename(Actividad_Ocupacional = P05C20) %>%
  select(Actividad_Ocupacional, Ingreso_Promedio, y)

################################################################################
# 4.12.	Salario o ingresos promedio mensual por sexo, según Categoría OCupacional
################################################################################

# Se crea la tabla de la base de personas asalariadas 
c4_12 <- asalariados%>%
  mutate (ingresoPonderado = ingreso*factor) %>%
  select(P03A02, P05C20, factor, ingresoPonderado) %>%
  group_by(P03A02, P05C20) %>%
  summarize(y = sum(ingresoPonderado)/sum(factor), casos = n()) %>%
  rename(z = P03A02) %>%
  rename(x = P05C20) %>%
  select(x, z, y)

g4_12 <- graficaCategorias(c4_12, tipo = "barra", decimales = TRUE, 
                           categoria_leyenda = "", leyenda = "lado")

exportarLatex(nombre = paste0(directorioGraficas, "g4_12.tex"), graph = g4_12)

################################################################################
# 4.13.	Salarios o ingresos promedio mensual, desagregado por sexo, según pueblo

# Se crea la tabla de la base de personas asalariadas 
#c4_13 <- asalariados%>%
  #mutate (ingresoPonderado = ingreso*factor) %>%
  #select(P03A02, P03A06, factor, ingresoPonderado) %>%
  #group_by(P03A02, P03A06,) %>%
  #summarize(y = sum(ingresoPonderado)/sum(factor), casos = n()) %>%
  #rename(z = P03A02) %>%
  #rename(x = P03A06) %>%
  #select(x, z, y, casos)

#se convierte la columna sexo (z) a dos columnas por categoría
#c4_13 <- pivot_wider(c4_13, names_from = z, values_from = c(y)) 

#g4_13 <- graficaColCategorias(data = c4_13, ruta = paste0(directorioGraficas,"g4_13.tex"),
                              #etiquetasCategorias = "A", etiquetas = "h")

################################################################################
# 4.13.	Tasa de desempleo en la población de 15 años o más por sexo, según 
# dominio de estudio (comparar 2018 y 2022)
################################################################################

#Calculo de tasa de desocupados 2022
#Se utiliza la base de desocupados para el claculo de desempleo del año 2022
Tasa_desempleo_22 <- desocupados %>%
  mutate(w = "2022") %>%
  select(dominio, P03A02, factor, w) %>%
  group_by(dominio,  P03A02, w) %>%
  summarise( y = sum(factor)/ PEATOTAL * 100, casos = n()) %>%
  rename(x = dominio) %>%
  rename(z = P03A02) %>%
  select(w, x, z, y)

#Calculo de tasa de desocupados 2018
#Se utiliza la base de desocupados para el claculo de desempleo del año 2018
Tasa_desempleo_18 <- desocupados_18 %>%
  mutate(w = "2018") %>%
  select(DOMINIO, PPA02, FACTOR, w) %>%
  group_by(DOMINIO,  PPA02, w) %>%
  summarise( y = sum(FACTOR)/ PEATOTAL_18 * 100, casos = n()) %>%
  rename(x = DOMINIO,) %>%
  rename(z = PPA02) %>%
  select(w,x, z, y)

c4_13 <- rbind(Tasa_desempleo_18, Tasa_desempleo_22)

g4_13 <- graficaCategoriasApiladas (c4_13, tipo = "barra", categoria_leyenda = "",
                                    leyenda = "abajo")

exportarLatex(nombre = paste0(directorioGraficas, "g4_13.tex"), graph = g4_13)


################################################################################
# 4.14.	Mujeres jefas de hogar por número de hijas/hijos en la PO
################################################################################

Mujeres_PO <- filter(PO, P03A02 == "Mujer" & P03A03 > 11) %>%
  filter(P03A12 != "No respondio" & P03A12 != 0) %>%
  select(P03A02, P03A03, P03A12, factor)

Total_Mujeres_PO <- sum(Mujeres_PO$factor)

c4_14 <- Mujeres_PO%>%
  mutate(x = case_when(P03A12 %in% c("1", "2", "3") ~ "1-3",
                                 P03A12 %in% c("4", "5", "6") ~ "4-6",
                                 TRUE ~ "6+")) %>%
  group_by(x) %>%
  summarize(y = sum(factor)/Total_Mujeres_PO * 100)

# g4_14 <- graficaAnillo(data = c4_14, nombre = paste0(directorioGraficas, "g4_14.tex"), preambulo = F)
g4_14 <- graficaAnillo(data = c4_14, nombre = paste0(directorioGraficas,"g4_14.tex"), preambulo = F)

################################################################################
# 4.15. Porción de tiempo dedicado a quehaceres domésticos y cuidados no 
#       remunerados por sexo 
################################################################################

# Crear la tabla apartir del indicadpr 9.1 de documento principales resultados 
# de la ENEI 2022. 

Mujer <- c(21.0)
Hombre <- c(3.9)

c4_15 <- data.frame(x = c("Mujer", "Hombre"), y = c(Mujer, Hombre))

g4_15 <- graficaCol(c4_15)
g4_15 <- etiquetasHorizontales(g4_15)
exportarLatex(graph = g4_15, paste0(directorioGraficas, "g4_15.tex"))

################################################################################
# 4.16.	Porción de tiempo dedicado a quehaceres domésticos y de 
#       cuidados no remunerados por grupos de edad
################################################################################

# PET por sexo
PET_mujeres <- filter(PET, P03A02 == "Mujer")
PET_hombres <- filter(PET, P03A02 == "Hombre")

PETTOTAL <- sum(PET$factor)

d16 <- PET %>%
  filter(P03A03 >14) %>%
  mutate( P07A01F = case_when( P07A01A == "Sí" ~ P07A01B + P07A01C/60 +
                                 P07A01D + P07A01E/60,
                               P07A01A == "No" ~ 0, TRUE ~ 0 ) )%>%
  mutate( P07A02F = case_when( P07A02A == "Sí" ~ P07A02B + P07A02C/60 +
                                 P07A02D + P07A02E/60,
                               P07A02A == "No" ~ 0, TRUE ~ 0 ) )%>%
  
  mutate( P07A03F = case_when( P07A03A == "Sí" ~ P07A03B + P07A03C/60 +
                                 P07A03D + P07A03E/60,
                               P07A03A == "No" ~ 0, TRUE ~ 0 ) ) %>%
  mutate( P07A04F = case_when( P07A04A == "Sí" ~ P07A04B + P07A04C/60 +
                                 P07A04D + P07A04E/60,
                               P07A04A == "No" ~ 0, TRUE ~ 0 ) ) %>%
  
  mutate(P07A05F = case_when( P07A05A == "Sí" ~ P07A05B + P07A05C/60 +
                                P07A05D + P07A05E/60,
                              P07A05A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate(P07A06F = case_when( P07A06A == "Sí" ~ P07A06B + P07A06C/60 +
                                P07A06D + P07A06E/60,
                              P07A06A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A07F = case_when( P07A07A == "Sí" ~ P07A07B + P07A07C/60 +
                                   P07A07D + P07A07E/60,
                                 P07A07A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A08F = case_when( P07A08A == "Sí" ~ P07A08B + P07A08C/60 +
                                   P07A08D + P07A08E/60,
                                 P07A07A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A09F = case_when( P07A09A == "Sí" ~ P07A09B + P07A09C/60 +
                                   P07A09D + P07A09E/60,
                                 P07A09A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate(   P07A10F = case_when( P07A10A == "Sí" ~ P07A10B + P07A10C/60 +
                                   P07A10D + P07A10E/60,
                                 P07A10A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate( P07Total = (P07A01F + P07A02F + P07A03F + P07A04F + P07A05F + P07A06F
                      + P07A07F + P07A08F + P07A09F + P07A10F)/7 )

d4_16 <- d16 %>%
  mutate( expandido = P07Total * factor ) %>%
  group_by(P03A02, GruposEdad) %>%
  summarize( y = sum(expandido)) %>%
  rename(x = P03A02, z = GruposEdad)

PET_mujeres_15_29 <- filter(PET_mujeres, P03A03 > 14 & P03A03 < 30)
PET_mujeres_15_29 <- sum(PET_mujeres_15_29$factor)

PET_mujeres_30_65 <- filter(PET_mujeres, P03A03 > 29 & P03A03 < 65)
PET_mujeres_30_65 <- sum(PET_mujeres_30_65$factor)

PET_mujeres_65 <- filter(PET_mujeres, P03A03 > 64)
PET_mujeres_65 <- sum(PET_mujeres_65$factor)

PET_Hombres_15_29 <- filter(PET_hombres, P03A03 > 14 & P03A03 < 30)
PET_Hombres_15_29 <- sum(PET_Hombres_15_29$factor)

PET_Hombres_30_65 <- filter(PET_hombres, P03A03 > 29 & P03A03 < 65)
PET_Hombres_30_65 <- sum(PET_Hombres_30_65$factor)

PET_Hombres_65 <- filter(PET_hombres, P03A03 > 64)
PET_hombre_65 <- sum(PET_Hombres_65$factor)

mujeres_15_29 <- d4_16[1,3]/PET_mujeres_15_29 /24*100
hombres_15_29 <- d4_16[4,3]/PET_Hombres_15_29/24*100
mujeres_30_65 <- d4_16[2,3]/PET_mujeres_30_65/24*100
hombres_30_65 <- d4_16[5,3]/PET_Hombres_30_65/24*100
mujeres_65 <- d4_16[3,3]/PET_mujeres_65/24*100
hombres_65 <- d4_16[6,3]/PET_hombre_65/24*100


c4_16 <- data.frame(z =c("Mujer", "Hombre"), 
                    y = c(mujeres_15_29[[1]], hombres_15_29[[1]] ), 
                    x =  c(mujeres_30_65[[1]], hombres_30_65[[1]] ),
                    w =  c(hombres_65[[1]], hombres_65[[1]] ) ) %>%
  rename('15-29' = y , '30-65' = x, '65+' = w )

c4_16 <- pivot_longer(c4_16, cols = 2:4, names_to = "x", values_to = "y") 
c4_16$z <- factor(c4_16$z, levels = c("Mujer", "Hombre"))

g4_16 <- graficaCategorias(c4_16, tipo = "columna", decimales = TRUE, 
                           categoria_leyenda = "", leyenda = "arriba")

exportarLatex(nombre = paste0(directorioGraficas, "g4_16.tex"), graph = g4_16)

################################################################################
# Porporción del dia dedicado a trabajo doméstico y de cuidados de 
# madres trabajadorras
#Revisión de numero de hijo por madres trabajadoras
#Madres_trabajadoras <- asalariados%>%
  #filter(P03A02 == "Mujer") %>%
  #filter(P03A12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) %>%
  #select( P03A02, P03A12, factor) %>%
  #group_by(P03A02, P03A12) %>%
  #summarise( y = sum(factor), casos = n()) 

Madres_trabajadoras <- asalariados%>%
  filter(P03A02 == "Mujer") %>%
  filter(P03A12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)) %>%

Trabajadorass <- filter(Madres_trabajadoras, P03A02 == "Mujer")

m10 <- Madres_trabajadoras %>%
  filter(P03A03 >14) %>%
  mutate( P07A01F = case_when( P07A01A == "Sí" ~ P07A01B + P07A01C/60 +
                                 P07A01D + P07A01E/60,
                               P07A01A == "No" ~ 0, TRUE ~ 0 ) )%>%
  mutate( P07A02F = case_when( P07A02A == "Sí" ~ P07A02B + P07A02C/60 +
                                 P07A02D + P07A02E/60,
                               P07A02A == "No" ~ 0, TRUE ~ 0 ) )%>%
  
  mutate( P07A03F = case_when( P07A03A == "Sí" ~ P07A03B + P07A03C/60 +
                                 P07A03D + P07A03E/60,
                               P07A03A == "No" ~ 0, TRUE ~ 0 ) ) %>%
  mutate( P07A04F = case_when( P07A04A == "Sí" ~ P07A04B + P07A04C/60 +
                                 P07A04D + P07A04E/60,
                               P07A04A == "No" ~ 0, TRUE ~ 0 ) ) %>%
  
  mutate(P07A05F = case_when( P07A05A == "Sí" ~ P07A05B + P07A05C/60 +
                                P07A05D + P07A05E/60,
                              P07A05A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate(P07A06F = case_when( P07A06A == "Sí" ~ P07A06B + P07A06C/60 +
                                P07A06D + P07A06E/60,
                              P07A06A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A07F = case_when( P07A07A == "Sí" ~ P07A07B + P07A07C/60 +
                                   P07A07D + P07A07E/60,
                                 P07A07A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A08F = case_when( P07A08A == "Sí" ~ P07A08B + P07A08C/60 +
                                   P07A08D + P07A08E/60,
                                 P07A07A == "No" ~ 0, TRUE ~ 0 )) %>%
  
  mutate(   P07A09F = case_when( P07A09A == "Sí" ~ P07A09B + P07A09C/60 +
                                   P07A09D + P07A09E/60,
                                 P07A09A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate(   P07A10F = case_when( P07A10A == "Sí" ~ P07A10B + P07A10C/60 +
                                   P07A10D + P07A10E/60,
                                 P07A10A == "No" ~ 0, TRUE ~ 0 )) %>%
  mutate( P07Total = (P07A01F + P07A02F + P07A03F + P07A04F + P07A05F + P07A06F
                      + P07A07F + P07A08F + P07A09F + P07A10F)/7 )

m10_01 <- m10 %>%
  mutate( expandido = P07Total * factor ) %>%
  group_by(GruposEdad) %>%
  summarize( y = sum(expandido)) %>%
  rename(z = GruposEdad)

Trabajadorass_15_29 <- filter(Trabajadorass, P03A03 > 14 & P03A03 < 30)
Trabajadorass_15_29 <- sum(Trabajadorass_15_29$factor)

Trabajadorass_30_65 <- filter(Trabajadorass, P03A03 > 29 & P03A03 < 65)
Trabajadorass_30_65 <- sum(Trabajadorass_30_65$factor)

Trabajadorass_65 <- filter(Trabajadorass, P03A03 > 64)
Trabajadorass_65 <- sum(Trabajadorass_65$factor)


mujeres_15_29 <- m10_01[1,2]/Trabajadorass_15_29 /24*100
mujeres_30_65 <- m10_01[2,2]/Trabajadorass_30_65/24*100
mujeres_65 <- m10_01[3,2]/Trabajadorass_65/24*100


m10_01 <- data.frame(z =c("Mujer"), 
                    y = c(mujeres_15_29[[1]]),
                    x =  c(mujeres_30_65[[1]]),
                    w =  c(mujeres_65[[1]])) %>%
  rename('15-29' = y , '30-65' = x, '65+' = w )

m10_01 <- pivot_longer(m10_01, cols = 2:4, names_to = "x", values_to = "y") 


################################################################################
## Proporción de la semana de madres con trabajo remunerado dedicada a apoyo a otros hogare o a la comunidad


Madres_trabajadoras <- asalariados%>%
  filter(P03A02 == "Mujer") %>%
  filter(P03A12 %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
  
Trabajadorass <- filter(Madres_trabajadoras, P03A02 == "Mujer")

m12 <- Madres_trabajadoras %>%
  filter(P03A03 >14) %>%
  mutate( P07A01F = case_when( P07A12A == "Sí" ~ P07A12B + P07A12C/60 +
                                 P07A12D + P07A12E/60,
                               P07A12A == "No" ~ 0, TRUE ~ 0 ) )%>%
  mutate( P07Total = (P07A01F/7))

m12_01 <- m12 %>%
  mutate( expandido = P07Total * factor ) %>%
  group_by(GruposEdad) %>%
  summarize( y = sum(expandido)) %>%
  rename(z = GruposEdad)

Comunitario_15_29 <- filter(Trabajadorass, P03A03 > 14 & P03A03 < 30)
Comunitario_15_29 <- sum(Comunitario_15_29$factor)

Comunitario_30_65 <- filter(Trabajadorass, P03A03 > 29 & P03A03 < 65)
Comunitario_30_65 <- sum(Comunitario_30_65$factor)

Comunitario_65 <- filter(Trabajadorass, P03A03 > 64)
Comunitario_65 <- sum(Comunitario_65$factor)


mujeres_comu_15_29 <- m12_01[1,2]/Comunitario_15_29*100
mujeres_comu_30_65 <- m12_01[2,2]/Comunitario_30_65*100
mujeres_comu_65 <- m12_01[3,2]/Comunitario_65*100


m12_01 <- data.frame(z =c("Mujer"), 
                     y = c(mujeres_comu_15_29[[1]]),
                     x =  c(mujeres_comu_30_65[[1]]),
                     w =  c(mujeres_comu_65[[1]])) %>%
  rename('15-29' = y , '30-65' = x, '65+' = w )

m12_01 <- pivot_longer(m12_01, cols = 2:4, names_to = "x", values_to = "y") 



