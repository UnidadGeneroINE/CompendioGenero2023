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
###                              CAPÍTULO 5                              ###
###                              Violencia                               ###
###                                                                      ###
############################################################################

################################################################################
# 5.1. Estado conyugal de personas de 12 a 17 años, 2018 
################################################################################

c5_01 <- filter(personasCENSO,  PCP34 != "Soltera(o)" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, PCP34) %>%
  summarise(y = n()) %>% 
  rename(x = PCP7) %>%
  rename(z = PCP6) %>%
  rename(w = PCP34) %>%
  select(z, w, x, y)

# Se paso las filas de "y" a columnas
c5_01 <- pivot_wider(c5_01, names_from = z, values_from = c(y))

################################################################################
# 5.2.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar 
# y Erradicar la Violencia Intrafamiliar por sexo, según departamento de registro 
################################################################################

################################################################################
# 5.3.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar 
# y Erradicar la Violencia Intrafamiliar por sexo, según tipo de agresión sufrida
################################################################################

################################################################################
# 5.4.	Denuncias por los delitos contemplados en la Ley para Prevenir, Sancionar
# y Erradicar la Violencia Intrafamiliar por sexo, según grupos de edad
################################################################################

################################################################################
# 5.5.	Víctimas de violencia intrafamiliar por sexo, según Pueblos 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.6.	Víctimas de violencia intrafamiliar por sexo, según tipo de agresión 
# sufrida (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.7.	Instituciones que prestan atención a víctimas de violencia intrafamiliar,
# según tipo de servicio brindado (período 2018 - 2022)
################################################################################

################################################################################
# 5.8.	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por departamento de registro
################################################################################

################################################################################
# 5.9.	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según grupos de edad 
################################################################################

################################################################################
# 5.10.	Denuncia por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según Pueblos
################################################################################

################################################################################
# 5.11.	Víctimas de violencia contra la mujer, según Pueblos 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.12.	Víctimas de violencia contra la mujer, según tipo de agresión sufrida 
# (serie histórica de 2018 a 2022)
################################################################################

################################################################################
# 5.13.	Muertes violentas por sexo, según causa de muerte y grupos de edad 
# (comparar 2018 y 2022)
################################################################################

################################################################################
# 5.14.	Muertes violentas de mujeres relacionadas con hechos delictivos 
# (serie histórica 2018 - 2022)
################################################################################

################################################################################
# 5.15.	Índice de mortalidad femenina (serie histórica 2018 - 2022)
################################################################################

################################################################################
# 5.16.	Mujeres privadas de libertad por tipo de delito 
# (serie histórica 2018 - 2022)
################################################################################
