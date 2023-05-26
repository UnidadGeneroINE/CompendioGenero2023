# READ ME: SCRIPT PARA COMPENDIO ESTADÍSTICO CON ENFOQUE DE GÉNERO 2022
# AUTOR: PAULA GÁLVEZ MOLINA - MARZO/ABRIL 2023


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
hogaresENEI <- read.spss(paste0(directorioBases, "BD_HOGARES.sav"),
                     use.value.labels = T,
                     to.data.frame = T)
personasENEI <- read.spss(paste0(directorioBases, "BASE_ENEI_22_PERSONAS.sav"), 
                          to.data.frame =T)

personasENEIINE <- read.spss(paste0(directorioBases, "ENEI2022INE.sav"), 
                             to.data.frame =T)

hogaresENEIINE <- read.spss(paste0(directorioBases, "ENEI2022INE_HOGARES.sav"), 
                             to.data.frame =T)

personasENEIINE2018 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_PERSONAS.sav"), 
                             to.data.frame =T)

hogaresENEIINE2018 <- read.spss(paste0(directorioBases, "BASE_ENEI_18_HOGARES.sav"), 
                            to.data.frame =T)

# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento. color1 es el principal, color2 es el secundariom del documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255)) 

################################################################################
# DEFINIR CONSTANTES
################################################################################

# Población total para censo 2018
poblacion2018 <- nrow(personasCenso)

# Población total para ENEI 2022
poblacion2022 <- sum(personasENEIINE$factor)

# Población Maya para censo 2018
poblacionMaya2018 <- nrow(filter(personasCenso, PCP12 == "Maya"))

# Total jefaturas de hogar ENEI 2022
jefaturas_de_hogar_22 <- filter(personasENEIINE, P03A05 == "Jefe (a) del hogar")
total_jefaturas_de_hogar_22 <- sum(jefaturas_de_hogar_22$factor)

# Agregando columna quinqueneo que indica el grupo de edad al que pertenece
# dependiendo de la edad reportada en el censo
quinqueneos <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', 
                 '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69',
                 '70-74', '75-79', '80-84', '85-89', '90-94', '95-99', '100+')
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
personasCenso$PCP6 <- factor(personasCenso$PCP6, levels = c("Mujer", "Hombre"))
personasENEI$P03A02 <- factor(personasENEI$P03A02, levels = c("Mujer", "Hombre"))
personasENEIINE$P03A02 <- factor(personasENEIINE$P03A02, levels = c("Mujer", "Hombre"))

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

g0_00 <- graficaColCategorias(data = poblacion_por_pueblos, ruta = paste0(directorioGraficas,"g0_00.tex"), 
                              etiquetas = "h")

################################################################################
# 1.1.	(OBSOLETA) Población por sexo, según grupos de edad - 2018
################################################################################

c1_01 <- personasCenso %>%
  group_by(PCP6, quinqueneo) %>%
  summarize(y = n()) %>%
  rename(z = PCP6, x = quinqueneo) %>%
  arrange(factor(x, levels = quinqueneos))

# Indica el orden en el que se debe mostrar los grupos quinquenales
c1_01$x <- factor(c1_01$x, levels = quinqueneos)

g1_01 <- graficaPiramide(data = c1_01, escala = 1000)
g1_01 <- exportarLatex(nombre = paste0(directorioGraficas, "g1_01.tex"), graph = g1_01)

################################################################################
# 1.1. Población por sexo, según grupos de edad (2022)
################################################################################

xlsxFile1 <- paste0(directorioBases, "Poblacion_estimacion_2022_por_sexo.xlsx")
c1_01 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "2022")) %>%
  arrange(factor(x, levels = quinqueneos))
c1_01$z <- factor(c1_01$z, levels = c("Mujer", "Hombre"))
c1_01$x <- factor(c1_01$x, levels = quinqueneos)

g1_01 <- graficaPiramide(data = c1_01, escala = 1000)
g1_01 <- exportarLatex(nombre = paste0(directorioGraficas, "g1_01.tex"), graph = g1_01)

################################################################################
# 1.2.	Población por sexo, según dominio de estudio
################################################################################

c1_02 <- personasENEIINE %>%
  group_by(P03A02, dominio) %>%
  summarize(y = sum(factor)/poblacion2022 *100) %>%
  rename(z = P03A02) %>%
  rename(x = dominio)

g1_02 <- graficaAnillosMultiples(c1_02, leyenda = "lado")

exportarLatex(nombre = paste0(directorioGraficas, "g1_02.tex"), graph = g1_02)

################################################################################
# 1.3.	Población por sexo, según Pueblos
################################################################################

c1_03 <- personasENEI %>%
  group_by(P03A02, P03A06) %>%
  summarize(y = sum(factor)/poblacion2022 *100) %>%
  rename(z = P03A02) %>%
  rename(x = P03A06) %>%
  mutate(x = case_when(x == "Afrodescendiente/Creole/Afro mestizo" ~ "Afrodescendiente*",
                            TRUE ~ x))

x <- c("Xinka", "Garífuna", "Ladino", "Afrodescendiente*", "Extranjero", "Maya")
Hombre <- c(as.numeric(c1_03[1,3]), as.numeric(c1_03[2,3]), 
             as.numeric(c1_03[3,3]), as.numeric(c1_03[4,3]), 
             as.numeric(c1_03[5,3]), as.numeric(c1_03[6,3]))
Mujer <- c(as.numeric(c1_03[7,3]), as.numeric(c1_03[8,3]), 
             as.numeric(c1_03[9,3]), as.numeric(c1_03[10,3]), 
             as.numeric(c1_03[11,3]), as.numeric(c1_03[12,3]))

c1_03 <- data.frame(x, Mujer, Hombre)

g1_03 <- graficaCategorias(c1_03, tipo = "barra", leyenda = "abajo")
exportarLatex(g1_03, nombre = paste0(directorioGraficas, "g1_03.tex"))

################################################################################
# 1.4.	Población por sexo, según comunidad lingüística
################################################################################
# Se optó a usar datos menos actualizados (Censo 2018) ya que la muestra de la 
# ENEI no permite para esta desagregación.

c1_04 <- personasCenso %>%
  filter(!is.na(PCP13)) %>%
  group_by(PCP6, PCP13) %>%
  summarize(y = n()) %>%
  rename(z = PCP6) %>%
  rename(x = PCP13)

g1_04 <- graficaPorcentajeApilada(c1_04, categoria_leyenda = "")

exportarLatex(nombre = paste0(directorioGraficas, "g1_04.tex"), graph = g1_04) 

################################################################################
# 1.5.	Población por sexo, según tipo de hogar (OBSOLETA)
################################################################################
# El tipo de hogar se define desde la página 9 del compendio del 2021 (miembros
# del hogar).

# Crea la categorización del tipo de vivienda al restringir quienes puedes ser
# miembros del hogar usando las funciones casewhen para hacerlo por casos
# y las funciones all() e %in% para restringir las personas miembros.
c1_05 <- personasENEIINE %>%
  group_by(hogar_num) %>%
  summarize(tipo_hogar = case_when(all(P03A05 == "Jefe (a) del hogar") ~ "Unipersonal",
                                   all(P03A05 %in% c("Jefe (a) del hogar","Esposo (a) o compañero (a)","Hijo (a)")) ~ "Nuclear",
                                   all(P03A05 %in% c("Jefe (a) del hogar", "Esposo (a) o compañero (a)",
                                                     "Hijo (a)", "Yerno o Nuera","Nieto (a)",
                                                     "Otro pariente", "Suegro (a)", "Padre o Madre", 
                                                     "Hermano (a)", "Cuñado (a)")) ~ "Ampliado",
                                   all(P03A05 %in% c("Jefe (a) del hogar", "Empleado domestico","Otro no pariente")) ~ "Corresidente",
                                   TRUE ~ "Compuesto"))

c1_05 <- merge(x = personasENEIINE, y = c1_05, by = "hogar_num") %>%
  group_by(tipo_hogar, P03A02) %>%
  summarise(casos = n())

  # TESTEANDO SI ESTÁ CORRECTO
compare <- personasENEIINE %>%
  group_by(hogar_num, P03A05) %>%
  summarize(integrantes = n()) %>%
  filter(hogar_num == 5252)

g1_05 <- graficaBarPorcentajeApilada(c1_05, "Sexo")

exportarLatex(nombre = paste0(directorioGraficas, "g1_04.tex"), graph = g1_05) 

################################################################################
# 1.6.	Población por sexo, según tipo de vivienda (PENDIENTE DE TERMINAR)
################################################################################

c1_06 <- merge(x = personasENEIINE, y = hogaresENEIINE, by = "hogar_num") %>%
  group_by(P03A02, P02A01) %>%
  summarize(y = sum(factor.x)/poblacion2022 *100) %>%
  mutate(P02A01 = case_when(P02A01 == "Cuarto_vecindad" ~ "Cuarto vecindad",
                            P02A01 == "Formal" ~ "Casa formal",
                            P02A01 == "Improvisada" ~ "Casa improvisada",
                            TRUE ~ P02A01)) %>%
  rename(z = P03A02) %>%
  rename(x = P02A01)

g1_06 <- graficaPorcentajeApilada(c1_06, tipo = "columna")

exportarLatex(nombre = paste0(directorioGraficas, "g1_06.tex"), graph = g1_06) 

################################################################################
# 1.7.	Mapa departamental por número de mujeres
################################################################################

# Datos obtenidos de https://www.ine.gob.gt/proyecciones/

################################################################################
# 1.8.	Mapa municipal por número de mujeres
################################################################################

# Datos obtenidos de https://www.ine.gob.gt/proyecciones/

################################################################################
# 1.9.	Esperanza de vida al nacer por sexo 
################################################################################

xlsxFile1 <- paste0(directorioBases, "Datos_vitales.xlsx")
c1_09 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "ESPERANZA_DE_VIDA"))
g1_09 <- graficaDobleLinea(c1_09, ruta = paste0(directorioGraficas,"g1_09.tex"), inicio = 60.0,  fin = 84.0)

################################################################################
# 1.10.	Tasa global de fecundidad (mujeres 15 a 49 años) 
################################################################################

xlsxFile1 <- paste0(directorioBases, "Datos_vitales.xlsx")
c1_10 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "TGF"))
g1_10 <- graficaLinea(c1_10, rotar = F)
exportarLatex(nombre = paste0(directorioGraficas,"g1_10.tex"), graph = g1_10)

################################################################################
# 1.11.	Tasa de fecundidad juvenil según edades simples (13 - 19 años)
################################################################################

xlsxFile1 <- paste0(directorioBases, "Datos_vitales.xlsx")
c1_11 <- data.frame(read.xlsx(xlsxFile = xlsxFile1, sheet = "ESPECIFICA"))
g1_11 <- graficaLinea(c1_11, rotar = F)
exportarLatex(nombre = paste0(directorioGraficas,"g1_11.tex"), graph = g1_11)

################################################################################
# 1.12.	Jefatura de hogar por sexo
################################################################################

c1_12 <- jefaturas_de_hogar_22 %>%
  group_by(P03A02) %>%
  summarize(y = sum(factor)/total_jefaturas_de_hogar_22 * 100)

g1_04 <- graficaBarPorcentajeApilada(c1_04, categoria_leyenda = "")
  
g1_12 <- graficaAnillo(c1_12, nombre = paste0(directorioGraficas,"g1_12.tex"), 
                       preambulo = F)

################################################################################
# 1.14.	Acceso a servicios básicos por sexo de jefatura de hogar
################################################################################
# Calculando el número de jefaturas por sexo
jefaturas_por_sexo <- jefaturas_de_hogar_22 %>% group_by(P03A02) %>% summarize(y = sum(factor))
jefas <- as.numeric(jefaturas_por_sexo[1,2])
jefes <- as.numeric(jefaturas_por_sexo[2,2])

# Uniendo bases de datos de personas (jefaturas) y hogares ENEI 2022
c1_14 <- merge(x = jefaturas_de_hogar_22, y = hogaresENEIINE, by = "hogar_num")

# Calculando el número (expandido) de jefaturas por sexo que tiene acceso a agua
agua <- filter(c1_14, P02B03 == "Tubería_dentro" | P02B03 == "Tubería_fuera") %>%
  group_by(P03A02) %>%
  summarize(y = sum(factor.x))
# Calculando el porcentaje que tiene acceso sobre el 100% de cada sexo
agua[1,2] <- as.numeric(agua[1,2])/jefas *100
agua[2,2] <- as.numeric(agua[2,2])/jefes *100

# Calculando el número (expandido) de jefaturas por sexo que tiene acceso a saneamiento
saneamiento <- filter(c1_14, P02B07 != "No_tiene") %>%
  group_by(P03A02) %>%
  summarize(y = sum(factor.x))
# Calculando el porcentaje que tiene acceso a saneamiento sobre el 100% de cada sexo
saneamiento[1,2] <- as.numeric(saneamiento[1,2])/jefas *100
saneamiento[2,2] <- as.numeric(saneamiento[2,2])/jefes *100

# Calculando el número (expandido) de jefaturas por sexo que tiene acceso a extracción de basura
extraccion <- filter(c1_14, P02B09 == "Servicio_municipal" | P02B09 == "Servicio_privado") %>%
  group_by(P03A02) %>%
  summarize(y = sum(factor.x))
# Calculando el porcentaje que tiene acceso a extracción sobre el 100% de cada sexo
extraccion[1,2] <- as.numeric(extraccion[1,2])/jefas *100
extraccion[2,2] <- as.numeric(extraccion[2,2])/jefes *100

# Calculando el número (expandido) de jefaturas por sexo que tiene acceso a electricidad de basura
electricidad <- filter(c1_14, P02A05C == "Si") %>%
  group_by(P03A02) %>%
  summarize(y = sum(factor.x))
# Calculando el porcentaje que tiene acceso a electricidad sobre el 100% de cada sexo
electricidad[1,2] <- as.numeric(electricidad[1,2])/jefas *100
electricidad[2,2] <- as.numeric(electricidad[2,2])/jefes *100

# Creando base de datos para graficar
agua <- cbind(x = c("agua", "agua"), agua)
extraccion <- cbind(x = c("extracción de basura", "extracción de basura"), extraccion)
electricidad <- cbind(x = c("electricidad", "electricidad"), electricidad)
c1_14 <- data.frame(rbind(agua, extraccion, electricidad)) %>%
  rename(z = P03A02)

g1_14 <- graficaCategorias(c1_14, leyenda = "lado")
exportarLatex(nombre = paste0(directorioGraficas,"g1_14.tex"), graph = g1_14)

################################################################################
# 1.15.	Jefatura de hogar por sexo, según dominio de estudio 
################################################################################

c1_15 <- jefaturas_de_hogar_22 %>%
  group_by(P03A02, dominio) %>%
  summarize(y = sum(factor)/total_jefaturas_de_hogar_22*100) %>%
  rename(x = dominio) %>%
  rename(z = P03A02)

g1_15<- graficaAnillosMultiples(c1_15, leyenda = "lado")

exportarLatex(nombre = paste0(directorioGraficas, "g1_15.tex"), graph = g1_15)
  
################################################################################
# 1.16.	Jefatura de hogar por sexo, según estado conyugal 
################################################################################

c1_16 <- jefaturas_de_hogar_22 %>%
  group_by(P03A02, P03A10) %>%
  summarize(y = sum(factor)/total_jefaturas_de_hogar_22*100) %>%
  rename(z = P03A02) %>%
  rename(x = P03A10)

g1_16 <- graficaPorcentajeApilada(c1_16, tipo = "barra", leyenda = "abajo")
exportarLatex(nombre = paste0(directorioGraficas, "g1_16.tex"), graph = g1_16)

################################################################################
# 1.17.	Mujeres jefas de hogar por número de hijas/hijos
################################################################################

c1_17 <- data.frame(x = c("0", "1-3", "4-6", "7+"), y = c(7.2, 45.8, 31.8, 15.1))
g1_17 <- graficaCol(c1_17)
g1_17 <- etiquetasHorizontales(graph = g1_17)
exportarLatex(nombre = paste0(directorioGraficas, "g1_17.tex"), graph = g1_17)

################################################################################
# DATOS EXTRAS DEL CAP. 1
################################################################################

esposasDeJefes <- filter(personasENEI, P03A02 == "Mujer" & 
                           P03A05 == "Esposo (a) o compañero (a)") %>%
  select(P03A02, P03A05, P03A10) %>%
  group_by(P03A05, P03A10) %>%
  summarize(y = n())

espososDeJefas <- filter(personasENEI, P03A02 == "Hombre" & 
                           P03A05 == "Esposo (a) o compañero (a)") %>%
  select(P03A02, P03A05, P03A10) %>%
  group_by(P03A05, P03A10) %>%
  summarize(y = n())


# Código para hacer tablas
TABLA <- kable(DATA, format = "latex", align = "c", digits = 1, booktabs = TRUE,
               linesep = "") %>%
  kable_styling(latex_options = "striped",
                stripe_color = lighten("#7470C8", amount = 0.25)) %>% 
  row_spec(0, bold = TRUE) %>% 
  #column_spec(1:ncol(c1_04), align = "c", 
  #latex_options = "m{2cm}") %>%
  cat(file = paste0(directorioGraficas, "TABLA.tex"))

############################# TESTEANDO 2018 ###################################
c1_05_2018 <- personasENEIINE2018 %>%
  group_by(NUM_HOGAR) %>%
  summarize(tipo_hogar = case_when(all(PPA05 == "Jefe(A) del hogar") ~ "Unipersonal",
                                   all(PPA05 %in% c("Jefe(A) del hogar","Esposo(a) o compañero(a)","Hijo(a)")) ~ "Nuclear",
                                   all(PPA05 %in% c("Jefe(A) del hogar", "Esposo(a) o compañero(a)",
                                                     "Hijo(a)", "Yerno o Nuera","Nieto(a)",
                                                     "Otro(a) pariente", "Suegro(a)", "Padre o madre", 
                                                     "Hermano(a)", "Cuñado(a)")) ~ "Ampliado",
                                   all(PPA05 %in% c("Jefe(A) del hogar", "Empleado(a) domestico(a)","Otro(a) no pariente", "Pensionista o huésped")) ~ "Corresidente",
                                   TRUE ~ "Compuesto"))

c1_05_2018 <- merge(x = personasENEIINE2018, y = c1_05_2018, by = "NUM_HOGAR") %>%
  group_by(tipo_hogar) %>%
  summarise(casos = sum(FACTOR)/poblacion2018ENEI*100)

poblacion2018ENEI <- sum(personasENEIINE2018$FACTOR)

################################################################################
  

tablaColApiladas(df, nombre_columnas = c("", "AH", "BEEE", "CI", "D", "EEH", "AH", "BEEE", "CI"), nombre_grupos= c("", "2018" = 4, "2019" = 4), ruta = paste0(directorioGraficas, "TABLA.tex"))


################################################################################