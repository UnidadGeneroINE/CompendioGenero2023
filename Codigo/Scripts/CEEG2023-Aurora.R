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

#Tabla latex 
library(magrittr) #install.packages("magrittr")




# Rutas del directorio de bases y gráficas
directorioBases <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\"
directorioGraficas <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Graficas\\"
directorioCenso <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\BASE_CENSO_2018\\"
baseviolencia <- "C:\\Users\\Unidadgenero\\OneDrive - ine.gob.gt\\Documentos\\Github\\CompendioGenero2023\\Codigo\\Bases\\datos_administrativos\\Indicadores_de_Género\\VIOLENCIA\\"

# Lectura de bases de datos

Datos_VIF <- paste0(baseviolencia, "DATOS_VIF_2022.xlsx")

Datos_VCM <- paste0(baseviolencia, "DATOS_VCM_2022.xlsx")

personasCENSO <- read.spss(paste0(directorioCenso, "PERSONA_CENSO_2018.sav"), 
                          to.data.frame =T)

# Definiendo color y otras caracterísiticas necesarias dependiendo del tipo de 
# documento. color1 es el principal, color2 es el secundariom del documento
anual(color1 = rgb(54,50,131, maxColorValue = 255), color2 = rgb(116, 112, 200, maxColorValue = 255))


################################################################################
# DEFINIR CONSTANTES
################################################################################

# Indica el orden en el que se deben mostrar los sexos para CENSO 2018

personasCENSO$PCP6 <- factor(personasCENSO$PCP6, levels = c("Mujer", "Hombre"))


EstadoConyugal <- c('Soltera(o)', 'Unida(o)', 'Casada(o)', 'Separada(o)', 'Divorciada(o)', 'Viuda(o)', 'Menor de 10 años')
personasCENSO <- personasCENSO %>%
  mutate( EstadoConyugal = case_when( PCP34 == "Unida(o)" ~ 'Unida(o)',
                                      PCP34 == "Casada(o)" ~ 'Casada(o)',
                                      PCP34 == "Viuda(o)" ~ 'Viuda(o)', 
                                      PCP34 == "Separada(o) de una unión libre" ~ 'Separada(o)*', 
                                      PCP34 == "Divorciada(o)" ~ 'Divorciada(o)',  
                                      PCP34 == "Separada(o) de un matrimonio" ~ 'Separada(o)*',
                                      PCP34 == "Soltera(o)" ~ 'Soltera(o)',
                                      PCP34 == "NA" ~ 'Menor de 10 años'))




############################################################################
###                                                                      ###
###                              CAPÍTULO 5                              ###
###                              Violencia                               ###
###                                                                      ###
############################################################################

################################################################################
# 5.1	Tasa de denuncia por cada 1,000 mujeres por departamento (2022)
################################################################################
c5_01 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_1"))

#Crear grafica y etiquetas
g5_01 <- graficaBar(c5_01, ancho = 0.6)
g5_01 <- etiquetasBarras(g5_01, precision = 1)

# Enviar grafica a latex
exportarLatex(nombre = paste0(directorioGraficas,"g5_01.tex"),
              graph = g5_01, preambulo = F)


################################################################################
# 5.2	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por departamento de registro (2022)
################################################################################

#Tabla 1 
c5_02 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_2")) %>%
  rename("El Progreso" = El.Progreso) %>%
  rename("Santa Rosa" = Santa.Rosa)
# Enviar a Latex 
Tabla5_02 <-  tablaLaTeX(c5_02, opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_02.tex"))

#Tabla 1A 
c5_02A <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_2A")) %>%
  rename("San Marcos" = San.Marcos)

# Enviar a Latex 
Tabla5_02A <-  tablaLaTeX(c5_02A, opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_02A.tex"))

#Tabla 1A 
c5_02B <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_2B")) %>%
  rename("Baja Verapaz" = Baja.Verapaz) %>%
  rename("Alta Verapaz" = Alta.Verapaz)

# Enviar a Latex 
Tabla5_02B <-  tablaLaTeX(c5_02B, opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_02B.tex"))

################################################################################
# 5.3.	Denuncias por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según grupos de edad 
################################################################################

c5_03 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_3")) %>%
  rename("Tipo de delito" = Tipo.de.delito) %>%
  rename("0-14" = X0.14) %>%
  rename("15-29" = X15.29) %>%
  rename("30-45" = X30.45) %>%
  rename("45-64" = X45.64) %>%
  rename("65+" = X65.)

# Enviar a Latex 
Tabla5_03 <-  tablaLaTeX(c5_03, nombre_grupos = c(" ", "Grupos de edad" = 5, " "),
                         opacidad_filas = 0.4, ruta = paste0(directorioGraficas, "Tabla5_03.tex"))

################################################################################
# 5.4.	Denuncia por los delitos contemplados en la Ley Contra el Femicidio 
# y Otras Formas de Violencia Contra a la Mujer por tipo de delito, 
# según Pueblos
################################################################################

c5_04 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_4")) %>%
  rename("Tipo de violencia(delito)" = Tipo.de.delito..violencia.) %>%
  rename("No indica" = No.indica)

# Enviar a Latex 
Tabla5_04 <-  tablaLaTeX(c5_04, nombre_grupos = c(" ", "Pueblo de pertenencia" = 5, " "),
                         opacidad_filas = 0.4, ruta = paste0(directorioGraficas, "Tabla5_04.tex"))

################################################################################
# 5.5.	Mujeres víctimas de violencia contra la mujer, según Pueblos
################################################################################

c5_05 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_5")) %>%
  rename("Pueblo" = Pueblo.de.pertenencia) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, "2022" = X2022)

# Enviar a Latex 
Tabla5_05 <-  tablaLaTeX(c5_05, opacidad_filas = 0.4, ruta = paste0(directorioGraficas, "Tabla5_05.tex"))


################################################################################
# 5.6. Mujeres víctimas de violencia contra la mujer, según tipo de violencia sufrida 
# (serie histórica de 2018 a 2022)
################################################################################

c5_06 <- data.frame(read.xlsx(xlsxFile = Datos_VCM, sheet = "5_6")) %>%
  rename("Tipo de violencia" = Tipo.de.violencia) %>%
  rename("2018" = X2018, "2019" = X2019, "2020" = X2020, "2021" = X2021, "2022" = X2022)

# Enviar a Latex 
Tabla5_06 <-  tablaLaTeX(c5_06, opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_06.tex"))

################################################################################
# 5.10.	Víctimas de violencia intrafamiliar por sexo, según Pueblos, 2018-22 
################################################################################

c5_10 <- data.frame(read.xlsx(xlsxFile = Datos_VIF, sheet = "5_10"))

# Enviar a Latex 
Tabla5_10 <-  tablaLaTeX(c5_10, nombre_grupos = c(" ", "2018" = 2, "2022" = 2), 
                         opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_10.tex"))

################################################################################
# 5.5.	Víctimas de violencia intrafamiliar por sexo, según tipo de agresión 
# sufrida (serie histórica de 2018 a 2022)
################################################################################

c5_05 <- data.frame(read.xlsx(xlsxFile = Datos_VIF, sheet = "5_5")) %>%
  rename("Tipo de Agresión" = Tipo.agresión)

# Enviar a Latex 
Tabla5_05 <-  tablaLaTeX(c5_05, nombre_grupos = c(" ", "2018" = 2, "2022" = 2), 
                         opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_05.tex"))

################################################################################
# 5.6.	Instituciones que prestan atención a víctimas de violencia intrafamiliar,
# según tipo de servicio brindado (período 2018 - 2022)
################################################################################

c5_06 <- data.frame(read.xlsx(xlsxFile = Datos_VIF, sheet = "5_6")) %>%
  rename("Institución que recibió la denuncia" = Institución.que.recibió.la.denuncia)

# Enviar a Latex 
Tabla5_06 <-  tablaLaTeX(c5_06, nombre_grupos = c(" ", "2018" = 2, "2022" = 2), 
                         opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Tabla5_06.tex"))


################################################################################
# 5.16. Estado conyugal en personas entre 12 a 17 años por sexo, 2018
################################################################################

#Sebe cargar la base del CENSO PARA HACER ALGUN CAMBIO
c5_16 <- filter(personasCENSO,  EstadoConyugal != "Soltera(o)" & PCP7 < 18) %>%
  group_by(PCP7, PCP6, EstadoConyugal) %>%
  summarise(y = n()) %>% 
  rename(x = PCP7) %>%
  rename(z = PCP6) %>%                                          
  rename(w = EstadoConyugal) %>%                                          
  select(w, x, z, y)

# Se paso las filas de "x" a columnas
c5_16 <- pivot_wider(c5_16, names_from = w, values_from = c(y))                                         

c5_16$z <- factor(c5_16$z, levels = c("Mujer", "Hombre"))

c5_16 <- c5_16 %>%    
  rename("Edad" = x, "Sexo" = z)

# ordenar segun estado conyugal
#c5_16$"Estado Conyugal" <- factor(c5_16$"Estado Conyugal", levels = c("Unida(o)", "Casada(o)", 	
                                                                      #"Separada(o) de una unión libre", "Separada(o) de un matrimonio",
                                                                     #"Divorciada(o)", 	"Viuda(o)")
Tabla5_16 <- tablaLaTeX(c5_16, nombre_grupos = c(" ", " ", "Estado conyugal" = 5),
                        ruta = paste0(directorioGraficas, "Tabla5_16.tex"))

rm(personasCENSO)
################################################################################
# Prueba para agrupar las filas en una tabla para latex
Test <- data.frame(
  Grupos = c("Grupo 1", "Grupo 1", "Grupo 2", "Grupo 2"),
  Categorias = c("Categoría 1", "Categoría 2", "Categoría 1", "Categoría 2"),
  Mujeres = c(10, 20, 30, 40),
  Hombres = c(40, 30, 20, 10))

Prueba <- tablaPrueba(Test, nombre_columnas = colnames(Test), 
                        nombre_grupos = c(" ", " ", "Valores" = 2 ), nombre_grupos_filas = c("Grupo 1" = 2 , "Grupo 2" = 2 ), 
                        opacidad_filas = 0.5, ruta = paste0(directorioGraficas, "Prueba.tex"))

tablaPrueba <- function(data, nombre_columnas = colnames(data), 
                       nombre_grupos = NULL, nombre_grupos_filas = NULL, opacidad_filas = 0.5, ruta){
  kbl(data, format = "latex", align = "c", digits = 1, booktabs = TRUE,
      linesep = "", col.names = nombre_columnas) %>%
    add_header_above(nombre_grupos, bold = TRUE) %>%
    kable_styling(latex_options = "striped",
                  stripe_color = lighten(pkg.env$color2, amount = opacidad_filas)) %>% 
    row_spec(0, bold = TRUE) %>% 
    group_rows(groups = nombre_grupos_filas, bold = TRUE) %>%
    save_kable(ruta)
  
  file <- readLines(ruta) %>%
    .[!grepl("\\\\centering", .)] %>%
    .[!grepl("\\\\begin\\{table\\}", .)] %>%
    .[!grepl("\\\\end\\{table\\}", .)]
  writeLines(file, ruta)
}


