################################# CATEGORÍAS ###################################
# Barras o Columnas por Categorías
graficaCategorias <- function(data, tipo = "columna", decimales = TRUE, 
                                    categoria_leyenda = "", leyenda = "lado") {
  # Verificando si es válida la entrada de los parámetros "tipo" y "leyenda"
  if(!tipo %in% c("barra", "columna")) {
    stop("El valor de tipo debe ser 'barra' o 'columna'")
  }
  
  if(!leyenda %in% c("lado", "abajo", "arriba")) {
    stop("El valor de leyenda debe ser 'lado', 'abajo' o 'arriba'")
  }
  
  # Cargando colores
  colores <- colorRampPalette(c(pkg.env$color1, pkg.env$color2))
  
  # Creando gráfica
  grafica <- ggplot(data, aes(x = x, y = y, fill = z)) +
    scale_fill_manual(name = categoria_leyenda,
                      values =  colores(length(unique(data$z))))
  
  # Rotando el eje si se requiere tipo barra
  if(tipo == "barra") {
    grafica <- grafica + geom_bar(position = "dodge", stat = "identity") +
      coord_flip() +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(color = "black")) +
      geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}), 
                size = 3, position = position_dodge(width = 1), hjust = -0.5)
  } else {
  # Dejando el eje por defecto tipo columna
    grafica <- grafica + geom_col(position = "dodge") +
      theme(axis.text.x = element_text(color = "black"),
            axis.text.y = element_blank()) +
      geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}), 
                size = 3, position = position_dodge(width = 1), vjust = -1)
  }
  
  # Cambiando posición de leyenda
  if (leyenda == "abajo") {
    grafica <- grafica + theme(legend.position = "bottom")
  } else if (leyenda == "arriba") {
    grafica <- grafica + theme(legend.position = "top")
    }
  
  #Configurando fondo, ejes, y tamaño de cuadros de leyenda
  grafica <- grafica + theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.ticks = element_blank()) +
    guides(fill = guide_legend(override.aes = list(size = 4), keywidth = 0.4, keyheight = 0.4))
  return(grafica)
}

######################## APILADA ###############################################
graficaApilada <-  function(data, categoria_leyenda = "", tipo = "columna", 
                            decimales = TRUE, leyenda = "lado"){
  # Verificando si es válida la entrada de los parámetros "tipo" y leyenda
  if(!tipo %in% c("barra", "columna")) {
    stop("El valor de tipo debe ser 'barra' o 'columna'")
  }
  
  if(!leyenda %in% c("lado", "abajo", "arriba")) {
    stop("El valor de leyenda debe ser 'lado', 'abajo' o 'arriba'")
  }
  
  # Crea colores para división de una misma columna
  colores <- colorRampPalette(c(pkg.env$color1, pkg.env$color2))
  # Crea gráfica de columnas apiladas
  grafica <- ggplot(data, aes(fill = z, y = y, x = x)) + 
    geom_bar(position="stack", stat="identity") +
    geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}, 
                  y = y, group = z), # Agrega etiquetas blancas en caada sección
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white") +
    scale_fill_manual(name = categoria_leyenda, 
                      values =  colores(length(unique(data$z)))) +
    #Configurando fondo, ejes, y tamaño de cuadros de leyenda 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    scale_y_continuous(breaks = NULL) +
    guides(fill = guide_legend(override.aes = list(size = 4), keywidth = 0.4, keyheight = 0.4))
  
  # Rotando el eje si se requiere tipo barra, por defecto tipo columna
  if(tipo == "barra") {
    grafica <- grafica + #geom_bar(position = "dodge", stat = "identity") +
      coord_flip()
  }
  
  # Cambiando posición de leyenda
  if (leyenda == "abajo") {
    grafica <- grafica + theme(legend.position = "bottom")
  } else if (leyenda == "arriba") {
    grafica <- grafica + theme(legend.position = "top")
  }
  
  return(grafica)
}

############################# PORCENTAJE APILADA ###############################
graficaPorcentajeApilada <-  function(data, categoria_leyenda = "", tipo = "columna", 
                            decimales = TRUE, leyenda = "lado"){
  # Verificando si es válida la entrada de los parámetros "tipo" y leyenda
  if(!tipo %in% c("barra", "columna")) {
    stop("El valor de tipo debe ser 'barra' o 'columna'")
  }
  
  if(!leyenda %in% c("lado", "abajo", "arriba")) {
    stop("El valor de leyenda debe ser 'lado', 'abajo' o 'arriba'")
  }
  
  # Crea colores para división de una misma columna
  colores <- colorRampPalette(c(pkg.env$color1, pkg.env$color2))
  # Crea gráfica de columnas apiladas
  grafica <- ggplot(data, aes(fill = z, y = y, x = x)) + 
    geom_bar(position="stack", stat="identity") +
    geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}, 
                  y = y, group = z), # Agrega etiquetas blancas en caada sección
              position = position_stack(vjust = 0.5), 
              size = 3, color = "white") +
    scale_fill_manual(name = categoria_leyenda, 
                      values =  colores(length(unique(data$z)))) +
    #Configurando fondo, ejes, y tamaño de cuadros de leyenda 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()) +
    scale_y_continuous(breaks = NULL) +
    guides(fill = guide_legend(override.aes = list(size = 4), keywidth = 0.4, keyheight = 0.4))
  
  # Rotando el eje si se requiere tipo barra, por defecto tipo columna
  if(tipo == "barra") {
    grafica <- grafica + #geom_bar(position = "dodge", stat = "identity") +
      coord_flip()
  }
  
  # Cambiando posición de leyenda
  if (leyenda == "abajo") {
    grafica <- grafica + theme(legend.position = "bottom")
  } else if (leyenda == "arriba") {
    grafica <- grafica + theme(legend.position = "top")
  }
  
  return(grafica)
}

########################### CATEGORÍAS APILADAS ################################
# Grafica barras o columnas por categorías agrupadas simultánea +
# con categorías apiladas
graficaCategoriasApiladas <- function(data, tipo = "columna", decimales = TRUE, 
                              categoria_leyenda = "", leyenda = "lado", opacidad_categoria = 0.75) {
  # Verificando si es válida la entrada de los parámetros "tipo" y "leyenda"
  if(!tipo %in% c("barra", "columna")) {
    stop("El valor de tipo debe ser 'barra' o 'columna'")
  }
  
  if(!leyenda %in% c("lado", "abajo", "arriba")) {
    stop("El valor de leyenda debe ser 'lado', 'abajo' o 'arriba'")
  }
  
  # Cargando colores
  colores <- colorRampPalette(c(pkg.env$color1, pkg.env$color2))
  
  # Creando gráfica
  grafica <- ggplot(data, aes(x = x, y = y, fill = z)) +
    scale_fill_manual(name = categoria_leyenda,
                      values =  colores(length(unique(data$z))))
  
  # Rotando el eje si se requiere tipo barra
  if(tipo == "barra") {
    grafica <- grafica + geom_bar(position="stack", stat="identity") +
      coord_flip() +
      facet_grid(rows = vars(w)) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(color = "black")) +
      geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}, 
                    y = y, group = z), # Agrega etiquetas blancas en caada sección
                position = position_stack(vjust = 0.5), 
                size = 3, color = "white")
  } else {
    # Dejando el eje por defecto tipo columna
    grafica <- grafica + geom_bar(position="stack", stat="identity") +
      facet_grid(~ w, switch = "y") +
      theme(axis.text.x = element_text(color = "black"),
            axis.text.y = element_blank()) +
      geom_text(aes(label = if(decimales == FALSE){y} else {as.numeric(sprintf(y, fmt = '%.1f'))}, 
                    y = y, group = z), # Agrega etiquetas blancas en caada sección
                position = position_stack(vjust = 0.5), 
                size = 3, color = "white")
  }
  
  # Cambiando posición de leyenda
  if (leyenda == "abajo") {
    grafica <- grafica + theme(legend.position = "bottom")
  } else if (leyenda == "arriba") {
    grafica <- grafica + theme(legend.position = "top")
  }
  
  #Configurando fondo, ejes, y tamaño de cuadros de leyenda
  grafica <- grafica + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             axis.title.x = element_blank(),
                             axis.title.y = element_blank(),
                             axis.ticks = element_blank(),
                             strip.background = element_rect(fill = lighten(pkg.env$color2, amount = opacidad_categoria))) +
    guides(fill = guide_legend(override.aes = list(size = 4), keywidth = 0.4, keyheight = 0.4))
  return(grafica)
}

############################## ANILLOS MÚLTIPLES ###############################
graficaAnillosMultiples <- function(data, categoria_leyenda = "", 
                                    leyenda = "arriba", decimales = TRUE){
  test <- graficaPorcentajeApilada(c1_02, tipo = "barra", leyenda = "arriba")
  
  # Creando segunda leyenda de orden de anillos
  leyenda2 <- ""
  for (i in 1:length(c1_02$x)) { if (duplicated(data$x)[i] == FALSE) {
    leyenda2 <- paste0(leyenda2, i, ": ", as.character(data$x[i]), "\n")}
  }
  
  # Creando gráfica
  grafica <- graficaPorcentajeApilada(data, categoria_leyenda = categoria_leyenda,
                                      decimales =  decimales, tipo = "barra",
                                      leyenda = leyenda) +
    coord_polar("y") +
    theme( axis.text.y = element_blank(),
           axis.text.x = element_blank()) +
    geom_text(aes(label = ifelse(duplicated(x), x, "")), position = position_fill(vjust = 0), 
              size = 4, color = "white", fontface = "bold") +
    labs(caption = leyenda2) +
    theme(plot.caption = element_text(size = 10, color = "black", hjust = 0))
  
  return(grafica)
}

################################ TEST ##########################################
# c_test <- data.frame(w = c("2018", "2018", "2018", "2018", "2018", "2018",
# "2022", "2022", "2022", "2022", "2022", "2022"),
# x = c("Xinka", "Xinka", "Maya", "Maya", "Garífuna", "Garífuna",
#       "Xinka", "Xinka", "Maya", "Maya", "Garífuna", "Garífuna"),
# z = c("Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre",
#       "Mujer", "Hombre", "Mujer", "Hombre", "Mujer", "Hombre"),
# y = c(12.3, 98.5, 44.5, 0.2, 14.7, 10.3,
#       14.2, 68.5, 25.3, 1.8, 36.6, 47.6)
# )

# test <- graficaApilada(c1_02, tipo = "barra", categoria_leyenda = "Sexo", leyenda = "arriba")
# 
# test <- graficaCategorias(c1_03, tipo = "barra", leyenda = "abajo")
# 
# test <- graficaCategoriasApiladas(c_test, tipo = "columna", categoria_leyenda = "",
#                                     leyenda = "abajo")

test <- graficaPorcentajeApilada(c1_04)

# t <- c1_03 %>%
#   rename(o = z) %>%
#   rename(z = x) %>%
#   rename(x = o)

# test <- graficaAnillosMultiples(c1_02, leyenda = "lado", categoria_leyenda = "Pueblo")
exportarLatex(nombre = paste0(directorioGraficas, "test.tex"), graph = test)

# 
# 
