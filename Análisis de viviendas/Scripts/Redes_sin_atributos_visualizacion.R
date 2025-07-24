################################################################################
#######################Buscar estructuras de las redes##########################
################################################################################
#Este script crea tipologías de las redes creadas a partir de la encuesta CASEN#
#Para crear estas tipologías primeramente quitamos los atributos de las redes kinship creadas anteriormente con el fin de ver únicamente su estructura y luego calcular la frecuencia de esa estructura en la lista de redes kinship#

#Liberías
library(BAMMtools)
library(igraph)
library(ggplot2)
library(grid)
library(gridExtra)
library(png)
library(dplyr)

## Cargar datos de redes
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")

## Función para obtener una huella estructural (fingerprint) de un grafo
graph_fingerprint <- function(graph) {
  # Copia el grafo
  g_clean <- graph
  
  # Elimina todos los atributos de vértice
  for (attr in vertex_attr_names(g_clean)) {
    g_clean <- delete_vertex_attr(g_clean, attr)
  }
  
  # Calcula permutación canónica (estructura pura)
  perm <- canonical_permutation(g_clean)
  canon_graph <- permute(g_clean, perm$labeling)
  
  # Obtiene matriz de adyacencia para fingerprint
  adj <- as_adjacency_matrix(canon_graph, sparse = FALSE)
  paste(adj, collapse = "")
}

# Paso 1: calcular fingerprints para todos los grafos
fingerprints <- sapply(kinship_igraph_no_attrs, function(x) {
  graph_fingerprint(x$kinship_net)
})
save(fingerprints, file = "Análisis de viviendas/Data/fingerprints.RData")
load("Análisis de viviendas/Data/fingerprints.RData")

# Paso 2: contar frecuencias de cada fingerprint único
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]  # Reordenamos la tabla
names(tipologia_freq) <- paste0("T", seq_along(unique_fps))

# Paso 3. Mostrar la tabla
print(tipologia_freq)

# 5. Guardar un grafo de ejemplo por tipología
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))  # Asignar nombres T1, T2, ...

# 6. Graficar el ejemplo más común
plot(
  examples[[1]],
  main = paste("Ejemplo de la tipología más frecuente (", names(examples)[[1]], ")"),
  edge.label = E(examples[[1]])$type,
  edge.arrow.size = 0.4,
  vertex.size = 25,
  vertex.label.cex = 0.8
)
plot(
  examples[[2]],
  main = paste("Ejemplo de la segunda tipología más frecuente (", names(examples)[[2]], ")"),
  edge.label = E(examples[[2]])$type,
  edge.arrow.size = 0.4,
  vertex.size = 25,
  vertex.label.cex = 0.8
)
plot(
  examples[[10]],
  main = paste("Ejemplo de la décima tipología más frecuente (", names(examples)[[10]], ")"),
  edge.label = E(examples[[10]])$type,
  edge.arrow.size = 0.4,
  vertex.size = 25,
  vertex.label.cex = 0.8
)

################################################################################
####################### Generación de Archivos de Resultados ###################
################################################################################

# Cargar librerías adicionales necesarias
required_packages <- c("igraph", "ggplot2", "grid", "gridExtra", "RColorBrewer", "purrr")
lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
})

################################################################################
## Paso 1: Crear directorios de salida
################################################################################

# Crear estructura de directorios si no existe
dir.create("Análisis de viviendas/Analisis/Resultados_Tipologias", showWarnings = FALSE)
dir.create("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos", showWarnings = FALSE)
dir.create("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes", showWarnings = FALSE)
dir.create("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Resumen_tamano", showWarnings = FALSE)

################################################################################
## Paso 2: Generar reporte tabular completo
################################################################################

# Crear dataframe con la información de las tipologías
tipologia_df <- data.frame(
  Tipologia = names(tipologia_freq),
  Frecuencia = as.numeric(tipologia_freq),
  Porcentaje = round(as.numeric(tipologia_freq)/length(fingerprints)*100, 2),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(Frecuencia))

# Agregar información estructural básica
tipologia_df$Nodos <- sapply(examples, vcount)
tipologia_df$Aristas <- sapply(examples, ecount)
tipologia_df$Densidad <- round(sapply(examples, edge_density), 4)

# Guardar como CSV
write.csv(tipologia_df, "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv", row.names = FALSE)

# Agregar al dataframe households que coincidan
# Crear una lista que mapee cada fingerprint a los household IDs correspondientes
household_mapping <- lapply(unique_fps, function(fp) {
  # Obtener los índices de todas las redes con este fingerprint
  indices <- which(fingerprints == fp)
  # Extraer los household IDs de estas redes
  sapply(indices, function(i) kinship_igraph_no_attrs[[i]]$household_i)
})

# Nombrar la lista con los nombres de las tipologías (T1, T2, ...)
names(household_mapping) <- names(tipologia_freq)

# Agregar los household IDs al dataframe como una columna de listas
tipologia_df_households <- tipologia_df
tipologia_df_households$Households <- household_mapping[tipologia_df$Tipologia]

# Convertir listas a strings para CSV
tipologia_df_households$Households <- sapply(household_mapping[tipologia_df$Tipologia], 
                                             function(x) paste(x, collapse = ";"))

# Guardar ambos formatos
write.csv(tipologia_df_households, "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.csv", row.names = FALSE)
saveRDS(tipologia_df_households, "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds")

# Versión expandida
expanded_df <- do.call(rbind, lapply(names(household_mapping), function(tip) {
  hh <- household_mapping[[tip]]
  data.frame(
    Tipologia = tip,
    Frecuencia = length(hh),
    Household = hh,
    stringsAsFactors = FALSE
  )
}))

write.csv(expanded_df, "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_detallados.csv", row.names = FALSE)
 ## agregar columna de frecuencia acumulada, relativa y absoluta.

################################################################################
## Paso 3: Determinar punto de corte de tipologías
################################################################################
tipologia_df <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv", header = T, sep=,)

## Jenks Breaks ################################################################
plotJenks <- function(data, nbreaks=3, brks.cex=0.70, top.margin=10, dist=5){ 
  df <- data.frame(sorted.values=sort(data, decreasing=TRUE))
  Jclassif <- classIntervals(df$sorted.values, nbreaks, style = "jenks") #requires the 'classInt' package
  test <- jenks.tests(Jclassif) #requires the 'classInt' package
  df$class <- cut(df$sorted.values, unique(Jclassif$brks), labels=FALSE, include.lowest=TRUE) #the function unique() is used to remove non-unique breaks, should the latter be produced. This is done because the cut() function cannot break the values into classes if non-unique breaks are provided
  if(length(Jclassif$brks)!=length(unique(Jclassif$brks))){
    info <- ("The method has produced non-unique breaks, which have been removed. Please, check '...$classif$brks'")
  } else {info <- ("The method did not produce non-unique breaks.")}
  loop.res <- numeric(nrow(df))
  i <- 1
  repeat{
    i <- i+1
    loop.class <- classIntervals(df$sorted.values, i, style = "jenks")
    loop.test <- jenks.tests(loop.class)
    loop.res[i] <- loop.test[[2]]
    if(loop.res[i]>0.9999){
      break
    }
  }
  max.GoF.brks <- which.max(loop.res)
  plot(x=df$sorted.values, y=c(1:nrow(df)), type="b", main=paste0("Jenks natural breaks optimization; number of breaks: ", nbreaks), sub=paste0("Goodness of Fit: ", round(test[[2]],4), ". Max GoF (", round(max(loop.res),4), ") with breaks:", max.GoF.brks), ylim =c(0, nrow(df)+top.margin), cex=0.75, cex.main=0.95, cex.sub=0.7, ylab="observation index", xlab="value (increasing order)")
  abline(v=Jclassif$brks, lty=3, col="red")
  text(x=Jclassif$brks, y= max(nrow(df)) + dist, labels=sort(round(Jclassif$brks, 2)), cex=brks.cex, srt=90)
  results <- list("info"=info, "classif" = Jclassif, "breaks.max.GoF"=max.GoF.brks, "class.data" = df)
  return(results)
}
library(classInt)
plotJenks(tipologia_df$Frecuencia, nbreaks = 2) # Salen solo las 6 primeras tipologías.
#'plotJenks': R function for plotting univariate classification using Jenks' natural break method
#September 2017
#DOI: 10.13140/RG.2.2.18011.05929
#Languages: RRepository: http://cainarchaeology.weebly.com/r-function-for-plotting-jenks-natural-breaks-classification.html

library(strucchange)
bp <- breakpoints(Frecuencia ~ 1, data = tipologia_df, h = 0.05)  # h = tamaño mínimo del segmento (5% de datos)
summary(bp)  # Muestra ubicación de los quiebres
plot(bp)     # Gráfico con los breakpoints
# Obtener la posición del breakpoint
bp_opt <- breakpoints(Frecuencia ~ 1, data = tipologia_df, h = 0.05)
summary(bp_opt)$breakpoints  # Devuelve la posición
plot(tipologia_df$Frecuencia, type = "o", pch = 19, main = "Breakpoints en Frecuencias")
abline(v = bp_opt$breakpoints, col = "red", lty = 2)  # Línea vertical en el quiebre
muestra_tipologias<- tipologia_df
muestra_tipologias$break_group <- ifelse(
  seq_along(muestra_tipologias$Frecuencia) <= bp_opt$breakpoints,
  "Grupo Alto",
  "Grupo Bajo"
)
break_pos <- bp$breakpoints  # Posición del mejor breakpoint
break_pos # dejan las tipologías hasta el 48
View(muestra_tipologias) 

#breaks <- getJenksBreaks(tipologia_df$Frecuencia,5)
#print(breaks)  

# Obtener breakpoints
break_pos <- bp$breakpoints  # Posición del mejor breakpoint
ggplot(muestra_tipologias, aes(x = seq_along(Frecuencia), y = Frecuencia)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  geom_vline(xintercept = break_pos, linetype = "dashed", color = "black") +
  annotate("text", x = break_pos, y = max(muestra_tipologias$Frecuencia), 
           label = paste("Breakpoint:", break_pos), vjust = -1) +
  labs(
    title = "Puntos de Quiebre en Frecuencias",
    x = "Orden de Tipologías (por frecuencia)",
    y = "Frecuencia"
  ) +
  theme_minimal()

#Filtrar data por breakpoint del "Grupo Alto"
muestra_tipologias <-muestra_tipologias %>% dplyr::filter(break_group == "Grupo Alto")
#Guardar data de tipologias
write.table(muestra_tipologias, file = "Análisis de viviendas/Data/muestra_tipologias.csv",
            sep = "\t", row.names = F)

################################################################################
## Paso 4: Visualización de tipologías
################################################################################
# Función para graficar tipologías
plot_tipologia <- function(g, nombre, freq) {
  # Configuración visual consistente
  V(g)$color <- "#6baed6"
  V(g)$size <- 25
  V(g)$label.cex <- 1
  V(g)$frame.color <- "white"
  V(g)$label.color <- "black"
  
  # Configuración de aristas
  edge_types <- if("type" %in% edge_attr_names(g)) E(g)$type else rep("descent", ecount(g))
  
  # Asignar colores y etiquetas según tipo
  edge_colors <- ifelse(edge_types == "marriage", "#4daf4a", "#ff7f00")  # Verde para marriage, naranja para descent
  edge_labels <- ifelse(edge_types == "marriage", "marriage", "descent")
  
  E(g)$color <- edge_colors
  E(g)$label <- edge_labels
  E(g)$label.color <- "black"
  E(g)$label.cex <- 1
  E(g)$arrow.size <- ifelse(edge_types == "marriage", 0.5, 0.4)  # Sin flecha para marriage
  E(g)$arrow.width <- ifelse(edge_types == "marriage", 0.5, 0.8)
  E(g)$width <- 1 #Esto es el ancho de la edge
  E(g)$curved <- 0 # Sin curvatura para distinguir aristas paralelas
  
  # Archivo de salida
  png_file <- paste0("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/", nombre, ".png")
  
  # Configurar dispositivo gráfico
  png(png_file, width = 850, height = 750, bg = "white")
  
  # Diseño del gráfico
  layout(matrix(c(1, 2,3), nrow = 3), heights = c(3, 1, 0.7))
  
  # Gráfico de la red
  par(mar = c(1, 1, 3, 1))
  plot(g, main = paste("Tipología:", nombre, "| Frecuencia:", freq, "casos"),
       vertex.label.family = "sans", edge.label.family = "sans")
  
  # Información adicional
  par(mar = c(1, 1, 1, 1))
  plot.new()
  text(0.5, 0.8, paste("Nodos:", vcount(g)), cex = 1.2, font = 2)
  text(0.5, 0.5, paste("Aristas:", ecount(g)), cex = 1.2, font = 2)
  text(0.5, 0.2, paste("Densidad:", round(edge_density(g), 4)), cex = 1.2, font = 2)
  
  #Leyenda personalizada
  par(mar = c(0, 1, 0, 1))
  plot.new()
  legend("center",
         legend = c("marriage", "descent"),
         col = c("#4daf4a", "#ff7f00"),  # Verde y naranja
         lwd = 3,
         lty = 1,
         pch = NA,
         bty = "n",
         horiz = TRUE,
         cex = 1.1,
         text.width = max(strwidth(c("marriage", "descent"))))
  
  dev.off()
}

# Graficar las 48 tipologías más frecuentes
mapply(function(tip, freq) {
  plot_tipologia(examples[[tip]], tip, freq)
}, 
muestra_tipologias$Tipologia,  # Nombres de las tipologías
muestra_tipologias$Frecuencia,         # Frecuencias
SIMPLIFY = FALSE)           # Para evitar simplificación a vector

################################################################################
## Paso 5: Gráfico consolidado por tamaño de vivienda
################################################################################
generate_consolidated_graphs <- function(tipologia_df, examples, output_dir) {
  
  # Crear directorio de salida
  img_dir <- "Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Resumen_tamanos"
  if (!dir.exists(img_dir)) dir.create(img_dir, recursive = TRUE)
  
  # Configuración de colores para tipos de aristas
  edge_type_colors <- c("marriage" = "#4daf4a",  # Verde
                        "descent" = "#ff7f00")    # Naranja
  
  # Agrupar tipologías por tamaño (número de nodos)
  tipologias_por_tamano <- split(tipologia_df, tipologia_df$Nodos)
  
  # Procesar cada grupo de tamaño
  for (tamano in names(tipologias_por_tamano)) {
    tipologias <- tipologias_por_tamano[[tamano]]
    
    # Crear lista de gráficos para este tamaño
    plots <- lapply(seq_len(nrow(tipologias)), function(i) {
      tip <- tipologias$Tipologia[i]
      freq <- tipologias$Frecuencia[i]
      g <- examples[[tip]]
      
      # Configurar estilo del grafo
      V(g)$color <- "#6baed6"  # Azul claro para nodos
      V(g)$size <- 15
      V(g)$label.cex <- 0.7
      V(g)$frame.color <- "white"
      
      # Configurar aristas según tipo
      edge_types <- if("type" %in% edge_attr_names(g)) E(g)$type else rep("descent", ecount(g))
      E(g)$color <- edge_type_colors[edge_types]
      E(g)$width <- 1.5
      E(g)$arrow.size <- ifelse(edge_types == "marriage", 0.5, 0.6)
      E(g)$arrow.width <- ifelse(edge_types == "marriage", 0.5, 0.8)
      E(g)$curved <- 0  # Sin curvatura
      
      # Crear gráfico con título que muestra frecuencia
      plot_file <- tempfile(fileext = ".png")
      png(plot_file, width = 300, height = 300, bg = "white")
      par(mar = c(0.5, 0.5, 2, 0.5))
      plot(g, main = paste("Freq:", freq))
      dev.off()
      
      rasterGrob(png::readPNG(plot_file), interpolate = TRUE)
    })
    
    # Crear leyenda personalizada
    legend_grob <- if(length(edge_type_colors) > 0) {
      legend_items <- lapply(names(edge_type_colors), function(type) {
        color_box <- rectGrob(width = unit(0.4, "cm"), height = unit(0.4, "cm"),
                              gp = gpar(fill = edge_type_colors[type], col = "black"))
        label <- textGrob(
          label = paste0(type, ifelse(type == "marriage", " (bidireccional)", " (unidireccional)")),
          x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 9))
        arrangeGrob(color_box, label, ncol = 2, widths = c(0.5, 2))
      })
      
      arrangeGrob(
        grobs = legend_items,
        ncol = 1,
        top = textGrob("Tipos de Relaciones", gp = gpar(fontface = "bold", fontsize = 11))
      )
    } else {
      NULL
    }
    
    # Organizar en grilla
    ncol <- ceiling(sqrt(nrow(tipologias)))
    grid_plot <- arrangeGrob(
      grobs = plots,
      ncol = ncol,
      top = textGrob(
        paste("Tamaño de hogar:", tamano, "nodos |", 
              "Tipologías únicas:", nrow(tipologias)),
        gp = gpar(fontface = "bold", fontsize = 12))
    )
    
    # Combinar con leyenda
    final_plot <- if (!is.null(legend_grob)) {
      arrangeGrob(grid_plot, legend_grob, nrow = 2, heights = c(4, 0.5))
    } else {
      grid_plot
    }
    
    # Guardar en PDF
    ggsave(
      filename = file.path(img_dir, paste0("Resumen_Tamano_", tamano, ".pdf")),
      plot = final_plot,
      width = 8.5, 
      height = 11,
      device = "pdf"
    )
  }
  
  message("Gráficos consolidados generados en: ", img_dir)
}

# Ejecutar la función con tus datos
generate_consolidated_graphs(tipologia_df, examples, "Resumen_Tamanos")

################################################################################
## Paso 6: Gráfico consolidado por frecuencia
################################################################################
plot_tipologia_pdf <- function(g, nombre, freq) {
  # Preparación de atributos visuales
  V(g)$color <- "#6baed6"
  V(g)$size <- 25
  V(g)$label.cex <- 1
  V(g)$frame.color <- "white"
  V(g)$label.color <- "black"
  
  edge_types <- if ("type" %in% edge_attr_names(g)) E(g)$type else rep("descent", ecount(g))
  edge_colors <- ifelse(edge_types == "marriage", "#4daf4a", "#ff7f00")
  E(g)$color <- edge_colors
  E(g)$arrow.size <- ifelse(edge_types == "marriage", 0.5, 0.4)
  E(g)$arrow.width <- ifelse(edge_types == "marriage", 0.5, 0.8)
  E(g)$width <- 1
  E(g)$curved <- 0
  
  # Título para cada subgráfico
  plot(g, main = paste(nombre, "-", freq, "casos"), vertex.label.family = "sans", edge.label.family = "sans")
}

# Número de gráficas por página
plots_per_page <- 20
total_plots <- length(muestra_tipologias$Tipologia)
pages <- ceiling(total_plots / plots_per_page)

# Abrir dispositivo PDF
pdf("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Tipologias_por_frecuencia.pdf", width = 11, height = 8.5)  # Formato horizontal A4

for (i in seq_len(pages)) {
  from <- (i - 1) * plots_per_page + 1
  to <- min(i * plots_per_page, total_plots)
  
  par(mfrow = c(5, 4), mar = c(1, 1, 3, 1))  # 5 filas x 4 columnas
  
  for (j in from:to) {
    tip <- muestra_tipologias$Tipologia[j]
    freq <- muestra_tipologias$Frecuencia[muestra_tipologias$Tipologia == tip]
    plot_tipologia_pdf(examples[[tip]], tip, freq)
  }
}

dev.off()