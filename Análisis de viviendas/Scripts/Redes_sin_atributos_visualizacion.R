library(igraph)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(parallel)
library(Cairo)

################################################################################
#######################Buscar estructuras de las redes##########################
################################################################################
## Cargar de datos optimizada
net_files <- c(
  "Análisis de viviendas/Redes/dependency_igraph_no_attrs.RData",
  "Análisis de viviendas/Redes/descent_igrpah_no_attrs.RData",
  "Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData",
  "Análisis de viviendas/Redes/marriage_igraph_no_attrs.RData"
)

lapply(net_files, load, .GlobalEnv)

## Filtrar y procesar redes con más de un nodo
process_large_network_list <- function(raw_list, net_name, min_nodes = 2) {
  # Paso 1: Filtrar
  filtered <- lapply(raw_list, function(x) {
    if (inherits(x$kinship_net, "igraph") && vcount(x$kinship_net) >= min_nodes) {
      return(x)
    }
    return(NULL)
  })
  filtered <- Filter(Negate(is.null), filtered)
  
  # Paso 2: Procesar
  processed <- lapply(filtered, function(x) {
    g <- x$kinship_net
    edge_attr(g, "network_type") <- net_name
    vertex_attr(g, "household_id") <- x$household_i
    g
  })
  
  message(sprintf(
    "Procesamiento completado: %d redes válidas de %d originales",
    length(processed), length(raw_list)
  ))
  
  return(processed)
}

all_networks <- process_large_network_list(
  kinship_igraph_no_attrs, 
  net_name = "kinship",
  min_nodes = 2
)

## Función mejorada para identificación de estructuras únicas
identify_unique_structures <- function(networks) {
  # Agrupar por tamaño de red
  networks_by_size <- split(networks, sapply(networks, vcount))
  
  lapply(networks_by_size, function(nets) {
    # Generar identificadores únicos basados en propiedades estructurales
    fingerprints <- sapply(nets, function(g) {
      deg <- sort(degree(g))
      edge_counts <- sort(count_multiple(g))
      paste(c(deg, edge_counts), collapse = "-")
    })
    
    # Encontrar estructuras únicas
    unique_ids <- unique(fingerprints)
    freq_table <- as.data.frame(table(fingerprints))
    
    list(
      unique_graphs = nets[match(unique_ids, fingerprints)],
      freq_table = freq_table
    )
  })
}

# Ejecutar análisis
size_results <- identify_unique_structures(all_networks)

# 5. Generación de reporte consolidado
generate_analysis_report <- function(results) {
  # Crear tabla de frecuencias
  freq_table <- bind_rows(lapply(names(results), function(size) {
    data.frame(
      Nodos = as.integer(size),
      Estructura = results[[size]]$freq_table$fingerprints,
      Frecuencia = results[[size]]$freq_table$Freq,
      stringsAsFactors = FALSE
    )
  }))
  
  # Guardar tabla
  write.csv(freq_table, "Análisis de viviendas/Analisis/resumen_estructuras.csv", row.names = FALSE)
  return(freq_table)
}

# Generar reporte final
final_report <- generate_analysis_report(size_results)

# Resultados clave
cat("\nResumen del análisis:\n")
cat("----------------------\n")
cat("- Total de redes analizadas:", length(all_networks), "\n")
cat("- Tamaños de hogar encontrados:", paste(names(size_results), collapse = ", "), "\n")
cat("- Estructuras únicas identificadas:", sum(sapply(size_results, function(x) nrow(x$freq_table))), "\n")
cat("- Archivos generados en directorio 'Analisis/'\n")
cat("- Tabla resumen guardada como 'resumen_estructuras.csv'\n")

########################### PLOTEAR ############################################
# Directorio de salida
output_dir <- "Análisis de viviendas/Analisis"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
## 2. Función para visualizar estructuras únicas ----
visualize_unique_structures <- function(size_results, output_dir) {
  # Crear subdirectorio para imágenes
  img_dir <- file.path(output_dir, "Estructuras_Unicas")
  if (!dir.exists(img_dir)) dir.create(img_dir)
  
  # Configuración visual
  plot_style <- list(
    vertex.size = 15,
    vertex.color = "lightblue",
    vertex.label.cex = 0.8,
    edge.width = 1.5,
    edge.arrow.size = 0.6,
    margin = c(0.5, 0.5, 0.5, 0.5)
  )
  
  # Procesar cada tamaño de hogar
  for (size in names(size_results)) {
    size_dir <- file.path(img_dir, paste0("Size_", size))
    if (!dir.exists(size_dir)) dir.create(size_dir)
    
    unique_graphs <- size_results[[size]]$unique_graphs
    freq_table <- size_results[[size]]$freq_table
    
    # Procesar cada estructura única
    for (i in seq_along(unique_graphs)) {
      g <- unique_graphs[[i]]
      freq <- freq_table$Freq[i]
      structure_id <- freq_table$fingerprints[i]
      
      # Nombre de archivo seguro
      safe_id <- gsub("[^[:alnum:]]", "_", structure_id)
      file_name <- file.path(size_dir, 
                             paste0("Estructura_", size, "_", safe_id, ".pdf"))
      
      # Generar imagen
      pdf(file_name, width = 8, height = 7)
      
      # Diseño de dos partes
      layout_matrix <- matrix(c(1, 2), nrow = 2)
      layout(layout_matrix, heights = c(3, 1))
      
      # Parte superior: Red
      par(mar = c(1, 1, 2, 1))
      plot(g,
           main = paste("Tamaño:", size, "nodos - Estructura", i),
           vertex.size = plot_style$vertex.size,
           vertex.color = plot_style$vertex.color,
           vertex.label.cex = plot_style$vertex.label.cex,
           edge.width = plot_style$edge.width,
           edge.arrow.size = plot_style$edge.arrow.size)
      
      # Parte inferior: Información
      par(mar = c(1, 1, 1, 1))
      plot.new()
      text(0.5, 0.7, paste("Configuración:", structure_id), cex = 1.2, font = 2)
      text(0.5, 0.4, paste("Frecuencia:", freq, "hogares"), cex = 1.1)
      text(0.5, 0.1, paste("Tipo:", unique(edge_attr(g, "network_type"))[1]), cex = 1.0)
      
      dev.off()
    }
    
    message("Tamaño ", size, ": ", length(unique_graphs), 
            " estructuras únicas guardadas")
  }
}

## 3. Función para resumen gráfico ----
create_summary_plot <- function(final_report, output_dir) {
  # Preparar datos
  plot_data <- final_report %>%
    group_by(Nodos) %>%
    mutate(Percent = Frecuencia / sum(Frecuencia) * 100) %>%
    ungroup()
  
  # Gráfico de resumen
  p <- ggplot(plot_data, aes(x = Nodos, y = Percent, fill = Estructura)) +
    geom_bar(stat = "identity") +
    labs(title = "Distribución de Estructuras Familiares por Tamaño de Hogar",
         x = "Número de miembros en el hogar",
         y = "Porcentaje de ocurrencia",
         fill = "Patrón de conexiones") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 7))
  
  # Guardar gráfico
  ggsave(file.path(output_dir, "resumen_estructuras.pdf"), 
         plot = p, width = 12, height = 8)
}
# Generar visualizaciones
visualize_unique_structures(size_results, output_dir)

# Crear resumen gráfico
create_summary_plot(final_report, output_dir)

# Mensaje final
message("\nAnálisis completado exitosamente!")
message("Resultados guardados en: ", normalizePath(output_dir))
message("- Tabla de frecuencias: resumen_estructuras.csv")
message("- Imágenes de redes: directorio Estructuras_Unicas/")
message("- Resumen gráfico: resumen_estructuras.pdf")