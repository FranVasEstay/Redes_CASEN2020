library(igraph)
library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(parallel)
library(Cairo)
library(png)

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


## Identificación de estructuras únicas
identify_unique_structures <- function(networks) {
  # Agrupar por tamaño de red
  networks_by_size <- split(networks, sapply(networks, vcount))
  
  lapply(networks_by_size, function(nets) {
    unique_graphs <- list()
    unique_fingerprints <- character()
    fingerprint_map <- data.frame(
      fingerprints = character(),
      household_id = character(),
      stringsAsFactors = FALSE
    )
    
    for (g in nets) {
      # Crear fingerprint robusto al orden
      deg_hist <- sort(table(degree(g)), decreasing = TRUE)
      mult_hist <- sort(table(count_multiple(g)), decreasing = TRUE)
      fingerprint <- paste(
        paste(names(deg_hist), deg_hist, sep = ":", collapse = ","),
        paste(names(mult_hist), mult_hist, sep = ":", collapse = ","),
        sep = "|"
      )
      
      is_duplicate <- FALSE
      
      if (fingerprint %in% unique_fingerprints) {
        # Comparar isomorfismo estructural con los ya guardados
        for (existing_g in unique_graphs) {
          if (graph.isomorphic(g, existing_g)) {
            is_duplicate <- TRUE
            break
          }
        }
      }
      
      if (!is_duplicate) {
        unique_graphs[[length(unique_graphs) + 1]] <- g
        unique_fingerprints <- c(unique_fingerprints, fingerprint)
      }
      
      # Agregar al mapa de fingerprints
      fingerprint_map <- rbind(
        fingerprint_map,
        data.frame(
          fingerprints = fingerprint,
          household_id = vertex_attr(g, "household_id")[1],
          stringsAsFactors = FALSE
        )
      )
    }
    
    freq_table <- as.data.frame(table(fingerprint_map$fingerprints))
    
    list(
      unique_graphs = unique_graphs,
      freq_table = freq_table,
      fingerprint_map = fingerprint_map
    )
  })
}

# Ejecutar análisis
size_results <- identify_unique_structures(all_networks)

# 5. Generación de reporte consolidado
generate_analysis_report <- function(results) {
  # Crear tabla de frecuencias extendida
  freq_table <- bind_rows(lapply(names(results), function(size) {
    basic <- results[[size]]$freq_table
    extended <- results[[size]]$fingerprint_map
    
    # Unir las tablas por fingerprint
    joined <- basic %>%
      left_join(extended, by = c("Var1" = "fingerprints")) %>%
      group_by(Var1) %>%
      summarise(
        Nodos = as.integer(size),
        Frecuencia = unique(Freq),
        #Households = paste(unique(household_id), collapse = "; "),
        .groups = "drop"
      ) %>%
      rename(fingerprints = Var1)
    
    joined %>% select(Nodos, fingerprints, Frecuencia, #Households#
    )  }))
  
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
cat("- Tabla resumen guardada como 'resumen_estructuras_con_ids.csv'\n")

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
      structure_id <- freq_table$Var1[i]
      
      # Nombre de archivo seguro
      safe_id <- substr(gsub("[^[:alnum:]]", "_", structure_id), 1, 50)
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

# Generar visualizaciones
visualize_unique_structures(size_results, output_dir)

# Mensaje final
message("\nAnálisis completado exitosamente!")
message("Resultados guardados en: ", normalizePath(output_dir))
message("- Tabla de frecuencias: resumen_estructuras.csv")
message("- Imágenes de redes: directorio Estructuras_Unicas/")
message("- Resumen gráfico: resumen_estructuras.pdf")

############ GENERAR UN SOLO PLOT POR TAMAÑO DE VIVIENDA #######################
library(grid)
library(gridExtra)
create_edge_color_legend <- function(graphs) {
  # Extraer todos los colores únicos y sus etiquetas si existen
  all_edges <- unlist(lapply(graphs, function(g) edge_attr(g, "color")), use.names = FALSE)
  all_types <- unlist(lapply(graphs, function(g) edge_attr(g, "type")), use.names = FALSE)
  
  if (is.null(all_edges)) return(NULL)
  
  df <- unique(data.frame(color = all_edges, label = all_types, stringsAsFactors = FALSE))
  df <- df[!is.na(df$color), ]
  
  if (nrow(df) == 0) return(NULL)
  
  # Crear una leyenda simple usando grobs
  legend_items <- lapply(seq_len(nrow(df)), function(i) {
    color_box <- rectGrob(width = unit(0.4, "cm"), height = unit(0.4, "cm"),
                          gp = gpar(fill = df$color[i], col = "black"))
    label <- textGrob(label = df$label[i], x = unit(0, "npc"), just = "left",
                      gp = gpar(fontsize = 9))
    arrangeGrob(color_box, label, ncol = 2, widths = c(0.5, 2))
  })
  
  legend_grob <- arrangeGrob(grobs = legend_items, ncol = 1,
                             top = textGrob("Leyenda de colores (relaciones)", gp = gpar(fontface = "bold")))
  return(legend_grob)
}
generate_consolidated_graphs <- function(size_results, output_dir) {
  img_dir <- file.path(output_dir, "Resumen_Tipologias")
  if (!dir.exists(img_dir)) dir.create(img_dir)
  
  for (size in names(size_results)) {
    unique_graphs <- size_results[[size]]$unique_graphs
    freq_table <- size_results[[size]]$freq_table
    
    plots <- lapply(seq_along(unique_graphs), function(i) {
      g <- unique_graphs[[i]]
      freq <- freq_table$Freq[i]
      structure_id <- freq_table$Var1[i]
      
      # Crear una imagen base
      plot_file <- tempfile(fileext = ".png")
      png(plot_file, width = 400, height = 400)
      par(mar = rep(0.5, 4))
      plot(g,
           vertex.size = 15,
           vertex.color = "lightblue",
           vertex.label.cex = 0.8,
           edge.width = 1.5,
           edge.arrow.size = 0.6,
           main = "")
      dev.off()
      
      # Leer la imagen como raster
      img <- png::readPNG(plot_file)
      rasterGrob(img, interpolate = TRUE)
    })
    
    labels <- lapply(seq_along(unique_graphs), function(i) {
      freq <- freq_table$Freq[i]
      structure_id <- freq_table$Var1[i]
      textGrob(
        label = paste0("Freq: ", freq),
        gp = gpar(fontsize = 10)
      )
    })
    
    # Combinar imagen y texto
    combined <- mapply(function(p, l) arrangeGrob(p, l, ncol = 1, heights = c(4, 1)),
                       plots, labels, SIMPLIFY = FALSE)
    
    # Organizar en grilla
    ncol <- ceiling(sqrt(length(combined)))
    grid_plot <- arrangeGrob(grobs = combined, ncol = ncol,
                             top = textGrob(paste("Tamaño de hogar:", size, "nodos"), gp = gpar(fontface = "bold")))
    # Generar leyenda de colores de flechas
    legend_grob <- create_edge_color_legend(unique_graphs)
    
    if (!is.null(legend_grob)) {
      # Agregar leyenda al final
      final_plot <- arrangeGrob(grid_plot, legend_grob, nrow = 2, heights = c(4, 1))
    } else {
      final_plot <- grid_plot
    }
    
    ggsave(
      filename = file.path(img_dir, paste0("Resumen_Tamano_", size, ".pdf")),
      plot = final_plot,
      width = 8.5, height = 11
    )
  }
  
  message("Gráficos consolidados generados en: ", img_dir)
}


generate_consolidated_graphs(size_results, output_dir)
