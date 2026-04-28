# LIBRERÍAS
library(dplyr)
library(ggplot2)
library(vcd)
library(purrr)
library(tidyr)
library(scales)

# CARGAR Y PREPARAR DATOS COMPLETO -------------------------------------------
load("Análisis de viviendas/Data/Data_con_tipología.RData")

clusters_df <- read.csv(
  "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_familiares.csv"
)

# Crear data_clusters con TODAS las variables necesarias
data_clusters <- data_con_tipologia %>%
  left_join(clusters_df %>% select(Tipologia, Cluster),
            by = c("tipologia" = "Tipologia")) %>%
  filter(!is.na(Cluster)) %>%
  mutate(
    Cluster = factor(Cluster),
    region = factor(region),
    sueldo_cat = factor(sueldo_cat, levels = paste0("Q", 1:5), ordered = TRUE),
    # CREAR LAS VARIABLES QUE FALTAN
    zona = case_when(
      region %in% c("Región de Arica y Parinacota", "Región de Tarapacá", 
                    "Región de Antofagasta") ~ "Norte",
      region %in% c("Región de Atacama", "Región de Coquimbo") ~ "Norte Chico",
      region %in% c("Región de Valparaíso", "Región Metropolitana de Santiago",
                    "Región del Libertador Gral. Bernardo O'Higgins", 
                    "Región del Maule", "Región de Ñuble") ~ "Centro",
      region %in% c("Región del Biobío", "Región de La Araucanía",
                    "Región de Los Ríos", "Región de Los Lagos") ~ "Sur",
      region %in% c("Región de Aysén del Gral. Carlos Ibáñez del Campo", 
                    "Región de Magallanes y de la Antártica Chilena") ~ "Austral",
      TRUE ~ "Otra"
    ),
    presencia_indigenas = ifelse(porc_ind > 0, "Sí", "No"),
    presencia_enfermos = ifelse(porc_enf > 0, "Sí", "No"),
    presencia_extranjeros = ifelse(porc_chi < 100, "Sí", "No")
  ) %>%
  # Asegurar que todas sean factores
  mutate(across(c(zona, presencia_indigenas, presencia_enfermos, presencia_extranjeros), 
                as.factor))

# VERIFICAR VARIABLES DISPONIBLES -------------------------------------------
cat("Variables disponibles en data_clusters:\n")
print(names(data_clusters))

# 1. ETIQUETAS DE CLUSTERS ----------------------------------------------------
cluster_labels <- c(
  "1" = "Cluster 1",
  "2" = "Cluster 2", 
  "3" = "Cluster 3",
  "4" = "Cluster 4",
  "5" = "Cluster 5"
)

# 2. FUNCIÓN CHI-CUADRADO MEJORADA -------------------------------------------
chi_test_clusters <- function(data, test_var) {
  
  # Verificar que la variable existe y tiene datos
  if(!test_var %in% names(data)) {
    stop(paste("Variable", test_var, "no encontrada"))
  }
  
  if(all(is.na(data[[test_var]]))) {
    stop(paste("Variable", test_var, "tiene solo NAs"))
  }
  
  # Filtrar NAs de la variable específica
  data_filtrada <- data %>% filter(!is.na(.data[[test_var]]))
  
  # Tabla de contingencia
  tbl <- table(data_filtrada$Cluster, data_filtrada[[test_var]])
  
  # Verificar que la tabla tiene dimensiones válidas
  if(any(dim(tbl) == 0)) {
    stop(paste("Tabla de contingencia vacía para", test_var))
  }
  
  # Test chi-cuadrado con manejo de errores
  chi_test <- tryCatch({
    chisq.test(tbl)
  }, error = function(e) {
    warning(paste("Error en chi-cuadrado para", test_var, ":", e$message))
    return(NULL)
  })
  
  if(is.null(chi_test)) return(NULL)
  
  # Residuales estandarizados
  std_res <- as.data.frame(chi_test$stdres)
  names(std_res) <- c("Cluster", "Categoria", "std_residual")
  
  # V de Cramer
  cramer_v <- sqrt(chi_test$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
  
  # Tabla detallada con porcentajes y conteos
  tbl_df <- as.data.frame(tbl) %>%
    rename(Cluster = Var1, Categoria = Var2, Conteo = Freq) %>%
    group_by(Cluster) %>%
    mutate(
      Porcentaje = Conteo / sum(Conteo) * 100
    ) %>%
    ungroup() %>%
    left_join(std_res, by = c("Cluster", "Categoria")) %>%
    mutate(
      Cluster = factor(Cluster, levels = names(cluster_labels), labels = cluster_labels)
    )
  
  return(list(
    test = chi_test,
    residuals = tbl_df,
    cramer_v = cramer_v,
    variable = test_var
  ))
}

# 3. VARIABLES CATEGÓRICAS A ANALIZAR -----------------------------------------
categorical_vars <- c(
  "region",
  "rural_cat",
  "sueldo_cat",
  "zona",
  "presencia_indigenas",
  "presencia_enfermos", 
  "presencia_extranjeros"
)

# Verificar qué variables existen y tienen datos
vars_validas <- categorical_vars[sapply(categorical_vars, function(var) {
  var %in% names(data_clusters) && !all(is.na(data_clusters[[var]]))
})]

cat("Variables válidas para análisis:", paste(vars_validas, collapse = ", "), "\n")

# 4. EJECUTAR CHI-CUADRADO CON MANEJO DE ERRORES -----------------------------
chi_results_clusters <- map(vars_validas, function(var) {
  cat("Analizando variable:", var, "\n")
  tryCatch({
    chi_test_clusters(data_clusters, var)
  }, error = function(e) {
    cat("✗ Error con", var, ":", e$message, "\n")
    return(NULL)
  })
})

# Eliminar resultados nulos
chi_results_clusters <- compact(chi_results_clusters)

# Verificar que tenemos resultados
if(length(chi_results_clusters) == 0) {
  stop("No se pudo calcular chi-cuadrado para ninguna variable")
}

# 5. RESUMEN DE RESULTADOS ----------------------------------------------------
chi_summary_clusters <- map_dfr(chi_results_clusters, function(result) {
  data.frame(
    Variable = result$variable,
    Chi2 = round(result$test$statistic, 1),
    df = result$test$parameter,
    p_value = result$test$p.value,
    Cramer_V = round(result$cramer_v, 3),
    stringsAsFactors = FALSE
  )
})

# Mostrar resumen
print("Resumen de pruebas Chi-cuadrado:")
print(chi_summary_clusters)

# 6. HEATMAP DE SIGNIFICANCIA ------------------------------------------------
heatmap_clusters <- ggplot(chi_summary_clusters, 
                           aes(x = Variable, y = "Clusters", fill = p_value < 0.05)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = paste0("χ²=", Chi2, 
                               "\np=", format.pval(p_value, digits = 2),
                               "\nV=", Cramer_V)),
            color = "black", size = 3, lineheight = 0.8) +
  scale_fill_manual(values = c("TRUE" = "#1f77b4", "FALSE" = "gray90"),
                    name = "Significativo\n(p < 0.05)") +
  labs(title = "Asociación entre Clusters y Variables Sociodemográficas",
       x = "Variable", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# Guardar heatmap
ggsave("Análisis de viviendas/Analisis/Resultados_finales/heatmap_clusters.png", 
       heatmap_clusters, width = 12, height = 6, bg = "white")

# MOSAIC PLOTS PARA CLUSTERS ------------------------------------------------
generar_mosaic_clusters <- function(variable_cat, nombre_legible) {
  library(vcd)
  
  tryCatch({
    formula_mosaic <- as.formula(paste("~ Cluster +", variable_cat))
    
    png(paste0("Análisis de viviendas/Analisis/Resultados_finales/mosaic_clusters_", 
               variable_cat, ".png"), 
        width = 1200, height = 800, bg = "white")
    
    # Configurar parámetros gráficos para fondo blanco
    par(bg = "white", mar = c(5, 4, 4, 12) + 0.1)  # Margen derecho más grande para leyenda
    
    vcd::mosaic(formula_mosaic,
                data = data_clusters,
                shade = TRUE,
                main = paste(nombre_legible, "vs Clusters"),
                labeling = labeling_border(
                  rot_labels = c(45, 0, 0, 0),
                  offset_varnames = c(0, 0, 0, 2)  # Espacio para etiquetas
                ),
                legend = TRUE,
                margins = c(5, 4, 4, 10))
    
    dev.off()
    
    cat("✓ Mosaic plot generado para clusters vs", nombre_legible, "\n")
    
  }, error = function(e) {
    dev.off()
    cat("✗ Error con clusters vs", nombre_legible, ":", e$message, "\n")
  })
}

# HEATMAP DE RESIDUALES PARA CLUSTERS ----------------------------------------
generar_heatmap_residuales_clusters <- function(variable_cat, nombre_legible) {
  
  # Obtener resultados chi-cuadrado para esta variable
  chi_result <- chi_results_clusters[[which(vars_validas == variable_cat)]]
  
  if(is.null(chi_result)) {
    cat("✗ No hay resultados chi-cuadrado para", variable_cat, "\n")
    return(NULL)
  }
  
  residuals_df <- chi_result$residuals
  
  p <- ggplot(residuals_df, 
              aes(x = Categoria, y = Cluster, fill = std_residual)) +
    geom_tile(color = "white", linewidth = 0.8) +
    geom_text(aes(label = round(std_residual, 2)), 
              color = "black", size = 3.5, fontface = "bold") +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue",
                         midpoint = 0, 
                         name = "Residual\nEstandarizado",
                         limits = c(-max(abs(residuals_df$std_residual)), 
                                    max(abs(residuals_df$std_residual)))) +
    labs(title = paste("Residuales Estandarizados:", nombre_legible, "vs Clusters"),
         subtitle = paste("χ² =", round(chi_result$test$statistic, 1),
                          "| p =", format.pval(chi_result$test$p.value, digits = 3),
                          "| V =", round(chi_result$cramer_v, 3)),
         x = nombre_legible, y = "Cluster",
         caption = "Nota: Residuales > |2| indican asociación significativa (p < 0.05)") +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = "white"),
      panel.background = element_rect(fill = "white", color = "white"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.subtitle = element_text(face = "italic"),
      plot.caption = element_text(face = "italic", size = 9),
      panel.grid = element_blank()
    )
  
  ggsave(paste0("Análisis de viviendas/Analisis/Resultados_finales/heatmap_residuals_clusters_", 
                variable_cat, ".png"), 
         p, width = 12, height = 8, dpi = 300, bg = "white")
  
  return(p)
}

# EJECUTAR PARA CLUSTERS ----------------------------------------------------
# Variables para análisis con clusters
cluster_variables <- list(
  list(var = "region", nombre = "Región"),
  list(var = "rural_cat", nombre = "Ruralidad"), 
  list(var = "sueldo_cat", nombre = "Nivel de Ingresos"),
  list(var = "zona", nombre = "Zona Geográfica"),
  list(var = "presencia_indigenas", nombre = "Presencia Indígena"),
  list(var = "presencia_enfermos", nombre = "Presencia de Enfermos"),
  list(var = "presencia_extranjeros", nombre = "Presencia de Extranjeros")
)

# Filtrar solo variables válidas que existen en data_clusters
cluster_variables_validas <- cluster_variables[
  sapply(cluster_variables, function(x) x$var %in% vars_validas)
]

cat("Generando mosaic plots para clusters vs:\n")
cat(paste(sapply(cluster_variables_validas, function(x) x$nombre), collapse = ", "), "\n")

# Generar mosaic plots para clusters
walk(cluster_variables_validas, ~ generar_mosaic_clusters(.x$var, .x$nombre))

# Generar heatmaps de residuales para clusters
heatmaps_clusters <- map(cluster_variables_validas, 
                         ~ generar_heatmap_residuales_clusters(.x$var, .x$nombre))

# Eliminar heatmaps nulos
heatmaps_clusters <- compact(heatmaps_clusters)

# Mostrar resultados
if(length(heatmaps_clusters) > 0) {
  cat("Mostrando heatmaps de residuales para clusters...\n")
  walk(heatmaps_clusters, print)
}

# DISTRIBUCIÓN DE CLUSTERS - GRÁFICO ADICIONAL ------------------------------
# Gráfico de distribución de clusters
distribucion_clusters <- data_clusters %>%
  distinct(household, .keep_all = TRUE) %>%
  count(Cluster) %>%
  mutate(
    Porcentaje = n / sum(n) * 100,
    Cluster_etiqueta = cluster_labels[as.character(Cluster)]
  )

p_distribucion <- ggplot(distribucion_clusters, 
                         aes(x = reorder(Cluster_etiqueta, -n), y = n, fill = Cluster_etiqueta)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución de Clusters en la Muestra",
       subtitle = paste("Total de hogares:", sum(distribucion_clusters$n)),
       x = "Cluster", y = "Número de Hogares") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white")
  )

ggsave("Análisis de viviendas/Analisis/Resultados_finales/distribucion_clusters.png", 
       p_distribucion, width = 10, height = 6, bg = "white")

print(p_distribucion)
