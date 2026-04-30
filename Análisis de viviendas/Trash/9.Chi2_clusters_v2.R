
#LIBRERÍA
library(dplyr)
library(ggplot2)
library(vcd)
library(purrr)
library(tidyr)
library(scales)

cluster_labels <- c(
  "1" = "Cluster 1",
  "2" = "Cluster 2",
  "3" = "Cluster 3",
  "4" = "Cluster 4",
  "5" = "Cluster 5"
)

# 1. Función de chi-cuadrado para clusters
chi_test_clusters <- function(data, test_var) {
  # Tabla de contingencia
  tbl <- table(data$Cluster, data[[test_var]])
  
  # Test chi-cuadrado
  chi_test <- chisq.test(tbl)
  
  # Residuales estandarizados
  std_res <- as.data.frame(chi_test$stdres)
  names(std_res) <- c("Cluster", test_var, "std_residual")
  
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
    left_join(std_res, by = c("Cluster", setNames(test_var, test_var))) %>%
    mutate(
      Cluster = factor(Cluster, levels = names(cluster_labels), labels = cluster_labels)
    )
  
  return(list(
    test = chi_test,
    residuals = tbl_df,
    cramer_v = cramer_v
  ))
}


# ------------------------
# 2. Variables categóricas a analizar
categorical_vars <- c(
  "rural_cat",
  "sueldo_cat",
  "zona",
  "presencia_indigenas",
  "presencia_enfermos",
  "presencia_extranjeros"
)

# ------------------------
# 3. Ejecutar chi-cuadrado para todas las variables
chi_test_clusters <- function(data, test_var) {
  # Tabla de contingencia
  tbl <- table(data$Cluster, data[[test_var]])
  
  # Test chi-cuadrado
  chi_test <- chisq.test(tbl)
  
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
    cramer_v = cramer_v
  ))
}

chi_results_clusters <- map(categorical_vars, ~chi_test_clusters(data_clusters, .x))
names(chi_results_clusters) <- categorical_vars

# 4. Guardar tabla detallada por variable, cluster y categoría
chi_detailed <- bind_rows(
  map2(chi_results_clusters, names(chi_results_clusters), function(x, var) {
    x$residuals %>%
      mutate(Variable = var)
  })
)

write.csv(
  chi_detailed,
  "Análisis de viviendas/Analisis/resultados_cluster/chi_detailed_clusters.csv",
  row.names = FALSE
)

# ------------------------
# 5. Resumen global de chi², p-value y Cramer’s V
chi_summary_clusters <- data.frame(
  Variable = categorical_vars,
  Chi2 = map_dbl(chi_results_clusters, ~.x$test$statistic),
  p_value = map_dbl(chi_results_clusters, ~.x$test$p.value),
  Cramer_V = map_dbl(chi_results_clusters, ~.x$cramer_v)
)

write.csv(
  chi_summary_clusters,
  "Análisis de viviendas/Analisis/resultados_cluster/chi_summary_clusters.csv",
  row.names = FALSE
)

# ------------------------
# 6. Mosaic plots con porcentajes en las celdas
generar_mosaic_cluster <- function(variable_cat) {
  datos_filtrados <- data_clusters %>%
    filter(!is.na(.data[[variable_cat]])) %>%
    mutate(Cluster = factor(Cluster, levels = names(cluster_labels), labels = cluster_labels))
  
  # Crear tabla para anotar porcentajes
  tbl_df <- datos_filtrados %>%
    count(Cluster, .data[[variable_cat]]) %>%
    group_by(Cluster) %>%
    mutate(Porcentaje = n / sum(n)) %>%
    ungroup()
  
  # Nombre del archivo
  nombre_archivo <- paste0("Análisis de viviendas/Analisis/resultados_cluster/",
                           variable_cat, "_vs_Cluster.png")
  
  # Guardar gráfico
  png(nombre_archivo, width = 1200, height = 900)
  
  # Mosaic con sombreado de residuos y títulos personalizados
  formula_mosaic <- as.formula(paste("~", variable_cat, "+ Cluster"))
  mosaic(formula_mosaic,
         data = datos_filtrados,
         shade = TRUE,
         main = paste("Mosaic plot:", variable_cat, "vs Clusters"),
         labeling = labeling_values(
           gp_labels = gpar(fontsize = 10),
           gp_varnames = gpar(fontsize = 12),
           rot_labels = c(0, 0, 0, 0)
         ))
  dev.off()
}

walk(categorical_vars, generar_mosaic_cluster)

# ------------------------
# 7. Heatmap de significancia
ggplot(chi_summary_clusters, aes(x = Variable, y = "Cluster", fill = p_value < 0.05)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = paste0("χ²=", round(Chi2, 2),
                               "\np=", round(p_value, 4),
                               "\nV=", round(Cramer_V, 3))),
            color = "black", size = 3.5, lineheight = 0.8) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray90"),
                    name = "Significativo\n(p < 0.05)") +
  labs(title = "Significancia estadística (Chi²) - Clusters vs Variables",
       x = "Variable", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("Análisis de viviendas/Analisis/resultados_cluster/heatmap_significancia.png",
       width = 10, height = 6)

# Rojo: la combinación aparece menos de lo esperado.
# Azul: la combinación aparece más de lo esperado.
# Blanco: la frecuencia observada está cerca de la esperada.
