################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
########################### ANÁLISIS DE CLUSTERS ###############################
################################################################################

# LIBRERÍAS
library(dplyr)
library(ggplot2)

# CARGAR Y PREPARAR DATOS
# Cargar dataset con tipología
load("Análisis de viviendas/Data/Data_con_tipología.RData")

# Cargar archivo con clusters por tipología
clusters_df <- read.csv(
  "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_familiares.csv"
)

# Unir clusters con los datos originales
data_clusters <- data_con_tipologia %>%
  left_join(clusters_df %>% select(Tipologia, Cluster),
            by = c("tipologia" = "Tipologia")) %>%
  filter(!is.na(Cluster)) %>%
  mutate(
    Cluster = factor(Cluster),
    region = factor(region),
    sueldo_cat = factor(sueldo_cat, levels = paste0("Q", 1:5), ordered = TRUE)
  )

# Clasificar zonas geográficas
data_clusters <- data_clusters %>%
  mutate(zona = case_when(
    region %in% c("Región de Arica y Parinacota", "Región de Tarapacá", 
                  "Región de Antofagasta", "Región de Atacama", 
                  "Región de Coquimbo") ~ "Norte",
    region %in% c("Región de Valparaíso", "Región Metropolitana de Santiago",
                  "Región del Libertador Gral. Bernardo O'Higgins", 
                  "Región del Maule", "Región de Ñuble") ~ "Centro",
    region %in% c("Región del Biobío", "Región de La Araucanía", 
                  "Región de Los Ríos", "Región de Los Lagos",
                  "Región de Aysén del Gral. Carlos Ibáñez del Campo", 
                  "Región de Magallanes y de la Antártica Chilena") ~ "Sur",
    TRUE ~ NA_character_
  )) %>%
  mutate(zona = factor(zona))
data_clusters <- data_clusters %>%
  mutate(
    presencia_indigenas = ifelse(porc_ind > 0, "Sí", "No"),
    presencia_enfermos = ifelse(porc_enf > 0, "Sí", "No"),
    presencia_extranjeros = ifelse(porc_chi < 100, "Sí", "No")
  ) %>%
  mutate(
    presencia_indigenas = factor(presencia_indigenas, levels = c("No", "Sí")),
    presencia_enfermos = factor(presencia_enfermos, levels = c("No", "Sí")),
    presencia_extranjeros = factor(presencia_extranjeros, levels = c("No", "Sí"))
  )
# Guardar dataset procesado
save(data_clusters, file = "Análisis de viviendas/Data/data_clusters.RData")

# ANÁLISIS DESCRIPTIVO
analyze_clusters <- function(data, suffix = "") {
  # 1 Tabla de frecuencias de clusters
  cluster_freq <- data %>%
    distinct(household, .keep_all = TRUE) %>%
    count(Cluster) %>%
    mutate(percent = n / sum(n) * 100) %>%
    arrange(desc(n))
  
  # 2 Estadísticas demográficas por cluster
  cluster_demographics <- data %>%
    group_by(Cluster) %>%
    summarise(
      avg_age = mean(edad.prom, na.rm = TRUE),
      prop_female = 100 - mean(porc_hombre, na.rm = TRUE),
      avg_income = median(sueldo, na.rm = TRUE),
      prop_indigenous = mean(porc_ind, na.rm = TRUE),
      prop_employed = mean(porc_empleo, na.rm = TRUE),
      prop_chilean = mean(porc_chi, na.rm = TRUE),
      avg_hh_size = n() / n_distinct(household),
      .groups = "drop"
    )
  
  # 3. Función auxiliar para gráficos con porcentajes sobre barras
  plot_with_percent <- function(df, x_var, fill_var, title, fill_palette="Set2") {
    ggplot(df, aes_string(x = x_var, fill = fill_var)) +
      geom_bar(position = "fill") +
      geom_text(stat = "count",
                aes(label = paste0(round((..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)] * 100, 1), "%")),
                position = position_fill(vjust = 0.5),
                color = "black",
                size = 3) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = fill_palette) +
      labs(title = title, x = "Cluster", y = "Porcentaje de hogares") +
      theme_minimal()
  }

  # 3. Gráficos
  ## Distribución de clusters
  p1 <- ggplot(cluster_freq, aes(x = reorder(Cluster, -n), y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(round(percent, 1), "%")),
              vjust = -0.5, size = 3) +
    labs(title = paste("Distribución de Clusters", suffix),
         x = "Cluster", y = "Número de hogares") +
    theme_minimal()
  
  ## Ingresos
  p2 <- plot_with_percent(data, "Cluster", "sueldo_cat",
                          paste("Distribución de ingresos por Cluster", suffix))
  
  ## Presencia de indígenas
  p3 <- plot_with_percent(data, "Cluster", "presencia_indigenas",
                          "Presencia de personas indígenas por cluster")
  
  ## Presencia de enfermos
  p4 <- plot_with_percent(data, "Cluster", "presencia_enfermos",
                          "Presencia de personas con enfermedad por cluster")
  
  ## Presencia de extranjeros
  p5 <- plot_with_percent(data, "Cluster", "presencia_extranjeros",
                          "Presencia de extranjeros por cluster")
  
  ## Ruralidad
  p6 <- plot_with_percent(data, "Cluster", "rural_cat",
                          "Distribución de ruralidad por cluster", fill_palette="Pastel1")
  
  ## Zona geográfica
  p7 <- plot_with_percent(data, "Cluster", "zona",
                          "Distribución de zonas geográficas por cluster", fill_palette="Pastel2")
  
  # 4. Guardar gráficos
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/distribucion_clusters.png", p1, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/ingresos_clusters.png", p2, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/presencia_indigenas.png", p3, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/presencia_enfermos.png", p4, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/presencia_extranjeros.png", p5, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/ruralidad_clusters.png", p6, width = 8, height = 5)
  ggsave("Análisis de viviendas/Analisis/resultados_cluster/zonas_clusters.png", p7, width = 8, height = 5)
  
  return(list(
    frequency = cluster_freq,
    demographics = cluster_demographics,
    plots = list(
      distribution = p1,
      income = p2,
      indigenous = p3,
      health = p4,
      foreigner = p5,
      rural = p6,
      geo = p7
    )
  ))
}

# Ejecutar análisis
results_clusters <- analyze_clusters(data_clusters, "(Clusters)")

# Ver tabla de frecuencias
results_clusters$frequency

# Ver estadísticas demográficas
results_clusters$demographics

# Ver gráficos
results_clusters$plots$distribution
results_clusters$plots$income
results_clusters$plots$indigenous
results_clusters$plots$health
results_clusters$plots$foreigner
results_clusters$plots$rural
results_clusters$plots$geo
