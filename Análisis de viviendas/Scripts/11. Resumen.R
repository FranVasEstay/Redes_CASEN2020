################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
####################### ANÁLISIS UNIFICADO CLUSTERS Y MACROGRUPOS ##############
################################################################################

# LIBRERÍAS
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(vcd)
library(ggmosaic)
library(FactoMineR)
library(factoextra)
library(sf)
library(chilemapas)
library(patchwork)
library(rstatix)
library(kableExtra)
library(ggrepel)

# ---------------------------
# 1. CONFIGURACIÓN INICIAL
# ---------------------------

# Crear directorios de resultados
dirs <- c(
  "Análisis de viviendas/Analisis/resultados_integrados",
  "Análisis de viviendas/Analisis/resultados_integrados/clusters",
  "Análisis de viviendas/Analisis/resultados_integrados/macrogrupos", 
  "Análisis de viviendas/Analisis/resultados_integrados/comparacion",
  "Análisis de viviendas/Analisis/resultados_integrados/mca",
  "Análisis de viviendas/Analisis/resultados_integrados/mapas"
)

for(dir in dirs) {
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# ---------------------------
# 2. CARGA Y PREPARACIÓN DE DATOS
# ---------------------------

# Cargar datos base
load("Análisis de viviendas/Data/Data_con_tipología.RData")
load("Análisis de viviendas/Data/data_macrotiplogias.RData")
load("Análisis de viviendas/Data/data_clusters.RData")

# Cargar clasificaciones
clusters_df <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_familiares.csv")
macrogrupos_df <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_macrogrupos.csv")

#Unir las clasificaciones
datos_integrados <- data_con_tipologia %>%
  # Unir clusters - clusters_df usa "Tipologia" con T mayúscula
  left_join(clusters_df %>% dplyr::select(Tipologia, Cluster), by = c("tipologia" = "Tipologia")) %>%
  # Unir macrogrupos - macrogrupos_df usa "tipologia" con t minúscula  
  left_join(macrogrupos_df %>% dplyr::select(tipologia, macrogrupo), by = "tipologia") %>%
  # Filtrar casos válidos
  filter(!is.na(Cluster) & !is.na(macrogrupo)) %>%
  # Convertir a factores
  mutate(
    Cluster = factor(Cluster),
    macrogrupo = factor(macrogrupo),
    tipologia = factor(tipologia)
  ) %>%
  # Crear variables de presencia migrante e indígena (MEJORADAS)
  mutate(
    # Presencia migrante: hogares con al menos un extranjero
    presencia_migrante = ifelse(porc_chi < 100, "Con migrantes", "Sin migrantes"),
    # Presencia indígena: hogares con al menos un indígena  
    presencia_indigena = ifelse(porc_ind > 0, "Con indígenas", "Sin indígenas"),
    # Intersección migrante-indígena
    diversidad_cultural = case_when(
      presencia_migrante == "Con migrantes" & presencia_indigena == "Con indígenas" ~ "Mixta",
      presencia_migrante == "Con migrantes" ~ "Migrante",
      presencia_indigena == "Con indígenas" ~ "Indígena", 
      TRUE ~ "Ninguna"
    ),
    # Crear zona geográfica
    zona = case_when(
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
    )
  ) %>%
  # Asegurar factores ordenados
  mutate(
    presencia_migrante = factor(presencia_migrante, levels = c("Sin migrantes", "Con migrantes")),
    presencia_indigena = factor(presencia_indigena, levels = c("Sin indígenas", "Con indígenas")),
    diversidad_cultural = factor(diversidad_cultural, 
                                 levels = c("Ninguna", "Indígena", "Migrante", "Mixta")),
    zona = factor(zona),
    rural_cat = factor(rural_cat),
    sueldo_cat = factor(sueldo_cat, levels = paste0("Q", 1:5), ordered = TRUE)
  )

# Verificar datos
cat("Datos integrados:", nrow(datos_integrados), "observaciones\n")
cat("Clusters:", paste(levels(datos_integrados$Cluster), collapse = ", "), "\n")
cat("Macrogrupos:", paste(levels(datos_integrados$macrogrupo), collapse = ", "), "\n")

# ---------------------------
# 3. ANÁLISIS COMPARATIVO: CLUSTERS vs MACROGRUPOS
# ---------------------------

## 3.1 Tabla de contingencia y análisis de concordancia
tabla_contingencia <- table(datos_integrados$Cluster, datos_integrados$macrogrupo)
prop_tabla <- prop.table(tabla_contingencia, margin = 1) * 100

# Guardar tabla
write.csv(
  as.data.frame.matrix(tabla_contingencia),
  "Análisis de viviendas/Analisis/resultados_integrados/comparacion/tabla_contingencia_clusters_macrogrupos.csv"
)

write.csv(
  as.data.frame.matrix(prop_tabla),
  "Análisis de viviendas/Analisis/resultados_integrados/comparacion/tabla_proporciones_clusters_macrogrupos.csv"
)

## 3.2 Visualización de la relación entre clasificaciones
p_comparacion <- ggplot(datos_integrados, aes(x = Cluster, fill = macrogrupo)) +
  geom_bar(position = "fill") +
  geom_text(stat = "count", aes(label = ..count..), 
            position = position_fill(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Distribución de Macrogrupos dentro de cada Cluster",
    subtitle = "Análisis de concordancia entre ambas clasificaciones",
    x = "Cluster", y = "Proporción", fill = "Macrogrupo"
  ) +
  theme_minimal()

ggsave("Análisis de viviendas/Analisis/resultados_integrados/comparacion/distribucion_cruzada.png", 
       p_comparacion, width = 12, height = 6)

## 3.3 Mosaic plot de la relación
png("Análisis de viviendas/Analisis/resultados_integrados/comparacion/mosaic_clusters_macrogrupos.png", 
    width = 1000, height = 800)
mosaic(~ Cluster + macrogrupo, data = datos_integrados,
       shade = TRUE, main = "Relación entre Clusters y Macrogrupos",
       labeling_args = list(rot_labels = c(45, 0, 0, 0)))
dev.off()

# ---------------------------
# 4. ANÁLISIS DE CLUSTERS 
# ---------------------------

## 4.1 Estadísticas descriptivas por cluster
cluster_stats <- datos_integrados %>%
  group_by(Cluster) %>%
  summarise(
    n_hogares = n_distinct(household),
    porc_total = round(n_hogares / n_distinct(datos_integrados$household) * 100, 1),
    # Variables sociodemográficas
    ingreso_mediano = median(sueldo, na.rm = TRUE),
    ingreso_promedio = mean(sueldo, na.rm = TRUE),
    edad_promedio = mean(edad.prom, na.rm = TRUE),
    porc_rural = mean(porc_rural, na.rm = TRUE),
    # Variables de diversidad cultural (NUEVAS)
    porc_migrantes = mean(presencia_migrante == "Con migrantes", na.rm = TRUE) * 100,
    porc_indigenas = mean(presencia_indigena == "Con indígenas", na.rm = TRUE) * 100,
    porc_mixta = mean(diversidad_cultural == "Mixta", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(n_hogares))

write.csv(cluster_stats, 
          "Análisis de viviendas/Analisis/resultados_integrados/clusters/estadisticas_clusters.csv", 
          row.names = FALSE)

## 4.2 Visualización de características por cluster

# Función para gráficos de composición por cluster
plot_composicion_cluster <- function(variable, titulo) {
  datos_integrados %>%
    ggplot(aes(x = Cluster, fill = .data[[variable]])) +
    geom_bar(position = "fill") +
    geom_text(stat = "count", aes(label = ..count..), 
              position = position_fill(vjust = 0.5), size = 2.5) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = titulo, y = "Proporción", fill = "") +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# Crear múltiples gráficos de composición
variables_composicion <- list(
  "presencia_migrante" = "Presencia de Migrantes por Cluster",
  "presencia_indigena" = "Presencia de Indígenas por Cluster", 
  "diversidad_cultural" = "Diversidad Cultural por Cluster",
  "rural_cat" = "Ruralidad por Cluster",
  "sueldo_cat" = "Nivel de Ingreso por Cluster",
  "zona" = "Zona Geográfica por Cluster"
)

# Generar y guardar todos los gráficos
for(i in seq_along(variables_composicion)) {
  p <- plot_composicion_cluster(names(variables_composicion)[i], variables_composicion[[i]])
  ggsave(paste0("Análisis de viviendas/Analisis/resultados_integrados/clusters/", 
                names(variables_composicion)[i], "_clusters.png"), 
         p, width = 10, height = 6)
}

## 4.3 Boxplot de ingresos por cluster
p_ingresos_clusters <- ggplot(datos_integrados, aes(x = Cluster, y = sueldo)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Distribución de Ingresos por Cluster",
    subtitle = "Comparación de niveles socioeconómicos entre clusters",
    x = "Cluster", y = "Ingreso del Hogar (CLP)"
  ) +
  theme_minimal()

ggsave("Análisis de viviendas/Analisis/resultados_integrados/clusters/boxplot_ingresos_clusters.png",
       p_ingresos_clusters, width = 10, height = 6)

# ---------------------------
# 5. ANÁLISIS DE MACROGRUPOS (MEJORADO)
# ---------------------------

## 5.1 Estadísticas descriptivas por macrogrupo
macrogrupo_stats <- datos_integrados %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n_distinct(household),
    porc_total = round(n_hogares / n_distinct(datos_integrados$household) * 100, 1),
    # Variables sociodemográficas
    ingreso_mediano = median(sueldo, na.rm = TRUE),
    ingreso_promedio = mean(sueldo, na.rm = TRUE),
    edad_promedio = mean(edad.prom, na.rm = TRUE),
    porc_rural = mean(porc_rural, na.rm = TRUE),
    # Variables de diversidad cultural (NUEVAS)
    porc_migrantes = mean(presencia_migrante == "Con migrantes", na.rm = TRUE) * 100,
    porc_indigenas = mean(presencia_indigena == "Con indígenas", na.rm = TRUE) * 100,
    porc_mixta = mean(diversidad_cultural == "Mixta", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(n_hogares))

write.csv(macrogrupo_stats, 
          "Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/estadisticas_macrogrupos.csv", 
          row.names = FALSE)

## 5.2 Visualización de características por macrogrupo

# Función para gráficos de composición por macrogrupo
plot_composicion_macrogrupo <- function(variable, titulo) {
  datos_integrados %>%
    ggplot(aes(x = macrogrupo, fill = .data[[variable]])) +
    geom_bar(position = "fill") +
    geom_text(stat = "count", aes(label = ..count..), 
              position = position_fill(vjust = 0.5), size = 2.5) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = titulo, y = "Proporción", fill = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom")
}

# Generar y guardar gráficos para macrogrupos
for(i in seq_along(variables_composicion)) {
  p <- plot_composicion_macrogrupo(names(variables_composicion)[i], variables_composicion[[i]])
  ggsave(paste0("Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/", 
                names(variables_composicion)[i], "_macrogrupos.png"), 
         p, width = 12, height = 6)
}

## 5.3 Boxplot de ingresos por macrogrupo (NUEVO)
p_ingresos_macrogrupos <- ggplot(datos_integrados, aes(x = macrogrupo, y = sueldo)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Distribución de Ingresos por Macrogrupo",
    subtitle = "Comparación de niveles socioeconómicos entre estructuras familiares",
    x = "Macrogrupo", y = "Ingreso del Hogar (CLP)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/boxplot_ingresos_macrogrupos.png",
       p_ingresos_macrogrupos, width = 12, height = 6)

# ---------------------------
# 6. ANÁLISIS DE DIVERSIDAD CULTURAL (NUEVO)
# ---------------------------

## 6.1 Composición cultural por tipo de clasificación
p_diversidad_clusters <- ggplot(datos_integrados, aes(x = Cluster, fill = diversidad_cultural)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Diversidad Cultural por Cluster",
    subtitle = "Composición étnica y migrante de los clusters",
    x = "Cluster", y = "Proporción", fill = "Diversidad Cultural"
  ) +
  theme_minimal()

p_diversidad_macrogrupos <- ggplot(datos_integrados, aes(x = macrogrupo, fill = diversidad_cultural)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Diversidad Cultural por Macrogrupo", 
    subtitle = "Composición étnica y migrante de las estructuras familiares",
    x = "Macrogrupo", y = "Proporción", fill = "Diversidad Cultural"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combinar gráficos
p_diversidad_combinado <- p_diversidad_clusters / p_diversidad_macrogrupos +
  plot_annotation(title = "Comparación de Diversidad Cultural entre Clasificaciones")

ggsave("Análisis de viviendas/Analisis/resultados_integrados/comparacion/diversidad_cultural_comparativa.png",
       p_diversidad_combinado, width = 14, height = 10)

## 6.2 Análisis de ingresos por diversidad cultural
p_ingresos_diversidad <- ggplot(datos_integrados, aes(x = diversidad_cultural, y = sueldo)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(
    title = "Distribución de Ingresos por Diversidad Cultural",
    subtitle = "Relación entre composición étnica/migrante y nivel socioeconómico",
    x = "Diversidad Cultural", y = "Ingreso del Hogar (CLP)"
  ) +
  theme_minimal()

ggsave("Análisis de viviendas/Analisis/resultados_integrados/comparacion/ingresos_diversidad_cultural.png",
       p_ingresos_diversidad, width = 10, height = 6)

# ---------------------------
# 7. ANÁLISIS MULTIVARIADO (MCA) - COMPLETO
# ---------------------------

## 7.1 Preparar datos para MCA
mca_data <- datos_integrados %>%
  select(
    macrogrupo,
    Cluster,
    zona,
    rural_cat,
    sueldo_cat,
    presencia_migrante,
    presencia_indigena,
    diversidad_cultural
  ) %>%
  mutate(across(everything(), as.factor))

## 7.2 Ejecutar MCA
mca_result <- MCA(mca_data, quali.sup = c(1, 2), ncp = 5, graph = FALSE)

## 7.3 Guardar TODOS los gráficos MCA

# Scree plot
p_scree <- fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Varianza explicada por cada dimensión (MCA)")
ggsave("Análisis de viviendas/Analisis/resultados_integrados/mca/MCA_screeplot.png", 
       p_scree, width = 10, height = 6)

# Variables activas
p_variables <- fviz_mca_var(mca_result, repel = TRUE, col.var = "contrib") +
  scale_color_gradient2(low = "white", high = "red", midpoint = 5) +
  ggtitle("Variables en el espacio MCA")
ggsave("Análisis de viviendas/Analisis/resultados_integrados/mca/MCA_variables.png", 
       p_variables, width = 12, height = 8)

# Individuos por macrogrupo
p_ind_macro <- fviz_mca_ind(mca_result, 
                            habillage = mca_data$macrogrupo,
                            addEllipses = TRUE,
                            ellipse.type = "confidence",
                            repel = TRUE) +
  ggtitle("Distribución de hogares por Macrogrupo en el espacio MCA")
ggsave("Análisis de viviendas/Analisis/resultados_integrados/mca/MCA_individuos_macrogrupo.png", 
       p_ind_macro, width = 12, height = 8)

# Individuos por cluster
p_ind_cluster <- fviz_mca_ind(mca_result,
                              habillage = mca_data$Cluster, 
                              addEllipses = TRUE,
                              ellipse.type = "confidence",
                              repel = TRUE) +
  ggtitle("Distribución de hogares por Cluster en el espacio MCA")
ggsave("Análisis de viviendas/Analisis/resultados_integrados/mca/MCA_individuos_cluster.png",
       p_ind_cluster, width = 12, height = 8)

# ---------------------------
# 8. ANÁLISIS ESTADÍSTICO COMPARATIVO
# ---------------------------

## 8.1 ANOVA para ingresos por cluster y macrogrupo
anova_cluster <- aov(sueldo ~ Cluster, data = datos_integrados)
anova_macrogrupo <- aov(sueldo ~ macrogrupo, data = datos_integrados)

# Resumen ANOVA
anova_summary <- data.frame(
  Clasificación = c("Cluster", "Macrogrupo"),
  F_value = c(summary(anova_cluster)[[1]]$`F value`[1], 
              summary(anova_macrogrupo)[[1]]$`F value`[1]),
  P_value = c(summary(anova_cluster)[[1]]$`Pr(>F)`[1],
              summary(anova_macrogrupo)[[1]]$`Pr(>F)`[1])
)

write.csv(anova_summary, 
          "Análisis de viviendas/Analisis/resultados_integrados/comparacion/anova_ingresos.csv",
          row.names = FALSE)

## 8.2 Pruebas Chi-cuadrado para diversidad cultural
chi_diversidad_cluster <- chisq.test(table(datos_integrados$Cluster, datos_integrados$diversidad_cultural))
chi_diversidad_macrogrupo <- chisq.test(table(datos_integrados$macrogrupo, datos_integrados$diversidad_cultural))

chi_summary <- data.frame(
  Clasificación = c("Cluster", "Macrogrupo"),
  Chi2 = c(chi_diversidad_cluster$statistic, chi_diversidad_macrogrupo$statistic),
  P_value = c(chi_diversidad_cluster$p.value, chi_diversidad_macrogrupo$p.value),
  Cramer_V = c(sqrt(chi_diversidad_cluster$statistic/(nrow(datos_integrados) * (min(dim(table(datos_integrados$Cluster, datos_integrados$diversidad_cultural))) - 1))),
               sqrt(chi_diversidad_macrogrupo$statistic/(nrow(datos_integrados) * (min(dim(table(datos_integrados$macrogrupo, datos_integrados$diversidad_cultural))) - 1))))
)

write.csv(chi_summary,
          "Análisis de viviendas/Analisis/resultados_integrados/comparacion/chi_diversidad_cultural.csv",
          row.names = FALSE)

# ---------------------------
# 10. CLUSTERING JERÁRQUICO DE MACROGRUPOS POR PERFIL SOCIODEMOGRÁFICO
# ---------------------------

## 10.1 Preparar datos para clustering de macrogrupos
macro_cluster_data <- datos_integrados %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n(),
    edad_promedio = mean(edad.prom, na.rm = TRUE),
    porc_indigena = mean(porc_ind, na.rm = TRUE),
    porc_rural = mean(porc_rural, na.rm = TRUE),
    ingreso_mediano = median(sueldo, na.rm = TRUE),
    porc_mujeres = 100 - mean(porc_hombre, na.rm = TRUE),
    # Añadir variables de diversidad cultural
    porc_migrantes = mean(presencia_migrante == "Con migrantes", na.rm = TRUE) * 100,
    porc_diversidad_mixta = mean(diversidad_cultural == "Mixta", na.rm = TRUE) * 100
  ) %>%
  column_to_rownames("macrogrupo")

# Mostrar datos preparados
print(macro_cluster_data)

## 10.2 Escalar datos y clustering jerárquico
macro_scaled <- scale(macro_cluster_data)
dist_macro <- dist(macro_scaled)
hc_macro <- hclust(dist_macro, method = "ward.D2")

# Dendrograma de macrogrupos
p_dendro_macro <- fviz_dend(hc_macro, k = 4, 
                            cex = 0.8, 
                            main = "Clustering Jerárquico de Macrogrupos por Perfil Sociodemográfico",
                            xlab = "Macrogrupos", ylab = "Distancia Euclidiana",
                            color_labels_by_k = TRUE)

print(p_dendro_macro)

# Guardar dendrograma
ggsave("Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/dendrograma_macrogrupos.png",
       p_dendro_macro, width = 12, height = 8, dpi = 300, bg = "white")

## 10.3 Análisis CLARA para macrogrupos
library(cluster)

# Determinar número óptimo de clusters con método del codo
wss <- sapply(1:5, function(k){clara(macro_scaled, k, samples = 50)$objective})
elbow_data <- data.frame(k = 1:5, wss = wss)

p_elbow <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Método del Codo para Número Óptimo de Clusters",
       x = "Número de Clusters (k)", y = "Suma de Cuadrados Within (WSS)") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:5)

ggsave("Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/elbow_clusters_macrogrupos.png",
       p_elbow, width = 8, height = 6)

# Aplicar CLARA con k=3 (basado en método del codo)
clara_result <- clara(macro_scaled, k = 3, samples = 50, pamLike = TRUE)

# Visualizar resultados CLARA
p_clara <- fviz_cluster(clara_result, data = macro_scaled,
                        palette = "jco", 
                        ggtheme = theme_minimal(),
                        main = "Agrupamiento de Macrogrupos con CLARA",
                        repel = TRUE)

print(p_clara)

ggsave("Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/clara_macrogrupos.png",
       p_clara, width = 10, height = 8, dpi = 300, bg = "white")

## 10.4 Interpretación de los clusters de macrogrupos
# Asignar clusters a los macrogrupos
macro_clusters <- data.frame(
  macrogrupo = rownames(macro_scaled),
  cluster_clara = clara_result$clustering,
  cluster_hierarchico = cutree(hc_macro, k = 3)
)

# Unir con datos originales para interpretación
macro_cluster_interpretation <- macro_cluster_data %>%
  rownames_to_column("macrogrupo") %>%
  left_join(macro_clusters, by = "macrogrupo") %>%
  mutate(
    perfil_cluster = case_when(
      cluster_clara == 1 ~ "Perfil Tradicional Rural",
      cluster_clara == 2 ~ "Perfil Urbano Moderno", 
      cluster_clara == 3 ~ "Perfil Mixto Intermedio"
    )
  )

write.csv(macro_cluster_interpretation,
          "Análisis de viviendas/Analisis/resultados_integrados/macrogrupos/interpretacion_clusters_macrogrupos.csv",
          row.names = FALSE)

# ---------------------------
# 11. COMPARACIÓN INTEGRADA: CLUSTERS vs MACROGRUPOS vs CLUSTERS-DE-MACROGRUPOS
# ---------------------------

## 11.1 Análisis de concordancia triple
comparacion_triple <- datos_integrados %>%
  left_join(macro_cluster_interpretation %>% select(macrogrupo, perfil_cluster), by = "macrogrupo") %>%
  group_by(Cluster, macrogrupo, perfil_cluster) %>%
  summarise(
    n_hogares = n_distinct(household),
    ingreso_promedio = mean(sueldo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Cluster, desc(n_hogares))

write.csv(comparacion_triple,
          "Análisis de viviendas/Analisis/resultados_integrados/comparacion/analisis_triple_clasificaciones.csv",
          row.names = FALSE)

## 11.2 Visualización de la relación triple
p_triple <- ggplot(comparacion_triple, aes(x = Cluster, y = macrogrupo, size = n_hogares, color = perfil_cluster)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 15), name = "Número de Hogares") +
  scale_color_brewer(palette = "Set1", name = "Perfil Cluster") +
  labs(
    title = "Relación Triple: Clusters vs Macrogrupos vs Perfiles de Macrogrupos",
    subtitle = "Tamaño del punto = número de hogares, Color = perfil del cluster de macrogrupos",
    x = "Cluster Sociodemográfico", y = "Macrogrupo Estructural"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("Análisis de viviendas/Analisis/resultados_integrados/comparacion/relacion_triple_clasificaciones.png",
       p_triple, width = 14, height = 10, dpi = 300)


# ---------------------------
# 12. GUARDAR DATOS FINALES
# ---------------------------
save(datos_integrados, cluster_stats, macrogrupo_stats, resumen_comparativo, fortalezas,
     file = "Análisis de viviendas/Data/datos_analisis_final.RData")

