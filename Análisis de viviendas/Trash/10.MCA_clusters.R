
# LIBRERÍAS
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(hexbin)
library(patchwork) 

# CARGAR Y PREPARAR DATOS
load("Análisis de viviendas/Data/data_macrotiplogias.RData")  # carga data_macro
# Asegurarse de que las variables existan
categorical_vars <- c("rural_cat", "sueldo_cat", "zona", 
                      "presencia_indigenas", "presencia_enfermos", "presencia_extranjeros")
suplementary_var <- "Cluster"

mca_data <- data_clusters %>%
  select(all_of(c(categorical_vars, suplementary_var))) %>%
  mutate(across(everything(), as.factor))

# MCA
# clusters como variable suplementaria (no influyen en dimensiones)
mca_result <- MCA(mca_data, quali.sup = ncol(mca_data), ncp = 5, graph = FALSE)

# Eigenvalores
eig_val <- get_eigenvalue(mca_result)
print(eig_val)
fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0, 60)) +
  ggtitle("Varianza explicada por cada dimensión (MCA)")

# coordenadas de individuos
coords <- as.data.frame(mca_result$ind$coord)
colnames(coords) <- gsub(" ", "_", colnames(coords))
coords$Cluster <- mca_data[[suplementary_var]]

# coordenadas de categorías
cat_coords <- as.data.frame(mca_result$var$coord)
cat_coords$variable <- rownames(cat_coords)
cat_coords$variable_group <- sapply(strsplit(cat_coords$variable, split = "="), `[`, 1)

# directorio
dir.create("Análisis de viviendas/Analisis/MCA_plots", recursive = TRUE, showWarnings = FALSE)

# gráfico básico
p_vars <- fviz_mca_var(mca_result, repel = TRUE, ggtheme = theme_minimal(),
                       title = "Categorías de variables (MCA)")
ggsave("Análisis de viviendas/Analisis/MCA_plots/MCA_variables.png", p_vars, width = 10, height = 8)

# gráfico de individuos (viviendas) por cluster
p_ind <- fviz_mca_ind(mca_result,
                      habillage = suplementary_var,
                      addEllipses = TRUE,
                      ellipse.type = "confidence",
                      geom = "point",
                      ggtheme = theme_minimal(),
                      title = "Distribución de hogares por Cluster (MCA)")
ggsave("Análisis de viviendas/Analisis/MCA_plots/MCA_individuos_clusters.png", p_ind, width = 10, height = 8)

# gráficos para variables categóricas
# Función para graficar una variable categórica
plot_single_var <- function(coords_df, var) {
  centroides <- coords_df %>%
    group_by(.data[[var]]) %>%
    summarise(Dim_1 = mean(Dim_1),
              Dim_2 = mean(Dim_2),
              n = n(), .groups = "drop")
  
  p <- ggplot() +
    geom_point(data = coords_df, aes(x = Dim_1, y = Dim_2, color = .data[[var]]),
               size = 0.5, alpha = 0.05) +
    geom_point(data = centroides, aes(x = Dim_1, y = Dim_2, color = .data[[var]]),
               size = 4, shape = 17) +
    stat_ellipse(data = coords_df, aes(x = Dim_1, y = Dim_2, color = .data[[var]]),
                 level = 0.95, linewidth = 1, alpha = 0.4) +
    geom_text_repel(data = centroides, aes(x = Dim_1, y = Dim_2, label = .data[[var]]),
                    size = 3, fontface = "bold") +
    theme_minimal() +
    labs(title = var, color = var)
  return(p)
}

# Crear directorio de salida
dir.create("Análisis de viviendas/Analisis/MCA_plots/Combinaciones", recursive = TRUE, showWarnings = FALSE)

# Variables categóricas
categorical_vars <- c("rural_cat", "sueldo_cat", "zona", 
                      "presencia_indigenas", "presencia_enfermos", "presencia_extranjeros")

# Dividir en grupos de 2 o 3 variables para combinar
groups <- split(categorical_vars, ceiling(seq_along(categorical_vars)/3))

# Loop para generar gráficos combinados
for (i in seq_along(groups)) {
  vars <- groups[[i]]
  plots <- lapply(vars, function(v) plot_single_var(coords, v))
  
  combined_plot <- wrap_plots(plots, ncol = length(vars)) +
    plot_annotation(title = paste("MCA - Combinación de variables:", paste(vars, collapse = ", ")))
  
  filename <- paste0("Análisis de viviendas/Analisis/MCA_plots/Combinaciones/MCA_combined_", i, ".png")
  ggsave(filename, combined_plot, width = 12, height = 6)
}

# Hexbin densidad por cluster
p_hex <- ggplot(coords, aes(x = Dim_1, y = Dim_2)) +
  geom_hex(bins = 50) +
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ Cluster) +
  theme_minimal() +
  labs(title = "Distribución de densidad por Cluster (hexbin)")
ggsave("Análisis de viviendas/Analisis/MCA_plots/MCA_hexbin_clusters.png", p_hex, width = 12, height = 8)

# Cada panel muestra cómo se concentran los hogares de un cluster en el espacio MCA.
# Zonas con muchos puntos se ven en colores más intensos, indicando que esas combinaciones de características (variables categóricas usadas en el MCA) son más frecuentes en ese cluster.
# Es una forma rápida de comparar la distribución espacial de los clusters en el MCA y ver si algunos clusters están más dispersos o concentrados en ciertas regiones del espacio.