
# Paquetes necesarios
#remotes::install_github("kassambara/factoextra")
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(dplyr)
library(ggrepel)

# Cargar datos
load("Análisis de viviendas/Data/data_macrotiplogias.RData")

# Preparar datos para MCA
mca_data <- data_macro %>%
  select(
   # household,
    macrogrupo,
    tipologia,
    zona,          
    rural_cat,        
    sueldo_cat
  ) %>%
  mutate(across(everything(), as.factor))


#MCA con macrogrupo como variable activa 
mca_result <- MCA(mca_data,quali.sup =c(1,2#,3
                                        ),ncp = 5, graph = FALSE)
nrow(mca_result$ind$coord) # debería ser 58.707
print(mca_result)
summary(mca_result)

# Eigenvalores
eig_val <- factoextra::get_eigenvalue(mca_result)
print(eig_val)

fviz_screeplot(mca_result, addlabels = TRUE, ylim = c(0, 60)) +
  ggtitle("Varianza explicada por cada dimensión (MCA)") # Es normal que estos valores sean bajos (Díaz Monroy & Morales Rivera, 2009)

# --- Gráfico básico de categorías ---
fviz_mca_var(
  mca_result,
  repel = FALSE,  # ⚡ Mucho más rápido
  ggtheme = theme_minimal(),
  title = "Categorías y variables"
)

# Tipologia
# Extraer coordenadas de variables suplementarias
quali_sup_coords <- as.data.frame(mca_result$quali.sup$coord)

# Renombrar columnas para que sean válidas en ggplot
colnames(quali_sup_coords) <- gsub(" ", "_", colnames(quali_sup_coords))
# Ahora las columnas se llaman Dim_1, Dim_2, etc.
quali_sup_coords$variable <- rownames(quali_sup_coords)

# Determinar si es tipología o macrogrupo
quali_sup_coords <- quali_sup_coords %>%
  mutate(
    tipo = ifelse(variable %in% levels(mca_data$macrogrupo), "macrogrupo", "tipologia"),
    grupo = variable
  )
# tipologia
#fviz_mca_biplot(
#  mca_result,
#  geom = "point",
#  habillage = mca_data$tipologia, # coloreamos por tipología
#  addEllipses = TRUE,             # elipses por grupo
#  ellipse.type = "confidence",    # intervalos de confianza
#  ggtheme = theme_minimal(),
#  title = "Distribución de hogares agrupados por tipología"
#) # No es informativo

# Graficar centroides de tipologías y macrogrupos
ggplot(quali_sup_coords, aes(x = Dim_1, y = Dim_2, color = tipo, label = grupo)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Posición de tipologías y macrogrupos en el espacio MCA",
    x = "Dimensión 1",
    y = "Dimensión 2",
    color = "Tipo de variable"
  )

# Visualización con variables suplementarias
# Hogares coloreados por macrogrupo
fviz_mca_biplot(
  mca_result,
  repel = FALSE,
  habillage = "macrogrupo",
  addEllipses = FALSE,
  geom = "point",
  alpha = 0.4,
  label = "var", 
  ggtheme = theme_minimal(),
  title = "MCA agrupado por macrogrupo"
)
fviz_mca_biplot(
  mca_result,
  geom = "point",
  habillage = mca_data$macrogrupo, # coloreamos por macrogrupo
  addEllipses = TRUE,
  ellipse.type = "confidence",
  ggtheme = theme_minimal(),
  title = "Distribución de hogares agrupados por macrogrupo"
)

coords <- as.data.frame(mca_result$ind$coord)
colnames(coords) <- gsub(" ", "_", colnames(coords))
coords$macrogrupo <- mca_data$macrogrupo

ggplot(coords, aes(x = Dim_1, y = Dim_2, color = macrogrupo)) +
  geom_point(size = 0.3, alpha = 0.3) +
  theme_minimal() +
  labs(
    title = "MCA - Distribución de los 58.707 hogares por macrogrupo",
    x = paste0("Dim1 (", round(mca_result$eig[1,2], 1), "%)"),
    y = paste0("Dim2 (", round(mca_result$eig[2,2], 1), "%)"),
    color = "Macrogrupo"
  ) # Aquí deberían verse todos los hogares peor no pasa
ggplot(coords, aes(x = Dim_1, y = Dim_2, color = macrogrupo)) +
  geom_count(alpha = 0.5) +
  theme_minimal() +
  ggtitle("Distribución de hogares por macrogrupo (tamaño = cantidad)")
# en gráfico hexagonal
library(hexbin)
# Gráfico de hexágonos para visualizar densidad
ggplot(coords, aes(x = Dim_1, y = Dim_2)) +
  geom_hex(bins = 50) +  # Agrupa puntos en hexágonos
  scale_fill_viridis_c(option = "plasma") +
  facet_wrap(~ macrogrupo) +  # Por macrogrupo
  theme_minimal() +
  labs(title = "Distribución de densidad por macrogrupo (hexbin)")
#alpha blending
# Puntos muy transparentes
ggplot(coords, aes(x = Dim_1, y = Dim_2, color = macrogrupo)) +
  geom_point(size = 0.8, alpha = 0.05) +  # Alpha muy bajo
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))
# Representacion de hogares por categoría
fviz_mca_ind(
  mca_result,
  geom = "point",                     # nube de puntos
  habillage = mca_data$macrogrupo,    # color por macrogrupo
  addEllipses = FALSE,                # sin elipses
  pointsize = 1.5,                    # tamaño de los puntos un poco más visible
  alpha = 0.5,                        # transparencia moderada
  ggtheme = theme_minimal(),
  title = "Distribución completa de hogares por macrogrupo"
) # se solapan los puntos
# Graficar nube de puntos con densidad visual
ggplot(coords, aes(x = Dim_1, y = Dim_2)) +
  geom_point(aes(color = macrogrupo), size = 1.5, alpha = 0.4) + # todos los puntos visibles
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.2, color = NA) + # zonas densas
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  ggtitle("Distribución de hogares por macrogrupo con densidad") +
  guides(color = guide_legend(title = "Macrogrupo"),
         fill = guide_colorbar(title = "Densidad"))
# Gráfico interactivo con plotly
library(plotly)
p <- ggplot(coords, aes(x = Dim_1, y = Dim_2, color = macrogrupo,
                        text = paste("Macrogrupo:", macrogrupo))) +
  geom_point(size = 1, alpha = 0.3) +
  theme_minimal()
p

# En general los gráficos por tipologia y macrotipologia no son conluyentes

#Gráfico de centroides
coords$tipologia <- mca_data$tipologia
coords$zona <- mca_data$zona
coords$rural_cat <- mca_data$rural_cat
coords$sueldo_cat <- mca_data$sueldo_cat
# Calcular centroides por zona
centroides <- coords %>%
  group_by(zona) %>%
  summarise(
    Dim_1 = mean(Dim_1),
    Dim_2 = mean(Dim_2),
    n = n()
  )

# Gráfico con centroides y elipses
ggplot() +
  # Todos los puntos (muy transparentes)
  geom_point(data = coords, aes(x = Dim_1, y = Dim_2, color = zona),
             size = 0.5, alpha = 0.05) +
  # Centroides
  geom_point(data = centroides, aes(x = Dim_1, y = Dim_2, color = zona),
             size = 5, shape = 17) +
  # Elipses de confianza
  stat_ellipse(data = coords, aes(x = Dim_1, y = Dim_2, color = zona),
               level = 0.95, linewidth = 1) +
  # Etiquetas de centroides
  geom_text_repel(data = centroides, aes(x = Dim_1, y = Dim_2, label = zona),
                  size = 4, fontface = "bold") +
  theme_minimal() +
  labs(title = "Centroides y distribución de 58,707 hogares por zona")

# Calcular centroides por rural
centroides <- coords %>%
  group_by(rural_cat) %>%
  summarise(
    Dim_1 = mean(Dim_1),
    Dim_2 = mean(Dim_2),
    n = n()
  )

# Gráfico con centroides y elipses
ggplot() +
  # Todos los puntos (muy transparentes)
  geom_point(data = coords, aes(x = Dim_1, y = Dim_2, color = rural_cat),
             size = 0.5, alpha = 0.05) +
  # Centroides
  geom_point(data = centroides, aes(x = Dim_1, y = Dim_2, color = rural_cat),
             size = 5, shape = 17) +
  # Elipses de confianza
  stat_ellipse(data = coords, aes(x = Dim_1, y = Dim_2, color = rural_cat),
               level = 0.95, linewidth = 1) +
  # Etiquetas de centroides
  geom_text_repel(data = centroides, aes(x = Dim_1, y = Dim_2, label = rural_cat),
                  size = 4, fontface = "bold") +
  theme_minimal() +
  labs(title = "Centroides y distribución de 58,707 hogares por rural")

# Calcular centroides por sueldo
centroides <- coords %>%
  group_by(sueldo_cat) %>%
  summarise(
    Dim_1 = mean(Dim_1),
    Dim_2 = mean(Dim_2),
    n = n()
  )

# Gráfico con centroides y elipses
ggplot() +
  # Todos los puntos (muy transparentes)
  geom_point(data = coords, aes(x = Dim_1, y = Dim_2, color = sueldo_cat),
             size = 0.5, alpha = 0.05) +
  # Centroides
  geom_point(data = centroides, aes(x = Dim_1, y = Dim_2, color = sueldo_cat),
             size = 5, shape = 17) +
  # Elipses de confianza
  stat_ellipse(data = coords, aes(x = Dim_1, y = Dim_2, color = sueldo_cat),
               level = 0.95, linewidth = 1) +
  # Etiquetas de centroides
  geom_text_repel(data = centroides, aes(x = Dim_1, y = Dim_2, label = sueldo_cat),
                  size = 4, fontface = "bold") +
  theme_minimal() +
  labs(title = "Centroides y distribución de 58,707 hogares por sueldo")


# Gráfico por categorías
coords <- as.data.frame(mca_result$ind$coord)
colnames(coords) <- gsub(" ", "_", colnames(coords))

coords <- coords %>%
  mutate(
    zona = mca_data$zona,
    rural_cat = mca_data$rural_cat,
    sueldo_cat = mca_data$sueldo_cat
  )

# Gráfico con tres leyendas visibles
ggplot(coords, aes(x = Dim_1, y = Dim_2)) +
  
  # Puntos coloreados por zona
  geom_point(aes(color = zona), alpha = 0.4, size = 2) +
  
  # Elipses por zona
  stat_ellipse(aes(color = zona), level = 0.95, linewidth = 1) +
  
  # Diferenciar forma según ruralidad
  geom_point(aes(shape = rural_cat), size = 3, alpha = 0.5) +
  
  # Contorno según sueldo
  geom_point(aes(fill = sueldo_cat), shape = 21, size = 2, color = "black", alpha = 0.5) +
  
  # Tema limpio
  theme_minimal() +
  
  labs(
    title = "Representación de hogares por categoría",
    x = "Dimensión 1",
    y = "Dimensión 2",
    color = "Zona",
    shape = "Ruralidad",
    fill = "Sueldo"
  ) +
  
  guides(
    color = guide_legend(override.aes = list(size = 4, alpha = 1)),
    shape = guide_legend(override.aes = list(size = 4, alpha = 1)),
    fill = guide_legend(override.aes = list(size = 4, alpha = 1))
  )