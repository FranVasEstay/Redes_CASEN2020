################################################################################
###################### Social Network: encuesta CASEN ##########################
###################### ANÁLISIS DE MACROTIPOLOGÍAS #############################
################################################################################

# LIBRERÍAS
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)
library(grid)
library(gridExtra)
library(png)
library(scales)

# =============================================================================
# 1. CARGA DE DATOS Y CREACIÓN DEL DATASET DE HOGARES CON MACROGRUPO
# =============================================================================

# Datos a nivel hogar (measurements) y a nivel persona (data_con_tipologia)
load("Análisis de viviendas/Descriptives/medidas_redes.RData")
load("Análisis de viviendas/Data/Data_con_tipología.RData")

# Clasificación de tipologías en macrogrupos
macrogrupos <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipo.csv",
                        stringsAsFactors = FALSE) %>%
  filter(Tipologia %in% paste0("T", 1:48))

# Extraer información a nivel hogar desde data_con_tipologia (una fila por hogar)
data_con_tipologia <-data_con_tipologia %>%
  mutate(
    macrozona = case_when(
      region %in% c("Arica y Parinacota", "Tarapacá","Antofagasta","Atacama","Coquimbo") ~ "Norte",
      region %in% c("Valparaíso","Metropolitana","O'Higgins", 
                    "Maule", "Ñuble") ~ "Centro",
      region %in% c("Biobío", "La Araucanía", 
                    "Los Ríos", "Los Lagos","Aysén", "Magallanes") ~ "Sur",
      TRUE ~ "Otra"
    ))

hogar_info <- data_con_tipologia %>%
  distinct(household, tipologia, porc_rural, rural_cat, macrozona)

# Construir dataset principal: measurements + tipología + variables geográficas + macrogrupo
hogares <- measurements %>%
  inner_join(hogar_info, by = "household") %>%
  inner_join(macrogrupos, by = c("tipologia" = "Tipologia")) %>%
  mutate(
    macrogrupo = factor(macrogrupo,
                        levels = c("Isolated","Childless Couple","Tradicional Nuclear","Single-Parent","Extended"))
  )

# Asegurarnos de que existan tiene_extranjero y tiene_indigena
# (en measurements ya deberían estar, pero por si acaso)
if (!"tiene_extranjero" %in% names(hogares)) {
  hogares <- hogares %>% mutate(tiene_extranjero = FALSE)
}
if (!"tiene_indigena" %in% names(hogares)) {
  hogares <- hogares %>% mutate(tiene_indigena = FALSE)
}
# Extraer las etiquetas de nivel_educ_jefe
hogares <- hogares %>%
  mutate(nivel_educ_jefe = haven::as_factor(nivel_educ_jefe))
# Total de hogares en el dataset (para calcular porcentajes reales)
total_hogares <- nrow(hogares)

hogares <- hogares %>%
  mutate(
    pobreza_hogar = case_when(
      grepl("Pobres extremos", pobreza_hogar) ~ "Pobres extremos",
      grepl("Pobres no extremos", pobreza_hogar) ~ "Pobres no extremos",
      grepl("No pobres", pobreza_hogar) ~ "No pobres",
      TRUE ~ NA_character_
    ),
    hacinamiento_cat = case_when(
      grepl("Sin hacinamiento", hacinamiento_cat) ~ "Sin hacinamiento",
      grepl("Hacinamiento medio", hacinamiento_cat) ~ "Hacinamiento medio",
      grepl("Hacinamiento alto", hacinamiento_cat) ~ "Hacinamiento alto",
      grepl("Hacinamiento crítico", hacinamiento_cat) ~ "Hacinamiento crítico",
      TRUE ~ NA_character_
    ),
    jefe_mujer = ifelse(sexo_jefe == "Mujer", 1, 0)
  )

# =============================================================================
# 2. TABLA DESCRIPTIVA COMPLETA POR MACROGRUPO
# =============================================================================
tabla_macro <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n(),
    porcentaje = round(n_hogares / total_hogares * 100, 1),
    # Edad
    edad_mediana = median(edad_prom, na.rm = TRUE),
    # Género
    porc_jefe_mujer = mean(jefe_mujer, na.rm = TRUE) * 100,
    # Ingresos
    ingreso_mediano = median(sueldo, na.rm = TRUE),
    Q1_ingreso = quantile(sueldo, 0.25, na.rm = TRUE),
    Q3_ingreso = quantile(sueldo, 0.75, na.rm = TRUE),
    # Ruralidad
    porc_rural = mean(rural_cat == "Rural", na.rm = TRUE) * 100,
    # Distribución por macrozona
    porc_Norte = mean(macrozona == "Norte", na.rm = TRUE) * 100,
    porc_Centro = mean(macrozona == "Centro", na.rm = TRUE) * 100,
    porc_Sur = mean(macrozona == "Sur", na.rm = TRUE) * 100,
    # Empleo (promedios de tasas hogareñas)
    tasa_desocupacion = mean(tasa_desocupacion, na.rm = TRUE),
    tasa_inactividad = mean(tasa_inactividad, na.rm = TRUE),
    # Salud
    porc_enfermos = mean(porc_enfermos, na.rm = TRUE),
    # Educación
    max_escolaridad_promedio = mean(max_escolaridad, na.rm = TRUE),
    porc_jefe_educ_superior = mean(nivel_educ_jefe == "Profesional completo", na.rm = TRUE) * 100,
    # Pobreza (ahora sí con etiquetas correctas)
    porc_pobreza_extrema = mean(pobreza_hogar == "Pobres extremos", na.rm = TRUE) * 100,
    porc_pobreza_no_extrema = mean(pobreza_hogar == "Pobres no extremos", na.rm = TRUE) * 100,
    porc_no_pobre = mean(pobreza_hogar == "No pobres", na.rm = TRUE) * 100,
    # Hacinamiento (ya con etiquetas reducidas)
    sin_hacinamiento = mean(hacinamiento_cat == "Sin hacinamiento", na.rm = TRUE) * 100,
    hacinamiento_medio = mean(hacinamiento_cat == "Hacinamiento medio", na.rm = TRUE) * 100,
    hacinamiento_alto = mean(hacinamiento_cat == "Hacinamiento alto", na.rm = TRUE) * 100,
    hacinamiento_critico = mean(hacinamiento_cat == "Hacinamiento crítico", na.rm = TRUE) * 100,
    # Allegamiento
    allegamiento_interno = mean(allegamiento_interno, na.rm = TRUE) * 100,
    allegamiento_externo = mean(allegamiento_externo, na.rm = TRUE) * 100,
    # Extranjeros e indígenas
    porc_hogares_extranjero = mean(tiene_extranjero, na.rm = TRUE) * 100,
    porc_hogares_indigena = mean(tiene_indigena, na.rm = TRUE) * 100,
    # Tipo generacional
    porc_Multigeneracional = mean(tipo_gen == "Multigeneracional", na.rm = TRUE) * 100,
    porc_Sin_gen_intermedia = mean(tipo_gen == "Sin generación intermedia", na.rm = TRUE) * 100,
    porc_Sin_adultos_mayores = mean(tipo_gen == "Sin adultos mayores", na.rm = TRUE) * 100,
    porc_Sin_menores_15 = mean(tipo_gen == "Sin menores de 15", na.rm = TRUE) * 100,
    porc_Solo_15_64 = mean(tipo_gen == "Solo 15-64", na.rm = TRUE) * 100,
    porc_Solo_mayores_64 = mean(tipo_gen == "Solo mayores de 64", na.rm = TRUE) * 100,
    .groups = "drop"
  )
write.csv(tabla_macro, "Análisis de viviendas/Analisis/descriptives_macrogrupos.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# =============================================================================
# 3. GRÁFICOS PRINCIPALES
# =============================================================================
library(tidyverse)
library(patchwork)
library(scales)
library(ggrepel)
library(RColorBrewer)

tabla_macro$macrogrupo <- as.factor(tabla_macro$macrogrupo)

# =============================================================================
# 3.1. FRECUENCIA DE HOGARES POR MACROGRUPO
# =============================================================================
p_freq <- hogares %>%
  count(macrogrupo) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = macrogrupo, y = n, fill = macrogrupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%\n(n = ", n, ")")), 
            vjust = -0.3, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(title = "Distribution of households by Macrogroup",
       x = "", y = "Number of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3.2. INGRESO QUINTIL – GRÁFICO DE LINES Y PUNTOS
# =============================================================================
p_income <- hogares %>%
  group_by(macrogrupo, quintil_ingreso) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100) %>%
  ggplot(aes(x = macrogrupo, y = porc, fill = quintil_ingreso)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_fill(vjust = 0.5),
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Income quintile") +
  labs(title = "Distribution of income quintiles by Macrogroup",
       x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3.3. RURALIDAD
# =============================================================================
datos_rural <- hogares %>%
  group_by(macrogrupo, rural_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100) %>%
  mutate(
    rural_cat = recode(rural_cat,
                       "Rural" = "Rural",
                       "Urbano" = "Urban",
                       "Mixto" = "Mixed",
                       .default = as.character(rural_cat)))
p_rural <- datos_rural %>%
  ggplot(aes(x = macrogrupo, y = porc, fill = rural_cat)) +
  geom_col(position = "fill", width = 0.7) +
  # Etiquetas de porcentaje dentro de cada segmento
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_fill(vjust = 0.5),
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Area") +
  labs(title = "Proportion of rural, mixed and urban households by Macrogroup",
       x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3.4. GÉNERO DEL JEFE DE HOGAR
# =============================================================================
p_genero <- tabla_macro %>%
  ggplot(aes(x = macrogrupo, y = porc_jefe_mujer, fill = macrogrupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porc_jefe_mujer, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(title = "Households with female headship",
       x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3.5. COMPOSICIÓN ÉTNICA: PRESENCIA INDÍGENA Y EXTRANJERA (dot‑plot conjunto)
# =============================================================================
indicadores_ie <- tabla_macro %>%
  select(macrogrupo, 
         Native = porc_hogares_indigena,
         Foreign = porc_hogares_extranjero) %>%
  pivot_longer(cols = -macrogrupo, names_to = "indicador", values_to = "porcentaje")

p_ie <- ggplot(indicadores_ie, aes(x = porcentaje, y = macrogrupo, color = indicador)) +
  geom_line(aes(group = macrogrupo), color = "gray70", linewidth = 0.5) +  # línea de conexión
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -0.8, size = 3.5, color = "black", show.legend = FALSE) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.8, name = "") +
  labs(title = "Households with indigenous and foreign presence",
       x = "Proportion of households", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# =============================================================================
# 3.6. SALUD
# =============================================================================
tabla_salud <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n(),
    hogares_con_enfermo = sum(porc_enfermos > 0, na.rm = TRUE),
    porc_hogares_con_enfermo = (hogares_con_enfermo / n_hogares) * 100
  )

p_salud <- tabla_salud %>%
  ggplot(aes(x = macrogrupo, y = porc_hogares_con_enfermo, fill = macrogrupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porc_hogares_con_enfermo, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(title = "Households with at least one member suffering from chronic illness",
       x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3.7. ZONA GEOGRÁFICA – GRÁFICO DE BARRAS APILADAS
# =============================================================================

hogares_zona <- hogares %>%
  count(macrogrupo, macrozona) %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100)

p_zona <- ggplot(hogares_zona, aes(x = macrogrupo, y = porc, fill = macrozona)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porc > 5, paste0(round(porc, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Macrozone") + 
  labs(title = "Geographic distribution by Macrogroup",
       x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 3.8. POBREZA (barras apiladas al 100%)
# -------------------------------------------------------------------------
pobreza_long <- tabla_macro %>%
  select(macrogrupo, porc_pobreza_extrema, porc_pobreza_no_extrema, porc_no_pobre) %>%
  pivot_longer(cols = -macrogrupo, names_to = "categoria", values_to = "porcentaje")

p_pobreza <- ggplot(pobreza_long, aes(x = macrogrupo, y = porcentaje, fill = categoria)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porcentaje > 5, paste0(round(porcentaje, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, 
                       name = "",
                       labels = c(porc_pobreza_extrema = "Extreme poverty",
                                  porc_pobreza_no_extrema = "Non-extreme poverty",
                                  porc_no_pobre = "Not poor")) +
  labs(title = "Poverty by Macrogroup", x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 3.9. HACINAMIENTO (barras apiladas)
# -------------------------------------------------------------------------
hacinamiento_long <- tabla_macro %>%
  select(macrogrupo, sin_hacinamiento, hacinamiento_medio, 
         hacinamiento_alto, hacinamiento_critico) %>%
  pivot_longer(cols = -macrogrupo, 
               names_to = "categoria", 
               values_to = "porcentaje") %>%
  mutate(categoria = recode(categoria,
                            sin_hacinamiento = "No overcrowding",
                            hacinamiento_medio = "Medium overcrowding",
                            hacinamiento_alto = "High overcrowding",
                            hacinamiento_critico = "Critical overcrowding"))

p_hacinamiento <- ggplot(hacinamiento_long, aes(x = macrogrupo, y = porcentaje, fill = categoria)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porcentaje > 5, paste0(round(porcentaje, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "") +
  labs(title = "Overcrowding by Macrogroup", x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 3.10. ALLEGAMIENTO (barras agrupadas)
# -------------------------------------------------------------------------
allegamiento_long <- tabla_macro %>%
  select(macrogrupo, allegamiento_interno, allegamiento_externo) %>%
  pivot_longer(cols = -macrogrupo, 
               names_to = "tipo", 
               values_to = "porcentaje") %>%
  mutate(tipo = recode(tipo,
                       allegamiento_interno = "Internal co-residence",
                       allegamiento_externo = "External co-residence"))

p_allegamiento <- ggplot(allegamiento_long, aes(x = macrogrupo, y = porcentaje, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.6, name = "") +
  labs(title = "Allegamiento (shared housing) by Macrogroup", x = "", y = "Proportion of households") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 3.11. TIPO GENERACIONAL (barras apiladas)
# -------------------------------------------------------------------------
generacional_long <- tabla_macro %>%
  select(macrogrupo, starts_with("porc_")) %>%
  select(macrogrupo, porc_Multigeneracional, porc_Sin_gen_intermedia,
         porc_Sin_adultos_mayores, porc_Sin_menores_15,
         porc_Solo_15_64, porc_Solo_mayores_64) %>%
  pivot_longer(cols = -macrogrupo, 
               names_to = "tipo", 
               values_to = "porcentaje") %>%
  mutate(tipo = recode(tipo,
                       porc_Multigeneracional = "Multigenerational",
                       porc_Sin_gen_intermedia = "No intermediate generation",
                       porc_Sin_adultos_mayores = "No older adults",
                       porc_Sin_menores_15 = "No children under 15",
                       porc_Solo_15_64 = "Only 15-64 years",
                       porc_Solo_mayores_64 = "Only over 64 years"))

p_generacional <- ggplot(generacional_long, aes(x = macrogrupo, y = porcentaje, fill = tipo)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porcentaje > 5, paste0(round(porcentaje, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 2.8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "") +
  labs(title = "Generational composition of households", x = "", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8))

# =============================================================================
# 3.12. COMPOSICIÓN FINAL Y GUARDADO
# =============================================================================
panel_composicion <- (p_freq + p_income) / 
  (p_rural + p_genero) +
  plot_annotation(
    title = "Basic Composition of households by Macrogroup",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )
ggsave("Análisis de viviendas/Analisis/Figure_1.png", 
       panel_composicion, width = 14, height = 10, dpi = 200, bg = "white")

panel_diversidad <- (p_ie + p_salud) + 
  plot_annotation(
    title = "Ethnic diversity, migration and health by Macrogroup",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )
ggsave("Análisis de viviendas/Analisis/Figure_3.png", 
       panel_diversidad, width = 14, height = 7, dpi = 200, bg = "white")


panel_vivienda <- (p_pobreza + p_hacinamiento + p_allegamiento) +
  plot_annotation(
    title = "Poverty, overcrowding and shared housing by Macrogroup",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )
ggsave("Análisis de viviendas/Analisis/Figure_4.png", 
       panel_vivienda, width = 18, height = 7, dpi = 200, bg = "white")

panel_territorio <- (p_zona / p_generacional) +
  plot_annotation(
    title = "Geographic distribution and generational composition",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )
ggsave("Análisis de viviendas/Analisis/Figure_5.png", 
       panel_territorio, width = 12, height = 12, dpi = 200, bg = "white")

# =============================================================================
# 4. MATRIZ DE ESTRUCTURAS BASE (EJEMPLOS DE CADA MACROGRUPO)
# =============================================================================
plot_typology_gg <- function(g, title_text, node_size = 8, label_size = 3) {
  # Convertir igraph a objeto tidygraph
  tg <- as_tbl_graph(g)
  
  # Determinar tipo de aristas (si no existe atributo 'type', asumir "descent")
  if ("type" %in% edge_attr_names(g)) {
    edge_type <- E(g)$type
  } else {
    edge_type <- rep("descent", ecount(g))
  }
  
  # Colores y tipos de línea
  edge_color <- ifelse(edge_type == "marriage", "#4daf4a", "#ff7f00")
  edge_linetype <- ifelse(edge_type == "marriage", "solid", "solid") # o "dashed" si quieres diferenciar
  
  # Creamos el gráfico
  p <- ggraph(tg, layout = "nicely") +   # layout automático
    geom_edge_link(aes(edge_colour = edge_type),
                   edge_width = 1.2,
                   arrow = arrow(type = "closed", length = unit(3, "mm")),
                   end_cap = circle(5, "mm"),
                   start_cap = circle(5, "mm")) +
    geom_node_point(aes(), size = node_size, color = "#6baed6", fill = "white", shape = 21, stroke = 1) +
    geom_node_text(aes(label = name), size = label_size, repel = TRUE) +
    scale_edge_colour_manual(values = c("marriage" = "#4daf4a", "descent" = "#ff7f00")) +
    theme_void() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold")) +
    labs(title = title_text)
  
  return(p)
}

# Carpeta con las imágenes PNG de cada tipología
path_graficos <- "Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/"

# Ejemplos representativos de cada macrogrupo
macros_ejemplo <- list(
  "Isolated (T1)"         = "T1",
  "Childless Couple (T2)" = "T2",
  "Nuclear (T3)"          = "T3",
  "Single-Parent (T9)"    = "T9",
  "Extended (T41)"        = "T41"
)

# Generar los gráficos como ggplots
plots_macro <- lapply(names(macros_ejemplo), function(nm) {
  t_code <- macros_ejemplo[[nm]]
  g <- examples[[t_code]]
  plot_typology_gg(g, title_text = nm, node_size = 6, label_size = 3)
})

panel_basico <- wrap_plots(plots_macro, nrow = 1) +
  plot_annotation(
    title = "Basic Structures of Macrogroups",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

ggsave("Análisis de viviendas/Analisis/matriz_macrogrupos_basicos.png",
       plot = panel_basico, width = 20, height = 6, dpi = 300, bg = "white")

png("Análisis de viviendas/Analisis/matriz_macrogrupos_basicos.png",
    width = 12, height = 6, units = "in", res = 150)
grid.arrange(grobs = panel_basico, nrow = 1,
             top = textGrob("Basic Structures of Macrogroups", gp = gpar(fontsize = 14)))
dev.off()

# =============================================================================
# 5. SUBTIPOS DE ESTRUCTURAS EXTENDIDAS
# =============================================================================

# Ejemplos de subcategorías de estructuras Extendidas
subcats_extendida <- list(
  "Tradicional Nuclear Extended (T43)"      = "T43",
  "Single-Parent Extended (T41)" = "T41",
  "Extended Couple (T30)"       = "T30",
  "Reconstituted (T14)"          = "T14",
  "Complex (T36)"               = "T36"
)

plots_ext <- lapply(names(subcats_extendida), function(nm) {
  t_code <- subcats_extendida[[nm]]
  g <- examples[[t_code]]
  # Texto más detallado abajo
  p <- plot_typology_gg(g, title_text = nm, node_size = 6, label_size = 3)
  # Añadir subtítulo o información adicional si quieres (opcional)
  return(p)
})

panel_extendido <- wrap_plots(plots_ext, ncol = 3) +
  plot_annotation(
    title = "Extended subgroups",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16))
  )

ggsave("Análisis de viviendas/Analisis/matriz_subtipos_extendidos.png",
       plot = panel_extendido, width = 18, height = 10, dpi = 300, bg = "white")

png("Análisis de viviendas/Analisis/matriz_subtipos_extendidos.png",
    width = 12, height = 8, units = "in", res = 150)
grid.arrange(grobs = panel_extendido, ncol = 3,
             top = textGrob("Extended subgroups", gp = gpar(fontsize = 14)))
dev.off()

# =============================================================================
# 6. GUARDAR DATASET FINAL
# =============================================================================
save(hogares, file = "Análisis de viviendas/Data/hogares_con_macrogrupo.RData")
