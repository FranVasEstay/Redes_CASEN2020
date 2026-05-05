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
  distinct(household, tipologia, porc_rural, rural_cat,area, macrozona)

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
    porc_rural = mean(area == "Rural", na.rm = TRUE) * 100,
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
# 1. FRECUENCIA DE HOGARES POR MACROGRUPO
# =============================================================================
p_freq <- hogares %>%
  count(macrogrupo) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = macrogrupo, y = n, fill = macrogrupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%\n(n = ", n, ")")), 
            vjust = -0.3, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(title = "Distribución de hogares por Macrogrupo",
       x = "", y = "Número de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 2. INGRESO QUINTIL – GRÁFICO DE LINES Y PUNTOS
# =============================================================================
p_income <- hogares %>%
  group_by(macrogrupo, quintil_ingreso) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100) %>%
  ggplot(aes(x = macrogrupo, y = porc, fill = quintil_ingreso)) +
  geom_col(position = "fill", width = 0.7) +
  # Etiquetas de porcentaje dentro de cada segmento
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_fill(vjust = 0.5),
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Quintil") +
  labs(title = "Distribución de quintiles de ingreso por Macrogrupo",
       x = "", y = "Proporción de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3. RURALIDAD
# =============================================================================
datos_rural <- hogares %>%
  group_by(macrogrupo, rural_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100)

p_rural <- datos_rural %>%
  ggplot(aes(x = macrogrupo, y = porc, fill = rural_cat)) +
  geom_col(position = "fill", width = 0.7) +
  # Etiquetas de porcentaje dentro de cada segmento
  geom_text(aes(label = paste0(round(porc, 1), "%")),
            position = position_fill(vjust = 0.5),
            size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Área") +
  labs(title = "Proporción de hogares rurales y urbanos por Macrogrupo",
       x = "", y = "Proporción de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 4. GÉNERO DEL JEFE DE HOGAR
# =============================================================================
p_genero <- tabla_macro %>%
  ggplot(aes(x = macrogrupo, y = porc_jefe_mujer, fill = macrogrupo)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(porc_jefe_mujer, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_viridis_d(option = "plasma", end = 0.8) +
  labs(title = "Hogares con jefatura femenina",
       x = "", y = "% de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 5. COMPOSICIÓN: PRESENCIA INDÍGENA Y EXTRANJERA (dot‑plot conjunto)
# =============================================================================
indicadores_ie <- tabla_macro %>%
  select(macrogrupo, 
         Indígena = porc_hogares_indigena,
         Extranjero = porc_hogares_extranjero) %>%
  pivot_longer(cols = -macrogrupo, names_to = "indicador", values_to = "porcentaje")

p_ie <- ggplot(indicadores_ie, aes(x = porcentaje, y = macrogrupo, color = indicador)) +
  geom_line(aes(group = macrogrupo), color = "gray70", linewidth = 0.5) +  # línea de conexión
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            vjust = -0.8, size = 3.5, color = "black", show.legend = FALSE) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_color_viridis_d(option = "plasma", end = 0.8, name = "") +
  labs(title = "Hogares con presencia indígena y extranjera",
       x = "% de hogares", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

p_ie

# =============================================================================
# 6. SALUD
# =============================================================================
salud_ordenado <- tabla_macro %>%
  arrange(porc_enfermos) %>%
  mutate(macrogrupo = factor(macrogrupo, levels = macrogrupo))

p_salud <- ggplot(salud_ordenado, aes(x = porc_enfermos, y = macrogrupo)) +
  geom_col(aes(fill = porc_enfermos), width = 0.6) +
  geom_text(aes(label = paste0(round(porc_enfermos, 1), "%")),
            hjust = -0.2, size = 3.5) +
  scale_x_continuous(labels = percent_format(scale = 1),
                     limits = c(0, max(tabla_macro$porc_enfermos) * 1.1)) +
  scale_fill_viridis_c(option = "plasma", begin = 0.2, end = 0.8, name = "% enfermos") +
  labs(title = "Promedio de miembros con enfermedad crónica por hogar",
       x = "% promedio de enfermos en el hogar", y = "") +
  theme_minimal()

p_salud

# =============================================================================
# 7. ZONA GEOGRÁFICA – GRÁFICO DE BARRAS APILADAS
# =============================================================================

hogares_zona <- hogares %>%
  count(macrogrupo, macrozona) %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100)

p_zona <- ggplot(hogares_zona, aes(x = macrogrupo, y = porc, fill = macrozona)) +
  geom_col(position = "fill", width = 0.7) +
  # Etiquetas de porcentaje (opcional, evita saturación si hay muchas zonas)
  geom_text(aes(label = ifelse(porc > 5, paste0(round(porc, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "Macrozona") +  # ← cambiar a _d
  labs(title = "Distribución geográfica por Macrogrupo",
       x = "", y = "Proporción de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 8. POBREZA (barras apiladas al 100%)
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
                       labels = c(porc_pobreza_extrema = "Pobreza extrema",
                                  porc_pobreza_no_extrema = "Pobreza no extrema",
                                  porc_no_pobre = "No pobre")) +
  labs(title = "Pobreza por macrogrupo", x = "", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 2. HACINAMIENTO (barras apiladas al 100%)
# -------------------------------------------------------------------------
hacinamiento_long <- tabla_macro %>%
  select(macrogrupo, sin_hacinamiento, hacinamiento_medio, 
         hacinamiento_alto, hacinamiento_critico) %>%
  pivot_longer(cols = -macrogrupo, 
               names_to = "categoria", 
               values_to = "porcentaje") %>%
  mutate(categoria = recode(categoria,
                            sin_hacinamiento = "Sin hacinamiento",
                            hacinamiento_medio = "Hacinamiento medio",
                            hacinamiento_alto = "Hacinamiento alto",
                            hacinamiento_critico = "Hacinamiento crítico"))

p_hacinamiento <- ggplot(hacinamiento_long, aes(x = macrogrupo, y = porcentaje, fill = categoria)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porcentaje > 5, paste0(round(porcentaje, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 3, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "") +
  labs(title = "Hacinamiento por macrogrupo", x = "", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# -------------------------------------------------------------------------
# 3. ALLEGAMIENTO (barras agrupadas / puntos conectados)
# -------------------------------------------------------------------------
allegamiento_long <- tabla_macro %>%
  select(macrogrupo, allegamiento_interno, allegamiento_externo) %>%
  pivot_longer(cols = -macrogrupo, 
               names_to = "tipo", 
               values_to = "porcentaje") %>%
  mutate(tipo = recode(tipo,
                       allegamiento_interno = "Allegamiento interno",
                       allegamiento_externo = "Allegamiento externo"))

# Opción A: barras agrupadas (más directa)
p_allegamiento <- ggplot(allegamiento_long, aes(x = macrogrupo, y = porcentaje, fill = tipo)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")),
            position = position_dodge(width = 0.7), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.6, name = "") +
  labs(title = "Allegamiento por macrogrupo", x = "", y = "% hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# Opción B: dot‑plot (como el de indígenas/extranjeros) – más compacto
p_allegamiento_dot <- allegamiento_long %>%
  ggplot(aes(x = porcentaje, y = macrogrupo, color = tipo)) +
  geom_line(aes(group = macrogrupo), color = "gray70", linewidth = 0.5) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            hjust = -0.2, size = 3) +
  scale_x_continuous(labels = percent_format(), limits = c(0, NA)) +
  scale_color_viridis_d(option = "plasma", end = 0.8, name = "") +
  labs(title = "Allegamiento interno vs externo", x = "% hogares", y = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# -------------------------------------------------------------------------
# 4. TIPO GENERACIONAL (barras apiladas al 100%)
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
                       porc_Multigeneracional = "Multigeneracional",
                       porc_Sin_gen_intermedia = "Sin gen. intermedia",
                       porc_Sin_adultos_mayores = "Sin adultos mayores",
                       porc_Sin_menores_15 = "Sin menores de 15",
                       porc_Solo_15_64 = "Solo 15-64",
                       porc_Solo_mayores_64 = "Solo mayores 64"))

p_generacional <- ggplot(generacional_long, aes(x = macrogrupo, y = porcentaje, fill = tipo)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(porcentaje > 5, paste0(round(porcentaje, 1), "%"), "")),
            position = position_fill(vjust = 0.5), size = 2.8, color = "white") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = "") +
  labs(title = "Composición generacional de los hogares", x = "", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 8))
p_pobreza
p_hacinamiento
p_allegamiento        # o p_allegamiento_dot
p_generacional


(p_pobreza | p_hacinamiento) / p_allegamiento + 
  plot_annotation(title = "Condiciones de la vivienda por macrogrupo")

# =============================================================================
# 10. COMPOSICIÓN FINAL Y GUARDADO
# =============================================================================
# Panel principal (frecuencia, ingreso, ruralidad, género)
panel_principal <- (p_freq + p_income) / (p_rural + p_genero)
ggsave("Analisis/graficos_principales_macrogrupos.png",
       panel_principal, width = 14, height = 10, dpi = 200, bg = "white")

# Panel secundario (indicadores + zona)
panel_secundario <- (p_indicadores | p_zona) + 
  plot_annotation(title = "Indicadores adicionales por Macrogrupo")
ggsave("Analisis/graficos_secundarios_macrogrupos.png",
       panel_secundario, width = 14, height = 7, dpi = 200, bg = "white")

# =============================================================================
# 5. MATRIZ DE ESTRUCTURAS BASE (EJEMPLOS DE CADA MACROGRUPO)
# =============================================================================

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

plots_macro <- list()
for (nm in names(macros_ejemplo)) {
  t_code <- macros_ejemplo[[nm]]
  img_path <- paste0(path_graficos, t_code, ".png")
  
  if (file.exists(img_path)) {
    img <- readPNG(img_path)
    g <- rasterGrob(img, interpolate = TRUE)
    plots_macro[[nm]] <- arrangeGrob(g, bottom = textGrob(nm, gp = gpar(fontsize = 10)))
  } else {
    plots_macro[[nm]] <- textGrob(paste("Falta:", t_code), gp = gpar(col = "red"))
  }
}

grid.arrange(grobs = plots_macro, nrow = 1,
             top = textGrob("Estructuras Base de Macrogrupos", gp = gpar(fontsize = 14)))

# =============================================================================
# 6. SUBTIPOS DE ESTRUCTURAS EXTENDIDAS
# =============================================================================

# Ejemplos de subcategorías de estructuras Extendidas
subcats_extendida <- list(
  "Tradicional Nuclear Extended"      = "T43",
  "Single-Parent Extended" = "T41",
  "Extended Couple"       = "T30",
  "Reconstituted"          = "T14",
  "Complex"               = "T36"
)

plots_ext <- list()
for (nm in names(subcats_extendida)) {
  t_code <- subcats_extendida[[nm]]
  img_path <- paste0(path_graficos, t_code, ".png")
  
  if (file.exists(img_path)) {
    img <- readPNG(img_path)
    g <- rasterGrob(img, interpolate = TRUE)
    plots_ext[[nm]] <- arrangeGrob(g, bottom = textGrob(paste0(nm, "\n(", t_code, ")"),
                                                        gp = gpar(fontsize = 9, fontface = "bold")))
  } else {
    plots_ext[[nm]] <- textGrob(paste("Falta:", t_code), gp = gpar(col = "red"))
  }
}

grid.arrange(grobs = plots_ext, ncol = 3,
             top = textGrob("Mosaico: Subcategorías Extendidas", gp = gpar(fontsize = 14)))

# =============================================================================
# 7. GUARDAR DATASET FINAL
# =============================================================================
save(hogares, file = "Análisis de viviendas/Data/hogares_con_macrogrupo.RData")
