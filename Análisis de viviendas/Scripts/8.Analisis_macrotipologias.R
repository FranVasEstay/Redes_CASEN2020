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
    area = ifelse(porc_rural > 50, "Rural", "Urbano"),
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
    # Reducir hacinamiento a una categoría corta
    hacinamiento_cat = case_when(
      grepl("Sin hacinamiento", hacinamiento_cat) ~ "Sin hacinamiento",
      grepl("Hacinamiento medio", hacinamiento_cat) ~ "Hacinamiento medio",
      grepl("Hacinamiento alto", hacinamiento_cat) ~ "Hacinamiento alto",
      grepl("Hacinamiento crítico", hacinamiento_cat) ~ "Hacinamiento crítico",
      TRUE ~ NA_character_
    )%>%
    mutate(jefe_mujer = ifelse(sexo_jefe == "Mujer", 1, 0))
  )

# =============================================================================
# 2. TABLA DESCRIPTIVA COMPLETA POR MACROGRUPO
# =============================================================================
tabla_macro <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n(),
    porcentaje = round(n_hogares / total_hogares * 100, 1),  # <-- corregido
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
# 2. INGRESO MEDIANO – GRÁFICO DE LINES Y PUNTOS
# =============================================================================
p_income<- hogares %>%
  group_by(macrogrupo, quant) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100) %>%
  ggplot(aes(x = macrogrupo, y = porc, fill = quant)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Quintil") +
  labs(title = "Distribución de quintiles de ingreso por Macrogrupo",
       x = "", y = "Proporción de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =============================================================================
# 3. RURALIDAD
# =============================================================================
p_rural <- tabla_macro %>%
  ggplot(aes(x = macrogrupo, y = area, color = macrogrupo)) +
  geom_segment(aes(xend = macrogrupo, yend = 0), size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(nrow(area=="Rural"), 1), "%")),
            vjust = -0.8, size = 3.5, color = "black") +
  scale_color_viridis_d(option = "plasma", end = 0.8) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Hogares en área rural (>70% rural)",
       x = "", y = "% de hogares rurales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

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
# 5. COMPOSICIÓN: PRESENCIA INDÍGENA, EXTRANJERA Y SALUD (dot‑plot conjunto)
# =============================================================================
# Preparamos datos largos para mostrar múltiples indicadores en un mismo gráfico
indicadores <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(
    Indígena = mean(tiene_indigena, na.rm = TRUE) * 100,
    Extranjero = mean(tiene_extranjero, na.rm = TRUE) * 100,
    `Enfermedad crónica` = mean(porc_enfermos, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -macrogrupo, names_to = "indicador", values_to = "porcentaje")

p_indicadores <- ggplot(indicadores, aes(x = porcentaje, y = macrogrupo, color = indicador)) +
  geom_line(aes(group = macrogrupo), color = "gray80", size = 0.5) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = paste0(round(porcentaje, 1), "%")), 
                  size = 3, box.padding = 0.3) +
  scale_x_continuous(labels = percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Presencia de indicadores sociodemográficos",
       x = "% de hogares (o promedio para enfermedad crónica)", y = "",
       color = "Indicador") +
  theme_minimal() +
  theme(legend.position = "bottom")

# =============================================================================
# 6. ZONA GEOGRÁFICA – GRÁFICO DE BARRAS APILADAS
# =============================================================================
hogares_zona <- hogares %>%
  count(macrogrupo, macrozona) %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100)

p_zona <- ggplot(hogares_zona, aes(x = macrogrupo, y = porc, fill = macrozona)) +
  geom_col(position = "fill", width = 0.7) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Blues", name = "Macrozona") +
  labs(title = "Distribución geográfica por Macrogrupo",
       x = "", y = "Proporción de hogares") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# =============================================================================
# 7. COMPOSICIÓN FINAL Y GUARDADO
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
