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
hogar_info <- data_con_tipologia %>%
  distinct(household, tipologia, porc_rural, rural_cat, area, macrozona)

# Construir dataset principal: measurements + tipología + variables geográficas + macrogrupo
hogares <- measurements %>%
  inner_join(hogar_info, by = "household") %>%
  inner_join(macrogrupos, by = c("tipologia" = "Tipologia")) %>%
  mutate(
    macrogrupo = factor(macrogrupo,
                        levels = c("Isolated","Chidless Couple","Tradicional Nuclear","Single-Parent","Extended"))
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

# Limpiar etiquetas de pobreza: extraer solo la parte relevante
# (o simplemente usar las cadenas exactas si ya coinciden)
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
    )
  )
cat("Hogares en measurements:", nrow(measurements), "\n")
cat("Hogares después de joins:", nrow(hogares), "\n")

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
    porc_hogares_masculinos = mean(porc_hombre > 50, na.rm = TRUE) * 100,
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

# 3.1 Distribución de hogares por macrogrupo
p_freq <- hogares %>%
  count(macrogrupo) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ggplot(aes(x = fct_reorder(macrogrupo, n), y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Distribución de hogares por Macrogrupo", x = "", y = "Número de hogares") +
  theme_minimal()

# 3.2 Ingreso mediano
p_income <- ggplot(tabla_macro, aes(x = fct_reorder(macrogrupo, ingreso_mediano),
                                    y = ingreso_mediano)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = scales::dollar(ingreso_mediano)), hjust = -0.1) +
  coord_flip() +
  labs(title = "Ingreso mediano por Macrogrupo", x = "", y = "Ingreso mediano (CLP)") +
  theme_minimal()

# 3.3 Ruralidad
p_rural <- ggplot(tabla_macro, aes(x = fct_reorder(macrogrupo, porc_rural),
                                   y = porc_rural)) +
  geom_col(fill = "orange") +
  geom_text(aes(label = paste0(round(porc_rural, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Hogares en área rural (>70% rural) por Macrogrupo", x = "", y = "% de hogares") +
  theme_minimal()

# 3.4 Mayoría masculina en el hogar
p_genero <- ggplot(tabla_macro, aes(x = fct_reorder(macrogrupo, porc_hogares_masculinos),
                                    y = porc_hogares_masculinos)) +
  geom_col(fill = "purple") +
  geom_text(aes(label = paste0(round(porc_hogares_masculinos, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Hogares con mayoría masculina por Macrogrupo", x = "", y = "% de hogares") +
  theme_minimal()

# Combinar y guardar
(p_freq + p_income) / (p_rural + p_genero)
ggsave("Análisis de viviendas/Analisis/graficos_macrogrupos.png",
       width = 14, height = 10, dpi = 150)

# =============================================================================
# 4. GRÁFICOS ADICIONALES (Presencia indígena, extranjera, salud, zona)
# =============================================================================

# 4.1 Presencia indígena
p_ind <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(porc = mean(tiene_indigena, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = fct_reorder(macrogrupo, porc), y = porc)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = paste0(round(porc, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Hogares con presencia indígena", x = "", y = "%") +
  theme_minimal()

# 4.2 Presencia extranjera
p_ext <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(porc = mean(tiene_extranjero, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = fct_reorder(macrogrupo, porc), y = porc)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(round(porc, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Hogares con presencia extranjera", x = "", y = "%") +
  theme_minimal()

# 4.3 Enfermedad crónica (% promedio de miembros con enfermedad)
p_salud <- hogares %>%
  group_by(macrogrupo) %>%
  summarise(porc = mean(porc_enfermos, na.rm = TRUE)) %>%
  ggplot(aes(x = fct_reorder(macrogrupo, porc), y = porc)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = paste0(round(porc, 1), "%")), hjust = -0.1) +
  coord_flip() +
  labs(title = "Promedio de miembros con enfermedad crónica", x = "", y = "%") +
  theme_minimal()

# 4.4 Zona geográfica (porcentaje de hogares en cada macrozona)
hogares_zona <- hogares %>%
  count(macrogrupo, macrozona) %>%
  group_by(macrogrupo) %>%
  mutate(porc = n / sum(n) * 100)

p_zona <- ggplot(hogares_zona, aes(x = macrogrupo, y = porc, fill = macrozona)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = "Distribución según macrozona", x = "", y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combinar en una figura múltiple
(p_ind + p_ext) / (p_salud + p_zona)
ggsave("Análisis de viviendas/Analisis/graficos_extra_macrogrupos.png",
       width = 12, height = 8, dpi = 150)

# Guardar objetos individuales como RDS
saveRDS(p_ind, "Análisis de viviendas/Analisis/Resultados_macrogrupos/presencia_indigenas.rds")
saveRDS(p_ext, "Análisis de viviendas/Analisis/Resultados_macrogrupos/presencia_extranjeros.rds")
saveRDS(p_salud, "Análisis de viviendas/Analisis/Resultados_macrogrupos/presencia_enfermos.rds")
saveRDS(p_zona, "Análisis de viviendas/Analisis/Resultados_macrogrupos/zonas_macrogrupos.rds")
saveRDS(p_rural, "Análisis de viviendas/Analisis/Resultados_macrogrupos/ruralidad_macrogrupos.rds")

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
  "Nuclear Extendida"      = "T43",
  "Monoparental Extendida" = "T41",
  "Pareja Extendida"       = "T30",
  "Reconstituida"          = "T14",
  "Compleja"               = "T36"
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
