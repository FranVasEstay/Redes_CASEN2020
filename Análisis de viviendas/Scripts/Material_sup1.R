## Material suplementario: Descripciones por tipología

################################################################################
# descriptives_tipologias.R
# 
# Flujo:
#   0. Librerías
#   1. Carga datos base (Data_analisis, medidas_redes, households_por_tipologia, rural CSV)
#   2. Prepara measurements con % rural
#   3. Crea tipologia_larga y data_con_tipologia (a nivel persona)
#   4. Calcula variables agregadas por hogar (porc_chi, porc_ind) y las añade a data_con_tipologia
#   5. Construye tabla descriptiva de las 48 tipologías (Script 4)
#   6. Prepara variables categóricas (rural_cat, sueldo_cat, macrozona) sobre data_con_tipologia
#   7. Realiza chi-cuadrado, heatmap de significancia y mosaic plots (Script 5)
################################################################################

# 0. Librerías
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(vcd)           # mosaic
library(rstatix)       # cramer_v
library(scales)
library(stringr)

# ============================================================================
# 1. CARGA DE DATOS BASE
# ============================================================================

# Datos individuales con atributos sociodemográficos
load("Análisis de viviendas/Data/Data_analisis.RData")          # data_analisis

# Datos de hogares (measurements) – ya calculados previamente
load("Análisis de viviendas/Descriptives/medidas_redes.RData")  # measurements

# Hogares que pertenecen a cada tipología (solo las 964 originales, luego filtramos)
households_por_tipologia <- readRDS("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds")

# Lista de las 48 tipologías dominantes
muestra_tipologias <- read.csv("Análisis de viviendas/Data/muestra_tipologias.csv",
                               sep = "\t", stringsAsFactors = FALSE)
tipologias_validas <- unique(muestra_tipologias$Tipologia)  # T1..T48

# Datos de ruralidad por comuna
porc_rural_comuna <- read.csv("Análisis de viviendas/Data/porc_rural_comuna.csv",
                              sep = ",", encoding = "UTF-8")

# ============================================================================
# 2. PREPARACIÓN DE measurements CON % RURAL POR COMUNA
# ============================================================================

# Limpiar nombre de comuna en el CSV de ruralidad
porc_rural_comuna <- porc_rural_comuna %>%
  mutate(
    comuna_clean = str_remove(Unidad.territorial, "\\s*\\(.*\\)"),
    comuna_clean = str_trim(comuna_clean),
    comuna_clean = iconv(comuna_clean, from = "UTF-8", to = "ASCII//TRANSLIT"),
    comuna_clean = str_to_title(comuna_clean)
  ) %>%
  select(comuna = comuna_clean, porc_rural = X2020)

# Limpiar comuna en measurements
measurements <- measurements %>%
  mutate(
    comuna_clean = iconv(comuna, from = "UTF-8", to = "ASCII//TRANSLIT"),
    comuna_clean = str_to_title(comuna_clean)
  )

# Unir ruralidad
measurements <- measurements %>%
  left_join(porc_rural_comuna, by = c("comuna_clean" = "comuna")) %>%
  select(-comuna_clean)

# Verificar comunas sin match
sin_match <- measurements %>% filter(is.na(porc_rural)) %>% distinct(comuna)
if (nrow(sin_match) > 0) {
  message("Las siguientes comunas no encontraron match:")
  print(sin_match)
} else {
  message("Todas las comunas encontraron match exitosamente")
}

# ============================================================================
# 3. CREAR data_con_tipologia (nivel persona)
# ============================================================================

# Expandir households por tipología
tipologia_larga <- households_por_tipologia %>%
  select(Tipologia, Households) %>%
  mutate(Households = str_split(Households, ";")) %>%
  unnest(Households) %>%
  rename(household = Households, tipologia = Tipologia) %>%
  mutate(household = as.numeric(household))

# Filtrar solo las 48 tipologías dominantes
tipologia_larga <- tipologia_larga %>%
  filter(tipologia %in% tipologias_validas)

# Unir con medidas de hogar (measurements) -> esto da datos a nivel hogar
# pero necesitamos también los datos individuales para los descriptivos.
# Primero creamos un data.frame a nivel hogar con las variables necesarias.
hogares_con_tipologia <- measurements %>%
  inner_join(tipologia_larga, by = "household")

# Ahora unimos los datos individuales (data_analisis) para poder calcular
# estadísticas como edad, sexo, etc. a nivel persona.
data_con_tipologia <- data_analisis %>%
  inner_join(hogares_con_tipologia %>% select(household, tipologia, porc_rural),
             by = "household")

# ============================================================================
# 4. INDICADORES DE PRESENCIA DE EXTRANJEROS E INDÍGENAS POR HOGAR
# ============================================================================

# Datos a nivel hogar desde measurements (una fila por hogar)
# measurements contiene variables por hogar, incluyendo:
# household, porc_hombre, porc_ind, tasa_desocupacion, tasa_inactividad,
# porc_enfermos, tiene_extranjero, sueldo, edad_prom, edad_sd, region,
# comuna, tipologia_dem08, tipo_gen, quintil_ingreso, pobreza_hogar,
# max_escolaridad, nivel_educ_jefe, hacinamiento_cat, allegamiento_interno,
# allegamiento_externo, porc_rural (ya unida)

# Para presencia de extranjeros e indígenas usamos la variable por hogar
# 'tiene_extranjero' ya existe en measurements como booleano.
# Para indígenas, calculamos la presencia a partir de los datos individuales
# (o podemos usar la ya existente 'porc_ind' si queremos presencia al menos uno).
# Vamos a calcular presencia indígena desde data_analisis.

presencia_indigena <- data_analisis %>%
  group_by(household) %>%
  summarise(tiene_indigena = any(r3 == "Pertenece", na.rm = TRUE), .groups = "drop")

# Unir a measurements (que ya tiene tiene_extranjero)
measurements <- measurements %>%
  left_join(presencia_indigena, by = "household")

# Ahora creamos hogares_con_tipologia (sin duplicados por household)
hogares_con_tipologia <- measurements %>%
  inner_join(tipologia_larga, by = "household")

# Agregamos las variables de presencia como indicadores de hogar
# y las dejamos listas para la tabla descriptiva

# ============================================================================
# 5. TABLA DESCRIPTIVA POR TIPOLOGÍA (con más índices de measurements)
# ============================================================================

# 5.1 Edad y sexo del jefe/a (se necesita data individual -> data_con_tipologia)
# Primero construimos data_con_tipologia (nivel persona) para los cálculos que requieren edad/sexo individual.
data_con_tipologia <- data_analisis %>%
  inner_join(hogares_con_tipologia %>% select(household, tipologia, porc_rural,
                                              quintil_ingreso, tiene_extranjero, tiene_indigena,
                                              tipo_gen, pobreza_hogar, hacinamiento_cat,
                                              max_escolaridad, nivel_educ_jefe,
                                              allegamiento_interno, allegamiento_externo),
             by = "household")

# Ya podemos calcular estadísticas de jefes/as
jefe_stats <- data_con_tipologia %>%
  filter(pco1 == "Jefe(a) de Hogar") %>%
  group_by(tipologia) %>%
  summarise(
    Edad_promedio_jefe = round(mean(edad, na.rm = TRUE), 1),
    Desviacion_edad_jefe = round(sd(edad, na.rm = TRUE), 1),
    "%_mujer_jefe" = round(mean(sex == "Mujer", na.rm = TRUE) * 100, 1),
    "%_hombre_jefe" = round(mean(sex == "Hombre", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# 5.2 Rural/Urbano y macrozonas
data_con_tipologia <- data_con_tipologia %>%
  mutate(
    area = ifelse(porc_rural > 50, "Rural", "Urbano"),
    macrozona = case_when(
      region %in% c("Región de Arica y Parinacota", "Región de Tarapacá",
                    "Región de Antofagasta", "Región de Atacama",
                    "Región de Coquimbo") ~ "Norte",
      region %in% c("Región de Valparaíso", "Región Metropolitana de Santiago",
                    "Región del Libertador Gral. Bernardo O'Higgins",
                    "Región del Maule", "Región de Ñuble") ~ "Centro",
      TRUE ~ "Sur"
    )
  )

ubicacion_stats <- data_con_tipologia %>%
  group_by(tipologia) %>%
  summarise(
    "%_Urbano" = round(mean(area == "Urbano", na.rm = TRUE) * 100, 1),
    "%_Rural" = round(mean(area == "Rural", na.rm = TRUE) * 100, 1),
    "%_Norte" = round(mean(macrozona == "Norte", na.rm = TRUE) * 100, 1),
    "%_Centro" = round(mean(macrozona == "Centro", na.rm = TRUE) * 100, 1),
    "%_Sur" = round(mean(macrozona == "Sur", na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# 5.3 Presencia de extranjeros e indígenas (porcentaje de hogares con al menos uno)
origen_stats <- data_con_tipologia %>%
  distinct(household, tipologia, tiene_extranjero, tiene_indigena) %>%
  group_by(tipologia) %>%
  summarise(
    "%_Hogares_con_extranjero" = round(mean(tiene_extranjero, na.rm = TRUE) * 100, 1),
    "%_Hogares_con_indigena" = round(mean(tiene_indigena, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# 5.4 Ingresos (mediana y cuartiles)
ingreso_stats <- data_con_tipologia %>%
  group_by(tipologia, household) %>%
  summarise(ingreso_promedio = mean(ytotcor, na.rm = TRUE), .groups = "drop") %>%
  group_by(tipologia) %>%
  summarise(
    Ingreso_mediano = median(ingreso_promedio, na.rm = TRUE),
    Q1 = quantile(ingreso_promedio, 0.25, na.rm = TRUE),
    Q3 = quantile(ingreso_promedio, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# 5.5 Otros índices de measurements a nivel de hogar
# Tomamos las variables por hogar y calculamos promedios/porcentajes por tipología
otros_stats_hogar <- hogares_con_tipologia %>%
  group_by(tipologia) %>%
  summarise(
    # Demográficos
    "%_Hombres_promedio" = round(mean(porc_hombre, na.rm = TRUE), 1),
    "Edad_promedio_hogar" = round(mean(edad_prom, na.rm = TRUE), 1),
    "Desviacion_edad_hogar" = round(mean(edad_sd, na.rm = TRUE), 1),
    # Empleo
    "Tasa_desocupacion_promedio" = round(mean(tasa_desocupacion, na.rm = TRUE), 3),
    "Tasa_inactividad_promedio" = round(mean(tasa_inactividad, na.rm = TRUE), 3),
    # Salud
    "%_Enfermos_promedio" = round(mean(porc_enfermos, na.rm = TRUE), 1),
    # Educación
    "Max_escolaridad_promedio" = round(mean(max_escolaridad, na.rm = TRUE), 1),
    # Vivienda: porcentajes de categorías (hacinamiento, pobreza, tipo generacional)
    "%_Hacinamiento_sin" = round(mean(hacinamiento_cat == "Sin hacinamiento", na.rm = TRUE) * 100, 1),
    "%_Hacinamiento_medio" = round(mean(hacinamiento_cat == "Hacinamiento medio", na.rm = TRUE) * 100, 1),
    "%_Hacinamiento_alto" = round(mean(hacinamiento_cat == "Hacinamiento alto", na.rm = TRUE) * 100, 1),
    "%_Hacinamiento_critico" = round(mean(hacinamiento_cat == "Hacinamiento crítico", na.rm = TRUE) * 100, 1),
    "%_Pobreza_extrema" = round(mean(pobreza_hogar == "Pobre extremo", na.rm = TRUE) * 100, 1),
    "%_Pobreza_no_extremo" = round(mean(pobreza_hogar == "Pobre no extremo", na.rm = TRUE) * 100, 1),
    "%_No_pobre" = round(mean(pobreza_hogar == "No pobre", na.rm = TRUE) * 100, 1),
    # Tipo generacional (resumido como porcentaje de la categoría más frecuente o todas)
    "%_Multigeneracional" = round(mean(tipo_gen == "Multigeneracional", na.rm = TRUE) * 100, 1),
    "%_Sin_generacion_intermedia" = round(mean(tipo_gen == "Sin generación intermedia", na.rm = TRUE) * 100, 1),
    "%_Sin_adultos_mayores" = round(mean(tipo_gen == "Sin adultos mayores", na.rm = TRUE) * 100, 1),
    "%_Sin_menores_15" = round(mean(tipo_gen == "Sin menores de 15", na.rm = TRUE) * 100, 1),
    "%_Solo_menores_15" = round(mean(tipo_gen == "Solo menores de 15", na.rm = TRUE) * 100, 1),
    "%_Solo_15_64" = round(mean(tipo_gen == "Solo 15-64", na.rm = TRUE) * 100, 1),
    "%_Solo_mayores_64" = round(mean(tipo_gen == "Solo mayores de 64", na.rm = TRUE) * 100, 1),
    # Allegamiento
    "%_Allegamiento_interno" = round(mean(allegamiento_interno, na.rm = TRUE) * 100, 1),
    "%_Allegamiento_externo" = round(mean(allegamiento_externo, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

# 5.6 Tabla final combinada
tabla_final <- jefe_stats %>%
  left_join(ubicacion_stats, by = "tipologia") %>%
  left_join(origen_stats, by = "tipologia") %>%
  left_join(ingreso_stats, by = "tipologia") %>%
  left_join(otros_stats_hogar, by = "tipologia") %>%
  mutate(tipologia = factor(tipologia, levels = paste0("T", 1:48), ordered = TRUE)) %>%
  arrange(tipologia)

write.csv(tabla_final,
          "Análisis de viviendas/Analisis/descriptives_tipologias.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# Identificar extremos (seleccionando solo columnas numéricas)
analisis_extremos <- tabla_final %>%
  select(tipologia, where(is.numeric)) %>%
  pivot_longer(-tipologia, names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    max_tipologia = tipologia[which.max(valor)],
    max_valor = max(valor, na.rm = TRUE),
    min_tipologia = tipologia[which.min(valor)],
    min_valor = min(valor, na.rm = TRUE),
    .groups = "drop"
  )
Publish::publish(analisis_extremos)

# ============================================================================
# 6. PREPARACIÓN DE VARIABLES CATEGÓRICAS PARA CHI-CUADRADO
# ============================================================================

# Usamos el quintil de ingreso de measurements (quintil_ingreso) y rural_cat
# Aseguramos factor
data_con_tipologia <- data_con_tipologia %>%
  mutate(
    rural_cat = cut(porc_rural,
                    breaks = c(0, 30, 70, 100),
                    labels = c("Urbano", "Mixto", "Rural"),
                    include.lowest = TRUE)
  )

# Ordenar
data_con_tipologia <- data_con_tipologia %>% arrange(tipologia)

# (Opcional) Guardar esta versión final
save(data_con_tipologia, file = "Análisis de viviendas/Data/Data_con_tipología.RData")

# ============================================================================
# 7. CHI-CUADRADO, HEATMAP Y MOSAIC PLOTS
# ============================================================================

variables_categoricas <- c("region", "rural_cat", "qaut")

chi_detallado <- function(tipologia_sel, variable_cat) {
  datos <- data_con_tipologia %>%
    mutate(grupo = tipologia == tipologia_sel)
  
  tabla <- table(datos[[variable_cat]], datos$grupo)
  prueba <- suppressWarnings(chisq.test(tabla))
  v_cramer <- suppressWarnings(cramer_v(tabla, correct = FALSE))
  
  tibble(
    Tipologia = tipologia_sel,
    Variable = variable_cat,
    Chi2 = as.numeric(prueba$statistic),
    df = as.integer(prueba$parameter),
    p_value = as.numeric(prueba$p.value),
    Min_esperado = min(prueba$expected),
    Cramer_V = v_cramer
  )
}

resultados_chi <- expand_grid(
  tipologia = tipologias_validas,
  variable = variables_categoricas
) %>%
  pmap_dfr(chi_detallado) %>%
  mutate(across(c(Chi2, p_value, Cramer_V, Min_esperado), ~round(.x, 4)))

write.csv(resultados_chi,
          "Análisis de viviendas/Analisis/resultados_chi_tipologias.csv",
          row.names = FALSE)

# Heatmap de significancia
resultados_chi %>%
  mutate(Significativo = p_value < 0.05) %>%
  ggplot(aes(x = Tipologia, y = Variable, fill = Significativo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(Chi2, 2))),
            angle = 90, vjust = 0.5, hjust = 0.5, size = 2.5, color = "black") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray90")) +
  labs(title = "Significancia estadística (Chi²) por tipología y variable",
       x = "Tipología", y = "Variable", fill = "p < 0.05") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())
ggsave("Análisis de viviendas/Analisis/heatmap_significancia.png",
       width = 10, height = 6, dpi = 300)

# Mosaic plots
dir.create("Análisis de viviendas/Analisis/mosaic_plots_por_tipologia", showWarnings = FALSE)

generar_mosaic <- function(tipologia_sel, variable_cat) {
  datos <- data_con_tipologia %>%
    mutate(grupo = ifelse(tipologia == tipologia_sel, tipologia_sel, "Otras")) %>%
    filter(!is.na(.data[[variable_cat]]))
  
  fml <- as.formula(paste("~", variable_cat, "+ grupo"))
  archivo <- paste0("Análisis de viviendas/Analisis/mosaic_plots_por_tipologia/",
                    variable_cat, "_", gsub("[^[:alnum:]]", "_", tipologia_sel), ".png")
  
  png(archivo, width = 1000, height = 800)
  mosaic(fml, data = datos,
         shade = TRUE,
         direction = c("v", "h"),
         main = paste("Mosaic plot:", variable_cat, "vs", tipologia_sel),
         labeling_args = list(
           set_varnames = c("Categoría", "Grupo"),
           rot_labels = c(30, 0, 0, 0)
         ),
         margins = c(5, 5, 10, 5))
  dev.off()
}

combinaciones <- expand_grid(
  tipologia = tipologias_validas,
  variable = variables_categoricas
)
pmap(combinaciones, generar_mosaic)

cat("Script unificado completado con éxito.\n")
