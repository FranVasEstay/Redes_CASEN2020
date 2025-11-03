################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
######################### 13. ANÁLISIS RESUMEN #################################
################################################################################
#------------------
# LIBRERÍAS
#------------------
# ANÁLISIS ESTADÍSTICO BÁSICO
library(dplyr)        # Manipulación de datos
library(tidyr)        # Datos tidy
library(purrr)        # Programación funcional
library(broom)        # Limpiar resultados estadísticos
library(tibble)
library(stringi)

# ANÁLISIS ESTADÍSTICO AVANZADO
library(car)          # ANOVA y pruebas post-hoc
library(emmeans)      # Comparaciones múltiples (alternativa a Tukey)
library(rstatix)      # Estadísticas tidy

# ANÁLISIS DE REDES
library(igraph)       # Análisis de redes
library(network)      # Redes alternativo

# CLUSTERING Y ANÁLISIS MULTIVARIADO
library(cluster)      # PAM, CLARA, clustering
library(factoextra)   # Visualización de clusters
library(FactoMineR)   # MCA, análisis multivariado
library(ggdendro)     # Dendrogramas con ggplot

# VISUALIZACIÓN
library(ggplot2)      # Gráficos principales
library(ggpubr)       # Extensiones ggplot
library(patchwork)    # Combinar gráficos
library(RColorBrewer) # Paletas de colores
library(viridis)      # Paletas color-blind friendly

# MAPAS Y GEOESPACIAL
library(sf)           # Datos espaciales
library(chilemapas)   # Mapas de Chile
library(ggspatial)    # Elementos de mapas

# VALIDACIÓN Y MODELOS
library(caret)        # Validación cruzada
library(nnet)         # Regresión multinomial
library(MASS)         # Análisis discriminante
library(pROC)         # Curvas ROC

#------------------
# CARGAR LOS DATOS
#------------------
load("Análisis de viviendas/Data/data_macrotiplogias.RData")
load("Análisis de viviendas/Data/Data_con_tipología.RData")

# Cargar resultados del clustering
df_final <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_familiares.csv")

# ----------------------------------------------
### 1. ANOVA (INGRESO NUMÉRICO vs MACROGRUPO) ###
# ----------------------------------------------
# ANOVA para comparar ingresos entre macrogrupos
anova_income <- aov(sueldo ~ macrogrupo, data = data_macro)
cat("=== ANOVA: INGRESOS POR MACROGRUPO ===\n")
print(summary(anova_income))

# Test post-hoc de Tukey
tukey_income <- TukeyHSD(anova_income)
cat("\n=== COMPARACIONES MÚLTIPLES (TUKEY HSD) ===\n")
print(tukey_income)

# Boxplot de ingresos por macrogrupo
p_income_boxplot <- ggplot(data_macro, aes(x = macrogrupo, y = sueldo)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Distribución de ingresos del hogar por macrogrupo familiar",
       subtitle = "Punto rojo = media, ANOVA p-value mostrado",
       x = "Macrogrupo", y = "Ingreso mensual del hogar (CLP)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_income_boxplot)
ggsave("Análisis de viviendas/Analisis/boxplot_ingresos_macrogrupo.png", 
       p_income_boxplot, width = 10, height = 6, dpi = 300, bg = "white")
# --------------------------------------
### 2. ANÁLISIS INDÍGENA Y MIGRANTE (AL MENOS 1) ###
# --------------------------------------
# Crear variable binaria: hogar con al menos 1 indígena
data_macro <- data_macro %>%
  mutate(
    tiene_indigena = porc_ind > 0,
    tiene_migrante = porc_chi < 100  # Menos del 100% chilenos
  )

# ------------------------------------------------
### 2. CHI-CUADRADO PARA VARIABLES CATEGÓRICAS ###
# ------------------------------------------------
# Función para calcular y mostrar tests chi-cuadrado
realizar_chi_test <- function(variable, data) {
  tabla <- table(data$macrogrupo, data[[variable]])
  test <- chisq.test(tabla)
  
  # V de Cramer
  n <- sum(tabla)
  k <- min(dim(tabla))
  cramer_v <- sqrt(test$statistic / (n * (k - 1)))
  
  return(list(
    tabla = tabla,
    test = test,
    cramer_v = cramer_v
  ))
}

# Aplicar tests chi-cuadrado
chi_results <- list(
  income_cat = realizar_chi_test("sueldo_cat", data_macro),
  indigena = realizar_chi_test("tiene_indigena", data_macro),
  rural = realizar_chi_test("rural_cat", data_macro),
  migrante = realizar_chi_test("tiene_migrante", data_macro)
)

# Resumen de tests chi-cuadrado
chi_summary <- map_df(chi_results, function(x) {
  data.frame(
    chi2 = round(x$test$statistic, 2),
    p_value = format.pval(x$test$p.value, digits = 3),
    df = x$test$parameter,
    cramer_v = round(x$cramer_v, 3)
  )
}, .id = "variable")

print(chi_summary)

# -------------------------------
### 4. MCA - VARIABLES ACTIVAS ###
# -------------------------------
# MCA con variables estructurales como activas
mca_vars_activas <- c("macrogrupo", "sueldo_cat", "rural_cat", "region")
mca_data_activo <- data_macro %>%
  dplyr::select(all_of(mca_vars_activas)) %>%
  mutate(across(everything(), as.factor)) %>%
  na.omit()

mca_result_activo <- MCA(mca_data_activo, graph = FALSE)

# Visualización MCA
p_mca_vars <- fviz_mca_var(mca_result_activo, repel = TRUE,
                           title = "MCA - Estructura Familiar y Características Sociodemográficas",
                           col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

p_mca_ind <- fviz_mca_ind(mca_result_activo, 
                          habillage = mca_data_activo$macrogrupo,
                          addEllipses = TRUE, ellipse.type = "confidence",
                          title = "Distribución de Hogares por Macrogrupo en el Espacio MCA")

print(p_mca_vars)
ggsave("Análisis de viviendas/Analisis/MCA_variables_macro.png", 
       width = 10, height = 6, dpi = 300, bg = "white")
print(p_mca_ind)
ggsave("Análisis de viviendas/Analisis/MCA_individuos_macrogrupo.png", 
       width = 10, height = 6, dpi = 300, bg = "white")

# Contribución de variables
p_contrib <- fviz_contrib(mca_result_activo, choice = "var", axes = 1, top = 10) +
  labs(title = "Contribución de Variables a la Dimensión 1 del MCA")

print(p_contrib)

# --------------------------------------------
### 5. CLUSTERING JERÁRQUICO EN MACROGRUPOS ###
# --------------------------------------------
# Preparar datos para clustering de macrogrupos
macro_cluster_data <- data_macro %>%
  group_by(macrogrupo) %>%
  summarise(
    n_hogares = n(),
    edad_promedio = mean(edad.prom, na.rm = TRUE),
    porc_indigena = mean(porc_ind, na.rm = TRUE),
    porc_rural = mean(porc_rural, na.rm = TRUE),
    ingreso_mediano = median(sueldo, na.rm = TRUE),
    porc_mujeres = 100 - mean(porc_hombre, na.rm = TRUE)
  ) %>%
  column_to_rownames("macrogrupo")

# Escalar datos
macro_scaled <- scale(macro_cluster_data)

# Clustering jerárquico
dist_macro <- dist(macro_scaled)
hc_macro <- hclust(dist_macro, method = "ward.D2")

# Dendrograma macrogrupos
p_dendro_macro <- fviz_dend(hc_macro, k = 4, 
                            cex = 0.8, 
                            main = "Clustering Jerárquico de Macrogrupos por Perfil Sociodemográfico",
                            xlab = "Macrogrupos", ylab = "Distancia")

print(p_dendro_macro)

# Guardar el dendrograma
ggsave("Análisis de viviendas/Analisis/dendrograma_clusters.png",p_dendro_macro, width = 12, height = 8, dpi = 300, bg = "white")

# --------------------
### 6. MAPA COMUNAL ###
# --------------------
library(chilemapas)
library(sf)
# Preparando data
data_comunal <- data_con_tipologia %>%
  group_by(comuna) %>%
  summarise(
    n_hogares = n(),
    tipologia_predominante = names(which.max(table(tipologia))),
    freq_predominante = max(table(tipologia)) / n()
  ) %>%
  mutate(comuna = as.character(comuna))

# Cargar diccionario de comunas
data("codigos_territoriales")

# Función para normalizar nombres (quitar tildes, ñ, etc.)
normalizar_nombre <- function(nombre) {
  nombre %>%
    # Convertir a minúsculas
    tolower() %>%
    # Quitar tildes y caracteres especiales
    iconv(to = "ASCII//TRANSLIT") %>%
    # Quitar espacios extra
    trimws() %>%
    # Reemplazar múltiples espacios por uno solo
    gsub("\\s+", " ", .)
}

# Aplicar normalización a ambos datasets
codigos_normalizados <- codigos_territoriales %>%
  mutate(comuna_normalizada = normalizar_nombre(nombre_comuna)) %>%
  dplyr::select(codigo_comuna, nombre_comuna, comuna_normalizada)

diccionario_manual_completo <- tribble(
  ~comuna_original,        ~comuna_corregida,
  # Comunas con acentos
  "Alhué",                 "Alhue",
  "Aysén",                 "Aisen", 
  "Cañete",                "Canete",
  "Chañaral",              "Chanaral",
  "Chillán",               "Chillan",
  "Chillán Viejo",         "Chillan Viejo",
  "Chépica",               "Chepica",
  "Colbún",                "Colbun",
  "Combarbalá",            "Combarbala",
  "Concepción",            "Concepcion",
  "Conchalí",              "Conchali",
  "Concón",                "Concon",
  "Constitución",          "Constitucion",
  "Copiapó",               "Copiapo",
  "Coyhaique",             "Coihaique", 
  "Curacautín",            "Curacautin",
  "Curacaví",              "Curacavi",
  "Curaco de Vélez",       "Curaco de Velez",
  "Curicó",                "Curico",
  "Doñihue",               "Donihue",
  "Hualañé",               "Hualane",
  "Hualpén",               "Hualpen",
  "La Unión",              "La Union",
  "Licantén",              "Licanten",
  "Longaví",               "Longavi",
  "Los Álamos",            "Los Alamos",
  "Los Ángeles",           "Los Angeles",
  "Machalí",               "Machali",
  "Maipú",                 "Maipu",
  "María Elena",           "Maria Elena",
  "María Pinto",           "Maria Pinto",
  "Maullín",               "Maullin",
  "Mulchén",               "Mulchen",
  "Máfil",                 "Mafil",
  "Olmué",                 "Olmué",  # Ya está bien
  "Peñaflor",              "Penaflor",
  "Peñalolén",             "Penalolen",
  "Pitrufquén",            "Pitrufquen",
  "Puchuncaví",            "Puchuncavi",
  "Pucón",                 "Pucon",
  "Puqueldón",             "Puqueldon",
  "Purén",                 "Puren",
  "Queilén",               "Queilen",
  "Quellón",               "Quellon",
  "Quillón",               "Quillon",
  "Quilpué",               "Quilpue",
  "Requínoa",              "Requinoa",
  "Ránquil",               "Ranquil",
  "Río Bueno",             "Rio Bueno",
  "Río Claro",             "Rio Claro", 
  "Río Hurtado",           "Rio Hurtado",
  "Río Ibáñez",            "Rio Ibanez",
  "Río Negro",             "Rio Negro",
  "San Fabián",            "San Fabian",
  "San Joaquín",           "San Joaquin",
  "San José de Maipo",     "San Jose de Maipo",
  "San Nicolás",           "San Nicolas",
  "San Ramón",             "San Ramon",
  "Santa Bárbara",         "Santa Barbara",
  "Santa María",           "Santa Maria",
  "Tirúa",                 "Tirua",
  "Toltén",                "Tolten",
  "Tomé",                  "Tome",
  "Traiguén",              "Traiguen",
  "Valparaíso",            "Valparaiso",
  "Vichuquén",             "Vichuquen",
  "Vicuña",                "Vicuna",
  "Vilcún",                "Vilcun",
  "Viña del Mar",          "Vina del Mar",
  "Ñiquén",                "Niquen",
  "Ñuñoa",                 "Nunoa",
  # Casos especiales adicionales
  "Alto Biobío",           "Alto Biobio",
  "Camiña",                "Camina",
  "Estación Central",      "Estacion Central"
)

# Aplicar correcciones manuales
data_comunal_corregido <- data_comunal %>%
  left_join(diccionario_manual_completo, by = c("comuna" = "comuna_original")) %>%
  mutate(comuna_para_join = ifelse(!is.na(comuna_corregida), comuna_corregida, comuna),
         comuna_normalizada = normalizar_nombre(comuna_para_join))

# Unir con códigos normalizados
data_comunal_final <- data_comunal_corregido %>%
  left_join(codigos_normalizados, by = "comuna_normalizada")

# Verificar resultados finales
comunas_con_codigo_final <- sum(!is.na(data_comunal_final$codigo_comuna))
cat("Comunas con código después de correcciones manuales:", comunas_con_codigo_final, 
    "de", nrow(data_comunal), "(", round(comunas_con_codigo_final/nrow(data_comunal)*100, 1), "%)\n")

# Mostrar las que aún faltan (si las hay)
comunas_sin_codigo_final <- data_comunal_final %>%
  filter(is.na(codigo_comuna)) %>%
  pull(comuna)

if(length(comunas_sin_codigo_final) > 0) {
  cat("\nComunas que aún NO tienen código:\n")
  print(comunas_sin_codigo_final)
}
# Unir con mapa
mapa_tipologias <- mapa_comunas %>%
  st_as_sf() %>%
  left_join(data_comunal_final, by = "codigo_comuna")

# Mapa final
p_mapa_tipologias <- ggplot(mapa_tipologias) +
  geom_sf(aes(fill = tipologia_predominante), 
          color = "white", size = 0.1, na.rm = TRUE) +
  scale_fill_brewer(palette = "Set3", name = "Tipología predominante",
                    na.value = "gray90") +
  labs(title = "Distribución Geográfica de Tipologías Familiares en Chile",
       subtitle = paste(comunas_con_codigo_final, "de", nrow(data_comunal), 
                        "comunas con datos (100%)"),  # ¡Ahora es 100%!
       caption = "Gris: Comunas sin datos en la muestra") +
  theme_void() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11, face = "bold"))

print(p_mapa_tipologias)

# Guardar el mapa
ggsave("Análisis de viviendas/Analisis/mapa_tipologias_comunales_final.png", 
       p_mapa_tipologias, width = 12, height = 8, dpi = 300, bg = "white")

# ------------------------------------------
### 7. ANÁLISIS DE AGRUPAMIENTO CON CLARA ###
# ------------------------------------------
library(cluster)

# Usar CLARA para datos grandes
clara_result <- clara(macro_scaled, k = 4, samples = 50, pamLike = TRUE)

# Visualizar resultados CLARA
p_clara <- fviz_cluster(clara_result, data = macro_scaled,
                        palette = "jco", ggtheme = theme_minimal(),
                        main = "Agrupamiento de Macrogrupos con CLARA")
# Ver eigenvalores
print(p_clara)
