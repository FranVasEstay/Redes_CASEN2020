################################################################################
###################### Social Network: encuesta CASEN ##########################
###################### ANÁLISIS DE NODOS AISLADOS ##############################
###################Y SU RELACIÓN CON EL JEFE DE HOGAR###########################
######################Por tipología y por macrogrupo############################
 
library(igraph)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS
# ------------------------------------------------------------------------------

# Redes con atributos (tiene pco1, sex, edad, etc.)
load("Análisis de viviendas/Redes/kinship_igrpah.RData")   # objeto: kinship_igrpah

# Mapeo de household -> tipología (todas las tipologías hasta T48)
households_por_tipologia <- readRDS(
  "Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds"
)

# Clasificación de tipologías en macrogrupos (solo T1 a T48)
macrogrupos <- read.csv(
  "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipo.csv",
  stringsAsFactors = FALSE
) %>% filter(Tipologia %in% paste0("T", 1:48))

# Si quieres limitarte a las tipologías del "Grupo Alto" (las más frecuentes),
# puedes cargar muestra_tipologias.csv y filtrar. Por defecto usaremos todas T1-T48.
# muestra_tipologias <- read.csv("Análisis de viviendas/Data/muestra_tipologias.csv", sep = "\t")
# tipologias_interes <- muestra_tipologias$Tipologia

tipologias_interes <- paste0("T", 1:48)   # o únicamente las que existen en macrogrupos

# ------------------------------------------------------------------------------
# 2. PREPARAR MAPEO household -> índice en la lista de redes
# ------------------------------------------------------------------------------

# Convertir household_i a character para evitar problemas de tipo
ids_redes <- data.frame(
  indice = seq_along(kinship_igrpah),
  household_i = sapply(kinship_igrpah, function(x) as.character(x$household_i)),
  stringsAsFactors = FALSE
)

# Expandir households_por_tipologia a formato largo (un row por household)
tipologia_larga <- households_por_tipologia %>%
  select(Tipologia, Households) %>%
  mutate(Households = strsplit(as.character(Households), ";")) %>%   # si es string separado por ;
  unnest(Households) %>%
  mutate(household = as.character(Households)) %>%
  select(Tipologia, household)

# Unir con los índices de red
hogares_con_indice <- tipologia_larga %>%
  inner_join(ids_redes, by = c("household" = "household_i"))

# Añadir macrogrupo
hogares_con_indice <- hogares_con_indice %>%
  left_join(macrogrupos, by = c("Tipologia" = "Tipologia"))

# Verificar cobertura
cat("Hogares únicos en tipologia_larga:", n_distinct(tipologia_larga$household), "\n")
cat("Hogares con red disponible en kinship_igrpah:", n_distinct(hogares_con_indice$household), "\n")

# ------------------------------------------------------------------------------
# 3. FUNCIÓN PARA PROCESAR UN HOGAR Y EXTRAER NODOS AISLADOS
# ------------------------------------------------------------------------------

extraer_aislados <- function(indice_red, tipologia, macrogrupo) {
  # Obtener el grafo con atributos
  g <- kinship_igrpah[[indice_red]]$kinship_net
  
  # Verificar que sea un grafo y tenga al menos un vértice
  if (!inherits(g, "igraph") || vcount(g) == 0) return(NULL)
  
  # Identificar al jefe de hogar (pco1 == "Jefe(a) de Hogar")
  # En los atributos, pco1 puede ser numérico (1 = jefe) o texto. Revisamos.
  pco1_vals <- vertex_attr(g, "pco1")
  if (is.null(pco1_vals)) {
    # Si no existe el atributo, intentamos obtener de otra forma (no debería pasar)
    warning(paste("Hogar", kinship_igrpah[[indice_red]]$household_i, "sin atributo pco1"))
    return(NULL)
  }
  
  # Convertir a character por si es factor
  pco1_char <- as.character(pco1_vals)
  jefe_idx <- which(pco1_char == "Jefe(a) de Hogar")
  if (length(jefe_idx) != 1) {
    # No hay jefe único (puede ocurrir si los datos están corruptos)
    return(NULL)
  }
  
  # Calcular grado total (sin considerar bucles, no los hay)
  grados <- degree(g, mode = "total")
  
  # Identificar nodos aislados (grado == 0) que NO sean el jefe
  aislados_idx <- which(grados == 0 & seq_along(grados) != jefe_idx)
  
  if (length(aislados_idx) == 0) return(NULL)
  
  # Extraer la relación (pco1) de esos nodos
  relaciones <- pco1_char[aislados_idx]
  
  # Crear un tibble con los resultados
  tibble(
    tipologia = tipologia,
    macrogrupo = macrogrupo,
    relacion = relaciones
  )
}

# ------------------------------------------------------------------------------
# 4. APLICAR LA FUNCIÓN A TODOS LOS HOGARES (paralelo opcional)
# ------------------------------------------------------------------------------

# Usamos map_df para ir acumulando resultados, con progreso
resultados <- hogares_con_indice %>%
  filter(Tipologia %in% tipologias_interes) %>%   # opcional
  mutate(result = pmap(list(indice, Tipologia, macrogrupo), 
                       ~extraer_aislados(..1, ..2, ..3))) %>%
  unnest(result, keep_empty = FALSE)

# Si no hay resultados, revisar los datos de entrada.
cat("Número de observaciones (nodo aislado - relación):", nrow(resultados), "\n")

# ------------------------------------------------------------------------------
# 5. TABLAS DE FRECUENCIAS Y PORCENTAJES POR TIPOLOGÍA
# ------------------------------------------------------------------------------

# Frecuencias absolutas por tipología y relación
tabla_tipologia_abs <- resultados %>%
  group_by(tipologia, relacion) %>%
  summarise(n = n(), .groups = "drop")

# Porcentajes dentro de cada tipología (suma de n's por tipologia = 100%)
tabla_tipologia_pct <- tabla_tipologia_abs %>%
  group_by(tipologia) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(tipologia, desc(porcentaje))

# Guardar CSV
write.csv(tabla_tipologia_pct, "Análisis de viviendas/Analisis/Relaciones_aislados_por_tipologia.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 6. TABLAS POR MACROGRUPO
# ------------------------------------------------------------------------------

# Reemplazar posibles NA en macrogrupo (si alguna tipología no fue clasificada)
resultados <- resultados %>%
  mutate(macrogrupo = ifelse(is.na(macrogrupo), "Sin clasificar", macrogrupo))

tabla_macro_abs <- resultados %>%
  group_by(macrogrupo, relacion) %>%
  summarise(n = n(), .groups = "drop")

tabla_macro_pct <- tabla_macro_abs %>%
  group_by(macrogrupo) %>%
  mutate(porcentaje = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(macrogrupo, desc(porcentaje))

write.csv(tabla_macro_pct, "Análisis de viviendas/Analisis/Relaciones_aislados_por_macrogrupo.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# ------------------------------------------------------------------------------
# 7. GRÁFICOS DE BARRAS (OPCIONAL)
# ------------------------------------------------------------------------------

# Gráfico para los principales macrogrupos (excluir "Sin clasificar" si existe)
datos_plot <- tabla_macro_pct %>%
  filter(macrogrupo != "Sin clasificar") %>%
  mutate(relacion = fct_reorder(relacion, porcentaje, .desc = FALSE))

ggplot(datos_plot, aes(x = porcentaje, y = relacion, fill = macrogrupo)) +
  geom_col(position = "dodge") +
  facet_wrap(~macrogrupo, scales = "free_y") +
  labs(title = "Relación con el jefe de hogar de los nodos aislados",
       x = "Porcentaje dentro del macrogrupo (%)", y = "Relación (pco1)") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Análisis de viviendas/Analisis/grafico_aislados_macrogrupos.png", width = 10, height = 6)

# Si quieres un gráfico para las tipologías más frecuentes (ej. primeras 10)
top_tipologias <- tabla_tipologia_pct %>%
  group_by(tipologia) %>%
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%
  slice_head(n = 10) %>%
  pull(tipologia)

tabla_tipologia_top <- tabla_tipologia_pct %>%
  filter(tipologia %in% top_tipologias)

ggplot(tabla_tipologia_top, aes(x = porcentaje, y = relacion, fill = tipologia)) +
  geom_col(position = "dodge") +
  facet_wrap(~tipologia, scales = "free_y") +
  labs(title = "Relaciones de nodos aislados (top 10 tipologías por frecuencia)",
       x = "Porcentaje dentro de la tipología (%)", y = "Relación") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave("Análisis de viviendas/Analisis/grafico_aislados_top10_tipologias.png", width = 12, height = 7)

cat("Análisis completado. Archivos guardados en la carpeta 'Analisis'.\n")