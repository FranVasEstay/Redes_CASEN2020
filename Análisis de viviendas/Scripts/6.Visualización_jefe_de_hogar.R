################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
########## 6. VISUALIZACIÓN GÉNERO - JEFE DE HOGAR - PROMEDIO EDAD#################
################################################################################

#Liberías
library(igraph)
library(ggraph)
library(patchwork)
library(tidyverse)
library(scales)
library(dplyr)
library(tibble)
library(purrr)

# Cargar datos de redes
load("Análisis de viviendas/Data/fingerprints.RData") 
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData") 
load("Análisis de viviendas/Redes/kinship_igrpah.RData") 
households_por_tipologia <- readRDS("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds")
load("Análisis de viviendas/Data/Data_analisis.RData")
load("Análisis de viviendas/Data/Data_con_tipología.RData")

# Cargar tipologías válidas
tipologias_validas <- unique(households_por_tipologia$Tipologia)

# Paso 3. Mostrar la tabla
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]
total_redes <- sum(freq_table)
tipologia_porcentaje <- round((freq_table / total_redes) * 100, 1)

# Grafo de ejemplo por tipología
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))
names(tipologia_porcentaje) <- names(examples)

# Unir datos completos
datos_completos <- data_analisis %>%
  left_join(data_con_tipologia %>% select(household, tipologia), by = "household")

# Filtrar tipologías válidas
convertir_id <- function(x) as.character(x)

datos_filtrados <- datos_completos %>%
  filter(grepl("^T([1-9]|[1-3][0-9]|4[0-8])$", tipologia)) %>%
  mutate(household = convertir_id(household)) %>%
  distinct(household, tipologia)

ids_redes <- data.frame(
  indice = seq_along(kinship_igrpah),
  household_i = sapply(kinship_igrpah, function(x) convertir_id(x$household_i))
)

redes_con_tipologia <- ids_redes %>%
  inner_join(datos_filtrados, by = c("household_i" = "household"))

redes_por_tipologia <- redes_con_tipologia %>%
  group_by(tipologia) %>%
  summarise(
    redes = list(kinship_igrpah[indice]),
    households = list(household_i),
    indice = list(indice),
    n_redes = n()
  ) %>%
  arrange(tipologia)
head(redes_por_tipologia)

# Ejemplo: Acceder a todas las redes de la tipología T1
redes_T1 <- redes_por_tipologia %>%
  filter(tipologia == "T1") %>%
  pull(redes) %>%
  first()
redes_T1[[1]]

# Jefe por nodo
calcular_jefatura_por_nodo <- function(tipologia_sel, redes_tipologia, redes_sin_attrs) {
  n_redes <- length(redes_tipologia)
  n_nodos <- vcount(redes_sin_attrs[[1]]$kinship_net)
  
  conteo <- rep(0, n_nodos)  # Inicializar con ceros del tamaño correcto
  
  for (i in seq_len(n_redes)) {
    red_con_attrs <- redes_tipologia[[i]]$kinship_net
    red_sin_attrs <- redes_sin_attrs[[i]]$kinship_net
    
    jefe_idx <- which(as.numeric(V(red_con_attrs)$pco1) == 1)
    if (length(jefe_idx) != 1) next
    
    perm <- canonical_permutation(red_sin_attrs)$labeling
    jefe_canonico <- match(jefe_idx, perm)
    
    if (!is.na(jefe_canonico)) {
      conteo[jefe_canonico] <- conteo[jefe_canonico] + 1
    }
  }
  
  porcentaje <- round((conteo / n_redes) * 100, 1)
  
  tibble(
    nodo = seq_len(n_nodos),
    frecuencia = conteo,
    porcentaje = porcentaje,
    tipologia = tipologia_sel
  )
}
# Crear lista de resultados por tipología
jefatura_por_nodo <- list()

for (tip in redes_por_tipologia$tipologia) {
  redes_tip <- redes_por_tipologia %>% filter(tipologia == tip)
  redes_con_attrs <- redes_tip$redes[[1]]
  idx_redes <- redes_tip$indice[[1]]
  redes_sin_attrs <- kinship_igraph_no_attrs[idx_redes]
  
  jefatura_por_nodo[[tip]] <- calcular_jefatura_por_nodo(
    tip,
    redes_con_attrs,
    redes_sin_attrs
  )
}
# sex por nodo
calcular_sex_por_nodo <- function(tipologia_sel, redes_tipologia, redes_sin_attrs) {
  n_redes <- length(redes_tipologia)
  n_nodos <- vcount(redes_sin_attrs[[1]]$kinship_net)
  
  conteo <- rep(0, n_nodos)  # Inicializar con ceros del tamaño correcto
  
  for (i in seq_len(n_redes)) {
    red_con_attrs <- redes_tipologia[[i]]$kinship_net
    red_sin_attrs <- redes_sin_attrs[[i]]$kinship_net
    
    perm <- canonical_permutation(red_sin_attrs)$labeling
    sex_vector <- as.numeric(V(red_con_attrs)$sex)
    sex_canonico <- sex_vector[perm]
    conteo <- conteo + (sex_canonico == 1) # conteo para todos los nodos que sean hombres
    }
  porcentaje <- round((conteo / n_redes) * 100, 1)
  
  tibble(
    nodo = seq_len(n_nodos),
    frecuencia = conteo,
    porcentaje = porcentaje,
    tipologia = tipologia_sel
  )
}
sex_por_nodo <- list()

for (tip in redes_por_tipologia$tipologia) {
  redes_tip <- redes_por_tipologia %>% filter(tipologia == tip)
  redes_con_attrs <- redes_tip$redes[[1]]
  idx_redes <- redes_tip$indice[[1]]
  redes_sin_attrs <- kinship_igraph_no_attrs[idx_redes]
  
  sex_por_nodo[[tip]] <- calcular_sex_por_nodo(
    tip,
    redes_con_attrs,
    redes_sin_attrs
  )
}
# Promedio de la edad
calcular_edad_por_nodo <- function(tipologia_sel, redes_tipologia, redes_sin_attrs) {
  n_redes <- length(redes_tipologia)
  n_nodos <- vcount(redes_sin_attrs[[1]]$kinship_net)
  
  suma_edades <- numeric(n_nodos)      # Suma acumulada de edades por nodo
  conteo_edades <- numeric(n_nodos)   # Cantidad de observaciones por nodo
  
  for (i in seq_len(n_redes)) {
    red_con_attrs <- redes_tipologia[[i]]$kinship_net
    red_sin_attrs <- redes_sin_attrs[[i]]$kinship_net
    
    edades <- as.numeric(V(red_con_attrs)$edad)
    perm <- canonical_permutation(red_sin_attrs)$labeling
    
    # Reordenar edades según permutación canónica (del grafo sin atributos)
    edades_canonicas <- edades[perm]
    # Acumular sumas y conteos para nodos no NA
    validos <- !is.na(edades_canonicas)
    suma_edades[validos] <- suma_edades[validos] + edades_canonicas[validos]
    conteo_edades[validos] <- conteo_edades[validos] + 1
  }
  # Calcular promedio
  promedio_edades <- suma_edades / conteo_edades
  promedio_edades[is.nan(promedio_edades)] <- NA  # Evitar NaN por división 0/0
  
  tibble(
    nodo = seq_len(n_nodos),
    suma = suma_edades,
    conteo = conteo_edades,
    promedio = round(promedio_edades, 1),
    tipologia = tipologia_sel
  )
}
edad_por_nodo <- list()

for (tip in redes_por_tipologia$tipologia) {
  redes_tip <- redes_por_tipologia %>% filter(tipologia == tip)
  redes_con_attrs <- redes_tip$redes[[1]]
  idx_redes <- redes_tip$indice[[1]]
  redes_sin_attrs <- kinship_igraph_no_attrs[idx_redes]
  
  edad_por_nodo[[tip]] <- calcular_edad_por_nodo(
    tip,
    redes_con_attrs,
    redes_sin_attrs
  )
}

draw_pie_at <- function(x, y, slices, radius, colors) {
  # Normalizar slices para que sumen 1
  slices <- slices / sum(slices)
  
  start <- 0
  for (i in seq_along(slices)) {
    # Ángulo inicial y final para sector
    end <- start + slices[i] * 2 * pi
    
    # Crear secuencia de puntos para el sector
    theta <- seq(start, end, length.out = 100)
    xs <- c(x, x + radius * cos(theta))
    ys <- c(y, y + radius * sin(theta))
    
    # Dibujar sector
    polygon(xs, ys, col = colors[i], border = NA)
    
    start <- end
  }
}


resumen_tipologias <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv")

# Crear un named vector con porcentajes reales
tipologia_porcentaje <- resumen_tipologias$Porcentaje
names(tipologia_porcentaje) <- resumen_tipologias$Tipologia

# ================================
# ORDENAR TIPOS NUMÉRICAMENTE USANDO CSV
# ================================
tipologias_ordenadas <- resumen_tipologias %>%
  arrange(as.numeric(gsub("T", "", Tipologia))) %>%
  pull(Tipologia)

# ================================
# MODIFICAR EL PLOT PARA USAR PORCENTAJES REALES
# ================================
plot_nodos <- function(grafo, datos_nodo, datos_sexo, tipologia) {
  perm <- canonical_permutation(grafo)$labeling
  
  porcentaje_jefatura <- numeric(length(perm))
  porcentaje_jefatura[perm] <- datos_nodo$porcentaje
  
  promedio_edad <- numeric(length(perm))
  promedio_edad[perm] <- datos_nodo$promedio
  promedio_edad[is.na(promedio_edad)] <- min(promedio_edad, na.rm = TRUE)
  
  porcentaje_hombre <- numeric(length(perm))
  porcentaje_mujer <- numeric(length(perm))
  porcentaje_hombre[perm] <- datos_sexo$porcentaje_hombre
  porcentaje_mujer[perm] <- datos_sexo$porcentaje_mujer
  porcentaje_hombre[is.na(porcentaje_hombre)] <- 0
  porcentaje_mujer[is.na(porcentaje_mujer)] <- 0
  
  V(grafo)$color <- "white"
  V(grafo)$size <- scales::rescale(promedio_edad, to = c(15, 40))
  V(grafo)$frame.color <- "white"
  V(grafo)$label.cex <- 0.9
  V(grafo)$label.color <- "black"
  
  lay <- layout_with_fr(grafo)
  
  node_sizes <- scales::rescale(promedio_edad, to = c(15, 40))
  radius_vals <- scales::rescale(node_sizes, to = c(0.07, 0.1))
  
  # 🔹 MODIFICACIÓN: usamos porcentajes reales del CSV
  plot(
    grafo,
    layout = lay,
    rescale = FALSE,
    xlim = range(lay[,1]) + c(-0.1, 0.1),
    ylim = range(lay[,2]) + c(-0.1, 0.1),
    main = paste0("Tipología ", tipologia, " (", tipologia_porcentaje[tipologia], "% de redes)"),
    sub = "Proporción de jefatura (%), edad promedio (tamaño), y sexo (colores)",
    vertex.label.family = "sans"
  )
  
  for (i in seq_along(porcentaje_hombre)) {
    slices <- c(porcentaje_hombre[i], porcentaje_mujer[i])
    slices[is.na(slices)] <- 0
    if (sum(slices) == 0) slices <- c(1, 0)
    draw_pie_at(
      x = lay[i, 1],
      y = lay[i, 2],
      slices = slices,
      radius = radius_vals[i],
      colors = c("lightblue", "pink")
    )
  }
  
  for (i in seq_along(porcentaje_jefatura)) {
    text(
      x = lay[i, 1],
      y = lay[i, 2],
      labels = paste0(round(porcentaje_jefatura[i], 1), "%"),
      cex = 0.8,
      col = "black"
    )
  }
  
  legend(
    "bottomleft",
    legend = c("Hombres", "Mujeres"),
    col = c("lightblue", "pink"),
    pt.bg = c("lightblue", "pink"),
    pch = c(21, 21),
    pt.cex = c(2, 2),
    bty = "n",
    text.col = "black"
  )
}

# ================================
# RECORRER TIPOS ORDENADOS CON PORCENTAJES REALES
# ================================
pdf("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Jefatura_edad_sexo_por_nodo.pdf", width = 10, height = 8)

for (tip in tipologias_ordenadas) {
  g <- examples[[tip]]
  datos_jefatura <- jefatura_por_nodo[[tip]]
  datos_edad <- edad_por_nodo[[tip]]
  datos_sexo_raw <- sex_por_nodo[[tip]]
  
  datos_sexo <- tibble(
    nodo = datos_sexo_raw$nodo,
    porcentaje_hombre = datos_sexo_raw$porcentaje,
    porcentaje_mujer = 100 - datos_sexo_raw$porcentaje
  )
  
  datos_combinados <- datos_jefatura %>%
    select(nodo, porcentaje) %>%
    left_join(datos_edad %>% select(nodo, promedio), by = "nodo")
  
  plot_nodos(g, datos_combinados, datos_sexo, tip)
}

dev.off()