################################################################################
# Figura1.R
# Genera un grid de 8×6 con las 48 tipologías más frecuentes.
# Cada panel muestra:
#   - Gráfico de red con nodos como gráficos de torta (♂/♀)
#   - Tamaño del nodo ∝ probabilidad de jefatura
#   - Estrella (★) en el nodo con mayor probabilidad de jefatura
#   - Aristas: verde = matrimonio, naranja = descendencia
#   - Título: ID + N nodos
################################################################################

library(igraph)
library(dplyr)
library(scales)

# ---------------------------------------------------------------------
# 1. Cargar datos necesarios
# ---------------------------------------------------------------------
load("Análisis de viviendas/Data/fingerprints.RData")
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")
load("Ergomitos/Redes/kinship_igrpah.RData")
households_por_tipologia <- readRDS("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds")
load("Análisis de viviendas/Data/Data_analisis.RData")
load("Análisis de viviendas/Data/Data_con_tipología.RData")

# ---------------------------------------------------------------------
# 2. Preparar datos para las 48 tipologías
# ---------------------------------------------------------------------
# Cargar la tabla de frecuencias y seleccionar las 48 primeras
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))[1:48]
total_redes <- sum(freq_table)

# Porcentaje de cada tipología
tipologia_porcentaje <- round((freq_table / total_redes) * 100, 3)

# Nos quedamos solo con las 48
tipologia_porcentaje_48 <- tipologia_porcentaje[paste0("T", 1:48)]

# Grafo de ejemplo por tipología (sin atributos)
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))
examples_48 <- examples[paste0("T", 1:48)]
names(tipologia_porcentaje_48) <- names(examples)
# ---------------------------------------------------------------------
# 3. Mapear households a tipologías (solo las 48)
# ---------------------------------------------------------------------
# Unir datos de tipología
datos_completos <- data_analisis %>%
  left_join(data_con_tipologia %>% select(household, tipologia), by = "household")

convertir_id <- function(x) as.character(x)

datos_filtrados <- datos_completos %>%
  filter(tipologia %in% paste0("T", 1:48)) %>%
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

# ---------------------------------------------------------------------
# 4. Funciones de cálculo
# ---------------------------------------------------------------------
calcular_jefatura_por_nodo <- function(tipologia_sel, redes_tipologia, redes_sin_attrs) {
  n_redes <- length(redes_tipologia)
  n_nodos <- vcount(redes_sin_attrs[[1]]$kinship_net)
  
  conteo <- rep(0, n_nodos)
  
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
  
  tibble(nodo = seq_len(n_nodos), frecuencia = conteo, porcentaje = porcentaje, tipologia = tipologia_sel)
}

calcular_sex_por_nodo <- function(tipologia_sel, redes_tipologia, redes_sin_attrs) {
  n_redes <- length(redes_tipologia)
  n_nodos <- vcount(redes_sin_attrs[[1]]$kinship_net)
  
  conteo <- rep(0, n_nodos)
  
  for (i in seq_len(n_redes)) {
    red_con_attrs <- redes_tipologia[[i]]$kinship_net
    red_sin_attrs <- redes_sin_attrs[[i]]$kinship_net
    
    perm <- canonical_permutation(red_sin_attrs)$labeling
    sex_vector <- as.numeric(V(red_con_attrs)$sex)
    sex_canonico <- sex_vector[perm]
    conteo <- conteo + (sex_canonico == 1)  # hombres
  }
  
  porcentaje <- round((conteo / n_redes) * 100, 1)
  tibble(nodo = seq_len(n_nodos), frecuencia = conteo, porcentaje = porcentaje, tipologia = tipologia_sel)
}

draw_pie_at <- function(x, y, slices, radius, colors) {
  slices <- slices / sum(slices)
  start <- 0
  for (i in seq_along(slices)) {
    end <- start + slices[i] * 2 * pi
    theta <- seq(start, end, length.out = 100)
    xs <- c(x, x + radius * cos(theta))
    ys <- c(y, y + radius * sin(theta))
    polygon(xs, ys, col = colors[i], border = NA)
    start <- end
  }
}

# ---------------------------------------------------------------------
# 5. Calcular estadísticas
# ---------------------------------------------------------------------
jefatura_por_nodo <- list()
sex_por_nodo <- list()

for (tip in paste0("T", 1:48)) {
  redes_tip <- redes_por_tipologia %>% filter(tipologia == tip)
  if (nrow(redes_tip) == 0) next
  redes_con_attrs <- redes_tip$redes[[1]]
  idx_redes <- redes_tip$indice[[1]]
  redes_sin_attrs <- kinship_igraph_no_attrs[idx_redes]
  
  jefatura_por_nodo[[tip]] <- calcular_jefatura_por_nodo(tip, redes_con_attrs, redes_sin_attrs)
  sex_por_nodo[[tip]]      <- calcular_sex_por_nodo(tip, redes_con_attrs, redes_sin_attrs)
}

# ---------------------------------------------------------------------
# 6. Función para dibujar panel
# ---------------------------------------------------------------------
dibujar_panel <- function(tip, g, datos_jef, datos_sex, pct) {
  # Obtener orden canónico
  perm <- canonical_permutation(g)$labeling
  n <- vcount(g)
  
  # Preparar vectores de % jefatura y % hombres/mujeres en orden canónico
  prob_jefe <- numeric(n)
  prob_jefe[perm] <- datos_jef$porcentaje / 100  # convertir a [0,1]
  
  pct_h <- numeric(n)
  pct_m <- numeric(n)
  pct_h[perm] <- datos_sex$porcentaje / 100
  pct_m[perm] <- 1 - pct_h  # asumiendo que los datos son % hombres
  
  # Asignar colores de aristas
  if ("type" %in% edge_attr_names(g)) {
    tipos <- E(g)$type
  } else {
    tipos <- rep("descent", ecount(g))
  }
  edge_col <- ifelse(tipos == "marriage", "#2CA02C", "#FF7F0E")
  E(g)$color <- edge_col
  E(g)$width <- 1
  E(g)$arrow.mode <- ifelse(tipos == "marriage", 0, 2)
  E(g)$arrow.size <- 1
  
  # Layout
  if (ecount(g) == n - 1 && all(tipos == "descent")) {
    raiz <- which.max(prob_jefe)
    if (length(raiz) == 0) raiz <- 1
    lo <- layout_as_tree(g, root = raiz)
  } else {
    lo <- layout_with_kk(g)
  }
  
  # Tamaño de nodos basado en probabilidad de jefatura (15-40)
  node_sizes <- scales::rescale(prob_jefe, to = c(15, 40))
  # Radio para los gráficos de torta
  radius_vals <- scales::rescale(node_sizes, to = c(0.07, 0.1))
  
  # Dibujar el grafo (sin nodos, solo aristas y layout)
  plot(g, layout = lo,
       main = paste0(tip, "   N = ", n, "   (", sprintf("%.1f", pct), "% of households)"),
       vertex.label = NA,
       vertex.size = 0,
       vertex.frame.color = NA,
       edge.color = edge_col,
       edge.width = E(g)$width,
       edge.arrow.mode = E(g)$arrow.mode,
       edge.arrow.size = E(g)$arrow.size,
       rescale = FALSE,
       xlim = range(lo[,1]) + c(-0.15, 0.15),
       ylim = range(lo[,2]) + c(-0.15, 0.15),
       margin = 0.2)
  
  # Dibujar gráficos de torta en cada posición
  for (i in 1:n) {
    slices <- c(pct_h[i], pct_m[i])
    if (sum(slices) == 0) slices <- c(1, 0)  # evita error
    draw_pie_at(lo[i,1], lo[i,2], slices, radius_vals[i], c("#3182BD", "#E377C2"))
  }
  
  # Mostrar el % de jefatura dentro de cada nodo
  for (i in 1:n) {
    text(lo[i,1], lo[i,2], labels = paste0(round(prob_jefe[i]*100), "%"),
         cex = 0.7, col = "black", font = 2)
  }
  
  # Marcar con estrella el nodo con mayor probabilidad de jefatura
  idx_max <- which.max(prob_jefe)
  if (length(idx_max) > 0) {
    text(lo[idx_max,1], lo[idx_max,2] + radius_vals[idx_max] + 0.03,
         labels = "★", cex = 2, col = "gold", font = 2)
  }
}

# Cargar resumen_tipologias (si no está ya cargado)
resumen_tipologias <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv")

# Crear un vector de porcentajes con nombres seguros
pct_dict <- setNames(resumen_tipologias$Porcentaje, resumen_tipologias$Tipologia)


# ---------------------------------------------------------------------
# 7. Generar el PDF con grid de 6 filas x 8 columnas
# ---------------------------------------------------------------------

pdf("Análisis de viviendas/Analisis/Figura1.pdf", width = 22, height = 17, bg = "white", pointsize = 8)
par(mfrow = c(6, 8), mar = c(3.5, 1.5, 2.5, 1.5))

for (tip in paste0("T", 1:48)) {
  g <- examples_48[[tip]]
  datos_jef <- jefatura_por_nodo[[tip]]
  datos_sex <- sex_por_nodo[[tip]]
  
  # Obtener porcentaje desde el CSV (robusto)
  pct <- pct_dict[tip]
  # Si por algún motivo es NA, usar 0 con advertencia
  if (is.na(pct)) {
    pct <- 0
    warning(paste("Porcentaje no encontrado para", tip))
  }
  
  if (!is.null(g) && !is.null(datos_jef) && !is.null(datos_sex)) {
    dibujar_panel(tip, g, datos_jef, datos_sex, pct)
  } else {
    plot.new()
    text(0.5, 0.5, paste(tip, "\nsin datos"))
  }
}

dev.off()

png("Análisis de viviendas/Analisis/Figura1.pdf", width = 22, height = 17, bg = "white", pointsize = 8)
par(mfrow = c(6, 8), mar = c(3.5, 1.5, 2.5, 1.5))

for (tip in paste0("T", 1:48)) {
  g <- examples_48[[tip]]
  datos_jef <- jefatura_por_nodo[[tip]]
  datos_sex <- sex_por_nodo[[tip]]
  
  # Obtener porcentaje desde el CSV (robusto)
  pct <- pct_dict[tip]
  # Si por algún motivo es NA, usar 0 con advertencia
  if (is.na(pct)) {
    pct <- 0
    warning(paste("Porcentaje no encontrado para", tip))
  }
  
  if (!is.null(g) && !is.null(datos_jef) && !is.null(datos_sex)) {
    dibujar_panel(tip, g, datos_jef, datos_sex, pct)
  } else {
    plot.new()
    text(0.5, 0.5, paste(tip, "\nsin datos"))
  }
}

dev.off()