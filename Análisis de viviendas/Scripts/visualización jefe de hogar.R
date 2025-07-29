#Liberías
library(igraph)
library(ggraph)
library(patchwork)
library(tidyverse)
library(scales)
library(dplyr)
library(tibble)
library(purrr)
################################################################################
# VISUALIZACIÓN GÉNERO - JEFE DE HOGAR - PROMEDIO EDAD
################################################################################
## Cargar datos de redes
load("Análisis de viviendas/Data/fingerprints.RData") #Tiene fingerprints de cada data
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData") # red SIN NINGÚN atributo
load("Análisis de viviendas/Redes/kinship_igrpah.RData") #red con atributos
households_por_tipologia <- readRDS("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds") # Tiene a las tipologías y una columna con los households que pertenecen a cada tipologpia separados por ";" 
load("Análisis de viviendas/Data/Data_analisis.RData")
load("Análisis de viviendas/Data/Data_con_tipología.RData")
### Variables en la data ###
# edad - edad numérico
# sexo - sexo binario levels: 1(Hombre), 2(Mujer)
# e6a  - nivel educativo: "¿Cuál es el nivel más alto alcanzado o el nivel educacional actual?", 17 labels de 1 "Nunca asistío" hasya 17 "Posgrado"
# o1   - ¿trabajó la semana pasada? binario: 1(si), 2(no)
# r1b_pais_esp - nacionalidad levels: 1(chileno), 2(extranjero), 3(no responde) 
# ecivil - estado civil levels: 1 "Casado(a)" hasta 9 "No sabe\\No responde"
# r3   - pueblo indígena levels:1-10 distintos pueblos indígenas, 11 - no pertenece a un pueblo indígena
# s28  - ha estado en tratamiento médico (12 meses) levels: 1(si), 2(no), 3(No sabe/No recuerda)
# comuna - comuna 
# region - región 
# ytotcor - Ingreso total del hogar numérico en pesos (CLP) 
# edad_laboral - binaria: 1 (>=15 años), 2(<= 15 años)
# edad_legal - binaria: 1 (>=18 años), 2(<= 18 años)
# edad_dependencia_estudios - binaria: 1 (>=28 años), 2(<= 28 años)
# pco1: parentesco con el jefe de hogar: 1 (Jefe de hogar)

# Cargar tipologías válidas (ya filtradas previamente)
tipologias_validas <- unique(households_por_tipologia$Tipologia)

# Paso 3. Mostrar la tabla
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]  # Reordenamos la tabla

# 5. Guardar un grafo de ejemplo por tipología
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))  # Asignar nombres T1, T2, ...

# Unir los datos para tener tipología + información individual
datos_completos <- data_analisis %>%
  left_join(data_con_tipologia %>% select(household, tipologia), by = "household")

######## Tabla de contingencia por tipología ###################################
# Procesar las redes kinship para cada tipología
convertir_id <- function(x) {
  as.character(x)  # Convierte 1.1011e+11 → 110110000000
}
# Filtrar datos hasta la tipología T48
datos_filtrados <- datos_completos %>% # Filtrar solo tipologías T1 a T48
  filter(grepl("^T([1-9]|[1-3][0-9]|4[0-8])$", tipologia)) %>% # Convertir household a entero (para que coincida con las redes)
  mutate(household = convertir_id(household)) %>% # Seleccionar solo las columnas necesarias y eliminar duplicados
  distinct(household, tipologia)


ids_redes <- data.frame(
  indice = seq_along(kinship_igrpah),  # Índice de cada red en la lista
  household_i = sapply(kinship_igrpah, function(x) convertir_id(x$household_i))  # IDs convertidos
) #extraer los ids de las redes
redes_con_tipologia <- ids_redes %>%
  inner_join(
    datos_filtrados,
    by = c("household_i" = "household")  # Unir por ID
  )

redes_por_tipologia <- redes_con_tipologia %>% #agrupar redes por tipología
  group_by(tipologia) %>%
  summarise(
    redes = list(kinship_igrpah[indice]),  # Lista de redes por tipología
    households = list(household_i),        # IDs de households incluidos
    indice = list(indice),  # indice
    n_redes = n()                          # Conteo de redes por grupo
  ) %>%
  arrange(tipologia)  # Ordenar alfabéticamente (T1, T10, T11, ..., T2, T20, ...)
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

# Plot de jefatura y edad
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
 # V(grafo)$label <- paste0(round(porcentaje_jefatura,1), "%")
  V(grafo)$label.color <- "black"
  
  lay <- layout_with_fr(grafo)
  
  porcentajes_validos <- porcentaje_jefatura
  porcentajes_validos[!is.finite(porcentajes_validos) | porcentajes_validos <= 0] <- 0
  #radius_vals <- scales::rescale(porcentajes_validos, to = c(0.03, 0.07))
  #radius_vals[porcentajes_validos == 0] <- 0.05
  node_sizes <- scales::rescale(promedio_edad, to = c(15, 40))  # igual que vertex.size
  max_size <- max(node_sizes)
  min_radius <- 0.07
  max_radius <- 0.1
  
  radius_vals <- scales::rescale(node_sizes, to = c(min_radius, max_radius))
  
  plot(
    grafo,
    layout = lay,
    rescale = FALSE,
    xlim = range(lay[,1]) + c(-0.1, 0.1),
    ylim = range(lay[,2]) + c(-0.1, 0.1),
    main = paste("Tipología", tipologia, ": proporción de jefatura (%), edad promedio (tamaño), y sexo (colores)"),
    vertex.label.family = "sans"
  )
  
  for (i in seq_along(porcentaje_hombre)) {
    slices <- c(porcentaje_hombre[i], porcentaje_mujer[i])
    slices[is.na(slices)] <- 0
    if (sum(slices) == 0) slices <- c(1, 0)
    
    radius_val <- radius_vals[i]
    
    # Usar la función manual para dibujar la torta
    draw_pie_at(
      x = lay[i, 1],
      y = lay[i, 2],
      slices = slices,
      radius = radius_val,
      colors = c("lightblue", "pink")
    )
  }
  # Superponer etiquetas (porcentaje de jefatura)
  for (i in seq_along(porcentaje_jefatura)) {
    text(
      x = lay[i, 1],
      y = lay[i, 2],
      labels = paste0(round(porcentaje_jefatura[i], 1), "%"),
      cex = 0.8,
      col = "black"
    )
  }
  # Añadir simbología
  legend(
    "bottomleft",
    legend = c("Hombres", "Mujeres"),
    col = c("lightblue", "pink", NA, NA),
    pt.bg = c("lightblue", "pink", NA, NA),
    pch = c(21, 21, NA, NA),
    pt.cex = c(2, 2, NA, NA),
    bty = "n",
    text.col = "black"
  )
  # Simular flechas de relaciones (matrimonio y filiación)
  #x0 <- par("usr")[1] + 0.12
  #y_base <- par("usr")[3] + 0.18
  # Flecha verde - matrimonio
  #arrows(
  #  x0 = x0,
  #  y0 = y_base + 0.05,
  #  x1 = x0 + 0.05,
  #  y1 = y_base + 0.05,
  #  col = "darkgreen",
  #  length = 0.1,
  #  lwd = 2
  #)
  #text(
  #  x = x0 + 0.06,
  #  y = y_base + 0.05,
  #  labels = "Matrimonio",
  #  pos = 4,
  #  cex = 0.8
  #)
  
  # Flecha naranja - filiación
  #arrows(
  #  x0 = x0,
  #  y0 = y_base - 0.02,
  #  x1 = x0 + 0.05,
  #  y1 = y_base - 0.02,
  #  col = "orange",
  #  length = 0.1,
  #  lwd = 2
  #)
 # text(
  #  x = x0 + 0.06,
  # y = y_base - 0.02,
  #  labels = "Filiación",
  #  pos = 4,
  #  cex = 0.8
  #)
}

# Crear PDF con todas las tipologías
pdf("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Jefatura_edad_sexo_por_nodo.pdf", width = 10, height = 8)

for (tip in names(jefatura_por_nodo)) {
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
