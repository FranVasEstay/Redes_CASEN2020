#Liberías
library(igraph)
library(ggraph)
library(patchwork)
library(tidyverse)
library(scales)
################################################################################
# PASO 2: VISUALIZACIÓN GÉNERO DE JEFE DE HOGAR
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
# Filtrar datos hasta la tipología T48
datos_filtrados <- datos_completos %>% # Filtrar solo tipologías T1 a T48
  filter(grepl("^T([1-9]|[1-3][0-9]|4[0-8])$", tipologia)) %>% # Convertir household a entero (para que coincida con las redes)
  mutate(household = convertir_id(household)) %>% # Seleccionar solo las columnas necesarias y eliminar duplicados
  distinct(household, tipologia)

# Procesar las redes kinship para cada tipología
convertir_id <- function(x) {
  as.character(x)  # Convierte 1.1011e+11 → 110110000000
}
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
    indice = list(indice),  # indice jeje
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


library(igraph)
library(dplyr)
library(tibble)
library(purrr)

# Función principal
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
plot_jefatura_nodos <- function(grafo, datos_nodo, tipologia) {
  # Obtener permutación canónica del grafo a plotear
  perm <- canonical_permutation(grafo)$labeling
  
  # Los porcentajes están ordenados según nodos canónicos
  # Reordenar los porcentajes al orden del grafo original
  porcentaje_orden_grafo <- numeric(length(perm))
  porcentaje_orden_grafo[perm] <- datos_nodo$porcentaje  # Mapear correctamente
  
  V(grafo)$color <- "#6baed6"
  V(grafo)$size <- rescale(porcentaje_orden_grafo, to = c(15, 40))
  V(grafo)$frame.color <- "white"
  V(grafo)$label.cex <- 0.9
  V(grafo)$label <- paste0(round(porcentaje_orden_grafo,1), "%")
  V(grafo)$label.color <- "black"
  
  plot(
    grafo,
    main = paste("Tipología", tipologia, "- % jefatura por nodo canónico"),
    vertex.label.family = "sans"
  )
}


# Exportar visualizaciones a PDF
pdf("Análisis de viviendas/Analisis/Resultados_Tipologias/Graficos/Jefatura_por_nodo.pdf", width = 10, height = 8)
for (tip in names(jefatura_por_nodo)) {
  g <- examples[[tip]]
  datos <- jefatura_por_nodo[[tip]]
  plot_jefatura_nodos(g, datos, tip)
}
dev.off()
