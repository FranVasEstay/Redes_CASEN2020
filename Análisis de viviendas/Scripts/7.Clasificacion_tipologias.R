################################################################################
###################### Social Network: encuesta CASEN ##########################
###################### 7. CLASIFICACIÓN DE TIPOLOGÍAS ##########################
################################################################################
library(cluster)
library(dplyr)
library(factoextra)
library(igraph)
library(purrr)
library(tidyr)   # para pivotar si es necesario

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS Y GENERAR EJEMPLOS
# ------------------------------------------------------------------------------
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")
load("Análisis de viviendas/Data/fingerprints.RData")
tipologia_df <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv")

# Calcular frecuencias de cada fingerprint
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]
names(tipologia_freq) <- paste0("T", seq_along(unique_fps))
print(tipologia_freq)

tipologias_validas <- paste0("T", 1:48)
tipologia_df <- tipologia_df %>% filter(Tipologia %in% tipologias_validas)

# Grafo ejemplo por tipología
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))

# ------------------------------------------------------------------------------
# 2. FUNCIONES PARA DETECTAR ESTRUCTURAS FAMILIARES
# ------------------------------------------------------------------------------
detectar_intergeneracional <- function(grafo) {
  if (vcount(grafo) < 3) return(FALSE)
  for (v in V(grafo)) {
    # Cadena ascendente
    padres <- neighbors(grafo, v, mode = "in")
    if (length(padres) >= 1) {
      for (p in padres) {
        abuelos <- neighbors(grafo, p, mode = "in")
        if (length(abuelos) >= 1) {
          if (all(E(grafo)[p %->% v]$type == "descent") && 
              all(E(grafo)[abuelos %->% p]$type == "descent")) {
            return(TRUE)
          }
        }
      }
    }
    # Cadena descendente
    hijos <- neighbors(grafo, v, mode = "out")
    if (length(hijos) >= 1) {
      for (h in hijos) {
        nietos <- neighbors(grafo, h, mode = "out")
        if (length(nietos) >= 1) {
          if (all(E(grafo)[v %->% h]$type == "descent") && 
              all(E(grafo)[h %->% nietos]$type == "descent")) {
            return(TRUE)
          }
        }
      }
    }
  }
  return(FALSE)
}

clasificar_tipologia <- function(grafo) {
  edge_types <- E(grafo)$type
  edge_ends <- ends(grafo, E(grafo))
  num_vertices <- vcount(grafo)
  num_edges <- ecount(grafo)
  
  resultados <- list(
    intergeneracional = FALSE,
    padrastros = FALSE,
    suegros = FALSE,
    nodos_aislados = FALSE,
    monoparental = FALSE,
    pareja_sin_hijos = FALSE
  )
  
  # Pareja sin hijos
  if (num_vertices == 2 && num_edges == 2 && "marriage" %in% edge_types) {
    resultados$pareja_sin_hijos <- TRUE
    return(resultados)
  }
  
  # Intergeneracional
  resultados$intergeneracional <- detectar_intergeneracional(grafo)
  
  # Padrastros
  matrimonios <- which(edge_types == "marriage")
  for (m in matrimonios) {
    pareja <- edge_ends[m, ]
    hijos_pareja <- neighborhood(grafo, order = 1, nodes = pareja, mode = "out")[[1]]
    if (length(hijos_pareja) > 0) {
      for (h in hijos_pareja) {
        padres_hijo <- neighborhood(grafo, order = 1, nodes = h, mode = "in")[[1]]
        if (!all(pareja %in% padres_hijo)) {
          resultados$padrastros <- TRUE
          break
        }
      }
    }
    if (resultados$padrastros) break
  }
  
  # Suegros
  if (length(matrimonios) > 0) {
    for (m in matrimonios) {
      pareja <- ends(grafo, m)
      for (p in pareja) {
        padres <- neighborhood(grafo, order = 1, nodes = p, mode = "in")[[1]]
        padres <- padres[E(grafo)[.from(padres) & .to(p)]$type == "descent"]
        if (length(padres) > 0) {
          resultados$suegros <- TRUE
          break
        }
      }
      if (resultados$suegros) break
    }
  }
  
  # Nodos aislados
  resultados$nodos_aislados <- any(degree(grafo) == 0)
  
  # Monoparental
  padres <- which(degree(grafo, mode = "out") > 0)
  if (length(padres) == 1 && sum(edge_types == "marriage") == 0) {
    resultados$monoparental <- TRUE
  }
  
  return(resultados)
}

# Aplicar a cada tipología
clasificaciones <- lapply(examples, clasificar_tipologia)
df_clasificacion <- bind_rows(clasificaciones, .id = "Tipologia") %>%
  mutate(across(where(is.logical), as.numeric))

write.csv(df_clasificacion, "Análisis de viviendas/Analisis/Resultados_tipologias/matriz_clasificacion.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. CLASIFICACIÓN EN MACROGRUPOS (BASE) Y SUBCATEGORÍAS PARA EXTENDIDAS
# ------------------------------------------------------------------------------
# Primero unir con los datos de frecuencias y aristas (suponiendo que tipologia_df tiene 'Aristas')
# Asegurarse de que tipologia_df contenga la columna 'Aristas' (número de aristas del grafo).
# Si no, se puede agregar después.
df_final <- tipologia_df %>%
  left_join(df_clasificacion, by = "Tipologia")

# --- Definir condiciones básicas (usando las variables binarias y Aristas) ---
df_final <- df_final %>%
  mutate(
    es_aislada = (nodos_aislados == 1 & Aristas == 0),
    es_pareja_sin_hijos = (pareja_sin_hijos == 1),
    es_monoparental = (monoparental == 1),
    # Nuclear tradicional: sin ninguna característica compleja
    es_nuclear_trad = (!es_aislada & !es_pareja_sin_hijos & !es_monoparental &
                         intergeneracional == 0 & padrastros == 0 & suegros == 0 &
                         !(nodos_aislados == 1 & Aristas > 0)),
    # Extendida: todo lo que no es ninguna de las anteriores
    es_extendida = (!es_aislada & !es_pareja_sin_hijos & !es_monoparental & !es_nuclear_trad)
  )

# Asignación de macrogrupo base (mutuamente excluyente)
df_final <- df_final %>%
  mutate(
    macrogrupo = case_when(
      es_aislada ~ "Aislada",
      es_pareja_sin_hijos ~ "Pareja sin hijos",
      es_monoparental ~ "Monoparental",
      es_nuclear_trad ~ "Nuclear tradicional",
      es_extendida ~ "Extendida",
      TRUE ~ "Otro"
    )
  )

# --- Subcategorías para los hogares Extendida ---
df_final <- df_final %>%
  mutate(
    subcategoria = case_when(
      macrogrupo != "Extendida" ~ NA_character_,
      
      # Prioridad: intergeneracional (aunque tenga otras características)
      intergeneracional == 1 ~ "Intergeneracional",
      
      # Reconstituida
      padrastros == 1 ~ "Reconstituida",
      
      # Con suegros
      suegros == 1 ~ "Con suegros",
      
      # Pareja extendida: cuando hay una pareja (sin hijos) y el resto son no parientes o parientes lejanos
      # Detectamos: si hay exactamente un marriage y ningún hijo, y nodos_aislados o presencia de otros
      (pareja_sin_hijos == 0) & (sum(intergeneracional, padrastros, suegros) == 0) &
        (nodos_aislados == 1 & Aristas > 0) ~ "Pareja extendida",
      
      # Otros casos: por ejemplo, nodos aislados con conexiones pero sin marriage/padres específicos
      nodos_aislados == 1 & Aristas > 0 ~ "Con otros miembros",
      
      # Resto
      TRUE ~ "Extendida simple"
    )
  )

# Guardar resultados de clasificación
df_macro <- df_final %>%
  select(Tipologia, macrogrupo, subcategoria)

write.csv(df_macro, "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_subcategorias.csv", row.names = FALSE)

# Tablas resumen
cat("\nDistribución de macrogrupos base:\n")
print(table(df_macro$macrogrupo))

cat("\nDistribución de subcategorías dentro de Extendida:\n")
print(table(df_macro$subcategoria, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 4. (OPCIONAL) CLUSTERING DE TIPOLOGÍAS (para validación)
# ------------------------------------------------------------------------------
# Si deseas mantener el análisis de clusters (aunque no lo usarás como clasificación principal),
# puedes ejecutar el siguiente bloque. Si no, coméntalo.

# Preparar matriz de características (6 variables binarias)
df_filtrado <- df_clasificacion[1:48, ]
matrix_clasificacion <- as.matrix(df_filtrado[, -1])
rownames(matrix_clasificacion) <- df_filtrado$Tipologia
colnames(matrix_clasificacion) <- names(df_filtrado)[-1]

d <- dist(matrix_clasificacion, method = "binary")

# Comparar métodos
sil_width_kmeans <- map_dbl(2:10, ~{
  kmeans_result <- kmeans(matrix_clasificacion, centers = .x, nstart = 25)
  mean(silhouette(kmeans_result$cluster, dist(matrix_clasificacion))[, 3])
})

sil_width_pam <- map_dbl(2:10, ~{
  pam_fit <- pam(d, k = .x, diss = TRUE)
  pam_fit$silinfo$avg.width
})

methods <- c("ward.D2", "complete", "average", "mcquitty")
cophenetic_cors <- map_dbl(methods, ~cor(d, cophenetic(hclust(d, method = .x))))

# Cluster final (k=5 como ejemplo)
hc_final <- agnes(d, method = "average")
clust <- cutree(hc_final, k = 5)

# Dendrograma
png("Análisis de viviendas/Analisis/Resultados_tipologias/dendrograma_5clusters.png",
    width = 1200, height = 800, res = 150)
par(mar = c(8,4,4,2)+0.1)
pltree(hc_final, cex = 0.6, hang = -1, main = "Dendrograma tipologías - 5 Clusters")
rect.hclust(hc_final, k = 5, border = 2:5)
dev.off()

# Integrar clusters a df_final
clust_df <- data.frame(Tipologia = rownames(matrix_clasificacion), Cluster = as.factor(clust))
df_final <- df_final %>% left_join(clust_df, by = "Tipologia")

# Estadísticas por cluster
cluster_stats <- df_final %>%
  group_by(Cluster) %>%
  summarise(
    n_tipologias = n(),
    freq_total = sum(Frecuencia, na.rm = TRUE),
    across(c(intergeneracional, padrastros, suegros, nodos_aislados,
             monoparental, pareja_sin_hijos), ~mean(., na.rm = TRUE), .names = "prop_{.col}")
  )

write.csv(cluster_stats, "Análisis de viviendas/Analisis/Resultados_tipologias/estadisticas_clusters.csv", row.names = FALSE)

# Mostrar resultados
cat("\nDistribución de clusters:\n")
print(table(clust))
cat("\nEstadísticas por cluster:\n")
print(cluster_stats)

# Opcional: guardar df_final completo
write.csv(df_final, "Análisis de viviendas/Analisis/Resultados_tipologias/df_final_completo.csv", row.names = FALSE)