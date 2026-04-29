################################################################################
###################### Social Network: encuesta CASEN ##########################
###################### 7. CLASIFICACIÓN DE TIPOLOGÍAS ##########################
################################################################################
library(cluster)
library(dplyr)
library(factoextra)
library(igraph)
library(purrr)
library(tidyr)

# ------------------------------------------------------------------------------
# 1. CARGAR DATOS Y GENERAR EJEMPLOS
# ------------------------------------------------------------------------------
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")
load("Análisis de viviendas/Data/fingerprints.RData")
tipologia_df <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv")

freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]
names(tipologia_freq) <- paste0("T", seq_along(unique_fps))

tipologias_validas <- paste0("T", 1:48)
tipologia_df <- tipologia_df %>% filter(Tipologia %in% tipologias_validas)

# Grafo ejemplo (sin atributos) por cada fingerprint
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))

# ------------------------------------------------------------------------------
# 2. FUNCIONES PARA DETECTAR ESTRUCTURAS FAMILIARES
# ------------------------------------------------------------------------------

# Detección de cadena intergeneracional (abuelo→padre→hijo)
detectar_intergeneracional <- function(grafo) {
  if (vcount(grafo) < 3) return(FALSE)
  for (v in V(grafo)) {
    padres <- neighbors(grafo, v, mode = "in")
    for (p in padres) {
      abuelos <- neighbors(grafo, p, mode = "in")
      if (length(abuelos) >= 1) {
        # Verificar tipo "descent" en las aristas
        edge_vp <- E(grafo)[p %->% v]     # padre→hijo
        edge_gp <- E(grafo)[abuelos %->% p]  # abuelo→padre
        if (length(edge_vp)>0 && length(edge_gp)>0 &&
            edge_attr(grafo, "type", edge_vp) == "descent" &&
            all(edge_attr(grafo, "type", edge_gp) == "descent")) {
          return(TRUE)
        }
      }
    }
    hijos <- neighbors(grafo, v, mode = "out")
    for (h in hijos) {
      nietos <- neighbors(grafo, h, mode = "out")
      if (length(nietos) >= 1) {
        edge_vh <- E(grafo)[v %->% h]
        edge_hn <- E(grafo)[h %->% nietos]
        if (length(edge_vh)>0 && length(edge_hn)>0 &&
            edge_attr(grafo, "type", edge_vh) == "descent" &&
            all(edge_attr(grafo, "type", edge_hn) == "descent")) {
          return(TRUE)
        }
      }
    }
  }
  return(FALSE)
}

# Clasificación completa de una tipología
clasificar_tipologia <- function(grafo) {
  edge_types <- E(grafo)$type
  edge_ends <- ends(grafo, E(grafo))
  num_vertices <- vcount(grafo)
  num_edges <- ecount(grafo)
  
  resultados <- list(
    intergeneracional = FALSE,
    padrastros      = FALSE,
    suegros         = FALSE,
    nodos_aislados  = FALSE,
    monoparental    = FALSE,
    pareja_sin_hijos = FALSE
  )
  
  # ---- Pareja sin hijos ----
  if (num_vertices == 2 && num_edges == 2 && "marriage" %in% edge_types) {
    resultados$pareja_sin_hijos <- TRUE
    return(resultados)
  }
  
  # ---- Intergeneracional ----
  resultados$intergeneracional <- detectar_intergeneracional(grafo)
  
  # ---- Padrastros / madrastras ----
  matrimonios <- which(edge_types == "marriage")
  for (m in matrimonios) {
    pareja <- edge_ends[m, ]
    hijos_pareja <- unique(unlist(neighborhood(grafo, order = 1, nodes = pareja, mode = "out")))
    # Quitar los propios cónyuges
    hijos <- setdiff(hijos_pareja, pareja)
    for (h in hijos) {
      padres_h <- neighbors(grafo, h, mode = "in")
      # Si al menos uno de los cónyuges no es padre biológico (no hay arista descent)
      if (!all(pareja %in% padres_h)) {
        resultados$padrastros <- TRUE
        break
      }
    }
    if (resultados$padrastros) break
  }
  
  # ---- Suegros ----
  if (length(matrimonios) > 0) {
    for (m in matrimonios) {
      pareja <- ends(grafo, m)
      for (p in pareja) {
        in_edges <- incident(grafo, p, mode = "in")
        if (length(in_edges) > 0) {
          if (any(edge_attr(grafo, "type", in_edges) == "descent")) {
            resultados$suegros <- TRUE
            break
          }
        }
      }
      if (resultados$suegros) break
    }
  }
  
  # ---- Nodos aislados ----
  resultados$nodos_aislados <- any(degree(grafo) == 0)
  
  # ---- Monoparental ----
  if (sum(edge_types == "marriage") == 0) {
    padres <- which(degree(grafo, mode = "out") > 0)
    if (length(padres) == 1) {
      resultados$monoparental <- TRUE
    }
  }
  
  return(resultados)
}

clasificaciones <- lapply(examples, clasificar_tipologia)

df_clasificacion <- bind_rows(clasificaciones, .id = "Tipologia") %>%
  mutate(across(where(is.logical), as.numeric))

write.csv(df_clasificacion,
          "Análisis de viviendas/Analisis/Resultados_tipologias/matriz_clasificacion.csv",
          row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. MACROTIPOLOGÍAS
# ------------------------------------------------------------------------------

df_final <- tipologia_df %>% 
  left_join(df_clasificacion, by = "Tipologia")

df_final <- df_final %>% 
  mutate(
    es_aislada = (nodos_aislados == 1 & Aristas == 0),
    es_pareja_sin_hijos = (pareja_sin_hijos ==1),
    es_monoparental = (monoparental == 1& !(nodos_aislados ==1 & Aristas >0)),
    es_nuclear_trad = (!es_aislada & !es_pareja_sin_hijos & !es_monoparental & intergeneracional == 0 & padrastros == 0 & suegros == 0 & !(nodos_aislados ==1 & Aristas >0)),
    es_extendida = (!es_aislada & !es_pareja_sin_hijos & !es_monoparental & !es_nuclear_trad)
    
  )

df_final <- df_final %>% 
  mutate(
    macrogrupo = case_when(
      es_aislada ~ "Isolated",
      es_pareja_sin_hijos ~ "Chidless Couple",
      es_monoparental ~ "Single-Parent",
      es_nuclear_trad ~ "Tradicional Nuclear",
      es_extendida ~ "Extended",
      TRUE ~ "Otro"
    )
  )

# Subcategorías para hogares Extendida
df_final <- df_final %>%
  mutate(
    # Calcular el número de rasgos de complejidad presentes (solo para extendidas)
    n_rasgos = if_else(
      macrogrupo == "Extended",
      (intergeneracional == 1) +
        (padrastros == 1) +
        (suegros == 1) +
        ((nodos_aislados == 1 & Aristas > 0) & !(pareja_sin_hijos == 0 & intergeneracional == 0 & padrastros == 0)) +
        (monoparental == 1),
      NA_integer_
    ),
    
    subcategoria = case_when(
      macrogrupo != "Extended" ~ NA_character_,
      n_rasgos >= 2 ~ "Complex",
      #intergeneracional == 1 ~ "Intergenerational",
      padrastros == 1 ~ "Reconstituted",
      (nodos_aislados == 1 & Aristas > 0) &
        (pareja_sin_hijos == 0 & intergeneracional == 0 & padrastros == 0 & suegros == 0 & monoparental == 0) ~ "Extended couple",
      monoparental == 1 ~ "Extended single-parent",
      TRUE ~ "Complex"
    )
  ) %>% select(-n_rasgos)

df_macro <- df_final %>% 
  select(Tipologia, macrogrupo, subcategoria, intergeneracional)

write.csv(df_macro, "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipo.csv", row.names = FALSE)
