################################################################################
###################### CLASIFICACIÓN DE TIPOLOGÍAS #############################
################################################################################

library(igraph)
library(dplyr)

#GENERAR EXAMPLES
## Cargar datos de redes
load("Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")
load("Análisis de viviendas/Data/fingerprints.RData")
tipologia_df <- read.csv("Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/resumen_tipologias.csv")

# Contar frecuencias de cada fingerprint único
freq_table <- table(fingerprints)
unique_fps <- names(sort(freq_table, decreasing = TRUE))
tipologia_freq <- freq_table[unique_fps]  # Reordenamos la tabla
names(tipologia_freq) <- paste0("T", seq_along(unique_fps))

# Paso 3. Mostrar la tabla
print(tipologia_freq)
tipologias_validas <- paste0("T", 1:48)
tipologia_df <- tipologia_df %>% filter(Tipologia %in% tipologias_validas)

# 5. Guardar un grafo de ejemplo por tipología
# Solo mantener redes de T1 a T48
examples <- lapply(tipologias_validas, function(tip) {
  idx <- which(names(tipologia_freq) == tip)
  fp <- unique_fps[idx]
  grafo <- kinship_igraph_no_attrs[[which(fingerprints == fp)[1]]]$kinship_net
  return(grafo)
})
names(examples) <- tipologias_validas

# Función independiente para detectar estructuras intergeneracionales
detectar_intrgeneracional <- function(grafo) {
  if (vcount(grafo) < 3) return(FALSE)
  
  descent_edges <- E(grafo)[type == "descent"]
  descent_grafo <- subgraph.edges(grafo, descent_edges, delete.vertices = FALSE)
  
  # Buscar caminos simples de largo exactamente 2: abuelo -> padre -> nieto
  caminos_largo_2 <- all_simple_paths(descent_grafo, mode = "out", cutoff = 2)
  
  for (camino in caminos_largo_2) {
    if (length(camino) == 3) {
      return(TRUE)  # Encontrado un camino intergeneracional
    }
  }
  return(FALSE)
}

# Función para detectar estructuras familiares en un grafo
clasificar_tipologia <- function(grafo) {
  # Extraer atributos de las aristas (relaciones)
  edge_types <- E(grafo)$type
  edge_ends <- ends(grafo, E(grafo))
  
  # Inicializar vector de resultados
  resultados <- list(
    intergeneracional = FALSE,
    padrastros = FALSE,
    suegros = FALSE,
    nodos_aislados = FALSE,
    monoparental = FALSE
  )
  
  # 1. Estructura intergeneracional (abuelo-padre-nieto) 
  # USAR NEIHBORHOOD
  resultados$intergeneracional <- detectar_intergeneracional(grafo)
  
  # 2. Presencia de padrastros/madrastras
  # Relación marriage donde al menos un hijo no es de ambos
  # VERIFICA SI TODOS LOS HIJOS TIENEN AMBOS PADRES EN LA RED
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
  
  # 3. Presencia de suegros
  # Un nodo conectado por marriage a otro que tiene hijos
  # SE USA NEIGHBORHOOD AQUÍ TAMBIÉN
  resultados$suegros <- FALSE
  matrimonios <- which(E(grafo)$type == "marriage")
  
  if (length(matrimonios) > 0) {
    for (m in matrimonios) {
      pareja <- ends(grafo, m)
      
      # Verificar si ALGÚN MIEMBRO DE LA PAREJA TIENE PADRES EN EL GRAFO
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
  
  # 4. Nodos aislados
  resultados$nodos_aislados <- any(degree(grafo) == 0)
  
  # 5. Estructura monoparental
  # Solo un padre/madre con hijos, sin relación marriage
  padres <- which(degree(grafo, mode = "out") > 0)
  if (length(padres) == 1 && sum(edge_types == "marriage") == 0) {
    resultados$monoparental <- TRUE
  }
  
  return(resultados)
}

# Aplicar a todas las tipologías
clasificaciones <- lapply(examples, clasificar_tipologia)

# Convertir a dataframe
df_clasificacion <- bind_rows(clasificaciones, .id = "Tipologia") %>%
  mutate(across(where(is.logical), as.numeric))  # Convertir TRUE/FALSE a 1/0

# Unir con datos de frecuencia
df_final <- tipologia_df %>%
  left_join(df_clasificacion, by = "Tipologia")

# Guardar resultados
write.csv(df_final, "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_familiares.csv", row.names = FALSE)

#### MACROGRUPOS ####
df_macro <- df_final %>%
  select(Tipologia, everything()) %>%
  mutate(
    macrogrupo = case_when(
      intergeneracional == 1 & padrastros == 0 & suegros == 0 & monoparental == 0 & nodos_aislados == 0 ~ "Intergeneracional",
      intergeneracional == 1 & (suegros == 1 | nodos_aislados == 1) ~ "Extendida",
      padrastros == 1 ~ "Reconstituida (presencia de mapadrastros)",
      monoparental == 1 ~ "Monoparental",
      nodos_aislados == 1 ~ "Aislada",
      TRUE ~ "Nuclear tradicional"
    ) 
  )%>%
  select(Tipologia,macrogrupo)
table(df_macro$macrogrupo)
write.csv(df_macro, "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_macrogrupos.csv", row.names = FALSE)
