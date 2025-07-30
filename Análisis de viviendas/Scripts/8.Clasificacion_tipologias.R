################################################################################
###################### Social Network: encuesta CASEN ##########################
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
examples <- lapply(unique_fps, function(fp) {
  idx <- which(fingerprints == fp)[1]
  kinship_igraph_no_attrs[[idx]]$kinship_net
})
names(examples) <- paste0("T", seq_along(unique_fps))  # Asignar nombres T1, T2, ...

# Función independiente para detectar estructuras intergeneracionales
detectar_intergeneracional <- function(grafo) {
  if (vcount(grafo) < 3) return(FALSE)
  
  # Buscar cadenas puras de 3 generaciones conectadas por 'descent'
  for (v in V(grafo)) {
    # Cadena ascendente: abuelo -> padre -> ego
    padres <- neighbors(grafo, v, mode = "in")
    if (length(padres) >= 1) {
      for (p in padres) {
        abuelos <- neighbors(grafo, p, mode = "in")
        if (length(abuelos) >= 1) {
          # Verificar conexiones consecutivas
          if (all(E(grafo)[p %->% v]$type == "descent") && 
              all(E(grafo)[abuelos %->% p]$type == "descent")) {
            return(TRUE)
          }
        }
      }
    }
    
    # Cadena descendente: ego -> hijo -> nieto
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
  rename(tipologia = Tipologia) %>% 
  select(tipologia, everything()) %>%
  mutate(
    macrogrupo = case_when(
      intergeneracional == 1 & padrastros == 0 & suegros == 0 & monoparental == 0 & nodos_aislados == 0 ~ "Intergeneracional",
      (suegros == 1 | nodos_aislados == 1) & Aristas != 0 ~ "Extendida",
      padrastros == 1 ~ "Reconstituida", # (presencia de mapadrastros)
      monoparental == 1 ~ "Monoparental",
      nodos_aislados == 1 & Aristas == 0 ~ "Aislada",
      TRUE ~ "Nuclear tradicional"
    ) 
  )%>%
  select(tipologia,macrogrupo)
table(df_macro$macrogrupo)
write.csv(df_macro, "Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_macrogrupos.csv", row.names = FALSE)
