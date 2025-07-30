################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
######################## REDES SIN VAR EN LOS NODOS ############################
################################################################################

###### LIBRERÍAS ######
library(tidyverse)
library(igraph)
library(haven)
library(tibble)
library(reshape2)
library(tryCatchLog)
library(futile.logger)
library(dplyr)
library(tidyr)
library(furrr)
library(doParallel)
library(iterators)
library(parallel)
library(progress)
library(doSNOW)
library(progress)
library(sjmisc)

######################## QUITAR ATRIBUTOS DE CADA RED ##########################
## Primero procesar los edges y luego quitar atributos.
load("Ergomitos/Redes/kinship_igrpah.RData") #Cargar red de Kinship
## Filtrar y procesar redes con más de un nodo
process_large_network_list <- function(raw_list, net_name, min_nodes = 3) {
  # Paso 1: Filtrar
  filtered <- lapply(raw_list, function(x) {
    if (inherits(x$kinship_net, "igraph") && vcount(x$kinship_net) >= min_nodes) {
      return(x)
    }
    return(NULL)
  })
  filtered <- Filter(Negate(is.null), filtered)
  
  # Paso 2: Procesar
  processed <- lapply(filtered, function(x) {
    g <- x$kinship_net
    edge_attr(g, "network_type") <- net_name
    vertex_attr(g, "household_id") <- x$household_i
    g
  })
  
  message(sprintf(
    "Procesamiento completado: %d redes válidas de %d originales",
    length(processed), length(raw_list)
  ))
  
  return(processed)
}

all_networks <- process_large_network_list(
  kinship_igrpah, 
  net_name = "kinship",
  min_nodes = 2
)
######################## QUITAR ATRIBUTOS DE LOS NODOS #########################
########################## RED DE DESCENDENCIA #################################
load("Ergomitos/Redes/descent_igrpah.RData")
# Eliminar todos los atributos de los nodos
# Función para eliminar todos los atributos de los nodos en una red
remove_all_vertex_attributes <- function(graph) {
  # Obtener los nombres de los atributos de los nodos
  vertex_attr_names <- vertex_attr_names(graph)
  
  # Eliminar cada atributo
  for (attr in vertex_attr_names) {
    graph <- delete_vertex_attr(graph, attr)
  }
  
  return(graph)
}

# Aplicar la función a cada red en la lista descent_igrpah
descent_igrpah_no_attrs <- lapply(descent_igrpah, function(network) {
  network$descent_net <- remove_all_vertex_attributes(network$descent_net)
  return(network)
})

# Guardar la lista de redes sin atributos
save(descent_igrpah_no_attrs, file = "Análisis de viviendas/Redes/descent_igrpah_no_attrs.RData")

########################## RED DE MATRIMONIO ###################################
load("Ergomitos/Redes/marriage_igraph.RData")
# Eliminar todos los atributos de los nodos
# Aplicar la función a cada red en la lista marriage_igraph
marriage_igraph_no_attrs <- lapply(marriage_igraph, function(network) {
  network$marriage_net <- remove_all_vertex_attributes(network$marriage_net)
  return(network)
})

# Guardar la lista de redes sin atributos
save(marriage_igraph_no_attrs, file = "Análisis de viviendas/Redes/marriage_igraph_no_attrs.RData")

########################## RED DE DEPENDENCIA ##################################
load("Ergomitos/Redes/dependency_igraph.RData")
# Eliminar todos los atributos de los nodos
# Aplicar la función a cada red en la lista dependency_igraph
dependency_igraph_no_attrs <- lapply(dependency_igraph, function(network) {
  network$dependency_net <- remove_all_vertex_attributes(network$dependency_net)
  return(network)
})

# Guardar la lista de redes sin atributos
save(dependency_igraph_no_attrs, file = "Análisis de viviendas/Redes/dependency_igraph_no_attrs.RData")

############################ RED KINSHIP #######################################
load("Ergomitos/Redes/kinship_igrpah.RData")
# Eliminar todos los atributos de los nodos
# Aplicar la función a cada red en la lista kinship_igraph
kinship_igraph_no_attrs <- lapply(kinship_igrpah, function(network) {
  network$kinship_net <- remove_all_vertex_attributes(network$kinship_net)
  return(network)
})

# Guardar la lista de redes sin atributos
save(kinship_igraph_no_attrs, file = "Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")


### Configuraciones únicas
library(igraph)

# Función para eliminar todos los atributos de los nodos en una red
remove_all_vertex_attributes <- function(graph) {
  for (attr in vertex_attr_names(graph)) {
    graph <- delete_vertex_attr(graph, attr)
  }
  return(graph)
}

# Eliminar atributos de nodos en cada red de kinship_igraph
kinship_igraph_no_attrs <- lapply(kinship_igrpah, function(network) {
  if (!is.null(network$kinship_net) && inherits(network$kinship_net, "igraph")) {
    network$kinship_net <- remove_all_vertex_attributes(network$kinship_net)
  }
  return(network)
})

# Guardar la lista de redes sin atributos
save(kinship_igraph_no_attrs, file = "Análisis de viviendas/Redes/kinship_igraph_no_attrs.RData")


