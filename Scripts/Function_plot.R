################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ PLOTEADOR REDES ###################################
################################################################################

###### LIBRERÍAS ######
library(igraph)
library(kinship2)
library(tidyverse)

# Función para definir el ploteo de redes
plot_network <- function(net, net_name, shapes, colrs, layout_type, graph_title) {
  plot(net,
       vertex.shape = shapes[V(net)$sex],
       vertex.color = colrs[V(net)$sex],
       vertex.size = log(V(net)$edad) * 4.5,
       vertex.label = V(net)$`vivienda_i$id_persona`,
       vertex.label.cex = 0.5,
       edge.width = 1,
       edge.arrow.size = 0.5,
       layout = layout_type,
       main = paste(graph_title, " - ", net_name)
  )
}

# Definir triángulo como una forma personalizada
mytriangle <- function(coords, v = NULL, params) {
  vertex.color <- params("vertex", "color")
  vertex.size <- 1/200 * params("vertex", "size")
  symbols(x = coords[, 1], y = coords[, 2], bg = vertex.color,
          stars = cbind(vertex.size, vertex.size, vertex.size),
          crt = 90, add = TRUE, inches = FALSE)
}

# Agregar la forma de triángulo
add_shape("triangle", clip = shapes("circle")$clip, plot = mytriangle)

# Definir colores y formas
shapes <- c("square", "circle")
colrs <- c("lightblue", "pink")

# Cambiar por el ID que corresponda
g <- "1101100101"
kinship_net <- grafos[g][[1]]$kinship_net
descent_net <- grafos[g][[1]]$descent_net
marriage_net <- grafos[g][[1]]$marriage_net

# Ploteo de las redes
plot_network(kinship_net, "Kinship", shapes, colrs, layout.reingold.tilford(kinship_net, mode = "all"), grafos[g][[1]]$i)
plot_network(descent_net, "Descent", shapes, colrs, layout.reingold.tilford(descent_net, mode = "in"), grafos[g][[1]]$i)
plot_network(marriage_net, "Marriage", shapes, colrs, layout.auto, grafos[g][[1]]$i)

# Ploteador versión Pedigree
a <- as.data.frame(get.edgelist(descent_net))
b <- as.data.frame(get.vertex.attribute(descent_net))
names(a) <- c("parent", "child")

# Identificar padres y convertir el formato
parent_sex <- inner_join(a, b)
names(parent_sex) <- c("parent", "name", "sex_parent", "edad")

inter <- parent_sex %>% 
  pivot_wider(names_from = sex_parent, values_from = parent, id_cols = name) %>%
  rename(momid = `2`, dadid = `1`)

pre_ped <- left_join(b, inter)

# Ajustar los padres en el formato de kinship2
pre_ped <- fixParents(id = pre_ped$name, dadid = pre_ped$dadid, momid = pre_ped$momid, sex = pre_ped$sexo)
ped <- pedigree(id = pre_ped$id, dadid = pre_ped$dadid, momid = pre_ped$momid, sex = pre_ped$sexo, missid = 0)

# Ploteo del pedigree
plot(ped)