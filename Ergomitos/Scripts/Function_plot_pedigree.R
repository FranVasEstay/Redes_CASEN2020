#### Plot de tipo pedigree

library(igraph)
library(dplyr)
library(tidyr)
library(pedigree)

# Función para construir el pedigree
plot_pedigree <- function(network_list, output_file) {
  
  # Iniciar el dispositivo PDF
  pdf(file = output_file, width = 10, height = 8)
  
  # Iterar sobre cada red en la lista
  for (i in seq_along(network_list)) {
    household_id <- network_list[[i]]$household_i
    descent_net <- network_list[[i]]$descent_net
    
    # Verificar si la red de descendencia no está vacía
    if (!is.null(descent_net) && vcount(descent_net) > 0) {
      
      # Obtener los edgelists y atributos de los vértices
      edge_list <- as.data.frame(get.edgelist(descent_net))
      vertex_names <- vertex_attr(descent_net, "name")
      vertex_sex <- vertex_attr(descent_net, "sex")
      
      # Comprobar si hay información suficiente
      if (nrow(edge_list) == 0 || length(vertex_names) == 0) {
        message(paste("No edges or vertices for Household ID:", household_id))
        plot.new()
        title(main = paste("No data for Household ID:", household_id))
        next
      }
      
      # Renombrar las columnas
      names(edge_list) <- c("parent", "child")
      vertex_attributes <- data.frame(name = vertex_names, sex = vertex_sex)
      
      # Combinar información de padres e hijos
      parent_info <- edge_list %>%
        left_join(vertex_attributes, by = c("parent" = "name")) %>%
        rename(sex_parent = sex) %>%
        select(parent, sex_parent, child)
      
      child_info <- vertex_attributes %>%
        rename(child = name)
      
      pedigree_data <- parent_info %>%
        left_join(child_info, by = "child") %>%
        rename(sex_child = sex) %>%
        select(child, parent, sex_parent, sex_child)
      
      # Verificar si hay datos suficientes para crear el pedigree
      if (nrow(pedigree_data) == 0 || all(is.na(pedigree_data$child))) {
        message(paste("No valid pedigree data for Household ID:", household_id))
        plot.new()
        title(main = paste("No valid pedigree for Household ID:", household_id))
        next
      }
      
      # Crear un data frame para el pedigree
      ped_df <- data.frame(
        id = pedigree_data$child,
        dadid = NA,
        momid = NA,
        sex = pedigree_data$sex_child
      )
      
      # Asignar padres
      ped_df$dadid <- ifelse(pedigree_data$sex_parent == 1, pedigree_data$parent, NA)
      ped_df$momid <- ifelse(pedigree_data$sex_parent == 2, pedigree_data$parent, NA)
      
      # Filtrar los id de padre y madre que están en el id
      ped_df <- ped_df %>%
        mutate(
          dadid = ifelse(dadid %in% ped_df$id, dadid, NA),
          momid = ifelse(momid %in% ped_df$id, momid, NA)
        )
      
      # Verificar longitudes de columnas
      if (length(ped_df$id) == 0 || length(ped_df$dadid) == 0 || length(ped_df$momid) == 0) {
        message(paste("Different lengths for Household ID:", household_id))
        plot.new()
        title(main = paste("Different lengths for Household ID:", household_id))
        next
      }
      
      # Crear el pedigree solo si no hay NAs en id, dadid y momid
      if (all(!is.na(ped_df$id)) && all(!is.na(ped_df$dadid)) && all(!is.na(ped_df$momid))) {
        ped <- pedigree(id = ped_df$id,
                        dadid = ped_df$dadid,
                        momid = ped_df$momid,
                        sex = ped_df$sex,
                        missid = 0)
        
        # Graficar el pedigree
        plot(ped, main = paste("Pedigree for Household ID:", household_id))
      } else {
        message(paste("NAs found in id, dadid, or momid for Household ID:", household_id))
        plot.new()
        title(main = paste("Incomplete data for Household ID:", household_id))
      }
    } else {
      message(paste("No Descend network for Household ID:", household_id))
      plot.new()
      title(main = paste("No Descend network for Household ID:", household_id))
    }
  }
  
  # Cerrar el dispositivo PDF
  dev.off()
}

# Ejecutar la función para visualizar los pedigrees
output_file <- "Redes/visualizaciones_pedigree_sample.pdf"
plot_pedigree(descent_igrpah_sample, output_file)

