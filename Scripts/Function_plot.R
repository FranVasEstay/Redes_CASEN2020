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

################################################################################
############################ REDES DE PRUEBA ###################################
################################################################################
# Cargar redes previamente creadas
load("Redes/descent_igrpah_subset1000.RData")
load("Redes/marriage_igrpah_subset1000.RData")
load("Redes/dependency_network_subset1000.RData")
load("Redes/kinship_igrpah_subset1000.RData")

########################## RED DE MATRIMONIO ###################################
plot_network_matri <- function(network_list, output_file) {
  
  # Iniciar el dispositivo PDF
  pdf(file = output_file, width = 10, height = 8)  # Puedes ajustar el tamaño según sea necesario
  
  # Definir formas y colores para los nodos
  shapes <- c("square", "circle")
  colors <- c("lightblue", "pink")
  
  # Iterar sobre cada red en la lista
  for (i in seq_along(network_list)) {
    # Extraer información del household_i
    household_id <- network_list[[i]]$household_i
    
    # Extraer la red de matrimonio
    marriage_net <- network_list[[i]]$marriage_net
    
    # Verificar si la red de matrimonio no está vacía
    if (!is.null(marriage_net) && vcount(marriage_net) > 0) {
      # Gráfico de la red de matrimonio
      plot(marriage_net,
           vertex.shape=shapes[V(marriage_net)$sex],
           vertex.color=colors[V(marriage_net)$sex],
           vertex.size=log(V(marriage_net)$edad + 1) * 4.5,  # Sumar 1 para evitar log(0)
           vertex.label=V(marriage_net)$name,
           vertex.label.cex=.5,
           edge.width=1,
           edge.arrow.size=.5,
           main=paste("Marriage Network for Household ID:", household_id))
    } else {
      message(paste("No marriage network for Household ID:", household_id))
    }

  }
  # Cerrar el dispositivo PDF
  dev.off()
}

# Ejecutar la función para visualizar todas las redes
output_file <- "Redes/visualizaciones_marriage_sample.pdf"
plot_network_matri(marriage_igrpah_sample,output_file)

########################## RED DE DESCENDENCIA #################################
plot_network_desce <- function(network_list, output_file) {
  
  # Iniciar el dispositivo PDF
  pdf(file = output_file, width = 10, height = 8)  # Puedes ajustar el tamaño según sea necesario
  
  # Definir formas y colores para los nodos
  shapes <- c("square", "circle")
  colors <- c("lightblue", "pink")
  
  # Iterar sobre cada red en la lista
  for (i in seq_along(network_list)) {
    # Extraer información del household_i
    household_id <- network_list[[i]]$household_i
    
    # Extraer la red de descendencia
    descent_net <- network_list[[i]]$descent_net  # Verifica que este sea el nombre correcto
    
    # Verificar si la red de descendencia no está vacía
    if (!is.null(descent_net) && vcount(descent_net) > 0) {
      # Gráfico de la red de descendencia
      plot(descent_net,
           vertex.shape=shapes[V(descent_net)$sex],
           vertex.color=colors[V(descent_net)$sex],
           vertex.size=log(V(descent_net)$edad + 1) * 4.5,  # Sumar 1 para evitar log(0)
           vertex.label=V(descent_net)$name,
           vertex.label.cex=.5,
           edge.width=1,
           edge.arrow.size=.5,
           main=paste("Descend Network for Household ID:", household_id))  # Imprimir el ID aquí
    } else {
      message(paste("No Descend network for Household ID:", household_id))
    }
  }
  # Cerrar el dispositivo PDF
  dev.off()
}

# Ejecutar la función para visualizar todas las redes
output_file <- "Redes/visualizaciones_descent_sample.pdf"
plot_network_desce(descent_igrpah_sample,output_file)

########################## RED DE DEPENDENCIA ##################################
plot_network_depen <- function(network_list, output_file) {
  
  # Iniciar el dispositivo PDF
  pdf(file = output_file, width = 10, height = 8)  # Puedes ajustar el tamaño según sea necesario
  
  # Definir formas y colores para los nodos
  shapes <- c("square", "circle")
  colors <- c("lightblue", "pink")
  
  # Iterar sobre cada red en la lista
  for (i in seq_along(network_list)) {
    # Extraer información del household_i
    household_id <- network_list[[i]]$household_i
    
    # Extraer la red de dependencia
    dependency_net <- network_list[[i]]$dependency_net
    
    # Verificar si la red de dependencia no está vacía
    if (!is.null(dependency_net) && vcount(dependency_net) > 0) {
      # Gráfico de la red de dependencia
      plot(dependency_net,
           vertex.shape=shapes[V(dependency_net)$sex],
           vertex.color=colors[V(dependency_net)$sex],
           vertex.size=log(V(dependency_net)$edad + 1) * 4.5,  # Sumar 1 para evitar log(0)
           vertex.label=V(dependency_net)$name,
           vertex.label.cex=.5,
           edge.width=1,
           edge.arrow.size=.5,
           main=paste("Dependency Network for Household ID:", household_id))
    } else {
      message(paste("No Dependency network for Household ID:", household_id))
    }
    
  }
  # Cerrar el dispositivo PDF
  dev.off()
}

output_file <- "Redes/visualizaciones_dependency_sample.pdf"
plot_network_depen(dependency_igrpah_sample,output_file)

############################ RED KINSHIP #######################################
plot_network_kinship <- function(network_list, output_file) {
  
  # Iniciar el dispositivo PDF
  pdf(file = output_file, width = 10, height = 8)  # Puedes ajustar el tamaño según sea necesario
  
  # Definir formas y colores para los nodos
  shapes <- c("square", "circle")
  colors <- c("lightblue", "pink")
  
  # Iterar sobre cada red en la lista
  for (i in seq_along(network_list)) {
    # Extraer información del household_i
    household_id <- network_list[[i]]$household_i
    
    # Extraer la red de kinship
    kinship_net <- network_list[[i]]$kinship_net
    
    # Verificar si la red de matrimonio no está vacía
    if (!is.null(kinship_net) && vcount(kinship_net) > 0) {
      # Gráfico de la red de matrimonio
      plot(kinship_net,
           vertex.shape=shapes[V(kinship_net)$sex],
           vertex.color=colors[V(kinship_net)$sex],
           vertex.size=log(V(kinship_net)$edad + 1) * 4.5,  # Sumar 1 para evitar log(0)
           vertex.label=V(kinship_net)$name,
           vertex.label.cex=.5,
           edge.width=1,
           edge.arrow.size=.5,
           main=paste("Kinship Network for Household ID:", household_id))
    } else {
      message(paste("No Kinship network for Household ID:", household_id))
    }
    
  }
  # Cerrar el dispositivo PDF
  dev.off()
}

output_file <- "Redes/visualizaciones_kinship_sample.pdf"
plot_network_kinship(kinship_igrpah_sample,output_file)


################################################################################
############################ REDES COMPLETAS ###################################
################################################################################
# Cargar redes previamente creadas
load("Redes/descent_igrpah.RData")
load("Redes/marriage_igrpah.RData")
load("Redes/dependency_network.RData")
load("Redes/kinship_igrpah.RData")

########################## RED DE MATRIMONIO ###################################

# Ejecutar la función para visualizar todas las redes
output_file <- "Redes/visualizaciones.pdf"
plot_network_matri(marriage_igrpah,output_file)

########################## RED DE DESCENDENCIA #################################

# Ejecutar la función para visualizar todas las redes
output_file <- "Redes/visualizaciones.pdf"
plot_network_desce(descent_igrpah,output_file)

########################## RED DE DEPENDENCIA ##################################

output_file <- "Redes/visualizaciones.pdf"
plot_network_depen(dependency_igrpah,output_file)

############################ RED KINSHIP #######################################

output_file <- "Redes/visualizaciones_kinship.pdf"
plot_network_kinship(kinship_igrpah,output_file)

