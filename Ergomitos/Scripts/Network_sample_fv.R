################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
######################## CREACIÓN DE REDES SAMPLE ##############################
############################# 1000 VIVIENDAS ###################################
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
library(doParallel)
library(iterators)
library(parallel)
library(progress)
library(doSNOW)
library(progress)
library(sjmisc)
library(intergraph)
library(network)

##################### ADMINISTRACIÓN DE LOS DATOS ##############################
load("Data/ori_Casen2020_rdata.RData")

data<- ori_Casen2020_STATA %>%
  select(id_vivienda, id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, r1b_pais_esp,nucleo, pco2, r3,s17,s28,y1,y1_preg, comuna, region) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = id_vivienda, sex = sexo) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    across(c(e6a,pco1, ecivil, pco2, r3, s17,o1, y1_preg), as_factor),
    r1b_pais_esp = ifelse(r1b_pais_esp == "", 1,
                          ifelse(r1b_pais_esp == "NO RESPONDE", 3, 2))
  )
save(data, file = "Data/Data.RData")
data <- data %>%
  mutate_all(~ ifelse(is.na(.), "No_aplica", .))

###Utiliza set de la data ###
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)
# Crear subset con los 1000 id_vivienda seleccionados
data_subset <- data %>%
  filter(household %in% id_vivienda_sample)
# Verificar el número de filas en el subset
nrow(data_subset)

#### Creacion de directorio ####
if (!dir.exists("Redes")) {
  dir.create("Redes")
}

########################### CREACION DE REDES ##################################
########################## RED DE DESCENDENCIA #################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores) # Corrige 'ncores' por 'num_cores'
registerDoParallel(cl)
start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[data_subset$household == i, ]
  
  # Crear lista de nodos con id_persona
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  # Crear los bordes de la red de descendencia
  edge_descent <- tibble(
    from = as.character(household_i$h5_1),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  
  # Si hay más de un padre (delimitado por "|"), separarlos
  if (any(grepl("\\|", edge_descent$from))) {
    edge_descent <- edge_descent %>%
      separate(from, c('parent1', 'parent2')) %>%
      pivot_longer(cols = c(parent1, parent2), names_to = NULL, values_drop_na = TRUE) %>%
      select(value, to) 
  }
  
  edge_descent$type <- "descent"
  edge_descent$color <- 1
  
  # Variables de los nodos
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s17", "s28", "y1", "y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear la red de descendencia
  descent_net <- graph_from_data_frame(d = edge_descent, vertices = nodes, directed = TRUE)
  
  # Verificar que la cantidad de nodos coincida con los atributos
  if (length(covariates$id_persona) == length(V(descent_net))) {
    V(descent_net)$sex <- as.integer(covariates$sex)
    V(descent_net)$edad <- as.integer(covariates$edad)
    V(descent_net)$e6a <- as.character(covariates$e6a)
    V(descent_net)$o1 <- as.character(covariates$o1)
    V(descent_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
    V(descent_net)$ecivil <- as.character(covariates$ecivil)
    V(descent_net)$r3 <- as.character(covariates$r3)
    V(descent_net)$s17 <- as.character(covariates$s17)
    V(descent_net)$s28 <- as.integer(covariates$s28)
    V(descent_net)$y1 <- as.character(covariates$y1)
    V(descent_net)$y1_preg <- as.character(covariates$y1_preg)
    V(descent_net)$comuna <- as.character(covariates$comuna)
    V(descent_net)$region <- as.character(covariates$region)
    
    grafo <- list(i = i, descent_net = descent_net)
  } else {
    warning("La cantidad de vértices no coincide con la cantidad de atributos para el hogar ", i)
  }
  
  return(grafo)
}

unique_households <- unique(data_subset$household)

# Usar foreach para ejecutar en paralelo
descent_igrpah_sample <- foreach(i = unique_households, .packages = c(
  "tidyverse", "igraph", "haven", "tibble", "reshape2", "tryCatchLog",
  "futile.logger", "dplyr", "tidyr", "doParallel", "iterators", "parallel", "progress"
)) %dopar% {
  household_process(i, data_subset)
}

end.time <- Sys.time()
stopCluster(cl) # Detener el clúster
time.taken_parallel <- end.time - start.time
time.taken_parallel

successful_graphs <- descent_igrpah_sample[!sapply(descent_igrpah_sample, is.null)]
failed_graphs <- descent_igrpah_sample[sapply(descent_igrpah_sample, is.null)]
unprocessed_households <- setdiff(unique(data_subset$household), names(successful_graphs))

# Resultados finales
message("Total hogares procesados: ", length(unique(data_subset$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

descent_igrpah_sample
descent_igrpah_sample[[100]]$i

# Guardar los resultados en un archivo
save(descent_igrpah_sample, file = paste0("Redes/descent_igrpah_subset1000.RData"))

#Creamos una lista en formato Network
a <- descent_igrpah_sample

descent_network_sample <- lapply(a, function(j) {
  j$descent_net <- asNetwork(j$descent_net)
  j
})

save(descent_network_sample, file = paste0("Redes/descent_network_subset1000.RData"))

########################## RED DE MATRIMONIO ###################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)
start.time <- Sys.time()

options("tryCatchLog.write.error.dump.file" = TRUE)

# Función para procesar cada hogar
household_process <- function(i, data_subset) {
  # Filtrar datos del hogar
  household_i <- data_subset[which(data_subset$household == i), ]
  
  # Validar si hay datos
  if (nrow(household_i) == 0) {
    warning(paste("Hogar", i, "no tiene datos"))
    return(list(household_id = i, marriage_net = NULL, warning = "No tiene datos"))
  }
  
  # Crear nodos y aristas
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  aux <- household_i %>% 
    group_by(h5) %>%
    summarise(to = id_persona[1], .groups = 'drop') %>%
    right_join(household_i, by = "h5") %>%
    filter(id_persona != to)
  
  aux <- rbind(
    aux %>% select(h5, id_persona, to),
    aux %>% select(h5, id_persona = to, to = id_persona)
  )
  
  aux$to[is.na(aux$h5)] <- NA
  
  colnames(aux)[colnames(aux) == "id_persona"] <- "from"
  aux2 <- aux[, -1]
  
  edge_marriage <- aux2 %>%
    filter(!is.na(to))
  
  edge_marriage$type <- "marriage"
  edge_marriage$color <- 3
  
  # Variables para los nodos
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", 
              "r3", "s17", "s28", "y1", "y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear el grafo
  marriage_net <- graph_from_data_frame(d = edge_marriage, vertices = nodes, directed = TRUE)
  
  # Validar que los atributos coincidan con los nodos
  if (length(V(marriage_net)) != nrow(covariates)) {
    warning(paste("Número de vértices no coincide con las covariables en el hogar", i))
    return(list(household_id = i, marriage_net = NULL, warning = "Vertices no coinciden con covariables"))
  }
  
  # Asignar atributos a los nodos
  covariates <- covariates[order(covariates$id_persona), ]
  V(marriage_net)$sex <- as.integer(covariates$sex)
  V(marriage_net)$edad <- as.integer(covariates$edad)
  V(marriage_net)$e6a <- as.character(covariates$e6a)
  V(marriage_net)$o1 <- as.character(covariates$o1)
  V(marriage_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(marriage_net)$ecivil <- as.character(covariates$ecivil)
  V(marriage_net)$r3 <- as.character(covariates$r3)
  V(marriage_net)$s17 <- as.character(covariates$s17)
  V(marriage_net)$s28 <- as.character(covariates$s28)
  V(marriage_net)$y1 <- as.character(covariates$y1)
  V(marriage_net)$y1_preg <- as.character(covariates$y1_preg)
  V(marriage_net)$comuna <- as.character(covariates$comuna)
  V(marriage_net)$region <- as.character(covariates$region)
  
  return(list(household_id = i, marriage_net = marriage_net, warning = NULL))
}

# Usar foreach para ejecutar en paralelo
marriage_igraph_sample <- foreach(i = unique(data_subset$household),
                           .packages = c(
                             "tidyverse",
                             "igraph",
                             "haven",
                             "tibble",
                             "reshape2",
                             "tryCatchLog",
                             "futile.logger",
                             "dplyr",
                             "tidyr",
                             "doParallel",
                             "iterators",
                             "parallel",
                             "progress")
) %dopar% {
  tryCatch(
    household_process(i, data_subset),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, marriage_net = NULL, warning = e$message))
    }
  )
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
stopCluster(cl)

# Filtrar resultados
successful_graphs <- marriage_igraph_sample[!sapply(marriage_igraph_sample, function(x) is.null(x$marriage_net))]
failed_graphs <- marriage_igraph_sample[sapply(marriage_igraph_sample, function(x) is.null(x$marriage_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_subset$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

# Guardar los resultados en un archivo
save(marriage_igraph_sample, file = paste0("Redes/marriage_igrpah_subset1000.RData"))

#Creamos una lista en formato Network

a <- marriage_igraph_sample


marriage_network_sample <- lapply(a, function(j) {
  j$marriage_net <- asNetwork(j$marriage_net)
  j
})

save(marriage_network_sample, file = paste0("Redes/marriage_network_subset1000.RData"))


########################## RED DE DEPENDENCIA ##################################
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[which(data_subset$household == i), ]
  
  # Crear nodos y aristas
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_dependency <- tibble(
    from = as.character(household_i$h5_2),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  
  edge_dependency$type <- "econ_support"
  edge_dependency$color <- 2
  
  # Variables para los nodos
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s17", "s28", "y1", "y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear el grafo
  dependency_net <- graph_from_data_frame(d = edge_dependency, vertices = nodes, directed = TRUE)
  
  # Asignar atributos a los nodos
  covariates <- covariates[order(covariates$id_persona), ]
  V(dependency_net)$sex <- as.integer(covariates$sex)
  V(dependency_net)$edad <- as.integer(covariates$edad)
  V(dependency_net)$e6a <- as.character(covariates$e6a)
  V(dependency_net)$o1 <- as.character(covariates$o1)
  V(dependency_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(dependency_net)$ecivil <- as.character(covariates$ecivil)
  V(dependency_net)$r3 <- as.character(covariates$r3)
  V(dependency_net)$s17 <- as.character(covariates$s17)
  V(dependency_net)$s28 <- as.character(covariates$s28)
  V(dependency_net)$y1 <- as.character(covariates$y1)
  V(dependency_net)$y1_preg <- as.character(covariates$y1_preg)
  V(dependency_net)$comuna <- as.character(covariates$comuna)
  V(dependency_net)$region <- as.character(covariates$region)
  
  return(list(household_id = i, dependency_net = dependency_net, warning = NULL))
}

unique_households <- unique(data_subset$household)

# Usar foreach para ejecutar en paralelo
dependency_igraph_sample <- foreach(i = unique_households,
                             .packages = c(
                               "tidyverse",
                               "igraph",
                               "haven",
                               "tibble",
                               "reshape2",
                               "tryCatchLog",
                               "futile.logger",
                               "dplyr",
                               "tidyr",
                               "doParallel",
                               "iterators",
                               "parallel",
                               "progress")
) %dopar% {
  tryCatch(
    household_process(i, data_subset),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, dependency_net = NULL, warning = e$message))
    }
  )
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
stopCluster(cl)

# Filtrar resultados
successful_graphs <- dependency_igraph_sample[!sapply(dependency_igraph_sample, function(x) is.null(x$dependency_net))]
failed_graphs <- dependency_igraph_sample[sapply(dependency_igraph_sample, function(x) is.null(x$dependency_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_subset$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs)) #2

# Guardar los resultados en un archivo. Acá cambié el nombre para evitar cambiar el archivo que ya está
save(dependency_igraph_sample, file = paste0("Redes/dependency_igrpah_subset1000.RData"))

#Y ahora creamos una lista igual, pero en formato Network

a <- dependency_igrpah_sample

dependency_network_sample <- lapply(a, function(j) {
  j$dependency_net <- asNetwork(j$dependency_net)
  j
})

save(dependency_network_sample, file = paste0("Redes/dependency_network_subset1000.RData"))

############################ RED KINSHIP #######################################
num_cores <- detectCores()-1
registerDoParallel(cores = num_cores)
start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[which(data_subset$household == i),]
  
  
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_kinship <- tibble(
    from = as.character(household_i$h5_2),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  
  
  edge_dependency <- tibble(from = household_i$h5_2, to = household_i$id_persona)
  edge_dependency <- edge_dependency[which(edge_dependency$from != 0),]
  edge_dependency$to <- as.character(edge_dependency$to)
  edge_dependency$from <- as.character(edge_dependency$from)
  
  edge_dependency$type <- "econ_support"
  edge_dependency$color <- 2
  edge_dependency<-rbind(edge_dependency)
  
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s17","s28","y1","y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  dependency_net <- graph_from_data_frame(d = edge_dependency, vertices = nodes, directed = TRUE)
  
  covariates <- covariates[order(covariates$id_persona),]
  V(dependency_net)$sex <- as.integer(covariates$sex)
  V(dependency_net)$edad <- as.integer(covariates$edad)
  V(dependency_net)$e6a <- as.character(covariates$e6a)
  V(dependency_net)$o1 <- as.character(covariates$o1)
  V(dependency_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(dependency_net)$ecivil <- as.character(covariates$ecivil)
  V(dependency_net)$r3 <- as.character(covariates$r3)
  V(dependency_net)$s17 <- as.character(covariates$s17)
  V(dependency_net)$s28 <- as.character(covariates$s28)
  V(dependency_net)$y1 <- as.character(covariates$y1)
  V(dependency_net)$y1_preg <- as.character(covariates$y1_preg)
  V(dependency_net)$comuna <- as.character(covariates$comuna)
  V(dependency_net)$region <- as.character(covariates$region)
  
  
  grafo <- list(household_i = i, dependency_net = dependency_net)
  return(grafo)
}

unique_households <- unique(data_subset$household)

# Usar foreach para ejecutar en paralelo. Acá cambié el nombre para evitar cambiar el archivo que ya está

dependency_igrpah_sexnum <- foreach(i = unique_households,
                                    #  .verbose =TRUE,
                                    .packages = c(
                                      "tidyverse",
                                      "igraph",
                                      "haven",
                                      "tibble",
                                      "reshape2",
                                      "tryCatchLog",
                                      "futile.logger",
                                      "dplyr",
                                      "tidyr",
                                      "doParallel",
                                      "iterators",
                                      "parallel",
                                      "progress")                
) %dopar% {
  household_process(i, data_subset)
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
time.taken_parallel
stopCluster(cl)

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl) 
start.time <- Sys.time()

# Función para procesar cada household
household_process <- function(i, data_subset) {
  household_i <- data_subset[which(data_subset$household == i),]
  
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_descent <- tibble(from = household_i$h5_1, to = household_i $id_persona)
  edge_descent <- edge_descent[which(edge_descent$from != 0 & edge_descent$from != ""),]
  edge_descent$to <- as.character(edge_descent$to)
  edge_descent$from <- as.character(edge_descent$from)
  
  #Solución para "madre|padre"
  
  if(TRUE %in% grepl("|", edge_descent$from, fixed = TRUE)){
    a <- edge_descent %>% separate(from, c('parent1', 'parent2'))
    a <- melt(a, na.rm = TRUE, id = 'to')
    a$from <- a$value
    a <- a %>% select(from, everything())
    a <- a[1:2]
    edge_descent <- a
  }
  
  ## Marriage network##
  
  aux<-household_i%>%group_by(h5)%>%
    summarise(to = id_persona[1])%>%
    right_join(household_i)%>%
    filter(id_persona!= to)
  
  aux<-rbind(aux%>%select(h5,id_persona,to),
             aux%>%select(h5,id_persona=to,to=id_persona))
  
  aux$to[is.na(aux$h5)]<-NA
  
  colnames(aux)[colnames(aux) == "id_persona"] <- "from"
  aux2 <- aux[,-1]
  edge_marriage <- aux2 %>%
    filter(!is.na(to))
  
  #### Visualition of multiple networks ####
  
  edge_descent$type    <- "descent"
  
  edge_marriage$type   <- "marriage"
  edge_descent$color    <- 1
  
  edge_marriage$color   <- 3
  
  edge_desc_depe<-rbind(edge_descent,edge_marriage)
  
  
  names(household_i)
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s17","s28","y1","y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  
  nodes <- sort(covariates$id_persona)
  
  kinship_net    <- graph_from_data_frame(d=edge_desc_depe, vertices=nodes, directed=T) # combine matrimonial and descent networks
  
  # adding attributes to igraph objects
  covariates <- covariates[order(covariates$id_persona),]
  V(kinship_net)$sex <- as.integer(covariates$sex)
  V(kinship_net)$edad <- as.integer(covariates$edad)
  V(kinship_net)$e6a <- as.character(covariates$e6a)
  V(kinship_net)$o1 <- as.character(covariates$o1)
  V(kinship_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(kinship_net)$ecivil <- as.character(covariates$ecivil)
  V(kinship_net)$r3 <- as.character(covariates$r3)
  V(kinship_net)$s17 <- as.character(covariates$s17)
  V(kinship_net)$s28 <- as.character(covariates$s28)
  V(kinship_net)$y1 <- as.character(covariates$y1)
  V(kinship_net)$y1_preg <- as.character(covariates$y1_preg)
  V(kinship_net)$comuna <- as.character(covariates$comuna)
  V(kinship_net)$region <- as.character(covariates$region)
  
  
  grafo <- list(household_i = i, kinship_net = kinship_net)
  return(grafo)
}
unique_households <- unique(data_subset$household)

kinship_igrpah_sample <- foreach(i = unique_households,
                                 #  .verbose =TRUE,
                                 .packages = c(
                                   "tidyverse",
                                   "igraph",
                                   "haven",
                                   "tibble",
                                   "reshape2",
                                   "tryCatchLog",
                                   "futile.logger",
                                   "dplyr",
                                   "tidyr",
                                   "doParallel",
                                   "iterators",
                                   "parallel",
                                   "progress")                
) %dopar% {
  tryCatch(
    household_process(i, data_subset),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, kinship_net = NULL, warning = e$message))
    }
  )
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
time.taken_parallel
stopCluster(cl)


# Filtrar resultados
successful_graphs <- kinship_igrpah_sample[!sapply(kinship_igrpah_sample, function(x) is.null(x$kinship_net))]
failed_graphs <- kinship_igrpah_sample[sapply(kinship_igrpah_sample, function(x) is.null(x$kinship_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_subset$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

save(kinship_igrpah_sample, file = paste0(getwd(), "/Ergomitos/Redes/kinship_igrpah_subset1000.RData"))

a <- kinship_igrpah_sample
kinship_network_sample<- lapply(a, function(j) {
  j$kinship_net <- asNetwork(j$kinship_net)
  j
})
save(kinship_network_sample, file = paste0(getwd(), "/Ergomitos/Redes/kinship_network_subset1000.RData"))
