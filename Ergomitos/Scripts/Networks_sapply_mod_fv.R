################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### NETWORKS_SAPPLY_MODIFICADO #############################
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
library(intergraph)
library(beepr)

###### CARGAR DATA ######
load("Ergomitos/Data/ori_Casen2020_rdata.RData")

##################### ADMINISTRACIÓN DE LOS DATOS ##############################
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

data_ergomitos<- ori_Casen2020_STATA %>%
  select(folio,id_vivienda,
       id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2,nucleo, pco2, r3,s28, comuna, region,ytotcor) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = folio, sex = sexo) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    across(c(e6a,pco1, ecivil, pco2, r3,o1, region, comuna), as_factor),
    r1b_pais_esp = ifelse(r1b_pais_esp == "", 1,
                          ifelse(r1b_pais_esp == "NO RESPONDE", 3, 2)),
    # Nuevas variables de edad
    edad_laboral = ifelse(edad >= 15, 1, 0),          # 15+ años
    edad_legal = ifelse(edad >= 18, 1, 0),            # 18+ años
    edad_dependencia_estudios = ifelse(edad >= 28, 1, 0),  # 28+ años
  ) %>%
  # Reemplazar NA por "No_aplica" solo en variables específicas
  mutate(across(
    .cols = -c(household, id_persona, edad, sex, pco1, h5, h5_1, pco2, comuna, region, h5_2, edad_laboral, edad_legal, edad_dependencia_estudios,ytotcor),
    .fns = ~ ifelse(is.na(.), "No_aplica", .)))

length(unique(data_ergomitos$household)) # 62537 con id_vivienda y filtro, 62911 con folio
save(data_ergomitos, file = "Ergomitos/Data/Data_Ergomitos.RData")

########################### CREACION DE REDES ##################################
load("Ergomitos/Data/Data_Ergomitos.RData")

########################## RED DE DESCENDENCIA #################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores) 
registerDoParallel(cl)
start.time <- Sys.time()

household_process <- function(i, data_ergomitos) {
  household_i <- data_ergomitos[data_ergomitos$household == i, ]
  
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
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s28", "region", "comuna","edad_laboral","edad_legal","edad_dependencia_estudios","ytotcor")
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
    V(descent_net)$s28 <- as.integer(covariates$s28)
    V(descent_net)$comuna <- as.integer(covariates$comuna)
    V(descent_net)$region <- as.integer(covariates$region)
    V(descent_net)$ytotcor <- as.character(covariates$ytotcor)
    V(descent_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
    V(descent_net)$edad_legal <- as.numeric(covariates$edad_legal)
    V(descent_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
    grafo <- list(i = i, descent_net = descent_net)
  } else {
    warning("La cantidad de vértices no coincide con la cantidad de atributos para el hogar ", i)
  }
  
  return(grafo)
}

unique_households <- unique(data_ergomitos$household)

# Usar foreach para ejecutar en paralelo
descent_igrpah <- foreach(i = unique_households, .packages = c(
  "tidyverse", "igraph", "haven", "tibble", "reshape2", "tryCatchLog",
  "futile.logger", "dplyr", "tidyr", "doParallel", "iterators", "parallel", "progress"
)) %dopar% {
  household_process(i, data_ergomitos)
}

end.time <- Sys.time()
stopCluster(cl) # Detener el clúster
time.taken_parallel <- end.time - start.time
time.taken_parallel

successful_graphs <- descent_igrpah[!sapply(descent_igrpah, is.null)]
failed_graphs <- descent_igrpah[sapply(descent_igrpah, is.null)]
unprocessed_households <- setdiff(unique(data_ergomitos$household), names(successful_graphs))

# Resultados finales
message("Total hogares procesados: ", length(unique(data_ergomitos$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

# Guardar los resultados en un archivo
save(descent_igrpah, file = paste0("Ergomitos/Redes/descent_igrpah.RData"))
plot(descent_igrpah[300][[1]]$descent_net)
#Creadescent_igrpah#Creamos una lista en formato Network
a <- descent_igrpah

descent_network <- lapply(a, function(j) {
  j$descent_net <- asNetwork(j$descent_net)
  j
})

save(descent_network, file = paste0("Ergomitos/Redes/descent_network.RData"))

########################## RED DE MATRIMONIO ###################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)
start.time <- Sys.time()

#options("tryCatchLog.write.error.dump.file" = TRUE)

# Función para procesar cada hogar
household_process <- function(i, data_ergomitos) {
  # Filtrar datos del hogar
  household_i <- data_ergomitos[which(data_ergomitos$household == i), ]
  
  # Validar si hay datos
  if (nrow(household_i) == 0) {
    warning(paste("Hogar", i, "no tiene datos"))
    return(list(household_id = i, marriage_net = NULL, warning = "No tiene datos"))
  }
  
  # Crear nodos y aristas
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_marriage <- household_i %>%
    filter(!is.na(h5) & h5 != 0) %>%
    group_by(h5) %>%
    #summarise(to = id_persona[1]) %>%
    #right_join(household_i) %>%
    #filter(id_persona != to)
    summarise(
      from = rep(id_persona, each = length(id_persona)),
      to = rep(id_persona, times = length(id_persona)),
      .groups = "drop"
    ) %>%
    filter(from != to) %>%  # Eliminar autoreferencias
    distinct() %>%  # Eliminar duplicados (A-B vs B-A)
    ungroup() %>%
    select(from, to) %>%
    mutate(type = "marriage", color = "darkgreen")
  
 # aux <- rbind(
  #  aux %>% select(h5, id_persona, to),
  #  aux %>% select(h5, id_persona = to, to = id_persona)
  #)
  
  #aux$to[is.na(aux$h5)] <- NA
  
  #colnames(aux)[colnames(aux) == "id_persona"] <- "from"
  #aux2 <- aux[, -1]
  
  #edge_marriage <- aux2 %>%
  #  filter(!is.na(to))
  
  edge_marriage$type <- "marriage"
  edge_marriage$color <- "darkgreen"
  edge_marriage<-rbind(edge_marriage)
  # Variables para los nodos
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s28", "region", "comuna","edad_laboral","edad_legal","edad_dependencia_estudios","ytotcor")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear el grafo
  marriage_net <- graph_from_data_frame(d = edge_marriage, vertices = nodes, directed = TRUE)
  
   # Validar que los atributos coincidan con los nodos
  if (length(V(marriage_net)) != nrow(covariates)) {
    warning(paste("Número de vértices no coincide con las covariables en el hogar", i))
    return(list(household_id = i, warning = "Vertices no coinciden con covariables"))
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
  V(marriage_net)$s28 <- as.character(covariates$s28)
  V(marriage_net)$comuna <- as.integer(covariates$comuna)
  V(marriage_net)$region <- as.integer(covariates$region)
  V(marriage_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(marriage_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(marriage_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(marriage_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  
  grafo <- list(household_i = i, marriage_net = marriage_net)
  
  return(grafo)
}

# Usar foreach para ejecutar en paralelo
marriage_igraph <- foreach(i = unique(data_ergomitos$household),
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
    household_process(i, data_ergomitos),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, marriage_net = NULL, warning = e$message))
    }
  )
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
stopCluster(cl)
beep(1)
# Filtrar resultados
successful_graphs <- marriage_igraph[!sapply(marriage_igraph, function(x) is.null(x$marriage_net))]
failed_graphs <- marriage_igraph[sapply(marriage_igraph, function(x) is.null(x$marriage_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_ergomitos$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

# Guardar los resultados en un archivo
save(marriage_igraph, file = "Ergomitos/Redes/marriage_igraph.RData")
plot(marriage_igraph[300][[1]]$marriage_net)

#Creamos una lista en formato Network

a <- marriage_igraph

marriage_network <- lapply(a, function(j) {
  j$marriage_net <- asNetwork(j$marriage_net)
  j
})

save(marriage_network, file = paste0("Ergomitos/Redes/marriage_network.RData"))

########################## RED DE DEPENDENCIA ##################################
num_cores <- detectCores()-1
registerDoParallel(cores = num_cores)
start.time <- Sys.time()

household_process <- function(i, data_ergomitos) {
  household_i <- data_ergomitos[which(data_ergomitos$household == i),]
  
  
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
  edge_dependency$color <- "orange"
  edge_dependency<-rbind(edge_dependency)
  
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s28", "region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
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
  V(dependency_net)$s28 <- as.character(covariates$s28)
  V(dependency_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(dependency_net)$comuna <- as.character(covariates$comuna)
  V(dependency_net)$region <- as.character(covariates$region)
  V(dependency_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(dependency_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(dependency_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  
  grafo <- list(household_i = i, dependency_net = dependency_net)
  return(grafo)
}

unique_households <- unique(data_ergomitos$household)

# Usar foreach para ejecutar en paralelo. Acá cambié el nombre para evitar cambiar el archivo que ya está

dependency_igraph<- foreach(i = unique_households,
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
    household_process(i, data_ergomitos),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, dependency_net = NULL, warning = e$message))
    }
  )
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
time.taken_parallel
stopCluster(cl)

# Filtrar resultados
successful_graphs <- dependency_igraph[!sapply(dependency_igraph, function(x) is.null(x$dependency_net))]
failed_graphs <- dependency_igraph[sapply(dependency_igraph, function(x) is.null(x$dependency_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_ergomitos$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs)) 

# Guardar los resultados en un archivo
save(dependency_igraph, file = "Ergomitos/Redes/dependency_igraph.RData")
plot(dependency_igraph[300][[1]]$dependency_net)
a <- dependency_igraph

dependency_network <- lapply(a, function(j) {
  j$dependency_net <- asNetwork(j$dependency_net)
  j
})

save(dependency_network, file = "Ergomitos/Redes/dependency_network.RData")


############################ RED KINSHIP #######################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl) 
start.time <- Sys.time()

# Función para procesar cada household
household_process <- function(i, data_ergomitos) {
  household_i <- data_ergomitos[which(data_ergomitos$household == i),]
  
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
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s28", "region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
  covariates <- household_i[myvars]
  
  nodes <- sort(covariates$id_persona)
  
  kinship_net    <- graph_from_data_frame(d=edge_desc_depe, vertices=nodes, directed=T) # combine matrimonial and descent networks
  
  # adding attributes to igraph objects
  covariates <- covariates[order(covariates$id_persona),]
  V(kinship_net)$sex <- as.numeric(covariates$sex)
  V(kinship_net)$edad <- as.numeric(covariates$edad)
  V(kinship_net)$e6a <- as.numeric(covariates$e6a)
  V(kinship_net)$o1 <- as.numeric(covariates$o1)
  V(kinship_net)$r1b_pais_esp <- as.numeric(covariates$r1b_pais_esp)
  V(kinship_net)$ecivil <- as.numeric(covariates$ecivil)
  V(kinship_net)$r3 <- as.numeric(covariates$r3)
  V(kinship_net)$s28 <- as.numeric(covariates$s28)
  V(kinship_net)$comuna <- as.numeric(covariates$comuna)
  V(kinship_net)$region <- as.numeric(covariates$region)
  V(kinship_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(kinship_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(kinship_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(kinship_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  grafo <- list(household_i = i, kinship_net = kinship_net)
  return(grafo)
}
unique_households <- unique(data_ergomitos$household)

kinship_igrpah <- foreach(i = unique_households,
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
    household_process(i, data_ergomitos),
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
successful_graphs <- kinship_igrpah[!sapply(kinship_igrpah, function(x) is.null(x$kinship_net))]
failed_graphs <- kinship_igrpah[sapply(kinship_igrpah, function(x) is.null(x$kinship_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data_ergomitos$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

save(kinship_igrpah, file = paste0(getwd(), "/Ergomitos/Redes/kinship_igrpah.RData"))
beep(1)
plot(kinship_igrpah[300][[1]]$kinship_net)

a <- kinship_igrpah
kinship_network<- lapply(a, function(j) {
  j$kinship_net <- asNetwork(j$kinship_net)
  j
})
save(kinship_network, file = paste0(getwd(), "/Ergomitos/Redes/kinship_network.RData"))
beep(8)