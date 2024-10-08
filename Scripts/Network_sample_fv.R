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
  select(id_vivienda, id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, r1b_pais_esp,nucleo, pco2, r3,s16,y1,y1_preg, comuna, region) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = id_vivienda, sex = sexo) %>%
  mutate(
    household = as.numeric(household),
    across(c(sex,e6a,pco1, ecivil, pco2, r3, s16,o1, y1_preg), as_factor),
    r1b_pais_esp = ifelse(r1b_pais_esp == "", 1,
                          ifelse(r1b_pais_esp == "NO RESPONDE", 3, 2))
  ) 
data$sex <- as.integer(data$sex)

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

###Loop###

#Inicio reloj
start.time <- Sys.time()
grafos <- list()
for(i in unique(data_subset$household)) {
  household_i <- data_subset[which(data_subset$household == i), ]
  
  nodes_list <- tibble(household_i$id_persona)
  edge_descent <- tibble(from =household_i$h5_1, to = household_i$id_persona)
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
  
  nodes_list$`household_i$id_persona` <- as.character(nodes_list$`household_i$id_persona`)
  edge_descent$type    <- "descent"
  edge_descent$color    <- 1
  edge_desc_depe<-rbind(edge_descent)
  myvars <- c("id_persona","sex","edad")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  descent_net    <- graph_from_data_frame(d=edge_descent, vertices=nodes, directed=T)
  V(descent_net)$sex <- covariates$sex
  V(descent_net)$edad <- covariates$edad
  descent_unet    <- graph_from_data_frame(d=edge_descent, vertices=nodes, directed=F)
  grafo <- list(i = i,descent_net = descent_net)
  grafos[[length(grafos) + 1]] <- grafo
  
}
###Fin reloj
end.time <- Sys.time()
time.taken_loop <- end.time - start.time
time.taken_loop

###Lapply###
start.time <- Sys.time()
household_process <- function(i) {
  household_i <- data_subset[data_subset$household == i, ]
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  edge_descent <- tibble(
    from = as.character(household_i$h5_1),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  if (any(grepl("\\|", edge_descent$from))) {
    edge_descent <- edge_descent %>%
      separate(from, c('parent1', 'parent2')) %>%
      pivot_longer(cols = c(parent1, parent2), names_to = NULL, values_drop_na = TRUE) %>%
      select(value, to) 
  }
  edge_descent$type <- "descent"
  edge_descent$color <- 1
  myvars <- c("id_persona", "sex", "edad")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  descent_net <- graph_from_data_frame(d = edge_descent, vertices = nodes, directed = TRUE)
  V(descent_net)$sex <- covariates$sex
  V(descent_net)$edad <- covariates$edad
  grafo <- list(i = i, descent_net = descent_net)
  return(grafo)
}
unique_households <- unique(data_subset$household)
grafos <- lapply(unique_households, household_process)

end.time <- Sys.time()
time.taken_lapply <- end.time - start.time
time.taken_lapply

###DoParallel Red Descendencia###

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- 4  
registerDoParallel(cores = num_cores)

start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[data_subset$household == i, ]
  
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_descent <- tibble(
    from = as.character(household_i$h5_1),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  
  if (any(grepl("\\|", edge_descent$from))) {
    edge_descent <- edge_descent %>%
      separate(from, c('parent1', 'parent2')) %>%
      pivot_longer(cols = c(parent1, parent2), names_to = NULL, values_drop_na = TRUE) %>%
      select(value, to) 
  }
  
  edge_descent$type <- "descent"
  edge_descent$color <- 1
  
  myvars <- c("id_persona", "sex", "edad")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  descent_net <- graph_from_data_frame(d = edge_descent, vertices = nodes, directed = TRUE)
  V(descent_net)$sex <- as.integer(covariates$sex) #Esto cambio
  V(descent_net)$edad <- as.integer(covariates$edad) #Esto también
  
  grafo <- list(i = i, descent_net = descent_net)
  
  return(grafo)
}

unique_households <- unique(data_subset$household)

# Usar foreach para ejecutar en paralelo, cambiando el nombre para no confundir con los otros:
descent_igrpah_sample <- foreach(i = unique_households, .packages= c(
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

# Guardar los resultados en un archivo
save(descent_igrpah_sample, file = paste0("Redes/descent_igrpah_subset1000.RData"))

#Y, al igual que con dependencia económica, creamos una lista igual, pero en formato Network

a <- descent_igrpah_sample


descent_network_sample <- lapply(a, function(j) {
  j$descent_net <- asNetwork(j$descent_net)
  j
})

save(descent_network_sample, file = paste0("Redes/descent_network_subset1000.RData"))

########################## RED DE MATRIMONIO ###################################

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- 4  
registerDoParallel(cores = num_cores)
options("tryCatchLog.write.error.dump.file" = TRUE)

start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[which(data_subset$household == i),]
  
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
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
  
  edge_marriage$type   <- "marriage"
  edge_marriage$color   <- 3
  edge_marriage<-rbind(edge_marriage)
  myvars <- c("id_persona","sex","edad")
  covariates <- household_i[myvars]
  
  nodes <- sort(covariates$id_persona)
  
  marriage_net   <- graph_from_data_frame(d=edge_marriage, vertices=nodes, directed=T)
  
  covariates <- covariates[order(covariates$id_persona),]
  V(marriage_net)$sex <- as.integer(covariates$sex)
  V(marriage_net)$edad <- as.integer(covariates$edad)
  
  grafo <- list(household_i = i, marriage_net = marriage_net)
  return(grafo)
}

# Usar foreach para ejecutar en paralelo
marriage_igrpah_sample <- foreach(i = unique(data_subset$household),
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


# Guardar los resultados en un archivo
save(marriage_igrpah_sample, file = paste0("Redes/marriage_igrpah_subset1000.RData"))

#Creamos una lista en formato Network

a <- marriage_igrpah_sample


marriage_network_sample <- lapply(a, function(j) {
  j$marriage_net <- asNetwork(j$marriage_net)
  j
})

save(marriage_network_sample, file = paste0("Redes/marriage_network_subset1000.RData"))


########################## RED DE DEPENDENCIA ##################################
# Establecer el número de núcleos para el procesamiento en paralelo

num_cores <- 4  
registerDoParallel(cores = num_cores)

start.time <- Sys.time()

household_process <- function(i, data_subset) {
  household_i <- data_subset[which(data_subset$household == i),]
  
  
  nodes_list <- tibble(household_id_persona = as.character(household_i$id_persona))
  
  edge_dependency <- tibble(
    from = as.character(household_i$h5_2),
    to = as.character(household_i$id_persona)
  ) %>%
    filter(from != "0" & from != "") %>%
    mutate(to = as.character(to))
  
  edge_dependency$type <- "econ_support"
  edge_dependency$color <- 2
  edge_dependency<-rbind(edge_dependency)
  
  myvars <- c("id_persona", "sex", "edad")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  dependency_net <- graph_from_data_frame(d = edge_dependency, vertices = nodes, directed = TRUE)
  
  covariates <- covariates[order(covariates$id_persona),]

  
  V(dependency_net)$sex <- as.integer(covariates$sex)
  V(dependency_net)$edad <- as.integer(covariates$edad)
  
  
  grafo <- list(household_i = i, dependency_net = dependency_net)
  return(grafo)
}

unique_households <- unique(data_subset$household)

# Usar foreach para ejecutar en paralelo. Acá cambié el nombre para evitar cambiar el archivo que ya está

dependency_igrpah_sample <- foreach(i = unique_households,
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

# Guardar los resultados en un archivo. Acá cambié el nombre para evitar cambiar el archivo que ya está
save(dependency_igrpah_sample, file = paste0("Redes/dependency_igrpah_subset1000.RData"))

#Y ahora creamos una lista igual, pero en formato Network

a <- dependency_igrpah_sample


dependency_network_sample <- lapply(a, function(j) {
  j$dependency_net <- asNetwork(j$dependency_net)
  j
})

save(dependency_network_sample, file = paste0("Redes/dependency_network_subset1000.RData"))

############################ RED KINSHIP #######################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- 4
registerDoParallel(cores = num_cores)

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
  myvars <- c("id_persona","sex","edad")
  covariates <- household_i[myvars]
  
  nodes <- sort(covariates$id_persona)
  
  kinship_net    <- graph_from_data_frame(d=edge_desc_depe, vertices=nodes, directed=T) # combine matrimonial and descent networks
  
  # adding attributes to igraph objects
  covariates <- covariates[order(covariates$id_persona),]
  V(kinship_net)$sex <- as.integer(covariates$sex) #Ojo que esto también cambió para lograr la solicitud.
  V(kinship_net)$edad <- as.integer(covariates$edad)
  
  
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
  household_process(i, data_subset)
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
time.taken_parallel

# Guardar los resultados en un archivo. Acá cambié el nombre para evitar cambiar el archivo que ya está
save(kinship_igrpah_sample, file = paste0("Redes/kinship_igrpah_subset1000.RData"))

#Y ahora creamos una lista igual, pero en formato Network

a <- kinship_igrpah_sample


kinship_network_sample <- lapply(a, function(j) {
  j$kinship_net <- asNetwork(j$kinship_net)
  j
})

save(kinship_network_sample, file = paste0("Redes/kinship_network_subset1000.RData"))


