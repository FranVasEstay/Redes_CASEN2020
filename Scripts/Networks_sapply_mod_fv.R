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

###### CARGAR DATA ######

load("Data/ori_Casen2020_rdata.RData")

##################### ADMINISTRACIÓN DE LOS DATOS ##############################

data<- ori_Casen2020_STATA %>%
  select(id_vivienda, id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, r1b_pais_esp,nucleo, pco2, r3,s16,y1,y1_preg, comuna, region) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = id_vivienda, sex = sexo) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    across(c(e6a,pco1, ecivil, pco2, r3, s16,o1, y1_preg), as_factor),
    r1b_pais_esp = ifelse(r1b_pais_esp == "", 1,
                          ifelse(r1b_pais_esp == "NO RESPONDE", 3, 2))
  ) 
data$sex <- as.integer(data$sex)
data <- data %>%
  mutate_all(~ ifelse(is.na(.), "No_aplica", .))
########################### CREACION DE REDES ##################################
########################## RED DE DESCENDENCIA #################################

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
start.time <- Sys.time()

household_process <- function(i, data) {
  household_i <- data[data$household == i, ]
  
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
  
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s17","s28","y1","y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  descent_net <- graph_from_data_frame(d = edge_descent, vertices = nodes, directed = TRUE)
  if (length(covariates$id_persona) == length(V(descent_net))) {
  V(descent_net)$sex <- as.integer(covariates$sex)
  V(descent_net)$edad <- as.integer(covariates$edad)
  V(descent_net)$e6a <- as.character(covariates$e6a)
  V(descent_net)$o1 <- as.character(covariates$o1)
  V(descent_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(descent_net)$ecivil <- as.character(covariates$ecivil)
  V(descent_net)$r3 <- as.character(covariates$r3)
  V(descent_net)$s16 <- as.character(covariates$s16)
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

unique_households <- unique(data$household)

# Usar foreach para ejecutar en paralelo, cambiando el nombre para no confundir con los otros:
descent_igrpah <- foreach(i = unique_households, .packages= c(
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
  household_process(i, data)
}

end.time <- Sys.time()
stopCluster(cl)
time.taken_parallel <- end.time - start.time
time.taken_parallel

# Guardar los resultados en un archivo
save(descent_igrpah, file = paste0("Redes/descent_igrpah.RData"))

#Creamos una lista en formato Network

a <- descent_igrpah


descent_network <- lapply(a, function(j) {
  j$descent_net <- asNetwork(j$descent_net)
  j
})

save(descent_network, file = paste0("Redes/descent_network.RData"))

########################## RED DE MATRIMONIO ###################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
options("tryCatchLog.write.error.dump.file" = TRUE)

start.time <- Sys.time()

household_process <- function(i, data) {
  household_i <- data[which(data$household == i),]
  
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
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s17","s28","y1","y1_preg", "region", "comuna")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  marriage_net   <- graph_from_data_frame(d=edge_marriage, vertices=nodes, directed=T)
  
  covariates <- covariates[order(covariates$id_persona),]
  V(marriage_net)$sex <- as.integer(covariates$sex)
  V(marriage_net)$edad <- as.integer(covariates$edad)
  V(marriage_net)$e6a <- as.character(covariates$e6a)
  V(marriage_net)$o1 <- as.character(covariates$o1)
  V(marriage_net)$r1b_pais_esp <- as.integer(covariates$r1b_pais_esp)
  V(marriage_net)$ecivil <- as.character(covariates$ecivil)
  V(marriage_net)$r3 <- as.character(covariates$r3)
  V(marriage_net)$s16 <- as.character(covariates$s16)
  V(marriage_net)$y1 <- as.character(covariates$y1)
  V(marriage_net)$y1_preg <- as.character(covariates$y1_preg)
  V(marriage_net)$comuna <- as.character(covariates$comuna)
  V(marriage_net)$region <- as.character(covariates$region)
  
  grafo <- list(household_i = i, marriage_net = marriage_net)
  return(grafo)
}

# Usar foreach para ejecutar en paralelo
marriage_igrpah <- foreach(i = unique(data$household),
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
  household_process(i, data)
}

end.time <- Sys.time()
time.taken_parallel <- end.time - start.time
time.taken_parallel
stopCluster(cl)

# Guardar los resultados en un archivo
save(marriage_igrpah, file = paste0("Redes/marriage_igrpah.RData"))

#Creamos una lista en formato Network

a <- marriage_igrpah


marriage_network <- lapply(a, function(j) {
  j$marriage_net <- asNetwork(j$marriage_net)
  j
})

save(marriage_network, file = paste0("Redes/marriage_network.RData"))


########################## RED DE DEPENDENCIA ##################################
# Establecer el número de núcleos para el procesamiento en paralelo

num_cores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
start.time <- Sys.time()

household_process <- function(i, data) {
  household_i <- data[which(data$household == i),]
  
  
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
  V(dependency_net)$s16 <- as.character(covariates$s16)
  V(dependency_net)$y1 <- as.character(covariates$y1)
  V(dependency_net)$y1_preg <- as.character(covariates$y1_preg)
  V(dependency_net)$comuna <- as.character(covariates$comuna)
  V(dependency_net)$region <- as.character(covariates$region)
  
  
  grafo <- list(household_i = i, dependency_net = dependency_net)
  return(grafo)
}

unique_households <- unique(data$household)

# Usar foreach para ejecutar en paralelo. Acá cambié el nombre para evitar cambiar el archivo que ya está

dependency_igrpah <- foreach(i = unique_households,
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
  household_process(i, data)
}

end.time <- Sys.time()

stopCluster(cl)
time.taken_parallel <- end.time - start.time
time.taken_parallel

# Guardar los resultados en un archivo. Acá cambié el nombre para evitar cambiar el archivo que ya está
save(dependency_igrpah, file = paste0("Redes/dependency_igrpah.RData"))

#Y ahora creamos una lista igual, pero en formato Network

a <- dependency_igrpah


dependency_network <- lapply(a, function(j) {
  j$dependency_net <- asNetwork(j$dependency_net)
  j
})

save(dependency_network, file = paste0("Redes/dependency_network.RData"))

############################ RED KINSHIP #######################################

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl) 

start.time <- Sys.time()

# Función para procesar cada household
household_process <- function(i, data) {
  household_i <- data[which(data$household == i),]
  
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
  V(kinship_net)$s16 <- as.character(covariates$s16)
  V(kinship_net)$y1 <- as.character(covariates$y1)
  V(kinship_net)$y1_preg <- as.character(covariates$y1_preg)
  V(kinship_net)$comuna <- as.character(covariates$comuna)
  V(kinship_net)$region <- as.character(covariates$region)
  
  grafo <- list(household_i = i, kinship_net = kinship_net)
  return(grafo)
}
unique_households <- unique(data$household)

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
  household_process(i, data)
}

end.time <- Sys.time()
stopCluster(cl)
time.taken_parallel <- end.time - start.time
time.taken_parallel

# Guardar los resultados en un archivo. Acá cambié el nombre para evitar cambiar el archivo que ya está
save(kinship_igrpah, file = paste0("Redes/kinship_igrpah.RData"))

#Y ahora creamos una lista igual, pero en formato Network

a <- kinship_igrpah


kinship_network <- lapply(a, function(j) {
  j$kinship_net <- asNetwork(j$kinship_net)
  j
})

save(kinship_network, file = paste0("Redes/kinship_network.RData"))
