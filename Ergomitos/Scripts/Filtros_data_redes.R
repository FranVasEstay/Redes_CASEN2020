################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ FILTRO DE VIVIENDAS ###############################
################################################################################

###### LIBRERÍAS ######
library(beepr) #beep(8)
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
library(progress)

##### DATA ####
data_filtrada <- ori_Casen2020_STATA %>%
  select(id_vivienda, id_persona, edad, sexo, e6a, o1, r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, r1b_pais_esp, nucleo, pco2, r3, s28, comuna, region, ytotcor) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = id_vivienda, sex = sexo) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    r1b_pais_esp = case_when(
      r1b_pais_esp == "" ~ 1,
      r1b_pais_esp %in% c("NO RESPONDE", "NO BIEN ESPECIFICADO") ~ 0,
      TRUE ~ 2
    ),
    r3 = ifelse(as.numeric(r3) >= 1 & as.numeric(r3) <= 10, 1, # Sí (1-10)
                ifelse(as.numeric(r3) == 11, 2, 0)), # No (11)
    s28 = case_when(
      s28 %in% 1:21 ~ "Si",
      s28 %in% c(22, 99) ~ "No",
      TRUE ~ as.character(s28)
    ),
    # Nuevas variables de edad
    edad_laboral = ifelse(edad >= 15, 1, 0),          # 15+ años (edad laboral)
    edad_legal = ifelse(edad >= 18, 1, 0),            # 18+ años (mayoría de edad)
    edad_dependencia_estudios = ifelse(edad >= 28, 1, 0), # 28+ años (límite dependencia estudios)
    across(c(e6a, pco1, ecivil, pco2, r3, o1, s28, r1b_pais_esp), as_factor)
  ) %>%
  # Calcular el tamaño del hogar y filtrar por tamaño (más de 1 integrante)
  group_by(household) %>%
  mutate(household_size = n()) %>% # Número de personas en cada hogar
  ungroup() %>%
  filter(household_size >= 1)

length(unique(data_filtrada$household)) #son 62537 viviendas
save(data_filtrada, file = "Ergomitos/Data/Data_filtrada.RData")


# Calcular casos completos
casos_completos <- data_filtrada %>%
  split(.$household) %>% # Dividir por vivienda
  lapply(function(household_data) {
    all(!apply(is.na(household_data), 1, any)) # Verificar si todas las filas están completas
  }) %>%
  unlist() %>%       # Convertir la lista a un vector
  which(.) %>%       # Encontrar índices con TRUE
  names() %>% 
  as.numeric()       # Convertir nombres a numéricos

# Resultados
cat("Viviendas con casos completos (sin NA):\n")
print(casos_completos) #no hay ninguna vivienda que no tenga Nas

tabla_nas <- data_filtrada %>%
  group_by(household) %>%
  summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}")) %>%
  pivot_longer(cols = -household, names_to = "variable", values_to = "n_na") %>%
  pivot_wider(names_from = variable, values_from = n_na) # Reorganizar para que cada variable sea una columna
View(tabla_nas)
summary(tabla_nas)

# Identificar variables sin NAs (donde todas las viviendas tienen 0 NA en esa variable)
variables_sin_nas <- tabla_nas %>%
  select(-household) %>%  # Excluir la columna 'household'
  summarise(across(everything(), ~ all(. == 0))) %>%  # Verificar si todas las celdas son 0 (sin NA)
  pivot_longer(cols = everything(), names_to = "variable", values_to = "sin_na") %>%
  filter(sin_na == TRUE)  # Filtrar las variables que tienen solo ceros (sin NA)

# Resultados
cat("Variables sin NAs (sin valores distintos a 0):\n")
print(variables_sin_nas) #edad,sexo, nivel educativo,nacionalidad,pueblo indígena,ha estado en tratamiento en los 12 ultimos meses.

#Cambiar NAs por 0
data_filtrada <- data_filtrada %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

#Cuales hogares presentan variación
variacion_respuestas <- data %>%
  group_by(household) %>%
  summarise(across(everything(), ~ n_distinct(., na.rm = TRUE) > 1)) %>%
  rowwise() %>%
  mutate(tiene_variacion = any(c_across(-household))) %>%
  filter(tiene_variacion) %>%
  select(household)

cat("\nViviendas con variación en las respuestas:\n")
print(variacion_respuestas)
total_viviendas <- n_distinct(data$household)
viviendas_con_variacion <- nrow(variacion_respuestas)
porcentaje_variacion <- (viviendas_con_variacion / total_viviendas) * 100

# Mostrar el porcentaje
cat("\nPorcentaje de viviendas con variación en las respuestas:", round(porcentaje_variacion, 2), "%\n")

# Crear tabla de variación por vivienda (porcentaje de variables con variación)
frecuencia_variacion <- data %>%
  group_by(household) %>%  # Agrupar por cada vivienda
  summarise(across(everything(), ~ n_distinct(., na.rm = TRUE), .names = "var_{.col}")) %>%
  mutate(variacion = rowSums(across(starts_with("var_"), ~ . > 1))) %>% 
  mutate(variacion_porcentaje = (variacion / (ncol(.) - 1)) * 100) %>% 
  select(household, variacion_porcentaje)

# Mostrar el resultado
cat("\nFrecuencia porcentual de la variación por vivienda:\n")
print(frecuencia_variacion)

# Calcular el porcentaje de variación por cada variable en cada hogar
tabla_frecuencia_variacion <- data %>%
  group_by(household) %>% 
  summarise(across(everything(), 
                   ~ mean(n_distinct(.)) / n() * 100, 
                   .names = "variacion_{.col}")) %>%  
  ungroup()  

# Mostrar la tabla de frecuencias de variación porcentual
cat("\nPorcentaje de variación por variable en cada hogar:\n")
View(tabla_frecuencia_variacion)

save(data_filtrada, file = "Ergomitos/Data/Data_filtrada.RData")



########################### CREACION DE REDES ##################################
load("Ergomitos/Data/Data_filtrada.RData")
#Cambiar NAs por 0
data <- data_filtrada %>%
  mutate_all(~ ifelse(is.na(.), 0, .))

########################## RED DE DESCENDENCIA #################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores) # Corrige 'ncores' por 'num_cores'
registerDoParallel(cl)
start.time <- Sys.time()

household_process <- function(i, data) {
  household_i <- data[data$household == i, ]
  
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
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s28","region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear la red de descendencia
  descent_net <- graph_from_data_frame(d = edge_descent, vertices = nodes, directed = TRUE)
  
  # Verificar que la cantidad de nodos coincida con los atributos
  if (length(covariates$id_persona) == length(V(descent_net))) {
    V(descent_net)$sex <- as.numeric(covariates$sex)
    V(descent_net)$edad <- as.numeric(covariates$edad)
    V(descent_net)$e6a <- as.numeric(covariates$e6a)
    V(descent_net)$o1 <- as.numeric(covariates$o1)
    V(descent_net)$r1b_pais_esp <- as.numeric(covariates$r1b_pais_esp)
    V(descent_net)$ecivil <- as.numeric(covariates$ecivil)
    V(descent_net)$r3 <- as.numeric(covariates$r3)
    V(descent_net)$s28 <- as.numeric(covariates$s28)
    V(descent_net)$comuna <- as.numeric(covariates$comuna)
    V(descent_net)$region <- as.numeric(covariates$region)
    V(descent_net)$ytotcor <- as.numeric(covariates$ytotcor)
    V(descent_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
    V(descent_net)$edad_legal <- as.numeric(covariates$edad_legal)
    V(descent_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
    grafo <- list(i = i, descent_net = descent_net)
  } else {
    warning("La cantidad de vértices no coincide con la cantidad de atributos para el hogar ", i)
  }
  
  return(grafo)
}

unique_households <- unique(data$household)

# Usar foreach para ejecutar en paralelo
descent_igrpah_filtred <- foreach(i = unique_households, .packages = c(
  "tidyverse", "igraph", "haven", "tibble", "reshape2", "tryCatchLog",
  "futile.logger", "dplyr", "tidyr", "doParallel", "iterators", "parallel", "progress"
)) %dopar% {
  household_process(i, data)
}

end.time <- Sys.time()
stopCluster(cl) # Detener el clúster
time.taken_parallel <- end.time - start.time
time.taken_parallel

successful_graphs <- descent_igrpah_filtred[!sapply(descent_igrpah_filtred, is.null)]
failed_graphs <- descent_igrpah_filtred[sapply(descent_igrpah_filtred, is.null)]
unprocessed_households <- setdiff(unique(data$household), names(successful_graphs))

# Resultados finales
message("Total hogares procesados: ", length(unique(data$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

# Guardar los resultados en un archivo
save(descent_igrpah_filtred, file = paste0("Ergomitos/Redes/descent_igrpah_filtrados.RData"))
beep(1)

#Creamos una lista en formato Network
a <- descent_igrpah_filtred

descent_network_filtred <- lapply(a, function(j) {
  j$descent_net <- asNetwork(j$descent_net)
  j
})

save(descent_network_filtred, file = paste0("Ergomitos/Redes/descent_network_filtrados.RData"))
beep(8)

########################## RED DE MATRIMONIO ###################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

start.time <- Sys.time()

options("tryCatchLog.write.error.dump.file" = TRUE)

# Función para procesar cada hogar
household_process <- function(i, data) {
  # Filtrar datos del hogar
  household_i <- data[which(data$household == i), ]
  
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
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s28",  "region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
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
  V(marriage_net)$sex <- as.numeric(covariates$sex)
  V(marriage_net)$edad <- as.numeric(covariates$edad)
  V(marriage_net)$e6a <- as.numeric(covariates$e6a)
  V(marriage_net)$o1 <- as.numeric(covariates$o1)
  V(marriage_net)$r1b_pais_esp <- as.numeric(covariates$r1b_pais_esp)
  V(marriage_net)$ecivil <- as.numeric(covariates$ecivil)
  V(marriage_net)$r3 <- as.numeric(covariates$r3)
  V(marriage_net)$s28 <- as.numeric(covariates$s28)
  V(marriage_net)$comuna <- as.numeric(covariates$comuna)
  V(marriage_net)$region <- as.numeric(covariates$region)
  V(marriage_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(marriage_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(marriage_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(marriage_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  return(list(household_id = i, marriage_net = marriage_net, warning = NULL))
}

# Usar foreach para ejecutar en paralelo
marriage_igraph_filtred <- foreach(i = unique(data$household),
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
    household_process(i, data),
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
successful_graphs <- marriage_igraph_filtred[!sapply(marriage_igraph_filtred, function(x) is.null(x$marriage_net))]
failed_graphs <- marriage_igraph_filtred[sapply(marriage_igraph_filtred, function(x) is.null(x$marriage_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

# Guardar los resultados en un archivo
save(successful_graphs, file = "Ergomitos/Redes/marriage_igraph_filtred_subset100.RData")
beep(1)
#Creamos una lista en formato Network

a <- marriage_igraph_filtred


marriage_network_filtred <- lapply(a, function(j) {
  j$marriage_net <- asNetwork(j$marriage_net)
  j
})

save(marriage_network_filtred, file = paste0("Ergomitos/Redes/marriage_network_filtred.RData"))
beep(8)

# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores() - 1
cl <- parallel::makeCluster(num_cores)
registerDoParallel(cl)

start.time <- Sys.time()
error.list<-list()

household_process <- function(i, data) {
  household_i <- data[which(data$household == i), ]
  
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
  myvars <- c("id_persona", "sex", "edad", "ecivil", "e6a", "o1", "r1b_pais_esp", "r3", "s28", "region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  # Crear el grafo
  dependency_net <- graph_from_data_frame(d = edge_dependency, vertices = nodes, directed = TRUE)
  
  # Asignar atributos a los nodos
  covariates <- covariates[order(covariates$id_persona), ]
  V(dependency_net)$sex <- as.numeric(covariates$sex)
  V(dependency_net)$edad <- as.numeric(covariates$edad)
  V(dependency_net)$e6a <- as.numeric(covariates$e6a)
  V(dependency_net)$o1 <- as.numeric(covariates$o1)
  V(dependency_net)$r1b_pais_esp <- as.numeric(covariates$r1b_pais_esp)
  V(dependency_net)$ecivil <- as.numeric(covariates$ecivil)
  V(dependency_net)$r3 <- as.numeric(covariates$r3)
  V(dependency_net)$s28 <- as.numeric(covariates$s28)
  V(dependency_net)$comuna <- as.numeric(covariates$comuna)
  V(dependency_net)$region <- as.numeric(covariates$region)
  V(dependency_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(dependency_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(dependency_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(dependency_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  return(list(household_id = i, dependency_net = dependency_net, warning = NULL))
}

unique_households <- unique(data$household)

# Usar foreach para ejecutar en paralelo
dependency_igraph_filtred <- foreach(i = unique_households,
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
    household_process(i, data),
    error = function(e) {
      message(paste("Error en el hogar", i, ":", e$message))
      return(list(household_id = i, dependency_net = NULL, warning = e$message))
    }
  )
}
beep(5)

########################## RED DE DEPENDENCIA ##################################
num_cores <- detectCores()-1
registerDoParallel(cores = num_cores)
start.time <- Sys.time()

household_process <- function(i, data) {
  household_i <- data[which(data$household == i),]
  
  
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
  
  myvars <- c("id_persona", "sex", "edad","ecivil","e6a","o1","r1b_pais_esp","r3","s28", "region", "comuna","ytotcor","edad_laboral","edad_legal","edad_dependencia_estudios")
  covariates <- household_i[myvars]
  nodes <- sort(covariates$id_persona)
  
  dependency_net <- graph_from_data_frame(d = edge_dependency, vertices = nodes, directed = TRUE)
  
  covariates <- covariates[order(covariates$id_persona),]
  V(dependency_net)$sex <- as.numeric(covariates$sex)
  V(dependency_net)$edad <- as.numeric(covariates$edad)
  V(dependency_net)$e6a <- as.numeric(covariates$e6a)
  V(dependency_net)$o1 <- as.numeric(covariates$o1)
  V(dependency_net)$r1b_pais_esp <- as.numeric(covariates$r1b_pais_esp)
  V(dependency_net)$ecivil <- as.numeric(covariates$ecivil)
  V(dependency_net)$r3 <- as.numeric(covariates$r3)
  V(dependency_net)$s28 <- as.numeric(covariates$s28)
  V(dependency_net)$comuna <- as.numeric(covariates$comuna)
  V(dependency_net)$region <- as.numeric(covariates$region)
  V(dependency_net)$ytotcor <- as.numeric(covariates$ytotcor)
  V(dependency_net)$edad_laboral <- as.numeric(covariates$edad_laboral)
  V(dependency_net)$edad_legal <- as.numeric(covariates$edad_legal)
  V(dependency_net)$edad_dependencia_estudios <- as.numeric(covariates$edad_dependencia_estudios)
  grafo <- list(household_i = i, dependency_net = dependency_net)
  return(grafo)
}

unique_households <- unique(data$household)

# Usar foreach para ejecutar en paralelo. Acá cambié el nombre para evitar cambiar el archivo que ya está

dependency_igraph_filtred <- foreach(i = unique_households,
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
    household_process(i, data),
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
successful_graphs <- dependency_igraph_filtred[!sapply(dependency_igraph_filtred, function(x) is.null(x$dependency_net))]
failed_graphs <- dependency_igraph_filtred[sapply(dependency_igraph_filtred, function(x) is.null(x$dependency_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs)) #93 fallidos

# Guardar los resultados en un archivo
save(dependency_igraph_filtred, file = "Ergomitos/Redes/dependency_igraph_filtred.RData")
beep(1)
# Convertir a formato Network
a <- dependency_igraph_filtred

dependency_network_filtred <- lapply(a, function(j) {
  j$dependency_net <- asNetwork(j$dependency_net)
  j
})

save(dependency_network_filtred, file = "Ergomitos/Redes/dependency_network_filtred.RData")
beep(5)

############################ RED KINSHIP #######################################
# Establecer el número de núcleos para el procesamiento en paralelo
num_cores <- detectCores()-1
cl <- parallel::makeCluster(num_cores)
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
unique_households <- unique(data$household)

kinship_igrpah_filtred <- foreach(i = unique_households,
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
    household_process(i, data),
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
successful_graphs <- kinship_igrpah_filtred[!sapply(kinship_igrpah_filtred, function(x) is.null(x$kinship_net))]
failed_graphs <- kinship_igrpah_filtred[sapply(kinship_igrpah_filtred, function(x) is.null(x$kinship_net))]

# Resultados finales
message("Total hogares procesados: ", length(unique(data$household)))
message("Total hogares completos: ", length(successful_graphs))
message("Total hogares fallidos: ", length(failed_graphs))

save(kinship_igrpah_filtred, file = paste0(getwd(), "/Ergomitos/Redes/kinship_igrpah_filtred.RData"))
beep(1)
a <- kinship_igrpah_filtred
kinship_network_filtred<- lapply(a, function(j) {
  j$kinship_net <- asNetwork(j$kinship_net)
  j
})
save(kinship_network_filtred, file = paste0(getwd(), "/Ergomitos/Redes/kinship_network_filtred.RData"))
beep(8)
