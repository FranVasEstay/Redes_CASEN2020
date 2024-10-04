################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ CREACIÓN DE REDES #################################
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

### Filtrar datos utilizados ###
##¿Cuantas viviendas tienen un id de persona repetido? ¿CuáleS?
a <- paste(ori_Casen2020_STATA$id_vivienda, ori_Casen2020_STATA$id_persona)
#Cuantas
cat("Número de viviendas con id de persona repetido:", sum(duplicated(a)), "\n")
#Cuales
cat("id_vivienda_repetida id_persona_repetida\n")
print(a[which(duplicated(a))])

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

##¿Cuantas viviendas tienen un id de persona repetido? ¿CuáleS?
a <- paste(data$household, data$id_persona)
#Cuantas
cat("Número de viviendas con id de persona repetido:", sum(duplicated(a)), "\n")
#Cuales
cat("id_vivienda_repetida id_persona_repetida\n")
print(a[which(duplicated(a))])

###Utiliza set de la data ###
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)

# Crear subset con los 1000 id_vivienda seleccionados
data_subset <- data %>%
  filter(household %in% id_vivienda_sample)

# Verificar el número de filas en el subset
nrow(data_subset)

### Crear listas y data frame vacios para recopilar información 
measurements <- data.frame()

#Error handling
options("tryCatchLog.write.error.dump.file" = TRUE)

for (i in unique(data$household)) {
  tryCatch({
    # Filtrar la vivienda actual
    vivienda_i <- data[data$household == i, ]
    
    # Contar hombres y mujeres
    n.hombres <- sum(vivienda_i$sexo == "Hombre", na.rm = TRUE)
    n.mujeres <- sum(vivienda_i$sexo == "Mujer", na.rm = TRUE)
    porc.hombre <- (n.hombres / (n.hombres + n.mujeres)) * 100
    porc.ind[is.nan(porc.ind)] <- 0
    
    # Media y desviación estándar de la edad
    edad.prom <- mean(vivienda_i$edad, na.rm = TRUE)
    edad.sd <- sd(vivienda_i$edad, na.rm = TRUE)
    
    # Contar indígenas
    indtab <- vivienda_i %>% count(r3, .drop = FALSE)
    n.ind <- sum(indtab$n, na.rm = TRUE) - indtab$n[which(indtab$r3 == "No indígena")]
    porc.ind <- ifelse(sum(!is.na(vivienda_i$r3)) == 0, 0, (n.ind / sum(!is.na(vivienda_i$r3))) * 100)
    porc.ind[is.na(porc.ind)] <- 0
    
    # Obtener región y comuna
    region <- unique(vivienda_i$region)
    comuna <- unique(vivienda_i$comuna)
    
    # Cálculo del empleo
    n.siW <-  sum(vivienda_i$o1 == "Sí", na.rm = TRUE)
    porc.empleo <- (n.siW / sum(!is.na(vivienda_i$o1)))* 100
    
    # Atención de salud
    n.saludsi <- sum(vivienda_i$s16 == "Sí", na.rm = TRUE)
    porc.salud <- (n.saludsi / sum(!is.na(vivienda_i$s16))) * 100
    
    # Nacionalidad
    n.chi <- sum(vivienda_i$r1b_pais_esp == 1, na.rm = TRUE)
    porc.chi <- (n.chi / sum(!is.na(vivienda_i$r1b_pais_esp))) * 100
    
    # Salario líquido
    n.siS <- sum(vivienda_i$y1_preg == "Sí", na.rm = TRUE)
    porc.sal <- (n.siS / sum(!is.na(vivienda_i$y1_preg))) * 100
    porc.sal[is.nan(porc.sal)] <- 0
    
    # Crear tabla de resultados
    table_households <- tibble(i, porc.hombre, porc.ind, porc.empleo, porc.salud, porc.chi, porc.sal, edad.prom, edad.sd, region, comuna)
    measurements <- rbind(measurements, table_households)
    measurements <- distinct(measurements)

  }, error = function(e) {
    message(paste("Error en id_vivienda:", i, ":", e$message))
  })
}


# Unir todos los resultados en un solo dataframe
measurements <- bind_rows(measurements)
head(measurements)

# Guardar resultados
save(measurements, file = "Descriptives/mediciones_redes.RData")
