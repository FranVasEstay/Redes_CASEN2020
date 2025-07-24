################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ ESTADÍSTICOS REDES ################################
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
library(Publish)

###### CARGAR DATA ######

load("Análisis de viviendas/Data/ori_Casen2020_rdata.RData")

##################### ADMINISTRACIÓN DE LOS DATOS ##############################
### Revisión de la base de datos ###
library(dplyr)

hogares_por_vivienda <- ori_Casen2020_STATA %>%
  group_by(id_vivienda) %>%
  summarise(n_hogares = n_distinct(hogar))   # Contamos cuántos hogares hay por vivienda

head(hogares_por_vivienda)

#¿cuántas viviendas tienen 1,2,3 hogares ?
resumen_hogares <- hogares_por_vivienda %>%
  count(n_hogares, name = "n_viviendas")

print(resumen_hogares) # Son 371 viviendas con 2 hogares.

#¿ qué viviendas tienen más de un hogar?
hogares_por_vivienda <- ori_Casen2020_STATA %>%
  group_by(id_vivienda) %>%
  summarise(n_hogares = n_distinct(hogar)) %>%
  filter(n_hogares > 1)  # Filtrar solo las viviendas con más de un hogar

head(hogares_por_vivienda)
length(hogares_por_vivienda$id_vivienda)

### Filtrar datos utilizados ###
##¿Cuantas viviendas tienen un id de persona repetido? ¿CuáleS?
a <- paste(ori_Casen2020_STATA$id_vivienda, ori_Casen2020_STATA$id_persona)
#Cuantas
cat("Número de viviendas con id de persona repetido:", sum(duplicated(a)), "\n")
#Cuales
cat("id_vivienda_repetida id_persona_repetida\n")
print(a[which(duplicated(a))])

## Variables en la data ###
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

data_analisis<- ori_Casen2020_STATA %>%
  select(folio,id_vivienda,
         id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2,nucleo, pco2, r3,s28, comuna, region,ytotcor) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = folio, sex = sexo) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    across(c(e6a,pco1,pco2, r3,o1, region, comuna), as_factor),
    #ecivil = case_when(
    #  as.numeric(ecivil) %in% 1:3 ~ 1,
    #  as.numeric(ecivil) %in% 4:8 ~ 2,
    #  TRUE ~ NA
    #) %>% factor(ecivil,levels = c(1, 2), labels = c("En pareja", "Sin pareja")),
    r3 = case_when(
      as.numeric(r3) %in% 1:10 ~ 1,
      TRUE ~ 2
    ) %>% factor(levels = c(1, 2), labels = c("Pertenece", "No pertenece")),
    r1b_pais_esp = case_when(
      r1b_pais_esp == "" ~ 1,
      r1b_pais_esp == "NO RESPONDE" ~ NA,
      TRUE ~ 2
    ) %>% factor(levels = c(1, 2), labels = c("Chileno", "Extranjero")),
    s28 = case_when(
      as.numeric(s28) %in% c(2, 22) ~ 1,
      as.numeric(s28) == 99 ~ NA,
      TRUE ~ 2
    ) %>% factor(levels = c(1, 2), labels = c("Sin enfermedad crónica", "Enfermedad crónica")),
    # Nuevas variables de edad
    edad_laboral = ifelse(edad >= 15, 1, 0),          # 15+ años
    edad_legal = ifelse(edad >= 18, 1, 0),            # 18+ años
    edad_dependencia_estudios = ifelse(edad >= 28, 1, 0),  # 28+ años
  )
length(unique(data_analisis$household)) # 62537 con id_vivienda y filtro, 62907 con folio
save(data_analisis, file = "Análisis de viviendas/Data/Data_analisis.RData")

 ##¿Cuantas viviendas tienen un id de persona repetido? ¿CuáleS?
a <- paste(data$household, data$id_persona)
#Cuantas
cat("Número de viviendas con id de persona repetido:", sum(duplicated(a)), "\n")
#Cuales
cat("id_vivienda_repetida id_persona_repetida\n")
print(a[which(duplicated(a))])

# Calcular el tamaño del hogar y los porcentajes
tamaño_hogares <- data %>%
  group_by(household) %>%
  summarise(tamaño = n()) %>% 
  ungroup() %>%
  group_by(tamaño) %>%
  summarise(n_hogares = n()) %>% 
  mutate(
    porcentaje = round((n_hogares / sum(n_hogares)) * 100, 2)
  )

# Mostrar los resultados
publish(tamaño_hogares)

###Utiliza set de la data ###
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)
# Crear subset con los 1000 id_vivienda seleccionados
data_subset <- data %>%
  filter(household %in% id_vivienda_sample)

# Verificar el número de filas en el subset
nrow(data_subset)

save(data_subset, file = "Análisis de viviendas/Data/Data_subset.RData")

load("Análisis de viviendas/Data/Data_subset.RData")
load("Análisis de viviendas/Data/Data.RData")

######################### CALCULO DE ESTADISTICOS ##############################
### Crear listas y data frame vacios para recopilar información 
measurements <- data.frame()

ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
#Error handling
options("tryCatchLog.write.error.dump.file" = TRUE)

for (i in unique(data_analisis$household)) {
  tryCatch({
    # Filtrar la vivienda actual
    vivienda_i <- data_analisis[data_analisis$household == i, ]
    # Variables básicas
    n_miembros <- nrow(vivienda_i)
    
    # Contar hombres y mujeres 
    n_hombres <- sum(vivienda_i$sex == "Hombre", na.rm = TRUE)
    porc_hombre <- ifelse(n_miembros > 0, (n_hombres/n_miembros)*100, 0)
    
    # Media y desviación estándar de la edad
    edad.prom <- mean(vivienda_i$edad, na.rm = TRUE)
    edad.sd <- sd(vivienda_i$edad, na.rm = TRUE)
    
    # Contar indígenas 
    n_indigena <- sum(vivienda_i$r3 == "Pertenece", na.rm = TRUE)
    porc_ind <- ifelse(n_miembros > 0, (n_indigena/n_miembros)*100, 0)

    # Obtener región y comuna
    region <- as.character(unique(vivienda_i$region))[1]  # Tomar el primer valor si hay múltiples
    comuna <- as.character(unique(vivienda_i$comuna))[1]

    # Cálculo del empleo (sale mal)
    n_empleo <- sum(vivienda_i$o1 == "Sí", na.rm = TRUE)
    total_empleo <- sum(!is.na(vivienda_i$o1))
    porc_empleo <- ifelse(total_empleo > 0, (n_empleo/total_empleo)*100, 0)
    
    # Condición de salud
    n_enfermo <- sum(vivienda_i$s28 == "Enfermedad crónica", na.rm = TRUE)
    total_salud <- sum(!is.na(vivienda_i$s28))
    porc_enf <- ifelse(total_salud > 0, (n_enfermo/total_salud)*100, 0)
    
    # Nacionalidad (sale mal)
    n_chileno <- sum(vivienda_i$r1b_pais_esp == "Chileno", na.rm = TRUE)
    total_nac <- sum(!is.na(vivienda_i$r1b_pais_esp))
    porc_chi <- ifelse(total_nac > 0, (n_chileno/total_nac)*100, 0)
    
    #Sueldo vivienda
    sueldo <- mean(vivienda_i$ytotcor)
    
    # Crear tabla de resultados
    table_households <- tibble(
      household = i, 
      porc_hombre, 
      porc_ind, 
      porc_empleo, 
      porc_enf, 
      porc_chi,
      sueldo, 
      edad.prom, 
      edad.sd, 
      region, 
      comuna
    )
    
    measurements <- rbind(measurements, table_households)
    
  }, error = function(e) {
    message(paste("Error en household:", i, ":", e$message))
  })
}

# Eliminar duplicados
measurements <- distinct(measurements)

stopCluster(cl)
head(measurements)

# Guardar resultados
if (!dir.exists("Análisis de viviendas/Descriptives")) {
  dir.create("Análisis de viviendas/Descriptives")
}
save(measurements, file = "Análisis de viviendas/Descriptives/medidas_redes.RData")

load("Análisis de viviendas/Descriptives/medidas_redes.RData")

