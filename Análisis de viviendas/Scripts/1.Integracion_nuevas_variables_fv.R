################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ 1. ESTADÍSTICOS REDES ################################
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

data_analisis <- ori_Casen2020_STATA %>%
  select(folio, id_vivienda, id_persona, 
         edad, sexo,                    # demografía básica
         r1b_pais_esp,                  # nacionalidad
         pco1,                          # relación con jefe de hogar
         r3,                            # pueblo indígena
         s28,                           # enfermedad crónica
         comuna, region,                # geografía
         ytotcor,                       # ingreso total hogar
         activ, esc, educ,              # empleo y educación
         qaut, pobreza,                 # quintil y pobreza
         tipohogar,                     # DEM08 (estructura hogar)
         hacinamiento, iai, iae) %>%    # vivienda
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = folio, sex = sexo)

data_analisis <- data_analisis %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    # Convertir a factor variables que vienen como numéricas con etiquetas
    across(c(pco1, r3, region, comuna, activ, qaut, pobreza, tipohogar, hacinamiento), as_factor),
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
      as.numeric(s28) == 1  ~ 1,
      as.numeric(s28) == 2  ~ 0,
      as.numeric(s28) == 3  ~ NA_integer_,
      is.na(s28)            ~ NA_integer_,
      TRUE                  ~ NA_integer_
    ) %>% factor(levels = c(0, 1), labels = c("Sin enfermedad crónica", "Enfermedad crónica"))
  )

# Orden geográfico de regiones (según manual, pág. 171)
region_order <- c(15, 1, 2, 3, 4, 5, 13, 6, 7, 16, 8, 9, 14, 10, 11, 12)
region_names <- c("Arica y Parinacota", "Tarapacá", "Antofagasta", "Atacama", "Coquimbo",
                  "Valparaíso", "Metropolitana", "O'Higgins", "Maule", "Ñuble", "Biobío",
                  "La Araucanía", "Los Ríos", "Los Lagos", "Aysén", "Magallanes")
data_analisis <- data_analisis %>%
  mutate(region_num = as.numeric(region),
         region_o = factor(region_num, levels = region_order, labels = region_names))

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
tamano_hogares <- data_analisis %>%
  group_by(household) %>%
  summarise(tamano = n()) %>%
  ungroup() %>%
  group_by(tamano) %>%
  summarise(n_hogares = n()) %>%
  mutate(porcentaje = round((n_hogares / sum(n_hogares)) * 100, 2))
publish(tamano_hogares)

###Utiliza set de la data ###
set.seed(400)
id_sample <- sample(unique(data_analisis$household), size = 1000, replace = FALSE)
data_subset <- data_analisis %>% filter(household %in% id_sample)
save(data_subset, file = "Análisis de viviendas/Data/Data_subset.RData")

# Verificar el número de filas en el subset
nrow(data_subset)

save(data_subset, file = "Análisis de viviendas/Data/Data_subset.RData")

load("Análisis de viviendas/Data/Data_subset.RData")
load("Análisis de viviendas/Data/Data.RData")

######################### CALCULO DE ESTADISTICOS ##############################
### Crear listas y data frame vacios para recopilar información 
measurements <- data.frame()

resultados_list <- list()

# Loop principal sobre cada hogar
for (i in unique(data_analisis$household)) {
  tryCatch({
    vivienda_i <- data_analisis[data_analisis$household == i, ]
    n_miembros <- nrow(vivienda_i)
    
    # --- Variables básicas (porcentaje hombres, edad, indígenas) ---
    n_hombres <- sum(vivienda_i$sex == "Hombre", na.rm = TRUE)
    porc_hombre <- ifelse(n_miembros > 0, (n_hombres / n_miembros) * 100, 0)
    edad_prom <- mean(vivienda_i$edad, na.rm = TRUE)
    edad_sd <- sd(vivienda_i$edad, na.rm = TRUE)
    n_indigena <- sum(vivienda_i$r3 == "Pertenece", na.rm = TRUE)
    tiene_indigena <- n_indigena > 0
    porc_ind <- ifelse(n_miembros > 0, (n_indigena / n_miembros) * 100, 0)
    
    # --- Geografía ---
    comuna <- as.character(unique(vivienda_i$comuna))[1]
    region <- as.character(unique(vivienda_i$region_o))[1]
    
    # --- Empleo ---
    n_activos <- sum(vivienda_i$activ %in% c("Ocupados", "Desocupados"), na.rm = TRUE)
    n_desocupados <- sum(vivienda_i$activ == "Desocupados", na.rm = TRUE)
    tasa_desocupacion <- ifelse(n_activos > 0, n_desocupados / n_activos, 0) # Proporción de desocupados dentro de los activos
    
    n_inactivos <- sum(vivienda_i$activ == "Inactivos", na.rm = TRUE)
    pob_ocupable <- sum(vivienda_i$edad >= 15, na.rm = TRUE)
    tasa_inactividad <- ifelse(pob_ocupable > 0, n_inactivos / pob_ocupable, NA) # Inactivos en edad de trabajar
    
    # --- Salud ---
    n_enfermos <- sum(vivienda_i$s28 == "Enfermedad crónica", na.rm = TRUE)
    porc_enfermos <- ifelse(n_miembros > 0, (n_enfermos / n_miembros) * 100, 0)
    
    # --- Nacionalidad ---
    tiene_extranjero <- any(vivienda_i$r1b_pais_esp == "Extranjero", na.rm = TRUE)
    
    # --- Ingreso total del hogar ---
    sueldo <- mean(vivienda_i$ytotcor, na.rm = TRUE)
    
    # --- DEM08 (tipohogar CASEN) ---
    tipologia_dem08 <- as.character(unique(vivienda_i$tipohogar)[1])
    
    # --- DEM09 ---
    gen1 <- any(vivienda_i$edad %in% 0:14, na.rm = TRUE)
    gen2 <- any(vivienda_i$edad %in% 15:64, na.rm = TRUE)
    gen3 <- any(vivienda_i$edad >= 65, na.rm = TRUE)
    tipo_gen <- case_when(
      gen1 & gen2 & gen3 ~ "Multigeneracional",
      gen1 & !gen2 & gen3 ~ "Sin generación intermedia",
      gen1 & gen2 & !gen3 ~ "Sin adultos mayores",
      !gen1 & gen2 & gen3 ~ "Sin menores de 15",
      gen1 & !gen2 & !gen3 ~ "Solo menores de 15",
      !gen1 & gen2 & !gen3 ~ "Solo 15-64",
      !gen1 & !gen2 & gen3 ~ "Solo mayores de 64",
      TRUE ~ "No clasificable"
    )
    
    # --- Quintil y pobreza ---
    quintil_ingreso <- unique(vivienda_i$qaut[!is.na(vivienda_i$qaut)])[1] %>% as.character()
    pobreza_hogar <- unique(vivienda_i$pobreza[!is.na(vivienda_i$pobreza)])[1] %>% as.character()
    
    # --- Nivel educacional del jefe ---
    niveles_educ <- levels(data_analisis$educ)
    jefe <- vivienda_i %>% filter(pco1 == "Jefe(a) de Hogar")
    if (nrow(jefe) == 0) {
      nivel_educ_jefe <- factor(NA_character_, levels = niveles_educ)
    } else {
      nivel_educ_jefe <- jefe$educ[1]
    }
    max_escolaridad <- if (all(is.na(vivienda_i$esc))) NA else max(vivienda_i$esc, na.rm = TRUE)

    
    # --- VIV03: Hacinamiento ---
    # hacinamiento es factor con niveles: 1 Sin, 2 Medio, 3 Alto, 4 Crítico
    hacinamiento_cat <- as.character(unique(vivienda_i$hacinamiento)[1])
    
    # --- VIV04: Allegamiento interno (iai) ---
    allegamiento_interno <- (unique(vivienda_i$iai)[1] == 1)
    
    # --- VIV05: Allegamiento externo (iae) ---
    allegamiento_externo <- (unique(vivienda_i$iae)[1] == 1)
    
    # --- Construir tibble de resultados ---
    res_fila <- tibble(
      household = i,
      porc_hombre,
      porc_ind,
      tiene_indigena,
      tasa_desocupacion,
      tasa_inactividad,
      porc_enfermos,
      tiene_extranjero,
      sueldo,
      edad_prom,
      edad_sd,
      region,
      comuna,
      tipologia_dem08,
      tipo_gen,
      quintil_ingreso,
      pobreza_hogar,
      max_escolaridad,
      nivel_educ_jefe,
      hacinamiento_cat,
      allegamiento_interno,
      allegamiento_externo
    )
    
    resultados_list[[length(resultados_list) + 1]] <- res_fila
    
  }, error = function(e) {
    message(paste("Error en household:", i, "-", e$message))
  })
}

measurements <- bind_rows(resultados_list) %>% distinct()

# Inspeccionar resultados
head(measurements)
dim(measurements)
table(measurements$tipologia_dem08, useNA = "ifany")
table(measurements$tipo_gen, useNA = "ifany")
table(measurements$quintil_ingreso, useNA = "ifany")
table(measurements$hacinamiento_cat, useNA = "ifany")
table(measurements$nivel_educ_jefe, useNA = "ifany")

# Guardar resultados
if (!dir.exists("Análisis de viviendas/Descriptives")) {
  dir.create("Análisis de viviendas/Descriptives")
}
save(measurements, file = "Análisis de viviendas/Descriptives/medidas_redes.RData")
load("Análisis de viviendas/Descriptives/medidas_redes.RData")