################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
#######################4. Buscar estructuras de las redes##########################
################################################################################
#Este script describe las tipologías de las redes creadas a partir de la encuesta CASEN#

#Librerías
library(dplyr)
library(tidyr)

#Datos
load("Análisis de viviendas/Data/Data_con_tipología.RData")
load("Análisis de viviendas/Data/Data_analisis.RData")

# --------------------------------------------
# 1. Estadísticas de EDAD (jefes de hogar)
# --------------------------------------------
edad_stats <- data_con_tipologia %>%
  filter(pco1 == "Jefe(a) de Hogar") %>%
  group_by(tipologia) %>%
  summarise(
    Edad_promedio_jefe = round(mean(edad, na.rm = TRUE), 1),
    Desviacion_edad_jefe = round(sd(edad, na.rm = TRUE), 1)
  )

# --------------------------------------------
# 2. GÉNERO de jefes de hogar (% Hombres/Mujeres)
# --------------------------------------------
genero_stats <- data_con_tipologia %>%
  filter(pco1 == "Jefe(a) de Hogar") %>%
  group_by(tipologia) %>%
  summarise(
    "%_mujer_jefe" = round(mean(sex == "Mujer", na.rm = TRUE) * 100, 1),
    "%_hombre_jefe" = round(mean(sex == "Hombre", na.rm = TRUE) * 100, 1)
  )

# --------------------------------------------
# 3. UBICACIÓN: Rural/Urbano y Macrozonas
# --------------------------------------------
ubicacion_stats <- data_con_tipologia %>%
  mutate(
    area = ifelse(porc_rural > 50, "Rural", "Urbano"),
    macrozona = case_when(
      region %in% c("Región de Arica y Parinacota", "Región de Tarapacá","Región de Antofagasta","Región de Atacama","Región de Coquimbo") ~ "Norte",
      region %in% c("Región de Valparaíso","Región Metropolitana de Santiago","Región del Libertador Gral. Bernardo O'Higgins", 
                    "Región del Maule", "Región de Ñuble") ~ "Centro",
      region %in% c("Región del Biobío", "Región de La Araucanía", 
                    "Región de Los Ríos", "Región de Los Lagos","Región de Aysén del Gral. Carlos Ibáñez del Campo", "Región de Magallanes y de la Antártica Chilena") ~ "Sur",
      TRUE ~ "Otra"
    )
  ) %>%
  group_by(tipologia) %>%
  summarise(
    # 1. Porcentaje rural/urbano DENTRO de cada tipología
    "%_Urbano" = round(mean(area == "Urbano", na.rm = TRUE) * 100, 1),
    "%_Rural" = round(mean(area == "Rural", na.rm = TRUE) * 100, 1),
    
    # 2. Porcentaje por macrozona DENTRO de cada tipología
    "%_Norte" = round(mean(macrozona == "Norte", na.rm = TRUE) * 100, 1),
    "%_Centro" = round(mean(macrozona == "Centro", na.rm = TRUE) * 100, 1),
    "%_Sur" = round(mean(macrozona == "Sur", na.rm = TRUE) * 100, 1)
  )

# --------------------------------------------
# 4. EXTRANJEROS y PUEBLOS ORIGINARIOS
# --------------------------------------------
origen_stats <- data_con_tipologia %>% 
  group_by(household, tipologia) %>%
  summarise(
    "%_Viviendas_Extranjeras" = round(mean(tiene_extranjero) * 100, 1),
    "%_Viviendas_Indigenas"   = round(mean(tiene_indigena) * 100, 1)
  )

# --------------------------------------------
# 5. INGRESO Mediano mejor
# --------------------------------------------
ingreso_por_vivienda <- data_con_tipologia %>%
  group_by(household, tipologia) %>%  # Agrupar por vivienda y tipología
  summarise(
    ingreso_promedio_vivienda = mean(ytotcor, na.rm = TRUE),  # Promedio interno
    n_miembros = n()  # Número de personas en la vivienda (opcional)
  ) %>%
  ungroup()

ingresos <- ingreso_por_vivienda %>%
  group_by(tipologia) %>%
  summarise(#"Ingreso_medio" =mean(ingreso_promedio_vivienda, na.rm=T),
    "Ingreso_mediano" = median(ingreso_promedio_vivienda, na.rm = TRUE),
            "Q1" =quantile(ingreso_promedio_vivienda, 0.25, na.rm =T),
            "Q3" =quantile(ingreso_promedio_vivienda, 0.75, na.rm =T))

# --------------------------------------------
# TABLA FINAL COMBINADA (por tipología)
# --------------------------------------------
tabla_final <- edad_stats %>%
  left_join(genero_stats, by = "tipologia") %>%
  left_join(ubicacion_stats, by = "tipologia") %>%
  left_join(origen_stats, by = "tipologia") %>%
  left_join(origen_stats_2, by = "tipologia") %>%
  left_join(ingresos, by = "tipologia") %>%
  mutate(tipologia = factor(tipologia, 
                            levels = paste0("T", 1:48),
                            ordered = TRUE)) %>%
  # Ordenar por tipologia
  arrange(tipologia)
# Mostrar tabla
View(tabla_final)
# Guardar
write.csv(tabla_final, "Análisis de viviendas/Analisis/descriptives_tipologias.csv", row.names = FALSE, fileEncoding = "UTF-8")
# Análisis para variables específicas
analisis_completo <- tabla_final %>%
  # Seleccionar variables numéricas (ajusta según tus columnas)
  select(tipologia, where(is.numeric)) %>%
  pivot_longer(
    cols = -tipologia,
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    max_tipologia = tipologia[which.max(valor)],
    max_valor = max(valor, na.rm = TRUE),
    min_tipologia = tipologia[which.min(valor)],
    min_valor = min(valor, na.rm = TRUE),
    .groups = 'drop'
  )

Publish::publish(analisis_completo)
