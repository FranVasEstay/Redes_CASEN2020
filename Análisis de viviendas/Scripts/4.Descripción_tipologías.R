################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
#######################Buscar estructuras de las redes##########################
################################################################################
#Este script describe las tipologías de las redes creadas a partir de la encuesta CASEN#

#Librerías
library(dplyr)
library(tidyr)

#Datos
load("Análisis de viviendas/Data/Data_con_tipología.RData")
load("Análisis de viviendas/Data/Data_analisis.RData")


datos_completos_T48 <- data_analisis %>%
  left_join(
    data_con_tipologia %>% 
      select(household, tipologia, porc_rural,porc_chi,porc_ind),  # Incluir porc_rural aquí
    by = "household"
  ) %>% filter(!is.na(tipologia))

# --------------------------------------------
# 1. Estadísticas de EDAD (jefes de hogar)
# --------------------------------------------
edad_stats <- datos_completos_T48 %>%
  filter(pco1 == "Jefe(a) de Hogar") %>%
  group_by(tipologia) %>%
  summarise(
    Edad_promedio_jefe = round(mean(edad, na.rm = TRUE), 1),
    Desviacion_edad_jefe = round(sd(edad, na.rm = TRUE), 1)
  )

# --------------------------------------------
# 2. GÉNERO de jefes de hogar (% Hombres/Mujeres)
# --------------------------------------------
genero_stats <- datos_completos_T48 %>%
  filter(pco1 == "Jefe(a) de Hogar") %>%
  group_by(tipologia) %>%
  summarise(
    "%_mujer_jefe" = round(mean(sex == "Mujer", na.rm = TRUE) * 100, 1),
    "%_hombre_jefe" = round(mean(sex == "Hombre", na.rm = TRUE) * 100, 1)
  )

# --------------------------------------------
# 3. UBICACIÓN: Rural/Urbano y Macrozonas
# --------------------------------------------
ubicacion_stats <- datos_completos_T48 %>%
  mutate(
    area = ifelse(porc_rural > 50, "Rural", "Urbano"),
    macrozona = case_when(
      region %in% c("Región de Arica y Parinacota", "Región de Tarapacá", 
                    "Región de Antofagasta") ~ "Norte",
      region %in% c("Región de Atacama","Región de Coquimbo") ~ "Norte Chico",
      region %in% c("Región de Valparaíso","Región Metropolitana de Santiago","Región del Libertador Gral. Bernardo O'Higgins", 
                    "Región del Maule", "Región de Ñuble") ~ "Centro",
      region %in% c("Región del Biobío", "Región de La Araucanía", 
                    "Región de Los Ríos", "Región de Los Lagos") ~ "Sur",
      region %in% c("Región de Aysén del Gral. Carlos Ibáñez del Campo", "Región de Magallanes y de la Antártica Chilena") ~ "Austral",
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
    "%_Norte_chico" = round(mean(macrozona == "Norte Chico", na.rm = TRUE) * 100, 1),
    "%_Centro" = round(mean(macrozona == "Centro", na.rm = TRUE) * 100, 1),
    "%_Sur" = round(mean(macrozona == "Sur", na.rm = TRUE) * 100, 1),
    "%_Austral" = round(mean(macrozona == "Austral", na.rm = TRUE) * 100, 1)
  )

# --------------------------------------------
# 4. EXTRANJEROS y PUEBLOS ORIGINARIOS
# --------------------------------------------
# Este cuenta a las personas extranjeras en la tipología
# Mide la composición demográfica general de cada tipología. (Foco demográfico)
# No distingue si los extranjeros están concentrados en pocas viviendas o distribuidos homogéneamente
# Puede sobrestimar la influencia en hogares donde solo 1 miembro de por ejemplo 5 sea extranjero
origen_stats <- datos_completos_T48 %>% 
  group_by(tipologia) %>%
  summarise(
    "%_Extranjero" = round(mean(r1b_pais_esp == "Extranjero", na.rm = TRUE) * 100, 1),
    "%_Pueblo_Originario" = round(mean(r3 == "Pertenece", na.rm = TRUE) * 100, 1)
  )

# viviendas extranjeras y viviendas indígneas con umbral del 50% (Foco en la "integración cultural" o segregación residencial)
# Identifica hogares con influencia cultural extranjera dominante (≥50% miembros).
# Pierde información de hogares con menos % de extranjeros/indígenas
viviendas_ext <- datos_completos_T48 %>%
  distinct(household, tipologia, porc_chi) %>%  # Evitar duplicados por hogar
  mutate(
    porc_extranjero = 100 - porc_chi,
    vivienda_extranjera = porc_extranjero >= 50  # Umbral 50%
  )
viviendas_ind <- datos_completos_T48 %>%
  distinct(household, tipologia, porc_ind) %>%  # Evitar duplicados
  mutate(
    vivienda_indigena = porc_ind >= 50  # Umbral 50%
  )
origen_stats_2 <- viviendas_ext %>%
  left_join(viviendas_ind, by = c("household", "tipologia")) %>%
  group_by(tipologia) %>%
  summarise(
    "%_Viviendas_Extranjeras" = round(mean(vivienda_extranjera, na.rm = TRUE) * 100, 1),
    "%_Viviendas_Indigenas" = round(mean(vivienda_indigena, na.rm = TRUE) * 100, 1)
  )

# --------------------------------------------
# 5. INGRESO Mediano mejor
# --------------------------------------------
ingreso_por_vivienda <- datos_completos_T48 %>%
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
  left_join(ingresos, by = "tipologia")
# Mostrar tabla
print(tabla_final)
View(tabla_final)
