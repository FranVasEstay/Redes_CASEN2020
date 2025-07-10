################################################################################
########### ANÁLISIS DE LAS TIPOLOGÍAS MÁS FRECUENTES ##########################
################################################################################
#Este script calcula varios estadísticos y crea visualizaciones que tienen por fin analizar las tipologías creadas de las redes kinship de la encuesta CASEN#

#Liberías
library(dplyr)
library(igraph)
library(ggplot2)
library(ggpubr)
library(purrr)
library(tidyr)
library(tidyverse)
library(scales)
library(stringr)
   
# Cargar datos necesarios
load("Ergomitos/Redes/kinship_igrpah.RData") #Redes
households_por_tipologia <- readRDS("C:/Users/Kibif/Desktop/Redes Diego/Directorio_proyecto/Redes_CASEN2020/Análisis de viviendas/Analisis/Resultados_Tipologias/Reportes/households_por_tipologia.rds") # contiene los households para cada tipología
muestra_tipologias <- read.csv("Análisis de viviendas/Data/muestra_tipologias.csv",sep = "\t") #tipologias
load("Análisis de viviendas/Descriptives/medidas_redes.RData") #Datos de medidas
porc_rural <- read.csv("Análisis de viviendas/Data/porc_rural_comuna.csv",sep = ",",encoding="UTF-8") #porcentaje de pob.rural por comuna

################################################################################
# PASO 1: PREPARACION DE DATOS
################################################################################
porc_rural <- porc_rural %>%
  mutate(
    # Extraer solo el nombre de la comuna (eliminando región y paréntesis)
    comuna_clean = str_remove(Unidad.territorial, "\\s*\\(.*\\)"),
    comuna_clean = str_trim(comuna_clean),
    # Corregir caracteres especiales
    comuna_clean = iconv(comuna_clean, from = "UTF-8", to = "ASCII//TRANSLIT"),
    comuna_clean = str_to_title(comuna_clean)
  ) %>%
  select(comuna = comuna_clean, porc_rural = X2020)

# Ver resultados
head(porc_rural)

# Primero asegurémonos que las comunas en measurements estén limpias
measurements <- measurements %>%
  mutate(
    comuna_clean = iconv(comuna, from = "UTF-8", to = "ASCII//TRANSLIT"),
    comuna_clean = str_to_title(comuna_clean)
  )
# Unir los datos # no quedan todos bien puestos
measurements <- measurements %>%
  left_join(porc_rural, by = c("comuna_clean" = "comuna")) %>%
  select(-comuna_clean)  # Eliminar columna temporal

sin_match <- measurements %>%
  filter(is.na(porc_rural)) %>%
  distinct(comuna)


if(nrow(sin_match) > 0) {
  message("Las siguientes comunas no encontraron match:")
  print(sin_match)
} else {
  message("Todas las comunas encontraron match exitosamente")
}

# Expandir households por tipología
tipologia_larga <- households_por_tipologia %>%
  select(Tipologia, Households) %>%
  mutate(Households = str_split(Households, ";")) %>% 
  unnest(Households) %>%
  rename(household = Households, tipologia = Tipologia) %>%
  mutate(household = as.numeric(household))  # Asegurarse de que household es numérico

# Unir con la data de personas
data_con_tipologia <- measurements %>%
  left_join(tipologia_larga, by = "household") %>%
  filter(!is.na(tipologia))

# Asegúrate de que el nombre de columna coincida (Tipologia con T mayúscula o minúscula según corresponda)
tipologias_validas <- unique(muestra_tipologias$Tipologia)

# Filtrar solo esas tipologías
data_con_tipologia <- data_con_tipologia %>%
  filter(tipologia %in% tipologias_validas)


# Preparación correcta de variables categóricas
data_con_tipologia <- data_con_tipologia %>%
  mutate(
    # Convertir porcentajes rurales en categorías
    rural_cat = cut(porc_rural, 
                    breaks = c(0, 30, 70, 100),
                    labels = c("Urbano", "Mixto", "Rural")),
    
    # Categorizar sueldos por quintiles
    sueldo_cat = ntile(sueldo, 5),
    sueldo_cat = factor(sueldo_cat, 
                        labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
  )
# Ordenar por tipologia
data_con_tipologia <- data_con_tipologia[order(data_con_tipologia$tipologia), ]

View(data_con_tipologia)
save(data_con_tipologia, file = "Análisis de viviendas/Data/Data_con_tipología.RData")
################################################################################
# PASO 2: CHI CUADRADO (PRUEBAS ESTADÍSTICAS)
################################################################################
# Analizar asociación entre tipologías y otras variables categóricas
# Variables categóricas corregidas
variables_categoricas <- c("comuna", "region", "rural_cat", "sueldo_cat")

# Función para pruebas estadísticas
realizar_pruebas <- function(tipologia_sel) {
  datos_filtrados <- data_con_tipologia %>%
    mutate(grupo = tipologia == tipologia_sel)
  
  # 1. Prueba para ingresos (Kruskal-Wallis para variable ordinal)
  prueba_ingreso <- kruskal.test(sueldo ~ grupo, data = datos_filtrados)
  
  # 2. Prueba para región (Chi-cuadrado)
  tabla_region <- table(datos_filtrados$region, datos_filtrados$grupo)
  prueba_region <- chisq.test(tabla_region)
  
  # 3. Prueba para ruralidad (Chi-cuadrado)
  tabla_rural <- table(datos_filtrados$rural_cat, datos_filtrados$grupo)
  prueba_rural <- chisq.test(tabla_rural)
  
  # Devolver resultados
  tibble(
    Tipologia = tipologia_sel,
    `p-valor (Ingreso)` = prueba_ingreso$p.value,
    `p-valor (Región)` = prueba_region$p.value,
    `p-valor (Ruralidad)` = prueba_rural$p.value,
    `Efecto Rural (V Cramer)` = sqrt(prueba_rural$statistic / sum(tabla_rural))
  )
}

# Aplicar a todas las tipologías
resultados_comparativos <- map_dfr(unique(data_con_tipologia$tipologia), realizar_pruebas) %>%
  mutate(across(contains("p-valor"), ~round(., 4)))

# Guardar resultados
write_csv(resultados_comparativos, "resultados_comparativos_tipologias.csv")

# Heatmap de significancia
resultados_comparativos %>%
  pivot_longer(cols = -Tipologia, names_to = "Variable", values_to = "Valor") %>%
  filter(str_detect(Variable, "p-valor")) %>%
  mutate(Significativo = Valor < 0.05) %>%
  ggplot(aes(x = Tipologia, y = Variable, fill = Significativo)) +
  geom_tile(color = "white") +
  #geom_text(aes(label = round(Valor, 3)), color = "black", size = 3) +
  scale_fill_manual(values = c("FALSE" = "gray90", "TRUE" = "steelblue")) +
  labs(title = "Significancia estadística por tipología y variable",
       x = "Tipología", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#p_value < 0.05 → Hay evidencia estadística de que la distribución de esa variable varía en esta tipología comparada con el resto.
#Ejemplo: T1 - sex - p = 0.00001 significa que en la red T1 hay una distribución de sexo distinta a las otras tipologías.

#p_value >= 0.05 → No hay evidencia estadística de asociación; la distribución de la variable no varía significativamente entre T1 y el resto.
#Ejemplo: T2 - r3 - p = 0.58 → los hogares indígenas o no indígenas están distribuidos igual en T2 y fuera de T2.