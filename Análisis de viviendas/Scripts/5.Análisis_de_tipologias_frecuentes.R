################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
########### ANÁLISIS DE LAS TIPOLOGÍAS MÁS FRECUENTES ##########################
################################################################################
#Este script calcula varios estadísticos y crea visualizaciones que tienen por fin analizar las tipologías creadas de las redes kinship de la encuesta CASEN#

#Liberías
library(dplyr)
library(igraph)
library(ggmosaic)
library(ggplot2)
library(ggpubr)
library(purrr)
library(tidyr)
library(tidyverse)
library(rstatix)
library(scales)
library(stringr)
library(vcd)
   
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
chi_detallado <- function(tipologia_sel, variable_cat) {
  datos_filtrados <- data_con_tipologia %>%
    mutate(grupo = tipologia == tipologia_sel)
  
  # Crear tabla de contingencia
  tabla <- table(datos_filtrados[[variable_cat]], datos_filtrados$grupo)
  
  # Ejecutar prueba de chi-cuadrado
  prueba <- suppressWarnings(chisq.test(tabla))
  
  # Medida de efecto: V de Cramer
  v_cramer <- suppressWarnings(cramer_v(tabla, correct = FALSE))
  
  # Extraer info en formato amigable
  tibble(
    Tipologia = tipologia_sel,
    Variable = variable_cat,
    Chi2 = as.numeric(prueba$statistic),
    df = as.integer(prueba$parameter),
    p_value = as.numeric(prueba$p.value),
    Min_esperado = min(prueba$expected),
    Cramer_V = v_cramer
  )
}
# Aplicar la función para cada combinación de tipología y variable
resultados_chi <- expand_grid(
  tipologia = unique(data_con_tipologia$tipologia),
  variable = variables_categoricas
) %>%
  pmap_dfr(~chi_detallado(..1, ..2))
# Redondear para presentación
resultados_chi <- resultados_chi %>%
  mutate(across(c(Chi2, p_value, Cramer_V, Min_esperado), ~round(., 4)))

# Guardar resultados
write_csv(resultados_chi, "Análisis de viviendas/Analisis/resultados_chi_tipologias.csv")

# Heatmap de significancia
resultados_chi %>%
  mutate(Significativo = p_value < 0.05) %>%
  ggplot(aes(x = Tipologia, y = Variable, fill = Significativo)) +
  geom_tile(color = "white") +
  geom_text(aes(label = format(round(Chi2, 2))),
            angle = 90,           # Rotar el texto
            vjust = 0.5,          # Ajuste vertical (centrado)
            hjust = 0.5,          # Ajuste horizontal (centrado)
            color = "black", size = 3) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray90")) +
  labs(title = "Significancia estadística (Chi²) por tipología y variable",
       x = "Tipología", y = "Variable", fill = "p < 0.05") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

#p_value < 0.05 → Hay evidencia estadística de que la distribución de esa variable varía en esta tipología comparada con el resto.
#Ejemplo: T1 - sex - p = 0.00001 significa que en la red T1 hay una distribución de sexo distinta a las otras tipologías.

#p_value >= 0.05 → No hay evidencia estadística de asociación; la distribución de la variable no varía significativamente entre T1 y el resto.
#Ejemplo: T2 - r3 - p = 0.58 → los hogares indígenas o no indígenas están distribuidos igual en T2 y fuera de T2.

# Mosaic plot
# Crear carpeta para guardar los gráficos
dir.create("Análisis de viviendas/Analisis/mosaic_plots_por_tipologia", showWarnings = FALSE)

# Función para generar mosaic por variable y tipología (comparando grupo vs resto)
generar_mosaic_por_tipologia <- function(tipologia_sel, variable_cat) {
  datos_filtrados <- data_con_tipologia %>%
    mutate(grupo = ifelse(tipologia == tipologia_sel, tipologia_sel, "Otras")) %>%
    filter(!is.na(.data[[variable_cat]]))
  
  fml <- as.formula(paste("~", variable_cat, "+ grupo"))
  
  nombre_archivo <- paste0("Análisis de viviendas/Analisis/mosaic_plots_por_tipologia/",
                           variable_cat, "_", gsub("[^[:alnum:]]", "_", tipologia_sel), ".png")
  
  png(nombre_archivo, width = 1000, height = 800)
  mosaic(
    fml,
    data = datos_filtrados,
    shade = TRUE,
    direction = c("v", "h"),
    main = paste("Mosaic plot:", variable_cat, "vs", tipologia_sel),
    labeling_args = list(
      set_varnames = setNames(c("Categoría", "Grupo"), c(variable_cat, "grupo")),
      rot_labels = c(30, 0, 0, 0)
    ),
    margins = c(5, 5, 10, 5)
  )
  dev.off()
}
variables_categoricas_2 <- c("region", "rural_cat", "sueldo_cat")

# Generar combinaciones de tipología y variable categórica
combinaciones <- expand_grid(
  tipologia = unique(data_con_tipologia$tipologia),
  variable = variables_categoricas_2
)

# Ejecutar la función usando pmap
pmap(
  combinaciones,
  generar_mosaic_por_tipologia
)
# Los colores azul y rojo representan los residuos de Pearson de una prueba de chi-cuadrado. Estos residuos indican cuánto difieren las frecuencias observadas de las esperadas bajo la hipótesis de independencia.
# Los casos rojos indican que las frecuencias observadas son menores que las esperadas, mientras que los casos azules indican que son mayores que las esperadas. Gris, lo observado es cercano a lo esperado, sin diferencia importante

# EJEMPLO:  Si la tipologúa 12 y sueldo Q1 está coloreado en azul intenso esto indica que hay más casos observados de sueldos bajos en Tipología 12 de lo que se esperaría si la tipología y el nivel de sueldo fueran independientes. Es decir, la Tipología 12 tiene una tendencia significativa a tener sueldos bajos (sobrerrepresentación).
# Si la celda está de color rojo intenso Hay menos casos observados de sueldos bajos en Tipología 12 de lo esperado bajo independencia. Esto implica que la Tipología 12 tiene una tendencia a no tener sueldos bajos (subrepresentación).