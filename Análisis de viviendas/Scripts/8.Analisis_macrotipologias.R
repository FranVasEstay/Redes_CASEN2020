################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
###################### ANÁLISIS DE MACROTIPOLOGÍAS #############################
################################################################################

# LIBRERÍAS
library(igraph)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(vcd)
library(ggmosaic)
library(tidyr)
library(purrr)
library(tidyr)
library(forcats)
library(patchwork)
library(rstatix)
library(kableExtra)
# --- 1. LOAD DATA ---
load("Análisis de viviendas/Data/Data_con_tipología.RData")
macrotipos <- read.csv("Análisis de viviendas/Analisis/Resultados_tipologias/clasificacion_tipologias_con_macrogrupos.csv")

# Merge macrotipologies with main data
data_macro <- data_con_tipologia %>%
  left_join(macrotipos, by = "tipologia") %>%
  filter(!is.na(macrogrupo)) %>%
  mutate(
    macrogrupo = factor(macrogrupo),
    # Ensure categorical variables are factors
    region = factor(region),
    sueldo_cat = factor(sueldo_cat, levels = paste0("Q", 1:5), ordered = TRUE),
    # Create sex variable from porcent_hombre
    sex = ifelse(porc_hombre > 50, "Hombre", "Mujer")
  )

# Clasificar regiones en norte-centro-sur
data_macro <- data_macro %>%
  mutate(zona = case_when(
    region %in% c("Región de Arica y Parinacota", "Región de Tarapacá", "Región de Antofagasta", 
                  "Región de Atacama", "Región de Coquimbo") ~ "Norte",
    region %in% c("Región de Valparaíso", "Región Metropolitana de Santiago", "Región del Libertador Gral. Bernardo O'Higgins", 
                  "Región del Maule", "Región de Ñuble") ~ "Centro",
    region %in% c("Región del Biobío", "Región de La Araucanía", "Región de Los Ríos", "Región de Los Lagos",
                  "Región de Aysén del Gral. Carlos Ibáñez del Campo", "Región de Magallanes y de la Antártica Chilena") ~ "Sur",
    TRUE ~ NA_character_  # Para cualquier valor no reconocido
  )) %>%
  mutate(zona = factor(zona))
save(data_macro,file="Análisis de viviendas/Data/data_macrotiplogias.RData")

# 2. DESCRIPTIVE ANALYSIS -----------------------------------------------------
# 2.1 Frequency table of macrogroups
analyze_macrogroups <- function(data, suffix = "") {
  # 2.1 Frequency table
  macro_freq <- data %>%
    distinct(household, .keep_all = TRUE) %>%
    count(macrogrupo) %>%
    mutate(percent = n/sum(n)*100) %>%
    arrange(desc(n))
  
  # 2.2 Demographic characteristics
  macro_demographics <- data %>%
    group_by(macrogrupo) %>%
    summarise(
      avg_age = mean(edad.prom, na.rm = TRUE),
      prop_female = 100 - mean(porc_hombre, na.rm = TRUE), # Using porc_hombre
      avg_income = median(sueldo, na.rm = TRUE),
      prop_indigenous = mean(porc_ind, na.rm = TRUE),
      prop_employed = mean(porc_empleo, na.rm = TRUE),
      prop_chilean = mean(porc_chi, na.rm = TRUE),
      avg_hh_size = n()/n_distinct(household),
      .groups = "drop"
    )
  
  
  # 2.3 Visualizations
  
  # Plot 1: Macrogroup distribution
  p1 <- ggplot(macro_freq, aes(x = reorder(macrogrupo, -n), y = n)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = paste0(round(percent,1), "%")), 
              vjust = -0.5) +
    labs(title = paste("Household Macrogroup Distribution", suffix),
         x = "", y = "Number of Households") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Plot 2: Income distribution by macrogroup
  p2 <- ggplot(data, aes(x = sueldo_cat, fill = macrogrupo)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Income Distribution by Macrogroup", suffix),
         x = "Income Quintile", y = "Percentage") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal()
  
  # Plot 3: Indigenous percentage by macrogroup
  p3 <- data %>%
    group_by(macrogrupo) %>%
    summarise(mean_porc_ind = mean(porc_ind, na.rm = TRUE)) %>%
    ggplot(aes(x = reorder(macrogrupo, -mean_porc_ind), y = mean_porc_ind)) +
    geom_col(fill = "darkgreen") +
    labs(title = paste("Average Indigenous Percentage by Macrogroup", suffix),
         x = "", y = "Percentage Indigenous") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(list(
    frequency = macro_freq,
    demographics = macro_demographics,
    plots = list(distribution = p1, income = p2, indigenous = p3)
  ))
}
# Analyze both datasets
results_t48 <- analyze_macrogroups(data_macro, "(Tipologies T1-T48)")

# View results
results_t48$frequency

# Combine and view plots
results_t48$plots$distribution
results_t48$plots$income
results_t48$plots$indigenous

