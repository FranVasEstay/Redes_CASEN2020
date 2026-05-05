# Cargar datos
load("Análisis de viviendas/Data/data_macrotiplogias.RData")
# 3. CHI-SQUARE TESTS  ---------------------------------------------------------
# Function for typology-wise chi-square tests
chi_test_macrogroups <- function(data, test_var) {
  # Crear tabla de contingencia
  tbl <- table(data$macrogrupo, data[[test_var]])
  
  # Realizar prueba chi-cuadrado
  chi_test <- chisq.test(tbl)
  
  # Calcular residuos estandarizados
  std_res <- as.data.frame(chi_test$stdres)
  names(std_res) <- c("macrogrupo", test_var, "std_residual")
  
  # Calcular V de Cramer
  cramer_v <- sqrt(chi_test$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
  
  return(list(
    test = chi_test,
    residuals = std_res,
    cramer_v = cramer_v
  ))
}

# Ejecutar pruebas para variables clave
chi_results <- list(
  region = chi_test_macrogroups(data_macro, "region"),
  rural = chi_test_macrogroups(data_macro, "rural_cat"),
  income = chi_test_macrogroups(data_macro, "sueldo_cat")
)

# 3.1 HEATMAP DE SIGNIFICANCIA - CORREGIDO
# Crear dataframe con resultados
chi_summary <- data.frame(
  Variable = c("region", "rural", "income"),
  Chi2 = c(chi_results$region$test$statistic,
           chi_results$rural$test$statistic,
           chi_results$income$test$statistic),
  p_value = c(chi_results$region$test$p.value,
              chi_results$rural$test$p.value,
              chi_results$income$test$p.value),
  Cramer_V = c(chi_results$region$cramer_v,
               chi_results$rural$cramer_v,
               chi_results$income$cramer_v)
)

# Heatmap de significancia
ggplot(chi_summary, aes(x = Variable, y = "Macrogrupos", fill = p_value < 0.05)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = paste0("χ²=", round(Chi2, 2), 
                               "\np=", round(p_value, 4),
                               "\nV=", round(Cramer_V, 3))),
            color = "black", size = 3.5, lineheight = 0.8) +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray90"),
                    name = "Significativo\n(p < 0.05)") +
  labs(title = "Significancia estadística (Chi²) - Macrogrupos vs Variables",
       x = "Variable", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

# 3.2 MOSAIC PLOTS POR MACROGRUPO - CORREGIDO
# Crear carpeta para guardar los gráficos
dir.create("Análisis de viviendas/Analisis/mosaic_plots_macrogrupos", 
           showWarnings = FALSE, recursive = TRUE)
# Función corregida para mosaic plots con ggplot2
generar_mosaic_gg_corregido <- function(macrogrupo_sel, variable_cat) {
  datos_filtrados <- data_macro %>%
    mutate(grupo = ifelse(macrogrupo == macrogrupo_sel, macrogrupo_sel, "Otros")) %>%
    filter(!is.na(.data[[variable_cat]])) %>%
    # Asegurar que las variables sean factores
    mutate(across(c({{variable_cat}}, grupo), as.factor))
  
  # Crear tabla de contingencia manualmente para usar con geom_tile
  tabla_contingencia <- datos_filtrados %>%
    count(.data[[variable_cat]], grupo) %>%
    group_by(grupo) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup()
  
  p <- ggplot(tabla_contingencia, 
              aes(x = .data[[variable_cat]], y = grupo, fill = prop)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(round(prop * 100, 1), "%")), 
              color = "black", size = 3) +
    scale_fill_gradient2(low = "white", high = "steelblue", 
                         midpoint = 0.5, labels = scales::percent) +
    labs(title = paste("Distribución:", variable_cat, "vs", macrogrupo_sel),
         x = variable_cat, y = "Grupo",
         fill = "Proporción") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  nombre_archivo <- paste0("Análisis de viviendas/Analisis/mosaic_plots_macrogrupos/",
                           variable_cat, "_", gsub("[^[:alnum:]]", "_", macrogrupo_sel), ".png")
  
  ggsave(nombre_archivo, p, width = 10, height = 8)
}

# Alternativa usando geom_col para mosaic-like plot
generar_mosaic_col <- function(macrogrupo_sel, variable_cat) {
  datos_filtrados <- data_macro %>%
    mutate(grupo = ifelse(macrogrupo == macrogrupo_sel, macrogrupo_sel, "Otros")) %>%
    filter(!is.na(.data[[variable_cat]])) %>%
    mutate(across(c({{variable_cat}}, grupo), as.factor))
  
  p <- ggplot(datos_filtrados, aes(x = grupo, fill = .data[[variable_cat]])) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Distribución:", variable_cat, "en", macrogrupo_sel, "vs Otros"),
         x = "Grupo", y = "Porcentaje",
         fill = variable_cat) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  nombre_archivo <- paste0("Análisis de viviendas/Analisis/mosaic_plots_macrogrupos/",
                           variable_cat, "_", gsub("[^[:alnum:]]", "_", macrogrupo_sel), ".png")
  
  ggsave(nombre_archivo, p, width = 10, height = 8)
}

# SOLUCIÓN DEFINITIVA: Usar vcd::mosaic con sintaxis correcta
generar_mosaic_vcd <- function(macrogrupo_sel, variable_cat) {
  datos_filtrados <- data_macro %>%
    mutate(grupo = ifelse(macrogrupo == macrogrupo_sel, macrogrupo_sel, "Otros")) %>%
    filter(!is.na(.data[[variable_cat]]))
  
  nombre_archivo <- paste0("Análisis de viviendas/Analisis/mosaic_plots_macrogrupos/",
                           variable_cat, "_", gsub("[^[:alnum:]]", "_", macrogrupo_sel), ".png")
  
  png(nombre_archivo, width = 1000, height = 800)
  
  # Sintaxis CORRECTA para vcd::mosaic
  formula_mosaic <- as.formula(paste("~", variable_cat, "+ grupo"))
  mosaic(formula_mosaic, 
         data = datos_filtrados,
         shade = TRUE,
         main = paste(variable_cat, "vs", macrogrupo_sel),
         labeling = labeling_border(
           rot_labels = c(45, 0, 0, 0),
           just_labels = c("left", "center")
         ))
  
  dev.off()
}

walk2(combinaciones_macro$macrogrupo, combinaciones_macro$variable,
      ~generar_mosaic_gg_corregido(.x, .y))

# 3.3 VISUALIZACIÓN DE RESIDUALES - CORREGIDO
# Combinar todos los residuales en un dataframe
all_residuals <- bind_rows(
  chi_results$region$residuals %>% mutate(Variable = "region"),
  chi_results$rural$residuals %>% mutate(Variable = "rural_cat"),
  chi_results$income$residuals %>% mutate(Variable = "sueldo_cat")
)

# Heatmap de residuales estandarizados
ggplot(all_residuals, aes(x = .data[[names(all_residuals)[2]]], 
                          y = macrogrupo, 
                          fill = std_residual)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(std_residual, 2)), size = 3, color = "black") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, name = "Residual\nEstandarizado") +
  facet_wrap(~ Variable, scales = "free_x", ncol = 1) +
  labs(title = "Residuales Estandarizados por Macrogrupo y Variable",
       x = "Categorías", y = "Macrogrupo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))
