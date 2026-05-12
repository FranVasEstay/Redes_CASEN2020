#Librerías
library(dplyr)
library(ggplot2)
library(vcd)
library(purrr)
library(tidyr)
library(scales)
library(pheatmap)
library(gridExtra)

# 1.  Cargar datos
load("Análisis de viviendas/Data/hogares.RData")

# 2. Gestionar Datos
data_chi <- hogares %>%
  dplyr::select(macrogrupo, quintil_ingreso, rural_cat, pobreza_hogar, macrozona) %>%
  dplyr::mutate(
    macrogrupo = as.factor(macrogrupo),
    quintil_ingreso = as.factor(quintil_ingreso),
    pobreza_hogar = factor(pobreza_hogar, levels = c("No pobres","Pobres no extremos","Pobres extremos")),
    macrozona = factor(macrozona, levels = c("Norte", "Centro", "Sur"))
  )

# Lista de variables a cruzar
vars <- c("quintil_ingreso", "rural_cat", "pobreza_hogar", "macrozona")

# 3. Contenedores de resultados
chi_results <- list()
heatmap_plots <- list()
grobs <- list()

# Bucle
for (v in vars) {
  # Tabla de contingencia
  tabla <- table(data_chi$macrogrupo, data_chi[[v]])
  
  # Prueba chi-cuadrado
  chi <- chisq.test(tabla)
  chi_results[[v]] <- chi
  
  # Contribuciones porcentuales
  obs <- chi$observed
  exp <- chi$expected
  contrib <- (obs - exp)^2 / exp
  contrib_pct <- 100 * contrib / chi$statistic
  
  # Heatmap (sin mostrar aún)
  pheatmap(contrib_pct,
           display_numbers = TRUE,
           cluster_rows = FALSE, cluster_cols = FALSE,
           main = paste(v),
           silent = FALSE)
  grobs[[v]] <- grid::grid.grab()
}
png(file = "Análisis de viviendas/Analisis/Chi_macro.png")
gridExtra::grid.arrange(grobs = grobs, ncol = 2, nrow = 2)
dev.off()

# Extraer un resumen tabular de los tests
resumen <- data.frame(
  Variable = vars,
  Chi_cuadrado = sapply(chi_results, `[[`, "statistic"),
  df = sapply(chi_results, `[[`, "parameter"),
  p_valor = sapply(chi_results, `[[`, "p.value")
)

print(resumen)

# Rojo: la combinación aparece más de lo esperado.
# Azul: la combinación aparece menos de lo esperado.
# Amarillo: la frecuencia observada está cerca de la esperada.