library(FactoMineR)
library(factoextra)

# 1.  Cargar datos
load("Análisis de viviendas/Data/hogares.RData")

# Seleccionar variables categóricas de interés (incluyendo macrogrupo)
data_mca <- hogares %>%
  dplyr::select(macrogrupo, quintil_ingreso, rural_cat, pobreza_hogar, macrozona) %>%
  dplyr::mutate(
    macrogrupo = as.factor(macrogrupo),
    quintil_ingreso = as.factor(quintil_ingreso),
    pobreza_hogar = factor(pobreza_hogar, levels = c("No pobres","Pobres no extremos","Pobres extremos")),
    macrozona = factor(macrozona, levels = c("Norte", "Centro", "Sur"))
  )
# Ejecutar ACM
res_mca <- MCA(data_mca, quali.sup = 1)  # macrogrupo como suplementaria
fviz_mca_biplot(res_mca, repel = TRUE, 
                title = "MCA - Relationship between macrogroups and socioeconomic variables")
