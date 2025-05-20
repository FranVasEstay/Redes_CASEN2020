################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################
### Versión 3 ###
## MCA usando con datos de redes y descriptivos
## Revisar tipologías por cluster

###### LIBRERÍAS ######
library(easypackages)
paquetes <- c("FactoMineR", "dplyr","tidyverse", "factoextra", "haven", "naniar", "corrplot","doParallel")
libraries(paquetes)


####################### MCA PRELIMINAL #########################################
###### CARGAR DATA ######
#Tabla de redes-cluster
load("Redes/tablas_redes_cluster.RData")
######################## RED DE DEPENDENCIA ####################################
head(results_list_dependency)
print(colnames(results_list_dependency))
# Crear categorías a partir de las métricas de red
network_data_categorized <- results_list_dependency %>%
  mutate(size_cat = cut(size, breaks = c(-Inf, 2, 4, Inf), labels = c("Pequeña", "Mediana", "Grande")),
         ties_cat = cut(ties, breaks = c(-Inf, 1, 3, Inf), labels = c("Bajo", "Medio", "Alto")),
         n_comp_cat = cut(n_comp, breaks = c(-Inf, 1, 2, Inf), labels = c("Simple", "Moderado", "Complejo")),
         isolates_cat = cut(isolates, breaks = c(-Inf, 0, 2, Inf), labels = c("Ninguno", "Algunos", "Muchos")))

# Combinar datos de redes categorizados con datos individuales
mca_data_combined <- dummy.data %>%
  left_join(network_data_categorized, by = c("household" = "id")) %>%
  select(sex,education, grupo_etario, work, nacion,civil,health,salario,quintil, parent, native,size_cat, ties_cat, n_comp_cat, isolates_cat) %>%
  mutate(across(everything(), as.factor)) %>%
  na.omit()
mca_data_combined
colnames(mca_data_combined)

###### MCA
# Ejecutar el MCA
mca_result <- MCA(mca_data_combined,quali.sup=2:11, graph = FALSE)

# Visualizar resultados
fviz_mca_var(mca_result, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Mapa de categorías con métricas de red",
             labelsize=1)

fviz_mca_var(mca_result, 
             col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             title = "Mapa de correlación de categorías con métricas de red",
             labelsize =1)
#Muestra una diferenciación respecto al trabajo
fviz_mca_ind(mca_result, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

#También existe cierto agrupamiento con estado civil soltero y "No_aplica"
fviz_mca_ind(mca_result, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

