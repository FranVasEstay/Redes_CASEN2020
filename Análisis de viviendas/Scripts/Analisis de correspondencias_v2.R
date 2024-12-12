################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################
### Versión 2 ###
## MCA usando sólo datos descriptivos de las redes 
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

#Cambiar id por household
dummy_dep <- results_list_dependency%>%
  rename(Cluster=grupos_dep.cluster)
colnames(dummy_dep)
variables <- dummy_dep %>% select(-id) # Excluir la columna 'household' y seleccionar solo las columnas categóricas
variables <- variables %>%
  mutate(across(everything(), as.factor))
str(variables)
ind.sup <- dummy_dep$id
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
dummy.mca.dep.v2 <-MCA(variables,ncp = 2,graph=T,quali.sup = 6)
stopCluster(cl)

###Visualizacion 
## Eigenvalores/varianzas
eig_val_dep <- factoextra::get_eigenvalue(dummy.mca.dep.v2)
fviz_screeplot(dummy.mca.dep.v2, addlabels = F)

##Biplot
fviz_mca_biplot(dummy.mca.dep.v2,
                repel = T,
                label = "none",
                title = "Categories and individuals - Dependency Network",
                alpha.ind = 0.1,
                alpha.var = 1, 
                ggtheme = theme_minimal())

###Descripcion de las dimensiones ### 
res.desc <- dimdesc(dummy.mca.dep.v2,
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)

res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables_dep <- get_mca_var(dummy.mca.dep.v2)
variables_dep
# Coordenadas
head(variables_dep$coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables_dep$cos2)

# Contribución en los factores o dimensiones
head(variables_dep$contrib)

##Correlaciones
#Entre variableS y las dimensiones o factores
fviz_mca_var(dummy.mca.dep.v2,
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE,
             title = "Correlation between variables and dimensions - Dependency Network",
             ggtheme = theme_minimal(),
)

#Coordenadas para la categoría de variables activas

fviz_mca_var(dummy.mca.dep.v2,
             repel = F,
             label="none",
             title = "Nube de puntos de las Modalidades/Categorías - Dependency Network",
             ggtheme = theme_minimal(),
             col.var="red", 
             shape.var = 10 
)

##Calidad de la representacion de las categorias
head(round(variables_dep $ cos2, 3), 20)

#En histograma
fviz_cos2(dummy.mca.dep.v2,
          choice = "var", # selección de las varianzas
          axes = 1:2)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables_dep $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.dep.v2, 
             choice = "var", #criterio a representar: varianza
             axes = 1, 
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.dep.v2, 
             choice = "var", #criterio a representar: varianza
             axes = 2, 
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.dep.v2, 
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, 
             top = 20) #selección de las 20 categorías con mayor contribución

### Analisis sobre individuos
house_dep <- get_mca_ind(dummy.mca.dep.v2)
house_dep
# coordinadas para las filas
head(house_dep $ coord, 3)
# cualidad de la representación
head(house_dep $ cos2, 3)
# contribución de cada individuo
head(house_dep $ contrib, 3)
#Calidad de representaicion de individuos
fviz_mca_ind(dummy.mca.dep.v2, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             title = "Individual representation quality - Dependency Network",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)

#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.dep.v2, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.dep.v2, 
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2,
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.dep.v2, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)

### Agrupamiento de individuos
#Según cluster
fviz_mca_ind(dummy.mca.dep.v2, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = variables$Cluster, 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
