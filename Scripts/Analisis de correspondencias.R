################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
############################ CREACIÓN DE REDES #################################
################################################################################

###### LIBRERÍAS ######
#install.packages(c("naniar","eaas","easypackages"))
library(easypackages)
paquetes <- c("FactoMineR", "tidyverse", "factoextra", "haven", "naniar", "corrplot")
libraries(paquetes)

###### CARGAR DATA ######
load("Data/ori_Casen2020_rdata.RData")
load("Redes/tablas_redes.RData")

##################### ADMINISTRACIÓN DE LOS DATOS ##############################
data<- ori_Casen2020_STATA %>%
  select(id_vivienda, id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, nucleo, pco2, r3,s16,y1,y1_preg, comuna, region) %>%
  filter(!id_vivienda %in% c(8102104907, 6106100505, 9115300202)) %>%
  rename(household = id_vivienda, sex = sexo, education = e6a, work = o1, nacion = r1b_pais_esp, parent = pco1, love= h5, civil = ecivil, son = h5_1, econ = h5_2, nuclear_parent = pco2, native = r3, health = s16, salario = y1_preg, money = y1) %>%
  mutate(
    sex = factor(sex, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    household = as.numeric(household),
    across(c(education,parent, civil, nuclear_parent, native, health, work, salario), as_factor),
    nacion = ifelse(nacion == "", 1,
                          ifelse(nacion == "NO RESPONDE", 3, 2)),
    nacion = factor(nacion, levels = c(1, 2), labels = c("Chileno", "Extranjero"))
  ) 
na_strings <- c("No sabe", "No sabe/No recuerda", "Nosabe\\No responde")
dummy.data <- data %>%
  select(sex, education, work, nacion, civil,native, health, salario, money) %>%
  naniar::replace_with_na_all(condition = ~.x %in% na_strings)


###Utiliza set de la data ###
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)
# Crear subset con los 1000 id_vivienda seleccionados
dummy_data_subset <- dummy.data %>%
  filter(household %in% id_vivienda_sample)

# Verificar el número de filas en el subset
nrow(dummy_data_subset)
dummy_data_subset <- na.omit(dummy_data_subset)

#Revisión de variables y distribución
dummy.data <- na.omit(dummy.data)
for (i in 1:10){
  plot(dummy.data[,i], main = colnames(dummy.data)[i],
       ylab = "Cantidad", col ="steelblue", las = 2)
}

##################### MCA PRELIMINAL ##############################
#Probando sólo con data_subset
dummy.mca <-MCA(dummy_data_subset,ncp = 4,graph=T)
print(dummy.mca)

###Visualizacion
## Eigenvalores/varianzas
eig_val <- factoextra::get_eigenvalue(dummy.mca)
head(eig_val)
fviz_screeplot(dummy.mca, addlabels = TRUE)
##Biplot
fviz_mca_biplot(dummy.mca, # resultados del análisis MCA
                repel = TRUE, # evitar la superposición de etiquetas en la gráfica
                alpha.ind = 0.1, # nivel de transparencia de los puntos filas o casos
                alpha.var = 1, # nivel de transparencia de los puntos columa o variables
                max.overlaps = "ggrepel.max.overlaps",
                ggtheme = theme_minimal()) # plantilla de estilo para la gráfica

###Descripcion de las dimensiones 
res.desc <- dimdesc(dummy.mca, #objeto tipo lista que contiene los resultados mca
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)
res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables <- get_mca_var(dummy.mca)
variables

# Coordenadas
head(variables $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables $ cos2)

# Contribución en los factores o dimensiones
head(variables $ contrib)

##Correlaciones
#Entre variable sy las dimensiones o factores
fviz_mca_var(dummy.mca, # objeto lista de resultados mca
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, # evitar la superposición de etiquetas
             ggtheme = theme_minimal()
)
#Coordenadas para la categoría de variables activas
fviz_mca_var(dummy.mca, #objeto lista con resultados mca 
             repel = TRUE, # evitar el traslape de etiquetas
             ggtheme = theme_minimal(),
             max.overlaps = "ggrepel.max.overlaps",
             col.var="black", # cambiar el color a las variables
             shape.var = 15 # cambiar la forma de representación de variables
)
##Calidad de la representacion de las categorias
head(round(variables $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca, #objeto tipo lista con resultados mca 
          choice = "var", # selección de las varianzas
          axes = 1:2) # ejes o dimensiones considerar en la gráfica
#En biplot
fviz_mca_var(dummy.mca, #objeto tipo lista con resultados mca
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal()
)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca, #objeto tipo lista con resultados mca
             choice = "var", #criterio a representar: varianza
             axes = 1, # selección del eje o dimensión a analizar = eje 1
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca, #objeto tipo lista con resultados mca 
             choice = "var", #criterio a representar: varianza
             axes = 2, # selección del eje o dimensión a analizar = eje 2
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(ddummy.mca, #objeto tipo lista con resultados mca
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, # ejes seleccionados en la representación = ejes 1 y 2 simultáneos
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
fviz_mca_var(dummy.mca, #objeto tipo lista con resultados mca 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #selección de la paleta de colores
             repel = TRUE, # evitar solapamiento de etiquetas
             max.overlaps = "ggrepel.max.overlaps", #aumentar la cantidad de solapamientos
             ggtheme = theme_minimal()
)

### Analisis sobre individuos
indiv <- get_mca_ind(dummy.mca)
indiv
# coordinadas para las filas
head(indiv $ coord, 3)
# cualidad de la representación
head(indiv $ cos2, 3)
# contribución de cada individuo
head(indiv $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca, #objeto tipo lista con los resultados mca
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, #ejes o dimensiones a incluir
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
fviz_mca_ind(dummy.mca, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "satisf_dem", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

fviz_ellipses(dummy.mca, # objeto tipo lista con resultados mca
              c("satisf_dem", "aprob_gob"), # definición de variables factor para agrupación
              geom = "point", # tipo de representación de los individuos
              alpha = 0.1 #transparencia de los puntos en la gráfica
)