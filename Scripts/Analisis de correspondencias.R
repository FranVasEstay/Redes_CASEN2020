################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################

###### LIBRERÍAS ######
#install.packages(c("naniar","eaas","easypackages"))
library(easypackages)
paquetes <- c("FactoMineR", "tidyverse", "factoextra", "haven", "naniar", "corrplot")
libraries(paquetes)

###### CARGAR DATA ######
load("Data/ori_Casen2020_rdata.RData")
load("Redes/tablas_redes.RData")
load("Redes/tablas_redes_sample.RData")
#head(results_list_dependency)
#head(results_list_descent)
#head(results_list_kinship)
#head(results_list_marriage)

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
levels(data$salario)
  
# Se seleccionan las variables necesarias
dummy.data<- data %>%
  mutate(quintil = ntile(money,5)) %>% #Se crean rangos basados en quintiles
  select(household,sex, education, work, nacion, civil,native, health, salario, quintil)%>% # Se seleccionan las variables 
  naniar::replace_with_na_all(condition = ~.x %in% na_strings) #Se reemplazan todos los NA

###Utiliza set de la data ###
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)
# Crear subset con los 1000 id_vivienda seleccionados
dummy_data_subset <- dummy.data %>%
  filter(household %in% id_vivienda_sample) %>%
  naniar::replace_with_na_all(condition = ~.x %in% na_strings)

####################### MCA PRELIMINAL #########################################

######################## RED DE DEPENDENCIA ####################################
head(results_list_dependency)
print(colnames(results_list_dependency))
head(dummy.data)
print(colnames(dummy.data))

#Cambiar id por household
dummy_dep <- results_list_dependency%>%
  rename(household=id)

#Unir ambas bases de datos por id_vivienda 
dummy_dep_bind <-merge(dummy.data,dummy_dep,by="household")
# excluir las columnas "id" y "gcluster"
cols_to_exclude <- c("household", "g.cluster")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_dep <- as.data.frame(dummy_dep_bind[, !(colnames(dummy_dep_bind) %in% cols_to_exclude)])
data_pcm_dep <- data_pcm_dep[, apply(data_pcm_dep, 2, var) != 0]  # Elimina las columnas con varianza cero
data_pcm_dep$quintil<-factor(data_pcm_dep$quintil)
data_pcm_dep$size<-factor(data_pcm_dep$size)
data_pcm_dep$ties<-factor(data_pcm_dep$ties)
data_pcm_dep$density<-factor(data_pcm_dep$density)
data_pcm_dep$n_comp<-factor(data_pcm_dep$n_comp)
data_pcm_dep$diameter<-factor(data_pcm_dep$diameter)
data_pcm_dep$isolates<-factor(data_pcm_dep$isolates)

#Revisando los datos
data_pcm_dep<-na.omit(data_pcm_dep)
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(3, 5))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:15){
  plot(data_pcm_dep[,i], main=colnames(data_pcm_dep)[i],
       ylab="Cantidad", col="pink",las=2)
}
str(data_pcm_dep)
data_pcm_dep <- select(data_pcm_dep, -density)#Eliminar density porque sólo tiene un level (REVISAR LUEGO)
colnames(data_pcm_dep)
#Calculo de Correspondencias múltiples
dummy.mca.dep <-MCA(data_pcm_dep,ncp = 4,graph=F)
print(dummy.mca.dep)

###Visualizacion
## Eigenvalores/varianzas
eig_val <- factoextra::get_eigenvalue(dummy.mca)
 fviz_screeplot(dummy.mca.dep, addlabels = TRUE)
##Biplot
fviz_mca_biplot(dummy.mca.dep,
                repel = TRUE,
                alpha.ind = 0.1,
                alpha.var = 1, 
                ggtheme = theme_minimal())

###Descripcion de las dimensiones
res.desc <- dimdesc(dummy.mca.dep,
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)
str(dummy.mca.dep)
res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables <- get_mca_var(dummy.mca.dep)
variables
# Coordenadas
head(variables $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables $ cos2)

# Contribución en los factores o dimensiones
head(variables $ contrib)

##Correlaciones
#Entre variableS y las dimensiones o factores
fviz_mca_var(dummy.mca.dep, # objeto lista de resultados mca
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, # evitar la superposición de etiquetas
             ggtheme = theme_minimal(),
)
#Coordenadas para la categoría de variables activas
fviz_mca_var(dummy.mca.dep,
             repel = T,
             ggtheme = theme_minimal()
             )
fviz_mca_var(dummy.mca.dep, #objeto lista con resultados mca 
             repel = TRUE, # evitar el traslape de etiquetas
             ggtheme = theme_minimal(),
             max.overlaps = "ggrepel.max.overlaps",
             col.var="black", # cambiar el color a las variables
             shape.var = 15 # cambiar la forma de representación de variables
)
##Calidad de la representacion de las categorias
head(round(variables $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.dep, #objeto tipo lista con resultados mca 
          choice = "var", # selección de las varianzas
          axes = 1:2) # ejes o dimensiones considerar en la gráfica
#En biplot
fviz_mca_var(dummy.mca.dep, #objeto tipo lista con resultados mca
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal()
)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.dep, #objeto tipo lista con resultados mca
             choice = "var", #criterio a representar: varianza
             axes = 1, # selección del eje o dimensión a analizar = eje 1
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.dep, #objeto tipo lista con resultados mca 
             choice = "var", #criterio a representar: varianza
             axes = 2, # selección del eje o dimensión a analizar = eje 2
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(ddummy.mca.dep, #objeto tipo lista con resultados mca
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, # ejes seleccionados en la representación = ejes 1 y 2 simultáneos
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
fviz_mca_var(dummy.mca.dep, #objeto tipo lista con resultados mca 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #selección de la paleta de colores
             repel = TRUE, # evitar solapamiento de etiquetas
             max.overlaps = "ggrepel.max.overlaps", #aumentar la cantidad de solapamientos
             ggtheme = theme_minimal()
)

### Analisis sobre individuos
indiv <- get_mca_ind(dummy.mca.dep)
indiv
# coordinadas para las filas
head(indiv $ coord, 3)
# cualidad de la representación
head(indiv $ cos2, 3)
# contribución de cada individuo
head(indiv $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.dep, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.dep, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.dep, #objeto tipo lista con los resultados mca
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, #ejes o dimensiones a incluir
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.dep, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
colnames(dummy_dep_bind)
#Según sexo
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según educacion
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "education", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según trabajo
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según nacion
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "nacion", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según civil
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según pueblo originario
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "native", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salud
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "health", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salario ### NO FUNCIONA
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "salario", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según ingreso ### DEMASIADOS INGRESOS DISTINTOS
fviz_mca_ind(dummy.mca.dep, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "money", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
fviz_ellipses(dummy.mca.dep, # objeto tipo lista con resultados mca
              c("satisf_dem", "aprob_gob"), # definición de variables factor para agrupación
              geom = "point", # tipo de representación de los individuos
              alpha = 0.1 #transparencia de los puntos en la gráfica
)

######################## RED DE DESCENDENCIA ###################################
head(results_list_descent)
print(colnames(results_list_descent))
head(dummy.data)
print(colnames(dummy.data))
#Cambiar id por household
dummy_des <- results_list_descent%>%
  rename(household=id)
#Unir ambas bases de datos por id_vivienda 
dummy_des_bind <-merge(dummy.data,dummy_des,by="household")
# excluir las columnas "id" y "gcluster"
cols_to_exclude <- c("household", "g.cluster")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_des <- as.data.frame(dummy_des_bind[, !(colnames(dummy_des_bind) %in% cols_to_exclude)])
data_pcm_des <- data_pcm_des[, apply(data_pca_des, 2, var) != 0]  # Elimina las columnas con varianza cero
data_pcm_des$money<-factor(data_pcm_des$money)
data_pcm_des$size<-factor(data_pcm_des$size)
data_pcm_des$ties<-factor(data_pcm_des$ties)
data_pcm_des$density<-factor(data_pcm_des$density)
data_pcm_des$n_comp<-factor(data_pcm_des$n_comp)
data_pcm_des$diameter<-factor(data_pcm_des$diameter)
data_pcm_des$isolates<-factor(data_pcm_des$isolates)
#Revisando los datos
data_pcm_des<-na.omit(data_pcm_des)
for(i in 1:10){
  plot(data_pcm_des[,i], main=colnames(data_pcm_des)[i],
       ylab="Cantidad", col="pink",las=2)
}

#Probando sólo con data
dummy.mca.des <-MCA(data_pcm_des,ncp = 4,graph=T)
print(dummy.mca.des)

###Visualizacion
## Eigenvalores/varianzas
eig_val <- factoextra::get_eigenvalue(dummy.mca.des)
head(eig_val)
fviz_screeplot(dummy.mca.des, addlabels = TRUE)
##Biplot
fviz_mca_biplot(dummy.mca.des, # resultados del análisis MCA
                repel = TRUE, # evitar la superposición de etiquetas en la gráfica
                alpha.ind = 0.1, # nivel de transparencia de los puntos filas o casos
                alpha.var = 1, # nivel de transparencia de los puntos columa o variables
                max.overlaps = "ggrepel.max.overlaps",
                ggtheme = theme_minimal()) # plantilla de estilo para la gráfica

###Descripcion de las dimensiones 
res.desc <- dimdesc(dummy.mca.des, #objeto tipo lista que contiene los resultados mca
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)

res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables <- get_mca_var(dummy.mca.des)
variables

# Coordenadas
head(variables $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables $ cos2)

# Contribución en los factores o dimensiones
head(variables $ contrib)

##Correlaciones
#Entre variable sy las dimensiones o factores
fviz_mca_var(dummy.mca.des, # objeto lista de resultados mca
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, # evitar la superposición de etiquetas
             ggtheme = theme_minimal()
)
#Coordenadas para la categoría de variables activas
fviz_mca_var(dummy.mca.des, #objeto lista con resultados mca 
             repel = TRUE, # evitar el traslape de etiquetas
             ggtheme = theme_minimal(),
             max.overlaps = "ggrepel.max.overlaps",
             col.var="black", # cambiar el color a las variables
             shape.var = 15 # cambiar la forma de representación de variables
)
##Calidad de la representacion de las categorias
head(round(variables $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.des, #objeto tipo lista con resultados mca 
          choice = "var", # selección de las varianzas
          axes = 1:2) # ejes o dimensiones considerar en la gráfica
#En biplot
fviz_mca_var(dummy.mca.des, #objeto tipo lista con resultados mca
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal()
)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.des, #objeto tipo lista con resultados mca
             choice = "var", #criterio a representar: varianza
             axes = 1, # selección del eje o dimensión a analizar = eje 1
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.des, #objeto tipo lista con resultados mca 
             choice = "var", #criterio a representar: varianza
             axes = 2, # selección del eje o dimensión a analizar = eje 2
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(ddummy.mca.des, #objeto tipo lista con resultados mca
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, # ejes seleccionados en la representación = ejes 1 y 2 simultáneos
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
fviz_mca_var(dummy.mca.des, #objeto tipo lista con resultados mca 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #selección de la paleta de colores
             repel = TRUE, # evitar solapamiento de etiquetas
             max.overlaps = "ggrepel.max.overlaps", #aumentar la cantidad de solapamientos
             ggtheme = theme_minimal()
)

### Analisis sobre individuos
indiv <- get_mca_ind(dummy.mca.des)
indiv
# coordinadas para las filas
head(indiv $ coord, 3)
# cualidad de la representación
head(indiv $ cos2, 3)
# contribución de cada individuo
head(indiv $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.des, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.des, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.des, #objeto tipo lista con los resultados mca
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, #ejes o dimensiones a incluir
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.des, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según educacion
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "education", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según trabajo
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según nacion
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "nacion", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según civil
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según pueblo originario
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "native", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salud
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "health", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salario ### NO FUNCIONA
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "salario", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según ingreso ### DEMASIADOS INGRESOS DISTINTOS
fviz_mca_ind(dummy.mca.des, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "money", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#fviz_ellipses(dummy.mca.des, # objeto tipo lista con resultados mca
#              c("satisf_dem", "aprob_gob"), # definición de variables factor para agrupación
#              geom = "point", # tipo de representación de los individuos
#              alpha = 0.1 #transparencia de los puntos en la gráfica
#)
######################## RED DE MATRIMONIO #####################################
head(results_list_marriage)
print(colnames(results_list_marriage))
head(dummy.data)
print(colnames(dummy.data))
#Cambiar id por household
dummy_mar <- results_list_marriage%>%
  rename(household=id)
#Unir ambas bases de datos por id_vivienda 
dummy_mar_bind <-merge(dummy.data,dummy_mar,by="household")
# excluir las columnas "id" y "gcluster"
cols_to_exclude <- c("household", "g.cluster")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_mar <- as.data.frame(dummy_mar_bind[, !(colnames(dummy_mar_bind) %in% cols_to_exclude)])
data_pcm_mar <- data_pcm_mar[, apply(data_pca_mar, 2, var) != 0]  # Elimina las columnas con varianza cero
data_pcm_mar$money<-factor(data_pcm_mar$money)
data_pcm_mar$size<-factor(data_pcm_mar$size)
data_pcm_mar$ties<-factor(data_pcm_mar$ties)
data_pcm_mar$density<-factor(data_pcm_mar$density)
data_pcm_mar$n_comp<-factor(data_pcm_mar$n_comp)
data_pcm_mar$diameter<-factor(data_pcm_mar$diameter)
data_pcm_mar$isolates<-factor(data_pcm_mar$isolates)
#Revisando los datos
data_pcm_mar<-na.omit(data_pcm_mar)
for(i in 1:10){
  plot(data_pcm_mar[,i], main=colnames(data_pcm_mar)[i],
       ylab="Cantidad", col="pink",las=2)
}
#Probando sólo con data
dummy.mca.mar <-MCA(data_pcm_mar,ncp = 4,graph=T)
print(dummy.mca.mar)

###Visualizacion
## Eigenvalores/varianzas
eig_val <- factoextra::get_eigenvalue(dummy.mca.mar)
head(eig_val)
fviz_screeplot(dummy.mca.mar, addlabels = TRUE)
##Biplot
fviz_mca_biplot(dummy.mca.mar, # resultados del análisis MCA
                repel = TRUE, # evitar la superposición de etiquetas en la gráfica
                alpha.ind = 0.1, # nivel de transparencia de los puntos filas o casos
                alpha.var = 1, # nivel de transparencia de los puntos columa o variables
                max.overlaps = "ggrepel.max.overlaps",
                ggtheme = theme_minimal()) # plantilla de estilo para la gráfica

###Descripcion de las dimensiones 
res.desc <- dimdesc(dummy.mca.mar, #objeto tipo lista que contiene los resultados mca
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)
res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables <- get_mca_var(dummy.mca.mar)
variables

# Coordenadas
head(variables $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables $ cos2)

# Contribución en los factores o dimensiones
head(variables $ contrib)

##Correlaciones
#Entre variable sy las dimensiones o factores
fviz_mca_var(dummy.mca.mar, # objeto lista de resultados mca
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, # evitar la superposición de etiquetas
             ggtheme = theme_minimal()
)
#Coordenadas para la categoría de variables activas
fviz_mca_var(dummy.mca.mar, #objeto lista con resultados mca 
             repel = TRUE, # evitar el traslape de etiquetas
             ggtheme = theme_minimal(),
             max.overlaps = "ggrepel.max.overlaps",
             col.var="black", # cambiar el color a las variables
             shape.var = 15 # cambiar la forma de representación de variables
)
##Calidad de la representacion de las categorias
head(round(variables $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.mar, #objeto tipo lista con resultados mca 
          choice = "var", # selección de las varianzas
          axes = 1:2) # ejes o dimensiones considerar en la gráfica
#En biplot
fviz_mca_var(dummy.mca.mar, #objeto tipo lista con resultados mca
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal()
)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.mar, #objeto tipo lista con resultados mca
             choice = "var", #criterio a representar: varianza
             axes = 1, # selección del eje o dimensión a analizar = eje 1
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.mar, #objeto tipo lista con resultados mca 
             choice = "var", #criterio a representar: varianza
             axes = 2, # selección del eje o dimensión a analizar = eje 2
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.mar, #objeto tipo lista con resultados mca
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, # ejes seleccionados en la representación = ejes 1 y 2 simultáneos
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
fviz_mca_var(dummy.mca.mar, #objeto tipo lista con resultados mca 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #selección de la paleta de colores
             repel = TRUE, # evitar solapamiento de etiquetas
             max.overlaps = "ggrepel.max.overlaps", #aumentar la cantidad de solapamientos
             ggtheme = theme_minimal()
)

### Analisis sobre individuos
indiv <- get_mca_ind(dummy.mca.mar)
indiv
# coordinadas para las filas
head(indiv $ coord, 3)
# cualidad de la representación
head(indiv $ cos2, 3)
# contribución de cada individuo
head(indiv $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.mar, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.mar, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.mar, #objeto tipo lista con los resultados mca
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, #ejes o dimensiones a incluir
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.mar, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según educacion
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "education", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

#Según trabajo
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según nacion
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "nacion", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según civil
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según pueblo originario
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "native", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salud
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "health", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salario ### NO FUNCIONA
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "salario", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según ingreso ### DEMASIADOS INGRESOS DISTINTOS
fviz_mca_ind(dummy.mca.mar, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "money", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
fviz_ellipses(dummy.mca.mar, # objeto tipo lista con resultados mca
              c("satisf_dem", "aprob_gob"), # definición de variables factor para agrupación
              geom = "point", # tipo de representación de los individuos
              alpha = 0.1 #transparencia de los puntos en la gráfica
)

############################# RED KINSHIP ######################################
head(results_list_kinship)
print(colnames(results_list_kinship))
head(dummy.data)
print(colnames(dummy.data))
#Cambiar id por household
dummy_kin <- results_list_kinship%>%
  rename(household=id)
#Unir ambas bases de datos por id_vivienda 
dummy_kin_bind <-merge(dummy.data,dummy_kin,by="household")
# excluir las columnas "id" y "gcluster"
cols_to_exclude <- c("household", "g.cluster")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_kin <- as.data.frame(dummy_kin_bind[, !(colnames(dummy_kin_bind) %in% cols_to_exclude)])
data_pcm_kin <- data_pcm_kin[, apply(data_pca_kin, 2, var) != 0]  # Elimina las columnas con varianza cero
data_pcm_kin$money<-factor(data_pcm_kin$money)
data_pcm_kin$size<-factor(data_pcm_kin$size)
data_pcm_kin$ties<-factor(data_pcm_kin$ties)
data_pcm_kin$density<-factor(data_pcm_kin$density)
data_pcm_kin$n_comp<-factor(data_pcm_kin$n_comp)
data_pcm_kin$diameter<-factor(data_pcm_kin$diameter)
data_pcm_kin$isolates<-factor(data_pcm_kin$isolates)
#Revisando los datos
data_pcm_kin<-na.omit(data_pcm_kin)
for(i in 1:10){
  plot(data_pcm_kin[,i], main=colnames(data_pcm_kin)[i],
       ylab="Cantidad", col="pink",las=2)
}
#Probando sólo con data
dummy.mca.kin <-MCA(data_pcm_kin,ncp = 4,graph=T)
print(dummy.mca.kin)

###Visualizacion
## Eigenvalores/varianzas
eig_val <- factoextra::get_eigenvalue(dummy.mca.kin)
head(eig_val)
fviz_screeplot(dummy.mca.kin, addlabels = TRUE)
##Biplot
fviz_mca_biplot(dummy.mca.kin, # resultados del análisis MCA
                repel = TRUE, # evitar la superposición de etiquetas en la gráfica
                alpha.ind = 0.1, # nivel de transparencia de los puntos filas o casos
                alpha.var = 1, # nivel de transparencia de los puntos columa o variables
                max.overlaps = "ggrepel.max.overlaps",
                ggtheme = theme_minimal()) # plantilla de estilo para la gráfica

###Descripcion de las dimensiones 
res.desc <- dimdesc(dummy.mca.kin, #objeto tipo lista que contiene los resultados mca
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)
res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables <- get_mca_var(dummy.mca.kin)
variables

# Coordenadas
head(variables $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables $ cos2)

# Contribución en los factores o dimensiones
head(variables $ contrib)

##Correlaciones
#Entre variable sy las dimensiones o factores
fviz_mca_var(dummy.mca.kin, # objeto lista de resultados mca
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, # evitar la superposición de etiquetas
             ggtheme = theme_minimal()
)
#Coordenadas para la categoría de variables activas
fviz_mca_var(dummy.mca.kin, #objeto lista con resultados mca 
             repel = TRUE, # evitar el traslape de etiquetas
             ggtheme = theme_minimal(),
             max.overlaps = "ggrepel.max.overlaps",
             col.var="black", # cambiar el color a las variables
             shape.var = 15 # cambiar la forma de representación de variables
)
##Calidad de la representacion de las categorias
head(round(variables $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.kin, #objeto tipo lista con resultados mca 
          choice = "var", # selección de las varianzas
          axes = 1:2) # ejes o dimensiones considerar en la gráfica
#En biplot
fviz_mca_var(dummy.mca.kin, #objeto tipo lista con resultados mca
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #definición de la paleta de colores
             repel = TRUE, # evitar solapamientos de etiquetas,
             max.overlaps = "ggrepel.max.overlaps", #aumentar el tamaño de solapamientos
             ggtheme = theme_minimal()
)
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.kin, #objeto tipo lista con resultados mca
             choice = "var", #criterio a representar: varianza
             axes = 1, # selección del eje o dimensión a analizar = eje 1
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.kin, #objeto tipo lista con resultados mca 
             choice = "var", #criterio a representar: varianza
             axes = 2, # selección del eje o dimensión a analizar = eje 2
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.kin, #objeto tipo lista con resultados mca
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, # ejes seleccionados en la representación = ejes 1 y 2 simultáneos
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
fviz_mca_var(dummy.mca.kin, #objeto tipo lista con resultados mca 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #selección de la paleta de colores
             repel = TRUE, # evitar solapamiento de etiquetas
             max.overlaps = "ggrepel.max.overlaps", #aumentar la cantidad de solapamientos
             ggtheme = theme_minimal()
)

### Analisis sobre individuos
indiv <- get_mca_ind(dummy.mca.kin)
indiv
# coordinadas para las filas
head(indiv $ coord, 3)
# cualidad de la representación
head(indiv $ cos2, 3)
# contribución de cada individuo
head(indiv $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.kin, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.kin, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overlaps = "ggrepel.max.overlaps",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.kin, #objeto tipo lista con los resultados mca
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, #ejes o dimensiones a incluir
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.kin, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según educacion
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "education", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según trabajo
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según nacion
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "nacion", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según civil
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según pueblo originario
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "native", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salud
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "health", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
#Según salario ### NO FUNCIONA
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "salario", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

#Según ingreso ### DEMASIADOS INGRESOS DISTINTOS
fviz_mca_ind(dummy.mca.kin, #objeto tipo lista con resultados mca 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "money", # colorear a los grupos 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)

fviz_ellipses(dummy.mca.kin, # objeto tipo lista con resultados mca
              c("satisf_dem", "aprob_gob"), # definición de variables factor para agrupación
              geom = "point", # tipo de representación de los individuos
              alpha = 0.1 #transparencia de los puntos en la gráfica
)