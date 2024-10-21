################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################

###### LIBRERÍAS ######
#install.packages(c("naniar","eaas","easypackages"))
library(easypackages)
paquetes <- c("FactoMineR", "tidyverse", "factoextra", "haven", "naniar", "corrplot","doParalell")
libraries(paquetes)

###### CARGAR DATA ######
load("Data/ori_Casen2020_rdata.RData")
load("Redes/tablas_redes.RData")
#load("Redes/tablas_redes_sample.RData")
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
  
# Se seleccionan las variables necesarias
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
dummy.data<- data %>%
  mutate(quintil = ntile(money,5)) %>% #Se crean rangos basados en quintiles
  select(household,sex, education, work, nacion, civil,native, health, salario, quintil)%>% # Se seleccionan las variables 
  naniar::replace_with_na_all(condition = ~.x %in% na_strings) #Se reemplazan todos los NA
stopCluster(cl)

save(dummy.data, file = "Data/Dummy_data_mca.RData")
###Utiliza set de la data ###
#set.seed(400)  # Fijar semilla para reproducibilidad
#id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)
# Crear subset con los 1000 id_vivienda seleccionados
#dummy_data_subset <- dummy.data %>%
#  filter(household %in% id_vivienda_sample) %>%
#  naniar::replace_with_na_all(condition = ~.x %in% na_strings)

####################### MCA PRELIMINAL #########################################
load("Data/Dummy_data_mca.RData")
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
#data_pcm_dep <- data_pcm_dep[, apply(data_pcm_dep, 2, var) != 0]  # Elimina las columnas con varianza cero
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
eig_val_dep <- factoextra::get_eigenvalue(dummy.mca.dep)
 fviz_screeplot(dummy.mca.dep, addlabels = TRUE)
##Biplot
png(filename = "Analisis/MCA_dep1.png")
fviz_mca_biplot(dummy.mca.dep,
                repel = F,
                label = "none",
                title = "Categories and individuals - Dependency Network",
                alpha.ind = 0.1,
                alpha.var = 1, 
                ggtheme = theme_minimal())
dev.off()
###Descripcion de las dimensiones ### Da error porque baja el salario a un sólo level. aún no sé por qué pasa esto
res.desc <- dimdesc(dummy.mca.dep,
                    axes = c(1,2) #definición de los ejes o dimensiones a describir
)

res.desc[[1]]
res.desc[[2]]

###Analisis sobre las variables

variables_dep <- get_mca_var(dummy.mca.dep)
variables_dep
# Coordenadas
head(variables_dep$coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables_dep$cos2)

# Contribución en los factores o dimensiones
head(variables_dep$contrib)

##Correlaciones
#Entre variableS y las dimensiones o factores
png("Analisis/MCA_dep2.png")
fviz_mca_var(dummy.mca.dep,
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE,
             title = "Correlation between variables and dimensions - Dependency Network",
             ggtheme = theme_minimal(),
)
dev.off()
#Coordenadas para la categoría de variables activas
png("Analisis/MCA_dep3.png")
fviz_mca_var(dummy.mca.dep,
             repel = TRUE,
             title = "Nube de puntos de las Modalidades/Categorías - Dependency Network",
             ggtheme = theme_minimal(),
             col.var="red", 
             shape.var = 10 
)
dev.off()
##Calidad de la representacion de las categorias
head(round(variables_dep $ cos2, 3), 20)

#En histograma
fviz_cos2(dummy.mca.dep,
          choice = "var", # selección de las varianzas
          axes = 1:2)
#En biplot
png("Analisis/MCA_dep4.png")
fviz_mca_var(dummy.mca.dep, 
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             title=" Nube de puntos de las Modalidades/Categorías - Depndency Network",
             ggtheme = theme_minimal()
)
dev.off()
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables_dep $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.dep, 
             choice = "var", #criterio a representar: varianza
             axes = 1, 
             top = 15) # selección de las 15 categorías con mayor contribuión
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.dep, 
             choice = "var", #criterio a representar: varianza
             axes = 2, 
             top = 15) # selección de las 15 categorías con mayor contribución
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.dep, 
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, 
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
png("Analisis/MCA_dep5.png")
fviz_mca_var(dummy.mca.dep, 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             title="Contribución de categorías - Depndency Network",
             ggtheme = theme_minimal()
)
dev.off()
### Analisis sobre individuos
house_dep <- get_mca_ind(dummy.mca.dep)
house_dep
# coordinadas para las filas
head(house_dep $ coord, 3)
# cualidad de la representación
head(house_dep $ cos2, 3)
# contribución de cada individuo
head(house_dep $ contrib, 3)
#Calidad de representaicion de individuos
fviz_mca_ind(dummy.mca.dep, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             title = "Individual representation quality - Dependency Network",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)

#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.dep, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.dep, 
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2,
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.dep, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)

### Agrupamiento de individuos
#Según sexo
png("Analisis/MCA_dep_sex.png")
fviz_mca_ind(dummy.mca.dep, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según educacion
png("Analisis/MCA_dep_edu.png")
fviz_mca_ind(dummy.mca.dep,  
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "education", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según trabajo
png("Analisis/MCA_dep_work.png")
fviz_mca_ind(dummy.mca.dep, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "work", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según nacion
png("Analisis/MCA_dep_nac.png")
fviz_mca_ind(dummy.mca.dep,
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "nacion", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según civil
png("Analisis/MCA_dep_civil.png")
fviz_mca_ind(dummy.mca.dep, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "civil",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según pueblo originario
png("Analisis/MCA_dep_ori.png")
fviz_mca_ind(dummy.mca.dep,
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "native",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salud
png("Analisis/MCA_dep_health.png")
fviz_mca_ind(dummy.mca.dep,  
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "health",  
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salario ### NO FUNCIONA
png("Analisis/MCA_dep_sal.png")
fviz_mca_ind(dummy.mca.dep, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "salario", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según ingreso
png("Analisis/MCA_dep_money.png")
fviz_mca_ind(dummy.mca.dep, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "quintil",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()

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
cols_to_exclude <- c("household")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_des <- as.data.frame(dummy_des_bind[, !(colnames(dummy_des_bind) %in% cols_to_exclude)])
data_pcm_des$quintil<-factor(data_pcm_des$quintil)
data_pcm_des$size<-factor(data_pcm_des$size)
data_pcm_des$ties<-factor(data_pcm_des$ties)
data_pcm_des$density<-factor(data_pcm_des$density)
data_pcm_des$n_comp<-factor(data_pcm_des$n_comp)
data_pcm_des$diameter<-factor(data_pcm_des$diameter)
data_pcm_des$isolates<-factor(data_pcm_des$isolates)
#Revisando los datos
data_pcm_des<-na.omit(data_pcm_des)
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(4, 4))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:16){
  plot(data_pcm_des[,i], main=colnames(data_pcm_des)[i],
       ylab="Cantidad", col="pink",las=2)
}
str(data_pcm_des)
data_pcm_des <- select(data_pcm_des, -density)#Eliminar density porque sólo tiene un level (REVISAR LUEGO)
colnames(data_pcm_des)
#Probando sólo con data
dummy.mca.des <-MCA(data_pcm_des,ncp = 4,graph=F)
print(dummy.mca.des)

###Visualizacion
## Eigenvalores/varianzas
eig_val_des <- factoextra::get_eigenvalue(dummy.mca.des)
head(eig_val_des)
fviz_screeplot(dummy.mca.des, addlabels = TRUE)
##Biplot
png(filename = "Analisis/MCA_des1.png")
fviz_mca_biplot(dummy.mca.des, 
                label = "none",
                repel = TRUE,
                title= "Categories and individuals - Descendency Network",
                alpha.ind = 0.1, 
                alpha.var = 1, 
                ggtheme = theme_minimal())
dev.off()
###Descripcion de las dimensiones # NO FUNCIONA REVISAR
res.desc_des <- dimdesc(dummy.mca.des, 
                    axes = c(1,2) 
)

res.desc_des[[1]]
res.desc_des[[2]]

###Analisis sobre las variables

variables_des <- get_mca_var(dummy.mca.des)
variables_des

# Coordenadas
head(variables_des $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables_des $ cos2)

# Contribución en los factores o dimensiones
head(variables_des $ contrib)

##Correlaciones
#Entre variable sy las dimensiones o factores
png(filename = "Analisis/MCA_des2.png")
fviz_mca_var(dummy.mca.des, 
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, 
             title= "Correlation between variables and dimentions - Descendency Network",
             ggtheme = theme_minimal()
)
dev.off()
#Coordenadas para la categoría de variables activas
png(filename = "Analisis/MCA_des3.png")
fviz_mca_var(dummy.mca.des,
             repel = TRUE, 
             ggtheme = theme_minimal(),
             title = "Nube de puntos de las Modalidades/Categorías - Descendent Network",
             col.var="red", # cambiar el color a las variables
             shape.var = 15 
)
dev.off()
##Calidad de la representacion de las categorias
head(round(variables_des $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.des,  
          choice = "var", # selección de las varianzas
          axes = 1:2) 
#En biplot
png("Analisis/MCA_des4.png")
fviz_mca_var(dummy.mca.des, 
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             title = "Nube de puntos modalidades/categorías - Descent Network",
             ggtheme = theme_minimal()
)
dev.off()
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables_des $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.des, 
             choice = "var", #criterio a representar: varianza
             axes = 1,
             top = 15) 
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.des, 
             choice = "var", #criterio a representar: varianza
             axes = 2,
             top = 15) 
# Total contribution to dimension 1 and 2
fviz_contrib(ddummy.mca.des, 
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, 
             top = 20)
# De manera grafica en un biplot
png("Analisis/MCA_des5.png")
fviz_mca_var(dummy.mca.des, 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             title = "contribución de categorías  - Descent Network",
             ggtheme = theme_minimal()
)
dev.off()
### Analisis sobre individuos
house_des <- get_mca_ind(dummy.mca.des)
house_des
# coordinadas para las filas
head(house_des $ coord, 3)
# cualidad de la representación
head(house_des $ cos2, 3)
# contribución de cada individuo
head(house_des $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.des, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.des, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.des, 
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2,
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.des, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
png("Analisis/MCA_des_sex.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según educacion
png("Analisis/MCA_des_edu.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none",
             habillage = "education",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según trabajo
png("Analisis/MCA_des_work.png")
fviz_mca_ind(dummy.mca.des,
             label = "none", 
             habillage = "work", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según nacion
png("Analisis/MCA_des_nac.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", 
             habillage = "nacion", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según civil
png("Analisis/MCA_des_civil.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", 
             habillage = "civil", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según pueblo originario
png("Analisis/MCA_des_ori.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", 
             habillage = "native", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salud
png("Analisis/MCA_des_health.png")
fviz_mca_ind(dummy.mca.des,
             label = "none",
             habillage = "health",  
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salario ### NO FUNCIONA
png("Analisis/MCA_des_sal.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", 
             habillage = "salario", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según ingreso
png("Analisis/MCA_des_money.png")
fviz_mca_ind(dummy.mca.des, 
             label = "none", 
             habillage = "quintil", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()

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
cols_to_exclude <- c("household")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_mar <- as.data.frame(dummy_mar_bind[, !(colnames(dummy_mar_bind) %in% cols_to_exclude)])
data_pcm_mar$size<-factor(data_pcm_mar$size)
data_pcm_mar$ties<-factor(data_pcm_mar$ties)
data_pcm_mar$density<-factor(data_pcm_mar$density)
data_pcm_mar$n_comp<-factor(data_pcm_mar$n_comp)
data_pcm_mar$diameter<-factor(data_pcm_mar$diameter)
data_pcm_mar$isolates<-factor(data_pcm_mar$isolates)
data_pcm_mar$quintil<-factor(data_pcm_mar$quintil)
#Revisando los datos
data_pcm_mar<-na.omit(data_pcm_mar)
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(3, 5))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:15){
  plot(data_pcm_mar[,i], main=colnames(data_pcm_mar)[i],
       ylab="Cantidad", col="pink",las=2)
}
dev.off()
#Probando sólo con data
dummy.mca.mar <-MCA(data_pcm_mar,ncp = 4,graph=F)
print(dummy.mca.mar)

###Visualizacion
## Eigenvalores/varianzas
eig_val_mar <- factoextra::get_eigenvalue(dummy.mca.mar)
head(eig_val_mar)
fviz_screeplot(dummy.mca.mar, addlabels = TRUE)
##Biplot
png(filename = "Analisis/MCA_mar1.png")
fviz_mca_biplot(dummy.mca.mar,
                repel = TRUE, 
                label = "none",
                alpha.ind = 0.1,
                alpha.var = 1, 
                ggtheme = theme_minimal()) 
dev.off()
###Descripcion de las dimensiones ## NO FUNCIONA 
res.desc_mar <- dimdesc(dummy.mca.mar, 
                    axes = c(1,2)
)
res.desc_mar[[1]]
res.desc_mar[[2]]

###Analisis sobre las variables

variables_mar <- get_mca_var(dummy.mca.mar)
variables_mar

# Coordenadas
head(variables_mar $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables_mar $ cos2)

# Contribución en los factores o dimensiones
head(variables_mar $ contrib)

##Correlaciones
#Entre variables y las dimensiones o factores
png(filename = "Analisis/MCA_mar2.png")
fviz_mca_var(dummy.mca.mar, 
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE,
             title= "Correlation between variables and dimentions - Marriage Network",
             ggtheme = theme_minimal()
)
dev.off()
#Coordenadas para la categoría de variables activas
png(filename = "Analisis/MCA_mar3.png")
fviz_mca_var(dummy.mca.mar, 
             repel = TRUE,
             ggtheme = theme_minimal(),
             title= "Nube de puntos de Modalidades/Categorías - Marriage Network",
             col.var="red", 
             shape.var = 15 
)
dev.off()
##Calidad de la representacion de las categorias
head(round(variables_mar $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.mar, 
          choice = "var", # selección de las varianzas
          axes = 1:2) 
#En biplot
png("Analisis/MCA_mar4.png")
fviz_mca_var(dummy.mca.mar, 
             col.var = "cos2", #definición de los colores a partir del valor cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             title= "Nube de puntos de Modalidades/Categorías - Marriage Network",
             ggtheme = theme_minimal()
)
dev.off()
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables_mar $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.mar, 
             choice = "var", #criterio a representar: varianza
             axes = 1, 
             top = 15) 
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.mar, 
             choice = "var", #criterio a representar: varianza
             axes = 2, 
             top = 15) 
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.mar, 
             choice = "var", #criterio de representación = varianzas
             axes = 1:2, 
             top = 20) 
# De manera grafica en un biplot
png("Analisis/MCA_mar5.png")
fviz_mca_var(dummy.mca.mar, 
             col.var = "contrib", #definir la coloración mediante la "contribución"
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE,
             title="Contribución de categorías - Marriage Network",
             ggtheme = theme_minimal()
)
dev.off()

### Analisis sobre individuos
house_mar <- get_mca_ind(dummy.mca.mar)
house_mar
# coordinadas para las filas
head(house_mar $ coord, 3)
# cualidad de la representación
head(house_mar $ cos2, 3)
# contribución de cada individuo
head(house_mar $ contrib, 3)
#Calidad de representacion de individuos
fviz_mca_ind(dummy.mca.mar, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none",
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.mar, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             label = "none",
             repel = TRUE,
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.mar, 
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, 
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.mar, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
png("Analisis/MCA_mar_sex.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none", # ocultar las etiquetas de los individuos
             habillage = "sex", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según educacion
png("Analisis/MCA_mar_edu.png")
fviz_mca_ind(dummy.mca.mar,  
             label = "none",
             habillage = "education", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según trabajo
png("Analisis/MCA_mar_work.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none", 
             habillage = "work", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según nacion
png("Analisis/MCA_mar_nac.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none", 
             habillage = "nacion", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según civil
png("Analisis/MCA_mar_civil.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none",
             habillage = "civil",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según pueblo originario
png("Analisis/MCA_mar_ori.png")
fviz_mca_ind(dummy.mca.mar,
             label = "none", 
             habillage = "native", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salud
png("Analisis/MCA_mar_health.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none", 
             habillage = "health",
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salario ### NO FUNCIONA
png("Analisis/MCA_mar_sal.png")
fviz_mca_ind(dummy.mca.mar,
             label = "none", 
             habillage = "salario", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según ingreso
png("Analisis/MCA_mar_money.png")
fviz_mca_ind(dummy.mca.mar, 
             label = "none",
             habillage = "quintil", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()

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
# excluir las columnas "id"
cols_to_exclude <- c("household")
# Filtra las columnas que no estén en la lista de exclusiones y luego aplica la operación
data_pcm_kin <- as.data.frame(dummy_kin_bind[, !(colnames(dummy_kin_bind) %in% cols_to_exclude)])
data_pcm_kin$quintil<-factor(data_pcm_kin$quintil)
data_pcm_kin$size<-factor(data_pcm_kin$size)
data_pcm_kin$ties<-factor(data_pcm_kin$ties)
data_pcm_kin$density<-factor(data_pcm_kin$density)
data_pcm_kin$n_comp<-factor(data_pcm_kin$n_comp)
data_pcm_kin$diameter<-factor(data_pcm_kin$diameter)
data_pcm_kin$isolates<-factor(data_pcm_kin$isolates)
#Revisando los datos
data_pcm_kin<-na.omit(data_pcm_kin)
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(3, 5))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:15){
  plot(data_pcm_kin[,i], main=colnames(data_pcm_kin)[i],
       ylab="Cantidad", col="pink",las=2)
}
dev.off()
str(data_pcm_kin)
data_pcm_kin <- select(data_pcm_kin, -density)#Eliminar density porque sólo tiene un level (REVISAR LUEGO)
colnames(data_pcm_kin)
#Probando sólo con data
dummy.mca.kin <-MCA(data_pcm_kin,ncp = 4,graph=F)
print(dummy.mca.kin)

###Visualizacion
## Eigenvalores/varianzas
eig_val_kin <- factoextra::get_eigenvalue(dummy.mca.kin)
head(eig_val_kin)
fviz_screeplot(dummy.mca.kin, addlabels = TRUE)
##Biplot
png(filename = "Analisis/MCA_kin1.png")
fviz_mca_biplot(dummy.mca.kin, 
                repel = TRUE, 
                label = "none",
                alpha.ind = 0.1, 
                alpha.var = 1, 
                ggtheme = theme_minimal()) 
dev.off()
###Descripcion de las dimensiones ## NO FUNCIONA 
res.desc_kin  <- dimdesc(dummy.mca.kin, 
                    axes = c(1,2) 
)
res.desc_kin [[1]]
res.desc_kin [[2]]

###Analisis sobre las variables

variables_kin  <- get_mca_var(dummy.mca.kin)
variables_kin 

# Coordenadas
head(variables_kin  $ coord)

# cosenos cuadrados: calidad en el mapa de factores
head(variables_kin  $ cos2)

# Contribución en los factores o dimensiones
head(variables_kin  $ contrib)

##Correlaciones
#Entre variables y las dimensiones o factores
png(filename = "Analisis/MCA_kin2.png")
fviz_mca_var(dummy.mca.kin, 
             choice = "mca.cor", # tipo de análisis solicitado: correlaciones
             repel = TRUE, 
             title= "Correlation between variables and dimentions - Kinship Network",
             ggtheme = theme_minimal()
)
dev.off()
#Coordenadas para la categoría de variables activas
png(filename = "Analisis/MCA_kin3.png")
fviz_mca_var(dummy.mca.kin, 
             repel = TRUE, 
             ggtheme = theme_minimal(),
             title= "Nube de puntos de Modalidades/Categorías - Kinship Network",
             col.var="red",
             shape.var = 15 
)
dev.off()
##Calidad de la representacion de las categorias
head(round(variables_kin $ cos2, 3), 5)
#En histograma
fviz_cos2(dummy.mca.kin,
          choice = "var", # selección de las varianzas
          axes = 1:2) 
#En biplot
png(filename = "Analisis/MCA_kin4.png")
fviz_mca_var(dummy.mca.kin, 
             col.var = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, 
             title= "Nube de puntos de Modalidades/Categorías - Marriage Network",
             ggtheme = theme_minimal()
)
dev.off()
##Contribucion de las categorías sobre las dimensiones creadas
head(round(variables_kin $ contrib, 2))
# Contribución de las categorías a la dimensión 1.
fviz_contrib(dummy.mca.kin,
             choice = "var",
             axes = 1, 
             top = 15) 
# Contribución de las categorías a la dimensión 2.
fviz_contrib(dummy.mca.kin, 
             choice = "var", #criterio a representar: varianza
             axes = 2,
             top = 15) 
# Total contribution to dimension 1 and 2
fviz_contrib(dummy.mca.kin, 
             choice = "var", 
             axes = 1:2, 
             top = 20) #selección de las 20 categorías con mayor contribución
# De manera grafica en un biplot
png(filename = "Analisis/MCA_kin5.png")
fviz_mca_var(dummy.mca.kin,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, 
             title="Contribución de categorías - Marriage Network",
             ggtheme = theme_minimal()
)
dev.off()
### Analisis sobre individuos
house_kin <- get_mca_ind(dummy.mca.kin)
house_kin
# coordinadas para las filas
head(house_kin $ coord, 3)
# cualidad de la representación
head(house_kin$ cos2, 3)
# contribución de cada individuo
head(house_kin $ contrib, 3)
#Calidad de representaicon de individuos
fviz_mca_ind(dummy.mca.kin, 
             col.ind = "cos2", # colorear los casos a partir del criterio de valor cos^2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             label = "none",
             ggtheme = theme_minimal()
)
#Contribucion de casos del analisis
fviz_mca_ind(dummy.mca.kin, 
             col.ind = "contrib", # colorear los casos a partir del valor de contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             label = "none",
             ggtheme = theme_minimal()
)
#Lo mismo en histograma
fviz_contrib(dummy.mca.kin, 
             choice = "ind", #selección de los individuos para representarlos
             axes = 1:2, 
             top = 20) #selección de los 20 casos con mayor nivel
fviz_cos2(dummy.mca.kin, 
          choice = "ind", 
          axes = 1:2, 
          top = 20)
### Agrupamiento de individuos
#Según sexo
png("Analisis/MCA_kin_sex.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "sex", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según educacion
png("Analisis/MCA_kin_edu.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "education", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según trabajo
png("Analisis/MCA_kin_work.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "work", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según nacion
png("Analisis/MCA_kin_nac.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "nacion", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según civil
png("Analisis/MCA_kin_civil.png")
fviz_mca_ind(dummy.mca.kin,  
             label = "none", 
             habillage = "civil",  
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según pueblo originario
png("Analisis/MCA_kin_ori.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "native", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salud
png("Analisis/MCA_kin_health.png")
fviz_mca_ind(dummy.mca.kin,  
             label = "none", 
             habillage = "health", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según salario ### NO FUNCIONA
png("Analisis/MCA_kin_sal.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "salario",  
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
#Según ingreso
png("Analisis/MCA_kin_money.png")
fviz_mca_ind(dummy.mca.kin, 
             label = "none", 
             habillage = "quintil", 
             addEllipses = TRUE,
             ggtheme = theme_minimal()
)
dev.off()
