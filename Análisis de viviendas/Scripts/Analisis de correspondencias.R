################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################
### Versión 1 ###
## MCA usando datos de redes (categorizados) y measurements categorizados. 
## Revisar tipologías por cluster

###### LIBRERÍAS ######
library(easypackages)
paquetes <- c("FactoMineR", "tidyverse", "factoextra", "haven", "naniar", "corrplot","doParallel")
libraries(paquetes)

###### CARGAR DATA ######
load("Análisis de viviendas/Descriptives/medidas_redes.RData")
load("Ergomitos/Redes/tablas_redes_cluster.RData")

##################### ADMINISTRACIÓN DE LOS DATOS ##############################
# Crea tipología de vivienda a partir de indices en measurements
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
dummy_measurements <- measurements %>%
  mutate(quintil = as.factor(quintil),  
         region = as.factor(region),    
         comuna = as.factor(comuna)) %>%  
  mutate(porc.hombre_cat = cut(porc.hombre, 
                               breaks = c(0, 50, 100), 
                               labels = c("Mujeres", "Hombres"), 
                               include.lowest = TRUE)) %>%
  mutate(porc.ind_cat = cut(porc.ind, 
                               breaks = c(0, 50, 100), 
                               labels = c("No-indígena", "Indígena"), 
                               include.lowest = TRUE)) %>%
  mutate(porc.empleo_cat = cut(porc.empleo, 
                            breaks = c(0,20,40,60,80,100), 
                            labels = c("Pocos","Algunos","Mitad","Mayoría", "Casi todos"), 
                            include.lowest = TRUE)) %>%
  mutate(porc.salud_cat = cut(porc.salud, 
                               breaks = c(0,20,40,60,80,100), 
                               labels = c("Pocos","Algunos","Mitad","Mayoría", "Casi todos"), 
                               include.lowest = TRUE)) %>%
  mutate(porc.enf_cat = cut(porc.enf, 
                              breaks = c(0,20,40,60,80,100), 
                              labels = c("Pocos","Algunos","Mitad","Mayoría", "Casi todos"), 
                              include.lowest = TRUE)) %>%
  mutate(porc.chi_cat = cut(porc.chi, 
                            breaks = c(0, 50, 100), 
                            labels = c("Extranjeros","Chilenos"), 
                            include.lowest = TRUE)) %>%
  mutate(porc.sal_cat = cut(porc.sal, 
                            breaks = c(0,20,40,60,80,100), 
                            labels = c("Pocos","Algunos","Mitad","Mayoría", "Casi todos"), 
                            include.lowest = TRUE)) %>%
  mutate(porc.edad_cat = cut(edad.prom, 
                            breaks = c(0,20,60,80,100), 
                            labels = c("Jovenes","Adultos","Tercera edad", "Cuarta edad"), 
                            include.lowest = TRUE))
stopCluster(cl)
 
save(dummy_measurements,file="Data/Dummy_measurements_mca.RData")

###Utiliza set de la data ###
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
set.seed(400)  # Fijar semilla para reproducibilidad
id_vivienda_sample <- sample(unique(data$household), size =1000,replace = F)# Crear subset con los 1000 id_vivienda seleccionados
dummy_measurements_subset <- dummy_measurements %>%
  filter(household %in% id_vivienda_sample) %>%
  stopCluster(cl)
save(dummy.data, file = "Análisis de viviendas/Data/Dummy_measurements_sample_mca.RData")


####################### MCA PRELIMINAL #########################################
#Toda la data
load("Data/Dummy_data_mca.RData")
#Data sample
load("Data/Dummy_data_sample_mca.RData")
#Tabla de redes-cluster
load("Redes/tablas_redes_cluster.RData")

######################## RED DE DEPENDENCIA ####################################
head(results_list_dependency)
print(colnames(results_list_dependency))
head(dummy_measurements)

#Cambiar id por household
dummy_dep <- results_list_dependency%>%
  rename(household=id, Cluster=grupos_dep.cluster)
colnames(dummy_dep)
measure_dep<-dummy_measurements%>%
  rename(household=i)
#Unir ambas bases de datos por id_vivienda 
dummy_dep_bind <-merge(measure_dep,dummy_dep,by="household")
#Cambiar variables a factor
dummy_dep_bind$household<-factor(dummy_dep_bind$household)
dummy_dep_bind$size<-factor(dummy_dep_bind$size)
dummy_dep_bind$ties<-factor(dummy_dep_bind$ties)
dummy_dep_bind$n_comp<-factor(dummy_dep_bind$n_comp)
dummy_dep_bind$diameter<-factor(dummy_dep_bind$diameter)
dummy_dep_bind$isolates<-factor(dummy_dep_bind$isolates)
dummy_dep_bind$Cluster<-factor(dummy_dep_bind$Cluster)
# Cambiar los niveles de comuna. Esto debería ir en la creación de dummy data
dummy_dep_bind$comuna<-factor(dummy_dep_bind$comuna)
etiquetas <- attr(dummy_dep_bind$region, "labels")
dummy_dep_bind$region <- factor(dummy_dep_bind$region, levels = niveles, labels = etiquetas[niveles])
# Cambiar los niveles de region. Esto debería ir en la creación de dummy data
niveles <- unique(dummy_dep_bind$region)
etiquetas <- attr(dummy_dep_bind$region, "labels")
dummy_dep_bind$region <- factor(dummy_dep_bind$region, levels = niveles, labels = etiquetas[niveles])
dummy_dep_bind$porc.hombre<-factor(dummy_dep_bind$porc.hombre)
dummy_dep_bind$porc.ind<-factor(dummy_dep_bind$porc.ind)
dummy_dep_bind$porc.empleo<-factor(dummy_dep_bind$porc.empleo)
dummy_dep_bind$porc.salud<-factor(dummy_dep_bind$porc.chi)
dummy_dep_bind$porc.sal<-factor(dummy_dep_bind$porc.salud)
dummy_dep_bind$edad.prom<-factor(dummy_dep_bind$edad.prom)
dummy_dep_bind$quintil<-factor(dummy_dep_bind$quintil)
dummy_dep_bind$porc.enf<-factor(dummy_dep_bind$porc.enf)

#Revisando los datos
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(4, 5))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:17){
  plot(dummy_dep_bind[,i], main=colnames(dummy_dep_bind)[i],
       ylab="Cantidad", col="pink",las=2)
}
str(dummy_dep_bind)
colnames(dummy_dep_bind)
sum(is.na(dummy_dep_bind))

# Reducir tamaño de la data.frame
library(data.table)
dummy_dep_bind<-as.data.table(dummy_dep_bind)

#Elegir subset
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
set.seed(600)
id_vivienda_sample <- sample(unique(dummy_dep_bind$household), size =1000,replace = F)# Crear subset con los 1000 id_vivienda seleccionados
dummy_dep_bind_sub <- dummy_dep_bind %>%
  filter(household %in% id_vivienda_sample)
stopCluster(cl)

#Calculo de Correspondencias múltiples
ind.sup <- 1
quali.sup <- 2:13
library(doParallel)
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
dummy.mca.dep <-MCA(dummy_dep_bind_sub,ncp = 2,graph=T,ind.sup=1,quali.sup=2:13,na.method = "Average")
stopCluster(cl)
print(dummy.mca.dep)

###Visualizacion 
## Eigenvalores/varianzas
eig_val_dep <- factoextra::get_eigenvalue(dummy.mca.dep)
 fviz_screeplot(dummy.mca.dep, addlabels = F)

 ##Biplot
png(filename = "Analisis/MCA_dep1.png")
fviz_mca_biplot(dummy.mca.dep,
                repel = T,
                label = "none",
                title = "Categories and individuals - Dependency Network",
                alpha.ind = 0.1,
                alpha.var = 1, 
                ggtheme = theme_minimal())
dev.off()
###Descripcion de las dimensiones ### 
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
png("Analisis/2.MCA_dep.png")
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
             repel = F,
             label="none",
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
#Según salario 
png("Analisis/MCA_dep_sal.png")
fviz_mca_ind(mca_result, 
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
