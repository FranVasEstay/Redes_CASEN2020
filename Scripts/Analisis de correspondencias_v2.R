################################################################################
###################### Social Network: encuesta CASEN ##########################
################################################################################
################################################################################
####################### ANALISIS DE CORRESPONDENCIAS ###########################
################################################################################

###### LIBRERÍAS ######
#install.packages(c("DoParalell"))
#install.packages("data.table") 
library(easypackages)
paquetes <- c("ca", "tidyverse", "factoextra", "haven", "naniar", "corrplot","doParallel")
libraries(paquetes)

###### CARGAR DATA ######
load("Data/ori_Casen2020_rdata.RData")
load("Redes/tablas_redes_cluster.RData")
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
    across(c(education,parent, civil, nuclear_parent, native, health, work), as_factor),
    salario =factor(salario, levels=c(1,2),labels=c("Si","No")),
    nacion = ifelse(nacion == "", 1,
                    ifelse(nacion == "NO RESPONDE", 3, 2)),
    nacion = factor(nacion, levels = c(1, 2), labels = c("Chileno", "Extranjero")),
    work = ifelse(is.na(work), "No_trabaja", work),
    salario = ifelse(is.na(salario), "No_trabaja", salario),
    civil = ifelse(is.na(civil), "No_aplica", civil)
  )

# Se seleccionan las variables necesarias
ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
dummy.data<- data %>%
  group_by(household) %>%
  summarise(total_money = sum(money, na.rm = TRUE), .groups = 'drop') %>%  # Sumar money por hogar
  mutate(quintil = ntile(total_money, 5)) %>%  # Calcular quintiles
  select(household, quintil) %>%  # Mantener solo household y quintil
  right_join(data, by = "household") %>%  # Unir de nuevo con los datos originales
  mutate(
    grupo_etario = cut(edad, 
                       breaks = c(-1, 11, 18, 30, 60, Inf), 
                       labels = c("Niñez", "Adolescencia", "Juventud", "Adultez", "Vejez"),
                       right = TRUE),  # Clasificación por grupos etarios
    health = ifelse(health == "No sabe/No recuerda", "No_sabe/No_recuerda", health),
    #    salario = ifelse(salario == "No sabe", "No_sabe", salario),
    civil = ifelse(civil == "No sabe\\No responde","No_sabe\\No_responde",civil)
  ) %>%
  select(grupo_etario,household,sex, education, work, nacion, parent, civil,native, health, salario, quintil,comuna,region) # Se seleccionan las variables 
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
  rename(household=id, Cluster=grupos_dep.cluster)
colnames(dummy_dep)
#Unir ambas bases de datos por id_vivienda 
dummy_dep_bind <-merge(dummy.data,dummy_dep,by="household")
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
#niveles <- unique(dummy_dep_bind$comuna)
#etiquetas <- attr(dummy_dep_bind$comuna, "labels")
#dummy_dep_bind$comuna <- factor(dummy_dep_bind$comuna, 
#                               levels = niveles, 
#                              labels = etiquetas[as.character(niveles)])

# Cambiar los niveles de region. Esto debería ir en la creación de dummy data
niveles <- unique(dummy_dep_bind$region)
etiquetas <- attr(dummy_dep_bind$region, "labels")
dummy_dep_bind$region <- factor(dummy_dep_bind$region, levels = niveles, labels = etiquetas[niveles])

dummy_dep_bind$civil<-factor(dummy_dep_bind$civil)
dummy_dep_bind$health<-factor(dummy_dep_bind$health)
dummy_dep_bind$salario<-factor(dummy_dep_bind$salario)
dummy_dep_bind$comuna<-factor(dummy_dep_bind$comuna)
dummy_dep_bind$region<-factor(dummy_dep_bind$region)
dummy_dep_bind$grupo_etario<-factor(dummy_dep_bind$grupo_etario)
dummy_dep_bind$quintil<-factor(dummy_dep_bind$quintil)
dummy_dep_bind$work<-factor(dummy_dep_bind$work)

#Revisando los datos
dev.new(width = 24, height = 16)
par(mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
par(mfrow = c(4, 5))  # 3 filas y 5 columnas para un total de 15 gráficos
for(i in 1:20){
  plot(dummy_dep_bind[,i], main=colnames(dummy_dep_bind)[i],
       ylab="Cantidad", col="pink",las=2)
}
str(dummy_dep_bind)
colnames(dummy_dep_bind)
sum(is.na(dummy_dep_bind))
dummy_dep_bind <- na.omit(dummy_dep_bind)
variables <- dummy_dep_bind[, !"household"] # Excluir la columna 'household' y seleccionar solo las columnas categóricas

ncores <- detectCores()-1
cl <- parallel::makeCluster(ncores)
registerDoParallel(cl)
variables <- dummy_dep_bind[, !"household"]
variable_pairs <- combn(names(variables), 2, simplify = FALSE)
contingency_tables <- lapply(variable_pairs, function(pair) {
  table(variables[[pair[1]]], variables[[pair[2]]])
})
names(contingency_tables) <- sapply(variable_pairs, function(pair) paste(pair, collapse = " x "))
print(contingency_tables[[2]]) # revisar las tablas por índice

class(contingency_tables)
length(contingency_tables)
names(contingency_tables)
dummy.ca.dep <- ca(contingency_tables$`isolates x Cluster`, nd = 2)
print(dummy.ca.dep)
dev.new(width = 24, height = 16)
plot(dummy.ca.dep)
