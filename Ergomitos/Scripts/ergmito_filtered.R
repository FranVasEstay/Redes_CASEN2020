# Cargar paquetes
library(ergmito)
library(ergm)
#library(purrr)
#library(futile.logger)
library(here)

# Establecer directorio
here()

# Cargar datasets

# subset 1000
#load(here(paste0('Ergomitos/Redes/dependency_network_subset1000.RData'))) # Red de apoyo económico
#load(here(paste0('Ergomitos/Redes/kinship_network_subset1000.RData'))) # Red de parentesco
#load(here(paste0('Ergomitos/Redes/descent_network_subset1000.RData'))) # Red de parentesco
#load(here(paste0('Ergomitos/Redes/marriage_network_subset1000.RData'))) # Red de parentesco

load(here(paste0('Ergomitos/Redes/dependency_network_filtred.RData'))) # Red de apoyo económico
#load(here(paste0('Ergomitos/Redes/kinship_network_filtred.RData'))) # Red de parentesco
#load(here(paste0('Ergomitos/Redes/descent_network_filtrados.RData'))) # Red de parentesco
#load(here(paste0('Ergomitos/Redes/marriage_network_filtred.RData'))) # Red de parentesco





# Initialize lists for filtered households
filtered_households <- list()

# Variables of interest
variables <- c("sex" 
               ,"e6a" 
               #,"r3" 
               ,"edad" 
               ,"r1b_pais_esp"
               #, "s28" 
               #,"s17"
               )

# Loop through each household/network
for (i in seq_along(dependency_network_filtred)) {
  # Extract the current household's network object
  household_network <- dependency_network_filtred[[i]][['dependency_net']]
  #marriage_network <- marriage_network_filtred[[i]][['marriage_net']]
  
  # Check if all variables exist as vertex attributes in the network
  if (all(variables %in% list.vertex.attributes(household_network))) {
    # Extract variable values as a list
    variable_values <- lapply(variables, function(var) get.vertex.attribute(household_network, var))
    
    # Check for no missing data and variation in all variables
    no_missing <- all(sapply(variable_values, function(x) all(!is.na(x))))
    has_variation <- all(sapply(variable_values, function(x) length(unique(x)) > 1))
    
    # Check conditions on household network size and edges
    if (no_missing && has_variation &&
        #length(household_network[["val"]]) >= 3 && length(household_network[["val"]]) <= 5 && # Size: exactly 3 nodes
        length(household_network[["val"]]) == 5 && # Size: exactly 3-5 nodes
        
        length(household_network[["mel"]]) >= 0 && length(household_network[["mel"]]) <= 10 # 1 to 5 edges
        # Add more conditions for marriage or other networks if needed
    ) {
      filtered_households[[length(filtered_households) + 1]] <- household_network
    }
  }
}

# Output the number of filtered households
cat("Number of households meeting all criteria:", length(filtered_households), "\n")




# Check attribute types for one network
attributes <- list.vertex.attributes(filtered_households[[1]])
sapply(attributes, function(attr) class(get.vertex.attribute(filtered_households[[1]], attr)))
#
## Convert Attributes to Numeric
#for (i in seq_along(filtered_households)) {
#  household <- filtered_households[[i]]
#  set.vertex.attribute(household, "edad", as.numeric(get.vertex.attribute(household, "edad")))
#}

# for (i in seq_along(filtered_households)) {
#for (i in seq_along(filtered_households)) {
#  household <- filtered_households[[i]]
#  if (any(is.na(get.vertex.attribute(household, "edad")))) {
#    cat("Missing data in 'edad' for network:", i, "\n")
#  }
#}

# Convert "edad" to numeric across networks
#for (i in seq_along(filtered_households)) {
#  household <- filtered_households[[i]]
#  set.vertex.attribute(household, "s28", as.numeric(get.vertex.attribute(household, "s28")))
#}

###Integración de nuevas variables
#sex:(1: Hombre; 2 mujer)
#edad (númerica)
#e6a nivel educativo (númerica)
#r1b_pais_esp nacionalidad (1: Chileno; 2: Extranjero)

#r3 pueblo indígena
#s17 tuvo consulta médica
#s28 ha estado en tratamiento médico (12 meses)
#y1 Monto sueldo líquido
#y1_preg Recibió sueldo
##o1 trabajó la semana pasada



# Estimación ERGMITO & GOF
system.time(
  ans <- ergmito(
    #lists[1:1000]
    filtered_households#[1:46]        
    ~ edges 
    +mutual
    #+opentriad
    ##+isolates
    ## degree-relates effectsƒ
    +ostar(2)
    #+istar(2)
    #+twopath 
    #+odegree(1)
    #+idegree(1)
    #+isolates
    # tryadic effects
    # transitive effects
    #+balance
    
    
    # covariates  
    
    # sex
    + nodeifactor("sex")  # Popularidad para variables categóricas
    + nodeofactor("sex")  # Actividad para variables categóricas
    + nodematch('sex', diff=T)    # Homofilia para variables categóricas
    
    # education level 
  #+ nodeicov("e6a")  # Popularidad para variables categóricas
  #+ nodeocov("e6a")  # Actividad para variables categóricas
  #+ absdiff('e6a')    # Homofilia para variables categóricas
    
    # indigenous background 
    #+ nodeicov("r3")  # Popularidad para variables categóricas
    #+ nodeocov("r3")  # Actividad para variables categóricas
    #+ nodematch('r3', diff=T)    # Homofilia para variables categóricas
    
    # nationality
    #+ nodeifactor("r1b_pais_esp")  # Popularidad para variables categóricas
    #+ nodeofactor("r1b_pais_esp")  # Actividad para variables categóricas
    #+ nodematch('r1b_pais_esp', diff=T)    # Homofilia para variables categóricas
    
    # medical treatment (no statistics)
    #+ nodeicov("s28")  # Popularidad para variables categóricas
    #+ nodeocov("s28")  # Actividad para variables categóricas
    #+ nodematch('s28', diff=T)    # Homofilia para variables categóricas
    
    # medical attention (no statistics)
    #+ nodeifactor("s17")  # Popularidad para variables categóricas
    #+ nodeofactor("s17")  # Actividad para variables categóricas
    #+ nodematch('s17', diff=F)    # Homofilia para variables categóricas
    
    # age
    + nodeicov("edad")  # Popularidad para variables númericas
    + nodeocov("edad")  # Actividad para variables númericas
    + absdiff('edad')    # Homofilia para variables númericas
    
    # wages 
    #+ nodeicov("y1")  # Popularidad para variables númericas
    #+ nodeocov("y1")  # Actividad para variables númericas
    #+ absdiff('y1')    # Homofilia para variables númericas
    
    # got wages last week
    #+ nodeifactor("o1")  # Popularidad para variables categóricas
    #+ nodeofactor("o1")  # Actividad para variables categóricas
    #+ nodematch('o1')    # Homofilia para variables categóricas
    
    # worked last week
    #+ nodeifactor("y1_")  # Popularidad para variables categóricas
    #+ nodeofactor("y1_")  # Actividad para variables categóricas
    #+ nodematch('y1_')    # Homofilia para variables categóricas
    
    #+ edgecov(descent)  # effect of kinship on dependency
    #+ edgecov(marriage)  # effect of kinship on dependency
    
    
    # network size 
    #,model_update = ~ I(edges * (n == 4)) # reference term for networks of size 5.
    
    
    
    ,control=control.ergm(parallel=12, parallel.type="PSOCK", seed=1)
    ,maxNumChangeStatVectors = 2^22#, force=T
  )
)
  # Tiempo de estimación
#system.time()
# Resumen de los resultados
summary(ans)

# guardar resultados
 #save(ans, file="Ergomitos/Modelos/viviendas_tamaño_3.RData")
 #save(ans, file="Ergomitos/Modelos/viviendas_tamaño_4.RData")
 #save(ans, file="Ergomitos/Modelos/viviendas_tamaño_5.RData")

# GOF
load("Ergomitos/Modelos/viviendas_tamaño_3.RData")
load("Ergomitos/Modelos/viviendas_tamaño_4.RData")
#load("Ergomitos/Modelos/viviendas_tamaño_5.RData")

summary(ans)


help("ergm")
