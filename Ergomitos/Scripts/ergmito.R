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
load(here(paste0('Ergomitos/Redes/dependency_network_subset1000.RData'))) # Red de apoyo económico
load(here(paste0('Ergomitos/Redes/kinship_network_subset1000.RData'))) # Red de parentesco
load(here(paste0('Ergomitos/Redes/descent_network_subset1000.RData'))) # Red de parentesco
load(here(paste0('Ergomitos/Redes/marriage_network_subset1000.RData'))) # Red de parentesco

# bases completas
 #load(here(paste0('Ergomitos/Redes/dependency_network.RData'))) # Red de apoyo económico
 #load(here(paste0('Ergomitos/Redes/kinship_network.RData'))) # Red de parentesco
 #load(here(paste0('Ergomitos/Redes/descent_network.RData'))) # Red de parentesco
 #load(here(paste0('Ergomitos/Redes/marriage_network.RData'))) # Red de parentesco


#dependency_network_sample<-sample(unique(dependency_network),size=1000,replace=F)

# Filtrar redes basado en tamaño (número de actores) y número de vínculos
lists <- list() # Crear una lista para guardar las redes filtradas

  i=1
for (i in seq_along(dependency_network_sample)) {
  graphs4 <- dependency_network_sample[[i]][['dependency_net']]  # Subconjunto de una red
  descent <- descent_network_sample[[i]][["descent_net"]]  # Subconjunto de una red
  marriage <- marriage_network_sample[[i]][["marriage_net"]]  # Subconjunto de una red
  
  # Condiciones para el filtrado
  if (length(graphs4[["val"]]) >= 3 & length(graphs4[["val"]]) <= 5 &  # Tamaño entre 4 y 5 actores Overall, the exact likelihood calculation is only possible when dealing with directed (undirected) networks size 5 (7).
      length(graphs4[["mel"]]) >= 1 & length(graphs4[["mel"]]) <= 5 & # Entre 1 y 5 vínculos (In general, directed (undirected) graphs with more than 5 (7) vertices should not be fitted using MLE)
      #length(descent[["mel"]]) >= 5 & length(descent[["mel"]]) <= 10
      length(marriage[["mel"]]) >= 1 & length(marriage[["mel"]]) <= 5
      
      ) # Entre 2 y 10 vínculos 

    { 
    lists[[length(lists) + 1]] <- graphs4 # Agregar red que cumple las condiciones a la lista
  }
   }

# Cantidad de redes filtradas
length(lists)

#i=1
#summary(marriage)
 #lapply(lists, network.size) # check size in each classroom
 #lapply(lists, network.density) # check network density in each classroom



# Estimación ERGMITO & GOF
system.time(
  ans <- ergmito(lists
                 ~ edges 
                 +mutual
                 #+isolates
          # degree-relates effectsƒ
                 #+ostar(2)
                 #+twopath 
                 #+odegree(1)
                 #+idegree(1)
                 #+isolates
                 # tryadic effects
                 # transitive effects
                 +balance
                
          
         # covariates  
                 
             # sex
                 #+ nodeicov("sex")  # Popularidad para variables categóricas
                 #+ nodeocov("sex")  # Actividad para variables categóricas
                 #+ nodematch('sex', diff=F)    # Homofilia para variables categóricas
            # education level 
                 #+ nodeifactor("e6a")  # Popularidad para variables categóricas
                 #+ nodeofactor("e6a")  # Actividad para variables categóricas
                 #+ nodematch('e6a')    # Homofilia para variables categóricas
         
            # indigenous background 
               #+ nodeifactor("r3")  # Popularidad para variables categóricas
               #+ nodeofactor("r3")  # Actividad para variables categóricas
               #+ nodematch('r3')    # Homofilia para variables categóricas
         
           # nationality
              #+ nodeifactor("r1b_pais_esp")  # Popularidad para variables categóricas
              #+ nodeofactor("r1b_pais_esp")  # Actividad para variables categóricas
              #+ nodematch('r1b_pais_esp')    # Homofilia para variables categóricas
         
            # medical treatment (no statistics)
                 #+ nodeifactor("s28")  # Popularidad para variables categóricas
                 #+ nodeofactor("s28")  # Actividad para variables categóricas
                 #+ nodematch('s28')    # Homofilia para variables categóricas
         
            # medical attention (no statistics)
                #+ nodeifactor("s17")  # Popularidad para variables categóricas
                #+ nodeofactor("s17")  # Actividad para variables categóricas
                #+ nodematch('s17')    # Homofilia para variables categóricas
         
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
                 ,maxNumChangeStatVectors = 2^22
  )
)

# Tiempo de estimación
   #system.time()

# Resumen de los resultados
summary(ans)

