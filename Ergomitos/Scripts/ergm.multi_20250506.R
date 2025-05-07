## ----Multi.ergm -----------------------------------------------------------------------------------------------------------------------

#load("~/OneDrive/Redes_CASEN2020/Ergomitos/Redes/kinship_network_filtred_subset100.RData")
load("~/OneDrive/Redes_CASEN2020/Ergomitos/Redes/dependency_network_filtred.RData")
load("~/OneDrive/Redes_CASEN2020/Ergomitos/Redes/descent_network_filtrados.RData")
#load("~/OneDrive/Redes_CASEN2020/Ergomitos/Redes/marriage_network_filtred.RData")

# Networks
load("Ergomitos/Redes/dependency_network.RData")
load("Ergomitos/Redes/descent_network.RData")

#load("Ergomitos/Redes/dependency_igraph.RData")
#load("Ergomitos/Redes/descent_network.RData")


# Covariates
load("Ergomitos/Data/Data_Ergomitos.RData")
#unique_household<-unique(data_ergomitos$household) # c


###### filter #########

# Filtrar redes basado en tamaño (número de actores) y número de vínculos
lists_dependency <- list() # Crear una lista para guardar las redes filtradas
lists_descent <- list() # Crear una lista para guardar las redes filtradas
#lists_marriage <- list() # Crear una lista para guardar las redes filtradas



i=1
for (i in seq_along(dependency_network)) {
  dependency <- dependency_network[[i]][['dependency_net']]  # Subconjunto de una red
  descent <- descent_network[[i]][["descent_net"]]  # Subconjunto de una red
  #marriage <- marriage_network[[i]][["marriage_net"]]  # Subconjunto de una red
  
  if (length(dependency[["val"]]) == 4 #& length(dependency[["val"]]) <= 5 
      &  # Tamaño entre 3 y 5 actores Overall, the exact likelihood calculation is only possible when dealing with directed (undirected) networks size 5 (7).
      length(dependency[["mel"]]) >= 3 #& length(dependency[["mel"]]) <= 3 & # Entre 1 y 5 vínculos (In general, directed (undirected) graphs with more than 5 (7) vertices should not be fitted using MLE)
      #&
      #length(marriage[["val"]]) == 3 &# length(marriage[["val"]]) <= 5  &
      #length(marriage[["mel"]]) >= 1 #& length(marriage[["mel"]]) <= 3 %
      &
      length(descent[["val"]]) == 4  #& length(descent[["val"]]) <= 5  #&
      #length(descent[["mel"]]) >= 2 #& length(descent[["mel"]]) <= 3
      
      #&
      #length(marriage[["val"]]) == 4  #& length(marriage[["val"]]) <= 4  #&
      
      
  ) # Entre 2 y 10 vínculos 
  
  { 
    #lists[[length(lists) + 1]] <- dependency # Agregar red que cumple las condiciones a la lista
    lists_dependency[[length(lists_dependency) + 1]] <- dependency # Agregar red que cumple las condiciones a la lista
    lists_descent[[length(lists_descent) + 1]] <- descent # Agregar red que cumple las condiciones a la lista
    #lists_marriage[[length(lists_marriage) + 1]] <- marriage # Agregar red que cumple las condiciones a la lista
    
    
  }
}
length(lists_dependency)
length(lists_descent)
#length(lists_marriage)




#for (i in seq_along(dependency_network_filtred)) {
#  dependency <- dependency_network_filtred[[i]][['dependency_net']]  # Subconjunto de una red
#  descent <- descent_network_filtred[[i]][["descent_net"]]  # Subconjunto de una red
#  #marriage <- marriage_network[[i]][["marriage_net"]]  # Subconjunto de una red
#  
#  if (length(dependency[["val"]]) == 4 & length(dependency[["val"]]) <= 5 &  # Tamaño entre 3 y 5 actores Overall, the exact likelihood calculation is only possible when dealing with directed (undirected) networks size 5 (7).
#      length(dependency[["mel"]]) >= 1 #& length(dependency[["mel"]]) <= 3 & # Entre 1 y 5 vínculos (In general, directed (undirected) graphs with more than 5 (7) vertices should not be fitted using MLE)
#      #&
#      #length(marriage[["val"]]) == 3 &# length(marriage[["val"]]) <= 5  &
#      #length(marriage[["mel"]]) >= 1 #& length(marriage[["mel"]]) <= 3 %
#      &
#      length(descent[["val"]]) == 4  #& length(descent[["val"]]) <= 4  #&
#      #length(descent[["mel"]]) >= 2 #& length(descent[["mel"]]) <= 3
#      
#      #&
#      #length(marriage[["val"]]) == 4  #& length(marriage[["val"]]) <= 4  #&
#      
#      
#  ) # Entre 2 y 10 vínculos 
#  
#  { 
#    #lists[[length(lists) + 1]] <- dependency # Agregar red que cumple las condiciones a la lista
#    lists_dependency[[length(lists_dependency) + 1]] <- dependency # Agregar red que cumple las condiciones a la lista
#    lists_descent[[length(lists_descent) + 1]] <- descent # Agregar red que cumple las condiciones a la lista
#    #lists_marriage[[length(lists_marriage) + 1]] <- marriage # Agregar red que cumple las condiciones a la lista
#    
#    
#  }
#}
#length(lists_dependency)
#length(lists_descent)




#library(knitr)
#library(rmarkdown)
#options(rmarkdown.html_vignette.check_title = FALSE)
#opts_chunk$set(message=FALSE, echo=TRUE, cache=TRUE, autodep=TRUE,
#               concordance=TRUE, error=FALSE, fig.width=7, fig.height=7)
#options(width=160)

## ----message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------
library(ergm.multi)
library(ergm)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(knitr)


#install.packages("ergm", type = "source")
#install.packages("ergm.multi", type = "source")


## Cargar el objeto dependency_network_filtred----------------------------------------------------------------------------------------------------------------
#G <- dependency_network_filtred %>% map("dependency_net")
#descent <- descent_network_filtred %>% map("descent_net")
#marriage <- marriage_network_filtred %>% map("marriage_net")

G <- lists_dependency
descent <- lists_descent 
 #marriage <- lists_marriage
#class(G)
#length(G)

#class(descent)
#length(descent)

## Convertir en tibble-----------------------------------------------------------------------------------------------------------------------------------------
#G %>% discard(`%n%`, "included") %>% map(as_tibble, unit="vertices")

G <- G %>% keep(~network.size(.) > 0)
#descent <- descent %>% keep(~network.size(.) > 0)

#descent <- lapply(descent, as.matrix) 
#class(descent)
descent <- lapply(descent, function(x) as.matrix(x))  # Convert all elements to matrices
for (i in seq_along(G)) {
  G[[i]] %n% "descent" <- descent[[i]]
}

#marriage <- lapply(marriage, function(x) as.matrix(x))  # Convert all elements to matrices
#for (i in seq_along(G)) {
#  G[[i]] %n% "marriage" <- marriage[[i]]
#}




## Filtrar redes incluidas-------------------------------------------------------------------------------------------------------------------------------------
#G <- G %>% keep(`%n%`, "included")

#G %>% 
#  map(~list(
#    sex = unique(as.character(. %v% "sex")),  # Extract unique values of "sex" from vertex attributes
#    n = network.size(.),
#    d = network.density(.)
#  )) %>% 
#  bind_rows() %>%
#  group_by(sex, n = cut(n, c(1,2,5,10))) %>%  # Adjust binning as needed
#  summarize(nnets = n(), p1 = mean(d==1), m = mean(d)) %>% 
#  kable()


## Resumen de las redes----------------------------------------------------------------------------------------------------------------------------------------
#G %>% map(~list(sex = . %n% "sex",
#                n = network.size(.),
#                d = network.density(.))) %>% bind_rows() %>%
#  group_by(sex, n = cut(n, c(1,2))) %>%
#  summarize(nnets = n(), p1 = mean(d==1), m = mean(d)) %>% kable()

## Filtrar redes de días de semana----------------------------------------------------------------------------------------------------------------------------
#G.wd <- G %>% keep(`%n%`, "weekday")
#length(G.wd)

## Extraer los roles presentes en las redes-------------------------------------------------------------------------------------------------------------------
region <- sort(unique(unlist(lapply(G, `%v%`, "region"))))



## Definir modelo ERGM ------------------------------------------------------------------------
f.wd <- Networks(G) ~
  
  # Model 1: Basic structural effects
  #N(~edges, lm = ~ I(n <= 3) + I(n >= 5)
    
  #N(~edges, lm = ~ I(n == 3) + I(n == 5)) + 
  
  # Model 2: Mutuality and isolates
  N(~edges+mutual +edgecov("descent")
    #+edgecov("marriage")#+ 
  
  # Model 3: Node attribute effect (age) applied to all layers

  #N(~F(~nodecov("edad"))#,
  #     #~nodematch("region", levels=I("13"))
  #)
  #N(~F(~
  #+odegree(2)
  #+isolates
  
  #+istar(2) # Not estimated
  #+transitive # Not estimated
#  +nodematch("sex")
#  +nodeocov("sex")
#  +nodeicov("sex")
#  #+nodeifactor("sex")
#  +absdiff("edad")
#  +nodeocov("edad")
#  +nodeicov("edad")  
#  
#  # education level 
#  + nodeicov("e6a")  # Popularidad para variables categóricas
#  + nodeocov("e6a")  # Actividad para variables categóricas
#  + absdiff('e6a')    # Homofilia para variables categóricas
  #    
  #    # indigenous background 
  #     + nodeicov("r3")  # Popularidad para variables categóricas
  #     + nodeocov("r3")  # Actividad para variables categóricas
       #+ nodematch('r3', diff=T)    # Homofilia para variables categóricas
  #    
     #+ nodeicov("edad_laboral")  # Popularidad para variables categóricas
     #+ nodeocov("edad_laboral")  # Actividad para variables categóricas
     #+ nodematch('edad_laboral', diff=T)    # Homofilia para variables categóricas

  #    # nationality
  #+ nodeifactor("r1b_pais_esp")  # Popularidad para variables categóricas
  #+ nodeofactor("r1b_pais_esp")  # Actividad para variables categóricas
  #+ nodematch('r1b_pais_esp',diff=T)    # Homofilia para variables categóricas
  #    
  #    # medical treatment (no statistics)
  #    #+ nodeicov("s28")  # Popularidad para variables categóricas
  #    #+ nodeocov("s28")  # Actividad para variables categóricas
  #    #+ nodematch('s28', diff=T)    # Homofilia para variables categóricas
  #    
  #    # medical attention (no statistics)
  #    #+ nodeifactor("s17")  # Popularidad para variables categóricas
  #    #+ nodeofactor("s17")  # Actividad para variables categóricas
  #    #+ nodematch('s17', diff=F)    # Homofilia para variables categóricas
  #    
  #    # age
  #    + nodeicov("edad")  # Popularidad para variables númericas
  #    + nodeocov("edad")  # Actividad para variables númericas
  #    + absdiff('edad')    # Homofilia para variables númericas
  #    
  #    # wages 
  #+ nodeicov("y1")  # Popularidad para variables númericas
  #+ nodeocov("y1")  # Actividad para variables númericas
  #+ absdiff('y1')    # Homofilia para variables númericas
  #    
  #    # got wages last week
  #    #+ nodeifactor("o1")  # Popularidad para variables categóricas
  #    #+ nodeofactor("o1")  # Actividad para variables categóricas
  #    #+ nodematch('o1')    # Homofilia para variables categóricas
  #    
  #    # worked last week
  #    #+ nodeifactor("y1_")  # Popularidad para variables categóricas
  #    #+ nodeofactor("y1_")  # Actividad para variables categóricas
  #    #+ nodematch('y1_')    # Homofilia para variables categóricas
  
  )
  #)
  
#class(G)
#class(descent)

#f1 <- combine_networks(list(G, descent))
#                      # blockName.vattr=".NetworkName")
#
#f1 <- combine_networks(list(dependency_network_filtred, descent_network_filtred))

library(parallel)
n_cores <- detectCores() - 1  # Use all available cores except one



## Ajustar modelo ERGM----------------------------------------------------------------------------------------------------------------------------------------
fit.wd <- ergm(f.wd, 
               #control=snctrl(seed=123)
               #,estimate = "MPLE"
               ,control = control.ergm(
                 #MCMLE.steplength = 0.3,  
                 #MCMC.burnin = 2000,  # Reduce burn-in
                 #MCMC.samplesize = 2000,  # Reduce sample size
                 #MCMC.interval = 500,  
                 #MCMC.effectiveSize = 100,  # Reduce required effective sample size
                 #SAN.maxit = 2,  # Lower stochastic approximation iterations
                 #parallel = 10  
                 parallel = n_cores,   # Enables parallel computation
                 parallel.type = "PSOCK" # Best for Mac
                 # Run in parallel (adjust based on your CPU)
               )
               
               )

## Resumen del modelo-----------------------------------------------------------------------------------------------------------------------------------------
summary(fit.wd)

## Filtrar redes sin días de semana---------------------------------------------------------------------------------------------------------------------------
#G.we <- G %>% discard(`%n%`, "weekday")
#fit.we <- ergm(Networks(G.we) ~
#                 N(~edges +
#                     mm("role", levels=I(roleset),
#                        levels2=~.%in%list(list(row="Father",col="Mother"),
#                                           list(row="Child",col="Father"),
#                                           list(row="Child",col="Mother"))) +
#                     F(~nodecov("age"), ~nodematch("role", levels=I("Child"))) +
#                     kstar(2) +
#                     triangles), control=snctrl(seed=123))
#
### Resumen del modelo ERGM------------------------------------------------------------------------------------------------------------------------------------
#summary(fit.we)

## Evaluación del ajuste del modelo---------------------------------------------------------------------------------------------------------------------------
#gof.wd <- gofN(fit.wd, GOF = ~ edges + kstar(2) + triangles)
#gof.wd <- gofN(fit.wd, GOF = ~ edges + mutual + isolates + odegree(2))
gof.wd <- gofN(fit.wd, GOF = ~ edges+edgecov("descent"))#+edgecov("marriage"))
gof.wd <- gofN(fit.wd, GOF = ~ edges)


#gof.wd <- gofN(fit.wd, control=control.gofN.ergm(nsim=400000))

  #[[1]]



library(ggrepel)
library(ggplot2)
#install.packages("ggrepel")
summary(gof.wd)

## Graficar ajuste del modelo---------------------------------------------------------------------------------------------------------------------------------
autoplot(gof.wd)
#plot(gof.wd)
## Comparación con otras métricas-----------------------------------------------------------------------------------------------------------------------------
autoplot(gof.wd, against=sqrt(.fitted))

autoplot(gof.wd, against=ordered(n))
