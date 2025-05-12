## ----Multi.ergm -----------------------------------------------------------------------------------------------------------------------



# Networks & Covariates files
load("Ergomitos/Redes/dependency_network.RData")
load("Ergomitos/Redes/descent_network.RData")

objs <-load("Ergomitos/Data/Data_Ergomitos.RData")
#print(objs)                # mira el nombre que trae el data.frame
# Reasignar
data_ergmitos <- get(objs[1])  




###### filter by network size #########

# Filtrar redes basado en tamaño (número de actores) y número de vínculos
lists_dependency <- list() # Crear una lista para guardar las redes filtradas
lists_descent <- list() # Crear una lista para guardar las redes filtradas
#lists_marriage <- list() # Crear una lista para guardar las redes filtradas


#i=1
for (i in seq_along(dependency_network)) {
  dependency <- dependency_network[[i]][['dependency_net']]  # Subconjunto de una red
  descent <- descent_network[[i]][["descent_net"]]  # Subconjunto de una red
  #marriage <- marriage_network[[i]][["marriage_net"]]  # Subconjunto de una red
  
  if (length(dependency[["val"]]) == 6 #& length(dependency[["val"]]) <= 5 
      &  # Tamaño entre 3 y 5 actores Overall, the exact likelihood calculation is only possible when dealing with directed (undirected) networks size 5 (7).
      length(dependency[["mel"]]) >= 1 #& length(dependency[["mel"]]) <= 3 & # Entre 1 y 5 vínculos (In general, directed (undirected) graphs with more than 5 (7) vertices should not be fitted using MLE)
      #&
      #length(marriage[["val"]]) == 3 &# length(marriage[["val"]]) <= 5  &
      #length(marriage[["mel"]]) >= 1 #& length(marriage[["mel"]]) <= 3 %
      &
      length(descent[["val"]]) == 6  #& length(descent[["val"]]) <= 5  #&
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
 #region <- sort(unique(unlist(lapply(G, `%v%`, "region"))))

### Variables en la data ###
# edad - edad numérico
# sexo - sexo binario levels: 1(Hombre), 2(Mujer)
# e6a  - nivel educativo: "¿Cuál es el nivel más alto alcanzado o el nivel educacional actual?", 17 labels de 1 "Nunca asistío" hasya 17 "Posgrado"
# o1   - ¿trabajó la semana pasada? binario: 1(si), 2(no)
# r1b_pais_esp - nacionalidad levels: 1(chileno), 2(extranjero), 3(no responde) 
# ecivil - estado civil levels: 1 "Casado(a)" hasta 9 "No sabe\\No responde"
# r3   - pueblo indígena levels:1-10 distintos pueblos indígenas, 11 - no pertenece a un pueblo indígena
# s28  - ha estado en tratamiento médico (12 meses) levels: 1(si), 2(no), 3(No sabe/No recuerda)
# comuna - comuna [nivel vivienda]* 
# region - región [nivel vivienda]*

# Listar atributos de nodo disponibles por red
nodo_attrs <- lapply(G, list.vertex.attributes)
nodo_attrs

## check variables values in different househoulds
 #sapply(G, function(net) table(get.vertex.attribute(net, "edad"), useNA = "always")) ## age
 #sapply(G, function(net) table(get.vertex.attribute(net, "sex"), useNA = "always")) ## sex
 #sapply(G, function(net) table(get.vertex.attribute(net, "e6a"), useNA = "always")) ## education level
 #sapply(G, function(net) table(get.vertex.attribute(net, "o1"), useNA = "always")) ## worked last week
 #sapply(G, function(net) table(get.vertex.attribute(net, "r1b_pais_esp"), useNA = "always")) ##  nationality
 #sapply(G, function(net) table(get.vertex.attribute(net, "ecivil"), useNA = "always")) ## marital status
 #sapply(G, function(net) table(get.vertex.attribute(net, "edad_legal"), useNA = "always")) ## legal age
 #sapply(G, function(net) table(get.vertex.attribute(net, "edad_dependencia_estudias"), useNA = "always")) ## economic dependency age
 #sapply(G, function(net) table(get.vertex.attribute(net, "r3"), useNA = "always")) ## indigenous background
 sapply(G2, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## medical treatment last week

# issues 
 #edad_dependencia_estudias # solo valores 6
 # r3 (valor 11?)
 # s28 (valor 11?)

 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *
 #sapply(G, function(net) table(get.vertex.attribute(net, "s28"), useNA = "always")) ## *



# ——— Asignar covariables individuales a cada red ———

library(network)
library(dplyr)

data_ergmitos$id_persona <- as.character(data_ergmitos$id_persona)

G2 <- lapply(seq_along(G), function(i) {
  net   <- G[[i]]
  raw   <- dependency_network[[i]][["dependency_net"]]
  hh_id <- dependency_network[[i]][["household_i"]]
  
  # Extrae y fija el atributo id_persona en la red
  ids <- as.character(raw$val[[1]]$vertex.names)
  net %v% "id_persona" <- ids
  
  # Data filtrada para esta vivienda
  df_sub <- data_ergmitos %>% filter(household == hh_id)
  
  # Vector final de asignación, uno por cada vértice
  covs <- setdiff(names(df_sub), c("household","id_persona"))
  for (cov in covs) {
    # Para cada vértice buscamos su fila en df_sub
    assigned <- sapply(net %v% "id_persona", function(pid) {
      row <- df_sub[df_sub$id_persona == pid, cov]
      if (length(row) == 0) return(NA)
      if (length(row) > 1) {
        warning(sprintf(
          "Household %s, cov %s: más de una fila para ID %s, usando la primera",
          hh_id, cov, pid
        ))
        row <- row[1]
      }
      return(row)
    })
    
    # Debug: longitud y tipos
    message(sprintf(
      "Household %s: asignando '%s' → length(assigned)=%d, network.size=%d, class=%s",
      hh_id, cov, length(assigned),
      network.size(net),
      paste(class(assigned), collapse="/")
    ))
    
    # Finalmente la asignación (ahora length(assigned)==network.size(net))
    net %v% cov <- assigned
  }
  
  net
})

G <- G2











## luego de calcular idx
#df_matched <- data_ergmitos[idx, , drop = FALSE]  
#
#
#
## verifica que nrow(df_matched) == network.size(net)
#stopifnot(nrow(df_matched) == network.size(G))
#
## lista de covariables a asignar
#covs <- setdiff(names(df_matched), c("household", "id_persona"))
#
#for (cov in covs) {
#  # net %v% cov <- df_matched[[cov]]
#  set.vertex.attribute(net, cov, df_matched[[cov]])
#}











## Definir modelo ERGM ------------------------------------------------------------------------
f.wd <- Networks(G) ~
  
  # Model 1: Basic structural effects
  #N(~edges, lm = ~ I(n <= 3) + I(n >= 5)
    
  #N(~edges, lm = ~ I(n == 3) + I(n == 5)) + 
  
  # Model 2: Mutuality and isolates
  N(~edges+mutual+edgecov("descent")
    #+edgecov("marriage")#+ 
  
  # Model 3: Node attribute effect (age) applied to all layers

  #N(~F(~nodecov("edad"))#,
  #     #~nodematch("region", levels=I("13"))
  #)
  #N(~F(~
#  +odegree(2)
#  #+isolates
#  
#  #+istar(2) # Not estimated
#  #+transitive # Not estimated
#  +nodematch("sex")
#  +nodeocov("sex")
#  #+nodeicov("sex")
#  + nodematch('sex', diff=F)    # Homofilia para variables categóricas
  
  #+nodeifactor("sex")
  #+absdiff("edad")
  #+nodeocov("edad")
  #+nodeicov("edad")  
#  
#  # education level 
  #+ nodeicov("e6a")  # Popularidad para variables categóricas
  #+ nodeocov("e6a")  # Actividad para variables categóricas
  #+ absdiff('e6a')    # Homofilia para variables categóricas
  #    
  #    # indigenous background 
  #     + nodeicov("r3")  # Popularidad para variables categóricas
  #     + nodeocov("r3")  # Actividad para variables categóricas
       #+ nodematch('r3', diff=T)    # Homofilia para variables categóricas
  #    
  #         + nodeicov("edad_laboral")  # Popularidad para variables categóricas
#+ nodeicov("ecivil")  # Popularidad para variables categóricas
#+ nodeocov("ecivil")  # Actividad para variables categóricas
#+ nodematch('ecivil', diff=F)    # Homofilia para variables categóricas

     + nodeifactor("edad_laboral")  # Popularidad para variables categóricas
     + nodeofactor("edad_laboral")  # Actividad para variables categóricas
     + nodematch('edad_laboral', diff=F)    # Homofilia para variables categóricas

+ nodeifactor("s28")  # Popularidad para variables categóricas
+ nodeofactor("s28")  # Actividad para variables categóricas
+ nodematch('s28', diff=F)    # Homofilia para variables categóricas

#+ nodeicov("edad_dependencia_estudios")  # Popularidad para variables categóricas
#+ nodeocov("edad_dependencia_estudios")  # Actividad para variables categóricas
#+ nodematch('edad_dependencia_estudios', diff=F)    # Homofilia para variables categóricas

#+ nodeicov("edad_legal")  # Popularidad para variables categóricas
#+ nodeocov("edad_legal")  # Actividad para variables categóricas
#+ nodematch('edad_legal', diff=F)    # Homofilia para variables categóricas


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
