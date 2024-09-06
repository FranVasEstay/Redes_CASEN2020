################################## Social Network: encuesta CASEN #############################
###############################################################################################

################# INTEGRAR NUEVAS VARIABLES EN LA DATA UTILIZADA ##############################

#### LIBRERÍAS ####
install.packages("haven")
library(data.table)
library(futile.logger)
library(haven)
library(skimr)
library(tictoc)
library(dplyr)
library(tryCatchLog)


#### CARGAR DATA ####
load(paste0(getwd(),"/Archivos r/ori_Casen2020_rdata.RData")) # Data antigua

#### ADMINISTRACIÓN DE LOS DATOS ####
### Filtrar datos utilizados ###
colnames(ori_Casen2020_STATA)
ori_Casen2020_STATA<-ori_Casen2020_STATA %>%
  select(id_vivienda, id_persona, edad, sexo,e6a,o1,r1b_pais_esp, pco1, h5, ecivil, h5_1, h5_2, r1b_pais_esp,nucleo, pco2, r3,s16,y1,y1_preg, comuna, region) %>% 
  filter(!id_vivienda %in% c(8102104907))

##¿Cuantas viviendas tienen un id de persona repetido? ¿CuáleS?
a <- paste(ori_Casen2020_STATA$id_vivienda, ori_Casen2020_STATA$id_persona)
#Cuantas
sum(duplicated(a))
#Cuales
print("id_vivienda_repetida id_persona_repetida")
print(a[which(duplicated(a) == TRUE)])

###Utiliza toda la data ###
all_set <- ori_Casen2020_STATA[ori_Casen2020_STATA$id_vivienda %in% unique(ori_Casen2020_STATA$id_vivienda), ]
set_data <- as_factor(all_set)

### Crear listas y data frame vacios para recopilar información 
measurements <- data.frame()
medidas <- data.frame()
grafos <- list()

#Error handling
options("tryCatchLog.write.error.dump.file" = TRUE)

##Original
for(i in unique(set_data$id_vivienda)) {
  tryCatchLog({
    vivienda_i <- set_data[which(set_data$id_vivienda == i), ]
    
    #Counter
    print(i)
    
    #Sex ratio
    sextab <- vivienda_i %>% count(sexo, .drop = FALSE)
    n.hombres <- sextab$n[1]
    n.mujeres <- sextab$n[2]
    porc.hombre <- (n.hombres/(n.hombres + n.mujeres))*100
    
    #Some measure of age distribution/composition
    edad.prom <- mean(vivienda_i$edad)
    edad.sd <- sd(vivienda_i$edad)
    edad.rango <- max(vivienda_i$edad) - min(vivienda_i$edad)
    
    #Indigenous peoples
    indtab <- vivienda_i %>% count(r3, .drop = FALSE)
    n.no_ind <- indtab$n[11]
    n.ind <- sum(indtab$n) - n.no_ind
    porc.ind <- (n.ind/(n.ind + n.no_ind))*100

    #geografica
    region <- vivienda_i$region
    comuna <- vivienda_i$comuna
    
    #Nivel educacional
    levels(set_data$e6a) #Son 17 levels
    edutab <- vivienda_i%>% count(e6a, .drop = FALSE)
    n.no <- edutab$n[1]
    n.sc <- edutab$n[2]
    n.jd <- edutab$n[3]
    n.kd <- edutab$n[4]
    n.es <- edutab$n[5]
    n.pr <- edutab$n[6]
    n.ba <- edutab$n[7]
    n.hu <- edutab$n[8]
    n.mc <- edutab$n[9]
    n.mn <- edutab$n[10]
    n.mt <- edutab$n[11]
    n.ti <- edutab$n[12]
    n.tc <- edutab$n[13]
    n.ui <- edutab$n[14]
    n.uc <- edutab$n[15]
    n.pi <- edutab$n[16]
    n.pc <- edutab$n[17]
    
    #Trabajo
    levels(set_data$o1) # Cambiar levels a 1 = si y 2 = no
    Wtab <- vivienda_i%>% count(o1, .drop=F)
    n.siW <- Wtab$n[1]
    n.noW <- Wtab$n[2]
    porc.empleo <- (n.siw/(n.now + n.siw))*100
    
    #Atención de salud
    levels(set_data$s16)
    saludtab <- vivienda_i%>% count(s16, .drop=F)
    n.saludsi <-saludtab$n[1]
    n.saludno <-saludtab$n[2]
    n.saludns <-saludtab$n[3]
    porc.salud <- (n.saludsi/(n.saludno+n.saludns+n.saludsi))*100
    
    #Chileno o extranjero
    levels(set_data$r1b_pais_esp) #No hay levels
    unique(set_data$r1b_pais_esp)
    nac_num <- ifelse(set_data$r1b_pais_esp == "", 1,
                                 ifelse(set_data$r1b_pais_esp == "NO RESPONDE", 3, 2))
    nactab <- vivienda_i%>%count(nac_num, .drop=F)
    n.chi <- nactab$n[nactab$nac_num == 1]
    n.ext <- nactab$n[nactab$nac_num == 2]
    n.ns  <- nactab$n[nactab$nac_num == 3]
    porc.
    
    #Salario líquido (Nivel de ingresos)
    levels(set_data$y1_preg)
    unique(set_data$y1)
    Stab <- vivienda_i%>% count(o1, .drop=F)
    n.siW <- Wtab$n[1]
    n.noW <- Wtab$n[2]
    
    # Add computed variables for each household
    table_households <-tibble(i, porc.hombre, porc.ind, edad.prom, edad.sd, region, comuna)
    measurements <- rbind(measurements, table_households)
    measurements <- distinct(measurements)
    
    #### Creating Networks ####
    
    ## Descent network##
    
    nodes_list <- tibble(vivienda_i$id_persona)
    edge_descent <- tibble(from = vivienda_i$h5_1, to = vivienda_i$id_persona)
    edge_descent <- edge_descent[which(edge_descent$from != 0 & edge_descent$from != ""),]
    edge_descent$to <- as.character(edge_descent$to)
    edge_descent$from <- as.character(edge_descent$from)
    
    #Solución para "madre|padre"
    
    if(TRUE %in% grepl("|", edge_descent$from, fixed = TRUE)){
      a <- edge_descent %>% separate(from, c('parent1', 'parent2'))
      a <- melt(a, na.rm = TRUE, id = 'to')
      a$from <- a$value
      a <- a %>% select(from, everything())
      a <- a[1:2]
      edge_descent <- a
    }
    
    nodes_list$`vivienda_i$id_persona` <- as.character(nodes_list$`vivienda_i$id_persona`)
    
    ## Marriage network##
    
    aux<-vivienda_i%>%group_by(h5)%>%
      summarise(to = id_persona[1])%>%
      right_join(vivienda_i)%>%
      filter(id_persona!= to)
    
    aux<-rbind(aux%>%select(h5,id_persona,to),
               aux%>%select(h5,id_persona=to,to=id_persona))
    
    aux$to[is.na(aux$h5)]<-NA
    
    colnames(aux)[colnames(aux) == "id_persona"] <- "from"
    aux2 <- aux[,-1]
    edge_marriage <- aux2 %>%
      filter(!is.na(to))
    
    
    ## Dependency network ##
    
    # edge_dependency <- tibble(from = vivienda_i$h5_2, to = vivienda_i$id_persona)
    #  edge_dependency <- edge_dependency[which(edge_dependency$from != 0),]
    #  edge_dependency$to <- as.character(edge_dependency$to)
    #  edge_dependency$from <- as.character(edge_dependency$from)
    #  nodes_list$`vivienda_i$id_persona` <- as.character(nodes_list$`vivienda_i$id_persona`)
    
    #### Visualition of multiple networks ####
    
    edge_descent$type    <- "descent"
    #  edge_dependency$type <- "econ_support"
    edge_marriage$type   <- "marriage"
    edge_descent$color    <- 1
    #  edge_dependency$color <- 2
    edge_marriage$color   <- 3
    
    edge_desc_depe<-rbind(edge_descent,edge_marriage)
    #links <- links[order(links$from, links$to),]
    
    names(vivienda_i)
    myvars <- c("id_persona","sexo","edad")
    covariates <- vivienda_i[myvars]
    
    #nodes <- as.data.frame(ide)
    nodes <- sort(covariates$id_persona)
    
    kinship_net    <- graph_from_data_frame(d=edge_desc_depe, vertices=nodes, directed=T) # combine matrimonial and descent networks
    # dependency_net <- graph_from_data_frame(d=edge_dependency, vertices=nodes, directed=T)
    descent_net    <- graph_from_data_frame(d=edge_descent, vertices=nodes, directed=T)
    marriage_net   <- graph_from_data_frame(d=edge_marriage, vertices=nodes, directed=T)
    
    # adding attributes to igraph objects
    covariates <- covariates[order(covariates$id_persona),]
    V(kinship_net)$sexo <- covariates$sexo
    V(kinship_net)$edad <- covariates$edad
    V(descent_net)$sexo <- covariates$sexo
    V(descent_net)$edad <- covariates$edad
    V(marriage_net)$sexo <- covariates$sexo
    V(marriage_net)$edad <- covariates$edad
    
    # Generate colors based on media type:
    covariates <- covariates[order(covariates$id_persona),]
    V(kinship_net)$sexo <- covariates$sexo
    V(kinship_net)$edad <- covariates$edad
    
    ######  Network-level measures (for undirected networks) ####
    
    kinship_unet    <- graph_from_data_frame(d=edge_desc_depe, vertices=nodes, directed=F) # combine matrimonial and descent networks
    #  dependency_unet <- graph_from_data_frame(d=edge_dependency, vertices=nodes, directed=F)
    descent_unet    <- graph_from_data_frame(d=edge_descent, vertices=nodes, directed=F)
    marriage_unet   <- graph_from_data_frame(d=edge_marriage, vertices=nodes, directed=F)
    
    # adding attributes to igraph objects
    V(kinship_unet)$sexo <- covariates$sexo
    V(kinship_unet)$edad <- covariates$edad
    #  V(dependency_unet)$sexo <- covariates$sexo
    #  V(dependency_unet)$edad <- covariates$edad
    V(descent_unet)$sexo <- covariates$sexo
    V(descent_unet)$edad <- covariates$edad
    V(marriage_unet)$sexo <- covariates$sexo
    V(marriage_unet)$edad <- covariates$edad
    
    
    # creating a combined network but without weighted ties
    edge_desc_depe1<-edge_desc_depe %>%
      count(Pair = str_c(pmin(from, to), ' - ',
                         pmax(from, to)), name = "Count")
    
    edge_desc_depe1 <- separate(edge_desc_depe1,
                                col = "Pair",
                                into = c("ego", "alter"),
                                sep = "\\s*[-]\\s*")
    edge_desc_depe1$Count<-NULL # deleting Count column
    
    kinship_unet  <- graph_from_data_frame(d=edge_desc_depe1, vertices=nodes, directed=F)
    
    #### dependency network ###
    # FIXME: solo estoy usando el output final, que al parecer es solo para kinship
    #  Size and density
    
    #  size=vcount(dependency_unet)
    #  ties=ecount(dependency_unet)
    
    #  size # network size
    #  ties # network n of ties
    
    #  edge_density(dependency_unet)
    
    #Components (networks can be composed of multiple components that are not connected to each other)
    #  components(dependency_unet)
    
    #Degree distributions
    #hist(degree(dependency_unet), breaks=10, col="gray")
    
    # compare the degree distributions of different networks (what proportion of nodes has degree = 1, degree = 2, etc)
    pk <- degree.distribution(kinship_net)
    #FIXME: plot(pk, pch=19, scale()) ### REVISAR !!
    
    # Average path length (path” is typically a shorthand for “geodesic path” or “shortest path”—the fewest number of edges that you would have to go on to get from one node to another)
    #  paths=distances(dependency_unet, algorithm="unweighted")
    #  paths
    
    # Diameter
    # option 1 (calculate the average path length while ignoring pairs of nodes that are in different components)
    #  mean_distance(dependency_unet)
    
    # option 2 (calculate the average path lengths and diameter separately for each component)
    #  comps=decompose(dependency_unet)
    #  comps
    #  path.list=lapply(comps, function(x) distances(x, algorithm="unweighted")) #make list object with two path length matrices
    #  avg.paths=sapply(path.list, mean) #average path length of each component
    #  diams=sapply(path.list, max) #diameter of each component
    #  avg.paths
    
    # transitivity (global: “ratio of triangles to connected triples”)
    #  g.cluster=transitivity(dependency_unet, "global", isolates = "NaN")
    #  g.cluster
    
    #### kinship network (matrimonial and descent networks) ###
    
    #  Size and density
    
    size=vcount(kinship_unet)
    ties=ecount(kinship_unet)
    
    size # network size
    ties # network n of ties
    
    density<-round(edge_density(kinship_unet),2)
    
    #Components (networks can be composed of multiple components that are not connected to each other)
    components<-components(kinship_unet)
    n_comp<-components$no
    
    #Degree distributions
    #hist(degree(kinship_unet), breaks=10, col="gray")
    
    # compare the degree distributions of different networks (what proportion of nodes has degree = 1, degree = 2, etc)
    #pk=degree.distribution(kinship_net)
    #plot(pk, pch=19, scale()) ### REVISAR !!
    
    # Average path length (path” is typically a shorthand for “geodesic path” or “shortest path”—the fewest number of edges that you would have to go on to get from one node to another)
    paths=distances(kinship_unet, algorithm="unweighted")
    paths
    
    # Diameter
    # option 1 (calculate the average path length while ignoring pairs of nodes that are in different components)
    diameter<-diameter(kinship_unet, unconnected = TRUE, directed = FALSE)
    
    # option 2 (calculate the average path lengths and diameter separately for each component)
    comps=igraph::decompose(kinship_unet)
    comps
    path.list=lapply(comps, function(x) distances(x, algorithm="unweighted")) #make list object with two path length matrices
    avg.paths=round(sapply(path.list, mean),2) #average path length of each component
    diams=sapply(path.list, max) #diameter of each component
    avg.paths
    
    # transitivity (global: “ratio of triangles to connected triples”)
    g.cluster=transitivity(kinship_unet, "global", isolates = "NaN")
    g.cluster
    
    
    
    #number of ties for marriage undirected
    ties_mar=ecount(marriage_unet)
    
    #number of ties for filiation undirected
    ties_fil=ecount(descent_unet)
    
    # diameter for filiation directed
    diameter_dir <-diameter(descent_net, unconnected = TRUE, directed = TRUE)
    
    #number of isolates for kinship undirected
    V(kinship_unet)$degree <- degree(kinship_unet)  # calculate degree for each node
    isolates <-as.numeric(sum(V(kinship_unet)$degree==0)) # Count nodes with degree 0
    
    table_nets <- tibble(i,size,ties,density,n_comp,diameter,g.cluster,ties_mar,ties_fil,diameter_dir,isolates) #Removí avg.paths por que es una medida por componente no por vivienda.
    #assign(paste0("dyads_",i),table_nets) # export dataset to environment
    #table_nets #FIXME: Este es solo kinship
    medidas <- rbind(medidas, table_nets)
    
    #This is an object that stores graphs per each household so they are easier to plot later based on id number
    
    grafo <- list(i = i, kinship_net = kinship_net, descent_net = descent_net, marriage_net = marriage_net, marriage_unet = marriage_unet)
    grafos[[length(grafos) + 1]] <- grafo
  })
}

names(grafos) <- c(as.character(unique(set_data$id_vivienda)))
# FIXME: I think this won't work if some households are not working: ID's might end up missassigned

comp_data <- merge(medidas, measurements, by = "i")
#colnames(comp_data)[1] <- "id_vivienda"
#View(medidas)
#View(measurements)
#View(comp_data)
comp_data

#Este es un objeto de la tabla con 10000 datos
comp_data_10000 <- comp_data
