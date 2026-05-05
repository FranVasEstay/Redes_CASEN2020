x <- 3202102202 #ingresamos el household_id de una vivienda en la que estamos interesados

#Funcion para buscar ese household en una lista de redes. Data es la lista de redes y x es el household
buscar <- function(x, data) {
  for (i in seq_along(data)) {
    if (data[[i]]$household_i == x) {
      return(i)
    }
  }
  return(NA)  # Retorna NA si no se encuentra
}

#Corremos la funcion. nos debería dar el número dentro de la lista de redes para poder plotearlo
buscar(x, kinship_igrpah)


#Vemos algunas redes para revisarlas aquí: 
View(filter(data_ergomitos,household==3202102202))

kinship_igrpah[[6775]]
plot(dependency_igraph[159][[1]]$dependency_net)
plot(kinship_igrpah[159][[1]]$kinship_net)

plot(descent_igrpah[159][[1]]$descent_net)
plot(descent_igrpah_no_attrs[159][[1]]$descent_net)
plot(dependency_igraph[159][[1]]$dependency_net)
plot(dependency_igraph_no_attrs[159][[1]]$dependency_net)
plot(marriage_igraph[159][[1]]$marriage_net)
plot(marriage_igraph_no_attrs[159][[1]]$marriage_net)
plot(kinship_igrpah[6775][[1]]$kinship_net)
plot(kinship_igraph_no_attrs[6775][[1]]$kinship_net)
