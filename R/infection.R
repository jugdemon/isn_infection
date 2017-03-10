devtools::use_package('GGally')
devtools::use_package('ggplot2')
devtools::use_package('RColorBrewer')

#' Infect Network
#'
#' A function that infects a random individual of a network and
#' then spreads the infection stepwise through the network return-
#' ing a state vector of infection.
#' 
#' @param network An undirected network object
#' @param repetitions Number of steps the infection will spread
#' @param network.size Number of individuals in the network
#' @param random.seed A fixed seed for random infection.
#' @keywords Infection, Network, Random
#' @export
#' @return vector stating for each node whether it is "Healthy" or "Infected".
#' @examples 
#' n=100
#' net = = network(rgraph(n, mode = "graph", tprob = 0.5), directed = FALSE)
#' diseasedIndex <- infect_network(net,5,n)
#' 
infect_network <- function(network, repetitions, network.size,random.seed = NULL){
  if (is.null(random.seed)){
    randIndex <- sample.int(network.size,1)
  } else {
    randIndex <- random.seed
  }
  
  diseased<-c(randIndex)
  for (i in 1:repetitions){
    temp <- c()
    for (i in diseased){
      temp <- c(temp,get.neighborhood(network,i))
    }
    diseased <- unique(c(temp,diseased))
  }
  diseasedIndex <- intervention <- rep(x="Healty",network.size)
  for (i in diseased){
    diseasedIndex[i] = "Infected"
  }
  
  return(diseasedIndex)
}

#' Compare Infections
#'
#' A function that takes takes two copies of a network,
#' one vaccinated and one unvaccinated
#' 
#' @param unvac.network Network with no vaccinations
#' @param vac.network Network with vaccinations
#' @param intervention Vector of nodes representing vaccinated people
#' @param infection.steps Number of steps that the infection will spread.
#' @keywords Infection, Network, Comparison
#' @export
#' @return List with the graph representation of the unvaccinated network [[1]],
#' the vaccinated network [[2]], the infected index under no vaccination [[3]],
#' and the infected index under vaccination [[4]].
#' @examples 
#' n=100
#' mat <- rgraph(n, mode = "graph", tprob = 0.5)
#' net <- network(mat, directed = FALSE)
#' results <- identify_vaccination_targets(igraph::betweenness,mat,n,5)
#' intervention <- results[[1]]
#' vaccinatedMat <- do.call(rbind,results[2])
#' vacNet <- network(vaccinatedMat, directed = FALSE)
#' results2 <- compare_infections(net,vacNet,intervention,3)
#' 
compare_infections <- function(unvac.network,vac.network,intervention,infection.steps){
  
  infected.index <- isnInfection::infect_network(unvac.network, infection.steps, n,seed)
  unvac.network %v% "States (1)" = infected.index
  unvacNetG<-GGally::ggnet2(unvac.network,size = 0, mode = c("x", "y"),
                    color="States (1)",
                    palette = "Set2",
                    legend.size = 26)+
    ggplot2::geom_point(ggplot2::aes(color = color), size = 10, color = "black", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = color), size = 9) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=12)))
  
  
  infected.vac.index <- isnInfection::infect_network(vac.network, infection.steps, n,seed)
  infected.vac.index <- ifelse(intervention=="Vaccinated",intervention, infected.vac.index)
  unvac.network %v% "States (1)" = infected.vac.index
  vacNetG <- GGally::ggnet2(unvac.network,size = 0, mode = c("x", "y"),
                    color="States (1)",
                    palette = "Set2",
                    legend.size = 26)+
    ggplot2::geom_point(ggplot2::aes(color = color), size = 10, color = "black", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = color), size = 9) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size=12)))
  
  return(list(unvacNetG,vacNetG,infected.index,infected.vac.index))
}