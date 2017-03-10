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