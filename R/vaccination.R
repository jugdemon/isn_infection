devtools::use_package('igraph')
#' Identify Vaccination Targets
#'
#' A function that finds vaccination targets based on some network
#' centrality measure.
#' 
#' @param centrality_measure an igraph centrality measure
#' @param data An adjacency matrix, possibly directed
#' @keywords Network, Centrality
#' @export
#' @return vector stating for each node whether it is "Vaccinated" or "Untreated".
#' @examples 
#' mat = matrix(rbinom(10 * 5, 1, 0.3), ncol = 10, nrow = 10)
#' vaccinatedIndex <- identify_vaccination_targets(igraph::betweenness,mat)
#' 
identify_vaccination_targets <- function(centrality_measure,data,network.size,treatments){
  
  g <- igraph::as.undirected(igraph::graph.adjacency(data),"collapse")
  intervention <- rep(x="Untreated",network.size)
  for (i in 1:treatments){
    index = which.max(centrality_measure(g))
    intervention[index]="Vaccinated"
    data[index,]=0
    data[,index]=0
    g <- igraph::as.undirected(igraph::graph.adjacency(data),"collapse")
  }
  
  return(list(intervention,data))
}