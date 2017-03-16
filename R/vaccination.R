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
  intervention <- rep(x=0,network.size)
  intervention <- ifelse(intervention==0,"Untreated","Untreated")
  for (i in 1:treatments){
    index = which.max(centrality_measure(g))
    intervention[index]="Vaccinated"
    data[index,]=0
    data[,index]=0
    g <- igraph::as.undirected(igraph::graph.adjacency(data),"collapse")
  }
  
  return(list(intervention,data))
}

#' Apply Vaccination Patterns
#'
#' A function that applies different vaccination patterns to a 
#' network.
#' 
#' @param centrality_measures alist of igraph centrality measure
#' @param data An adjacency matrix, possibly directed
#' @param data.size Number of columns in the matrix
#' @param treatments Number of treatments available
#' @keywords Network, Centrality, COmparison
#' @export
#' @return List of Lists with all the vaccination vectors [[1]] and
#' the vaccinated matrices [[2]] in the order of the centrality_measures
#' list.
#' @examples 
#' mat = matrix(rbinom(10 * 5, 1, 0.3), ncol = 10, nrow = 10)
#' vaccinatedIndex <- apply_vaccination_patterns(list(igraph::betweenness,igraph::closeness),mat,10,2)
#' 
apply_vaccination_patterns <- function(centrality_measures,data,data.size,treatments){
  interventions = list()
  matrices = list()
  i <- 1
  for (measure in centrality_measures){
    results <- identify_vaccination_targets(measure,data,data.size,treatments)
    interventions[[i]] <- results[[1]]
    matrices[[i]] <- results[[2]]
    i <- i + 1
  }
  return(list(interventions,matrices))
}