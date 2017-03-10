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
identify_vaccination_targets <- function(centrality_measure,data,network.size){
  g <- igraph::as.undirected(igraph::graph.adjacency(data),"collapse")
  index1 = which.max(centrality_measure(g))
  needle.sharing1 <- data
  needle.sharing1[index1,]=0
  needle.sharing1[,index1]=0
  g1 <- igraph::as.undirected(igraph::graph.adjacency(needle.sharing1),"collapse")
  
  index2 = which.max(centrality_measure(g1))
  needle.sharing2 <- needle.sharing1
  needle.sharing2[index2,]=0
  needle.sharing2[,index2]=0
  g2 <- as.undirected(graph.adjacency(needle.sharing2),"collapse")
  
  index3 = which.max(igraph::betweenness(g2))
  needle.sharing3 <- needle.sharing2
  needle.sharing3[index3,]=0
  needle.sharing3[,index3]=0
  g3 <- as.undirected(graph.adjacency(needle.sharing3),"collapse")
  
  index4 = which.max(centrality_measure(g3))
  needle.sharing4 <- needle.sharing3
  needle.sharing4[index4,]=0
  needle.sharing4[,index4]=0
  g4 <- as.undirected(graph.adjacency(needle.sharing4),"collapse")
  
  index5 = which.max(centrality_measure(g4))
  needle.sharing5 <- needle.sharing4
  needle.sharing5[index5,]=0
  needle.sharing5[,index5]=0
  g5 <- as.undirected(graph.adjacency(needle.sharing5),"collapse")
  
  intervention <- rep(x=0,network.size)
  intervention[index1]=1
  intervention[index2]=1
  intervention[index3]=1
  intervention[index4]=1
  intervention[index5]=1
  intervention <- ifelse(intervention==1,"Vaccinated","Untreated")
  
  return(list(intervention,needle.sharing5))
}