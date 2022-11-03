#' Convert \link[qgraph]{qgraph} edge list to adjacency matrix
#'
#' Convenience function to convert \link[qgraph]{qgraph} edge list to adjacency matrix.
#'
#' @param netob A qgraph object.
#' @return A matrix. Returns the adjacency matrix.
#' @examples
#' \dontrun{ 
#' data(Obama)
#' CorMat <- qgraph::cor_auto(Obama[,1:25])
#' EBICgraph <- qgraph::qgraph(CorMat, graph = "glasso", sampleSize = nrow(bfi),
#'                 tuning = 0.5, layout = "spring", title = "BIC", details = TRUE)
#' adjmat <- ed2ad(EBICgraph)}
#' 
#' @author Pedro Henrique Ribeiro Santiago, \email{pedro.ribeirosantiago@@adelaide.edu.au} [ctb]
#' 
#' Gustavo Hermes Soares, \email{gustavo.soares@@adelaide.edu.au} [rev]
#' 
#' Adrian Quintero, \email{adrianquintero987@@hotmail.com} [rev]
#' 
#' Lisa Jamieson, \email{lisa.jamieson@@adelaide.edu.au} [rev] 

ed2ad <- function(netob) { 
  adjmat <- matrix(nrow=netob$graphAttributes$Graph$nNodes,ncol=netob$graphAttributes$Graph$nNodes) # matrix size is equivalent to number of items in model
  adjmat[is.na(adjmat)] = 0
  for (i in 1:length(netob$Edgelist$from)) {
    adjmat[netob$Edgelist$from[i], netob$Edgelist$to[i]] <- netob$Edgelist$weight[i]
    adjmat[netob$Edgelist$to[i], netob$Edgelist$from[i]] <- netob$Edgelist$weight[i]
  }
  return(adjmat)
}
