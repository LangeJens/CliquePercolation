#' Fuzzy Modularity of a community structure of a graph
#'
#' Function calculates the fuzzy modularity of a (disjoint or non-disjoint division) of a graph into subgraphs.
#'
#' @param graph The input graph.
#' @param membership Numeric vector or list indicating the membership structure.
#' @param abs Should fuzzy modularity be calculated based on absolute values of network edges? Default is TRUE.
#' @return A numeric scalar, the fuzzy modularity score of the given configuration.
#' @examples 
#' g <- igraph::disjoint_union(igraph::make_full_graph(5),igraph::make_full_graph(4))
#' g <- igraph::add_edges(g, c(2,6, 2,7, 2,8, 2,9))
#' wc <- list(c(1,2,3,4,5),c(2,6,7,8,9))
#' FuzzyMod(graph=g, membership=wc, abs=TRUE)
#' 
#' @references 
#' 
#' Newman, M. E., & Girvan, M. (2004). Finding and evaluating community structure in networks. 
#' \emph{Physical review E}, 69(2), 026113. 
#' 
#' Fan, Y., Li, M., Zhang, P., Wu, J., & Di, Z. (2007). Accuracy and precision of methods for community 
#' identification in weighted networks. \emph{Physica A: Statistical Mechanics and its Applications}, 377(1), 363-372. 
#' 
#' Chen, D., Shang, M., Lv, Z., & Fu, Y. (2010). Detecting overlapping communities of weighted networks 
#' via a local algorithm. \emph{Physica A: Statistical Mechanics and its Applications}, 389(19), 4177-4187. 
#' 
#' @author Pedro Henrique Ribeiro Santiago, \email{pedro.ribeirosantiago@@adelaide.edu.au} [ctb]
#' 
#' Gustavo Hermes Soares, \email{gustavo.soares@@adelaide.edu.au} [rev]
#' 
#' Adrian Quintero, \email{adrianquintero987@@hotmail.com} [rev]
#' 
#' Lisa Jamieson, \email{lisa.jamieson@@adelaide.edu.au} [rev]
#' 
#' @details The modularity of a graph with respect to some division is a measure of how good 
#' the division is. The traditional \emph{modularity} Q was proposed by Newman and Girvan (2004):
#' 
#' \deqn{Q=\frac{1}{2m} \sum_{c\epsilon_C} \sum_{u,v\epsilon_V} (A_{uv}-\frac{k_{u}k_{v}}{2m}) \delta_{cu} \delta_{cv}}
#' 
#' where m is the total number of edges, C is the set of communities corresponding to a partition,
#' V is the set of vertices (i.e. nodes) in the network, \eqn{A_{uv}} is the element of the 
#' A adjacency matrix in row \eqn{i} and column \eqn{j}, and \eqn{k_{u}} and \eqn{k_{v}} are the node 
#' degrees of nodes \eqn{u} and \eqn{v}, respectively. \eqn{\delta_{cu}} indicates whether 
#' node \eqn{u} belongs to community \eqn{c}, which equals 1 if u and v belongs to 
#' community c and 0 otherwise. The product \eqn{\delta_{cu}*\delta_{cv}} is a Kronecker delta function
#' which equals 1 if \eqn{u} and \eqn{v} belongs to community \eqn{c} and 0 otherwise.
#' 
#' In the case of \emph{weighted} networks, Fan, Li, Zhang, Wu, and Di (2007) proposed that to calculate 
#' \emph{modularity} Q, m should be the total edge weights, and \eqn{k_{u}} and \eqn{k_{v}} should be 
#' the node strengths of nodes \eqn{u} and \eqn{v}, respectively. 
#' 
#' One limitation of \emph{modularity} Q proposed by Newman and Girvan (2004) was that modularity could 
#' not be calculated for non-disjoint community partitions (i.e. networks in which a node is assigned
#' to more than one community). As such, Chen, Shang, Lv, and Fu (2010) proposed a generalisation 
#' in terms of fuzzy modularity:
#' 
#' \deqn{Q=\frac{1}{2m} \sum_{c\epsilon_C} \sum_{u,v\epsilon_V} \alpha_{cu} \alpha_{cv} (A_{uv}-\frac{k_{u}k_{v}}{2m}}
#'  
#' where \eqn{\alpha_{cu}} is the \emph{belonging coefficient}. The \emph{belonging coefficient} reflects 
#' how much the node \eqn{u} belongs to community \eqn{c}. The belonging coefficient is calculated as:
#' 
#' \deqn{\alpha_{cu} = \frac{k_{cu}}{\sum_{c\epsilonC}k_{cu}}}
#'
#' In case of a disjoint solution, the fuzzy modularity Q proposed by Chen, Shang, Lv, and Fu (2010) reduces to the 
#' modularity Q proposed by Newman and Girvan (2004).
#'
#' @export

FuzzyMod <- function(graph, membership, abs=TRUE) {
  
  #Transform into network if edge list from qgraph
  if(class(graph)[[1]]=='qgraph') {
    graph <- ed2ad(graph)
  }
  
  #Transform into network if edge list from igraph
  if(class(graph)[[1]]=='igraph') {
    if(length(igraph::E(graph)$weight)>0) {
      graph <- as.matrix(igraph::as_adjacency_matrix(graph, attr="weight"))
    } else {
      graph <- as.matrix(igraph::as_adjacency_matrix(graph))
    }
  }
  
  #Transform all edges to absolute values if requested
  if(abs==TRUE) {
    graph <- abs(graph)
  }
  
  #Give names to columns and rows if not present
  if (is.null(colnames(graph))==TRUE) {
    colnames(graph) <- lessR::to("", ncol(graph))
    colnames(graph) <- sub("^0+", "", colnames(graph))
  }
  
  if (is.null(rownames(graph))==TRUE) {
    rownames(graph) <- lessR::to("", nrow(graph))
    rownames(graph) <- sub("^0+", "", rownames(graph))
  }
  
  #Abbreviate the column and row names to match CP algorithm
  abnam <- abbreviate(colnames(graph), 3)
  colnames(graph) <- abnam
  rownames(graph) <- abnam
  
  #Creates the vector of membership
  mem <- vector()
  if (length(membership)>0 & class(membership)=="list") {
    for (i in 1:length(membership)) {
      for (j in 1:length(membership[[i]])) {
        if (i==1) {
          mem[j] <- i
        } else {
          mem[length(unlist(membership[c(1:(i-1))]))+j] <- i}}}
  } else {
    mem <- membership
    names(mem) <-  abbreviate(names(membership), 3)
  }
  
  if (class(membership)=="list") {
    names(mem) <- unlist(membership)
    orignames <- names(mem)
  }
  
  if (length(mem[!duplicated(names(mem))])<ncol(graph)) {
    init <- ifelse(length(mem)>0,max(mem)+1,1) 
    mem <- unlist(c(mem, seq(from=init, to=((ncol(graph)-length(mem[!duplicated(names(mem))]))+(init-1)), by=1)))
    names(mem) <- c(orignames, setdiff(colnames(graph), unlist(membership)))
  }
  
  #Creates the matrix of the belonging coefficients
  indfac <- t(matrix(vector(),length(unique(as.numeric(mem))), ncol(graph)))
  for (i in 1:nrow(indfac)) {
    for (j in 1:ncol(indfac)) {
      indfac[i,j] <- sum(graph[names(mem[mem==j]==TRUE),names(mem[mem==j]==TRUE), drop=FALSE]
                         [,which(!is.na(match(names(mem[mem==j]==TRUE),colnames(graph)[i])))])
    }}
  
  #This will normalize the belonging coefficients
  for (i in 1:nrow(indfac)) {
    if (sum(indfac[i,])>0) {
      indfac[i,] <- ohenery::normalize(indfac[i,])
    }}
  
  #This will create the belonging for isolated nodes
  for (i in 1:nrow(indfac)) {
    for (j in 1:ncol(indfac)) {
      if (sum(indfac[i,]>0, na.rm = TRUE)==0 & i==j) {
        indfac[i,mem[names(mem)==colnames(graph)[i]]] <- 1
      }}}
  
  indfac <- round(indfac, digits=2)
  
  #Calculates the matrix of the product of belonging coeffients
  prodmat <- indfac %*% t(indfac)
  
  #Calculates the fuzzy modularity  
  diag(graph) <- rep(0,ncol(graph)) 
  m <- sum(graph)/2 # number of links
  k <- colSums(graph) #degree
  kk <- outer(k,k,"*") 
  Pij <- kk/(2*m) # probability of link
  rawmod <- as.vector((graph-Pij)*prodmat)
  modf <- 1/(2*m)*(sum((graph-Pij)*prodmat))
  return(modf)
}