#' Fuzzy Modularity of a community structure of a graph
#'
#' Function calculates the fuzzy modularity of a (disjoint or non-disjoint division) of a graph into subgraphs.
#'
#' @param graph The input graph.
#' @param membership Numeric vector or list indicating the membership structure.
#' @param abs Should fuzzy modularity be calculated based on absolute values of network edges? Default is TRUE.
#' @return A numeric scalar, the fuzzy modularity score of the given configuration.
#' @examples 
#' library(igraph)
#' g <- make_full_graph(5) %du% make_full_graph(4)
#' g <- add_edges(g, c(2,6, 2,7, 2,8, 2,9))
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
#' \deqn{Q=1/(2m) \sum_{c\epsilonC} \sum_{u,v\epsilonV} \delta_{cu} \delta_{cv} (A_{uv}-\frac{k_{u}k_{v}}{2m}}
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
#' \deqn{Q=1/(2m) \sum_{c\epsilonC} \sum_{u,v\epsilonV} \alpha_{cu} \alpha_{cv} (A_{uv}-\frac{k_{u}k_{v}}{2m}}
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
  
  require(lessR)
  
  #Transform into network if edge list from qgraph
  if(class(graph)[[1]]=='qgraph') {
    graph <- ed2ad(graph)
  }
  
  #Transform into network if edge list from igraph
  if(class(graph)[[1]]=='igraph') {
    if(length(E(g)$weight)>0) {
     graph <- as.matrix(as_adjacency_matrix(graph, attr="weight"))
    } else {
      graph <- as.matrix(as_adjacency_matrix(graph))
    }
  }
  
  #Transform all edges to absolute values if requested
  if(abs==TRUE) {
    graph <- abs(graph)
  }
  
  #Give names to columns and rows if not present
  if (is.null(colnames(graph))==TRUE) {
    colnames(graph) <- to("", ncol(graph))
    colnames(graph) <- sub("^0+", "", colnames(graph))
  }
  
  if (is.null(rownames(graph))==TRUE) {
    rownames(graph) <- to("", nrow(graph))
    rownames(graph) <- sub("^0+", "", rownames(graph))
  }
  
  #Transform membership information to list (input from CliquePercolation or EGANet)
  if (class(membership)[1]=='list'){
    membership <-  membership
  }
  
  if (class(membership)[1]=='numeric' | class(membership)[1]=='integer'){
    memberlist <- list()
    if (length(names(membership))==0) {
      names(membership) <- colnames(graph)
      
    }
    for(i in unique(membership)){
      memberlist[[i]] <- names(membership[membership==i])
    }
    membership <- memberlist
  }
  
  #Get the shared nodes
  shared <- unique(unlist(membership)[duplicated(unlist(membership))])
  
  #Get the isolated nodes
  isolatednodes <- setdiff(colnames(graph), unlist(membership))
  
  #Creates an object with the assignment of each node to one community
  if (length(membership)>0){ 
    #In case there are communities
    membership.new <- membership
    for(i in 1:length(membership.new))
      membership.new[[i]] <- rep(i, length(membership.new[[i]]))
    wc <- unlist(membership.new)
    wc <- t(as.data.frame(wc))
    colnames(wc) <- unlist(membership)
    comlab <- wc 
  } else { #In case all nodes are isolated (singleton communities)
    comlab <- as.data.frame(t(seq(from=1, to=nrow(graph), by=1)))
    colnames(comlab) <- colnames(graph)
    comlab <- as.matrix(comlab)
  } 
  
  #Creates an object with the assignment of each node to one community (transposed)
  indfac <- data.frame(matrix(vector(),length(unique(as.numeric(comlab))), ncol(graph)))
  colnames(indfac) <- colnames(graph)
  indfac[is.na(indfac)] <- 0
  for (z in 1:length(unique(as.numeric(comlab)))) {
    indfac[z,match(names(comlab[,comlab==z]),colnames(indfac[z,]))] <- 1
  }
  
  indfac <- t(indfac)
  indfac <- as.data.frame(indfac)
  rownames(indfac) <- colnames(graph)
  facnam <- vector()
  for(i in 1:length(unique(as.numeric(comlab)))) {
    facnam[i] <- paste("F",i,sep="")                   
  }
  colnames(indfac) <- facnam
  
  #Calculates sum of item weights
  itw <- vector()
  for (z in 1:ncol(graph)) {
    itw[z] <- sum(graph[,z])
  }
  itw <- t(as.data.frame(itw))
  colnames(itw) <- colnames(graph)
  itw <- as.data.frame(itw) #Vector with sum of items weights
  
  #Calculate total sum of weights
  totnet <- sum(graph)/2
  
  #Calculates the belonging coefficient
  if (length(shared)>0) {
    itf <- vector(mode = "list", length = length(shared))
    for (j in 1:length(shared)) {
      for (k in 1:length(unique(as.numeric(comlab)))) {
        if (length(intersect(membership[[k]], shared[j]))>0) {
          itf[[j]][k] <- sum(graph[shared[j],membership[[k]]])
        } else {
          itf[[j]][k] <- NA
        }
      }}
    for (k in 1:length(itf)) {
      itf[[k]][is.na(itf[[k]])] <-  0
    }
    names(itf) <- shared
    
    sumitf <- vector()
    for (k in 1:length(itf)) {
      sumitf[k] <- sum(abs(itf[[k]]))
    }
    for (k in 1:length(itf)) {
      for (j in 1:length(itf[[k]])) {
        if (sumitf[k] > 0) {
          itf[[k]][j] <- abs(itf[[k]][j])/sumitf[k]
        } else {
          itf[[k]][j] <- 0
        }
      }
    }
  } else {
    itf <- 1
  }
  
  #Replaces the belonging coefficient
  for (i in 1:length(itf)) {
    indfac[names(itf)[[i]],] <- round(itf[[i]],2)
  }
  
  #Creates a list of all possible item combinations
  parcomb <- vector("list", nrow(expand.grid(rep(list(colnames(graph)), 2))))
  for (w in 1:nrow(expand.grid(rep(list(colnames(graph)), 2)))) {
    parcomb[[w]][1] <- as.character(expand.grid(rep(list(colnames(graph)), 2))[w,1])
    parcomb[[w]][2] <- as.character(expand.grid(rep(list(colnames(graph)), 2))[w,2])
  }
  
  #Establishes the product of the belonging coefficients across factors
  for (j in 1:ncol(indfac)) {
    for (i in 1:length(parcomb)) {
      parcomb[[i]][j+2] <- as.numeric(indfac[parcomb[[i]][1],][j])*as.numeric(indfac[parcomb[[i]][2],][j])
    }}
  
  #Calculates fuzzy modularity
  mod <- vector()
  
  if (length(membership)==1 & length(isolatednodes)==0) { #If unidimensional, modularity is zero
    modf <- 0
  } else {
    for (i in 1:ncol(indfac)) {
      for (z in 1:length(parcomb)) {
        mod <- c(mod,(as.numeric((parcomb[[z]][i+2]))*((graph[parcomb[[z]][1], parcomb[[z]][2]])-((itw[,parcomb[[z]][1]]*itw[,parcomb[[z]][2]])/(2*totnet)))))
      }}
    modf <- (sum(mod))/(2*totnet)
  }
  
  return(modf)
}