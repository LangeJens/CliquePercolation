#' Signed Fuzzy Modularity of a community structure of a graph
#'
#' Function calculates the fuzzy modularity of a (disjoint or non-disjoint division) of a graph into subgraphs
#' for signed weighted networks.
#'
#' @param netinput The input graph.
#' @param membassigned Numeric vector or list indicating the membership structure.
#' @return A numeric scalar, the fuzzy modularity score for signed weighted networks of the given configuration.
#' @examples
#' `%du%` <- igraph::`%du%` 
#' g <- igraph::make_full_graph(6) %du% igraph::make_full_graph(6)
#' g <- igraph::add_edges(g, c(1,7, 2,8))
#' edges <- rep(1,32)
#' edges[31] <- -1
#' igraph::E(g)$weight <- edges
#' plot(g, edge.label=round(igraph::E(g)$weight, 3))
#' wc <- list(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
#' SignedFuzzyMod(netinput=g, membassigned=wc)
#' 
#' @references 
#' 
#' Gomez, S., Jensen, P., & Arenas, A. (2009). Analysis of community structure in networks of correlated data. 
#' \emph{Physical review E}, 80(1), 016114. 
#' 
#' @author Pedro Henrique Ribeiro Santiago, \email{phrs16@@gmail.com} [ctb]
#' 
#' Gustavo Hermes Soares, [rev]
#' 
#' Adrian Quintero, [rev]
#' 
#' Lisa Jamieson, [rev] 
#' 
#' @details For \emph{signed} weighted networks (i.e. networks with positive and negative edges), the 
#' calculation of the modularity Q is problematic. Gomez, Jensen, and Arenas (2009) explain that, 
#' when calculating modularity Q for unweighted (Newman & Girvan, 2004) or weighted networks 
#' (Fan, Li, Zhang, Wu, & Di, 2007), the term \eqn{\frac{k_{u}}{2m}} indicates the probability of 
#' node \eqn{u} making connections with other nodes in the network, if connections between nodes 
#' were random. Gomez, Jensen, and Arenas (2009) discuss how, when networks are signed, the 
#' positive and negative edges cancel each other out and the term \eqn{\frac{k_{u}}{2m}} loses its 
#' probabilistic meaning. To deal with this limitation, Gomez, Jensen, and Arenas (2009) proposed modularity Q for signed 
#' weighted networks, generalised to fuzzy modularity Q for signed weighted networks:
#' 
#' \deqn{Q=(\frac{2w^{+}}{2w^{+}+2w^{-}})(\frac{1}{2m^{+}}) \sum_{c\epsilon_C} \sum_{u,v\epsilon_V} \alpha_{cu}^{+} \alpha_{cv}^{+} 
#' (A_{uv}^{+}-\frac{k_{u}^{+}k_{v}^{+}}{2m})-
#' (\frac{2w^{-}}{2w^{+}+2w^{-}})(\frac{1}{2m^{-}}) \sum_{c\epsilon_C} \sum_{u,v\epsilon_V} \alpha_{cu}^{-} \alpha_{cv}^{-} 
#' (A_{uv}^{-}-\frac{k_{u}^{-}k_{v}^{-}}{2m})}
#' 
#' where the sign + indicates positive edge weights and the sign - indicates negative edge weights, respectively.
#'
#' @seealso \code{\link{FuzzyMod}}
#'
#' @export

SignedFuzzyMod <- function(netinput, membassigned) {
  
  #Transform into network if edge list from qgraph
  if(class(netinput)[1]=='qgraph') {
    netinput <- qgraph::getWmat(netinput)
  }
  
  #Transform into network if edge list from igraph
  if(class(netinput)[[1]]=='igraph') {
    if(length(igraph::E(netinput)$weight)>0) {
      netinput <- as.matrix(igraph::as_adjacency_matrix(netinput, attr="weight"))
    } else {
      netinput <- as.matrix(igraph::as_adjacency_matrix(netinput))
    }
  }
  
  #Calculates the fuzzy modularity among the positive edges
  netpos <- matrix(0, nrow=nrow(netinput),ncol=ncol(netinput))
  for (j in 1:ncol(netinput)) {
    for (k in 1:nrow(netinput)) {
      if (netinput[j,k]>0){
        netpos[j,k] <- netinput[j,k] 
      }}}
  colnames(netpos) <- colnames(netinput)
  rownames(netpos) <- rownames(netinput)
  if (length(netpos[netpos!=0])>0) {
    modPos <- FuzzyMod(graph=netpos, membership=membassigned, abs=FALSE) 
  } else {
    modPos <- 0
  }
  totnetPos <- sum(netpos)/2
  
  #Calculates the fuzzy modularity among the negative edges
  netneg <- matrix(0, nrow=nrow(netinput),ncol=ncol(netinput))
  for (j in 1:ncol(netinput)) {
    for (k in 1:nrow(netinput)) {
      if (netinput[j,k]<0){
        netneg[j,k] <- netinput[j,k] 
      }}}
  colnames(netneg) <- colnames(netinput)
  rownames(netneg) <- rownames(netinput)
  if (length(netneg[netneg!=0])>0) {
    modNeg <- FuzzyMod(graph=netneg, membership=membassigned, abs=FALSE)
  } else {
    modNeg <- 0
  }
  totnetNeg <- sum(netneg)/2
  
  #Calculates the fuzzy modularity for signed weighted networks
  ModSigned <- (((2*totnetPos)/(2*totnetPos+2*totnetNeg))*modPos)-(((2*totnetNeg)/(2*totnetPos+2*totnetNeg))*modNeg)
  return(ModSigned)
}