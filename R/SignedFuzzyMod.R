#' Signed Fuzzy Modularity of a community structure of a graph
#'
#' Function calculates the fuzzy modularity of a (disjoint or non-disjoint division) of a graph into subgraphs
#' for signed weighted networks.
#'
#' @param netinput The input graph.
#' @param membassigned Numeric vector or list indicating the membership structure.
#' @return A numeric scalar, the fuzzy modularity score for signed weighted networks of the given configuration.
#' @examples 
#' library(igraph)
#' g <- make_full_graph(6) %du% make_full_graph(6)
#' g <- add_edges(g, c(1,7, 2,8))
#' edges <- rep(1,32)
#' edges[31] <- -1
#' E(g)$weight <- edges
#' plot(g, edge.label=round(E(g)$weight, 3))
#' wc <- list(c(1,2,3,4,5,6),c(7,8,9,10,11,12))
#' SignedFuzzyMod(netinput=g, membassigned=wc)
#' 
#' @references 
#' 
#' Gomez, S., Jensen, P., & Arenas, A. (2009). Analysis of community structure in networks of correlated data. 
#' \emph{Physical review E}, 80(1), 016114. 
#' 
#' @author Pedro Henrique Ribeiro Santiago, \email{pedro.ribeirosantiago@@adelaide.edu.au} [ctb]
#' 
#' Gustavo Hermes Soares, \email{gustavo.soares@@adelaide.edu.au} [rev]
#' 
#' Adrian Quintero, \email{adrianquintero987@@hotmail.com} [rev]
#' 
#' Lisa Jamieson, \email{lisa.jamieson@@adelaide.edu.au} [rev] 
#' 
#' @details For \emph{signed} weighted networks (i.e. networks with positive and negative edges), the 
#' calculation of the modularity Q is problematic. In unweighted networks, the original formulation 
#' of modularity Q by Newman and Girvan (2004) included the term \eqn{frac{k_{u}}{2m}}, which
#' is the division of node \eqn{u} degree by two times the total number of edges, indicating the probability
#' of node \eqn{u} making connections with other nodes in the network. In weighted networks, 
#' Fan, Li, Zhang, Wu, and Di (2007) proposed that the term \eqn{frac{k_{u}}{2m}} should be 
#' calculated instead as the division of node \eqn{u} strength by two times the total edge weights. However, 
#' when weighted networks are signed, positive and negative edges cancel each other out and the term 
#' \eqn{frac{k_{u}}{2m}} loses its probabilistic meaning.
#' 
#' To deal with this limitation, Gomez, Jensen, and Arenas (2009) proposed modularity Q for signed weighted networks,
#' generalised to fuzzy modularity Q for signed weighted networks:
#' 
#' \deqn{Q=(\frac{2w^{+}}{2w^{+}+2w^{-}})(1/(2m^{+}) \sum_{c\epsilonC} \sum_{u,v\epsilonV} \alpha_{cu}^{+} \alpha_{cv}^{+} 
#' (A_{uv}^{+}-\frac{k_{u}^{+}k_{v}^{+}}{2m})-
#' (\frac{2w^{-}}{2w^{+}+2w^{-}})(1/(2m^{-}) \sum_{c\epsilonC} \sum_{u,v\epsilonV} \alpha_{cu}^{-} \alpha_{cv}^{-} 
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
    netinput <- ed2ad(netinput)
  }
  
  #Transform into network if edge list from igraph
  if(class(netinput)[[1]]=='igraph') {
    if(length(E(g)$weight)>0) {
      netinput <- as.matrix(as_adjacency_matrix(netinput, attr="weight"))
    } else {
      netinput <- as.matrix(as_adjacency_matrix(netinput))
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
  if (any(netpos[netpos!=0])) {
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
  if (any(netneg[netneg!=0])) {
    modNeg <- FuzzyMod(graph=netneg, membership=membassigned, abs=FALSE)
  } else {
    modNeg <- 0
  }
  totnetNeg <- sum(netneg)/2
  
  #Calculates the fuzzy modularity for signed weighted networks
  ModSigned <- (((2*totnetPos)/(2*totnetPos+2*totnetNeg))*modPos)-(((2*totnetNeg)/(2*totnetPos+2*totnetNeg))*modNeg)
  return(ModSigned)
}