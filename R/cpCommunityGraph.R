#' Plotting Clique Percolation Community Network
#' 
#' Function for plotting a network with nodes representing communities from clique percolation
#' community detection and edges representing the number of shared nodes of the communities.
#' 
#' @param list.of.communities List object taken from results of cpAlgorithm
#'    function; see also \link{cpAlgorithm}
#' @param node.size.method String indicating how node size is plotted (\code{"proportional"}
#'    or \code{"normal"}); see Details
#' @param max.node.size Integer indicating size of the node representing the
#'    largest community, if \code{node.size.method = "proportional"}
#' @param ... any additional argument from qgraph; see also \link[qgraph]{qgraph}
#' 
#' @return
#'   The function primarily plots the community network. Additionally, it returns a list with
#'   the weights matrix (\code{community.weights.matrix}) of the community network.
#' 
#' @details
#'   The function takes the results of cpAlgorithm (see also \link{cpAlgorithm}),
#'   that is, either the \code{list.of.communities.numbers} or the
#'   \code{list.of.communities.labels} and plots the community network. Each node represents
#'   a community. Edges connecting two nodes represent the number of shared nodes between
#'   the two communities.
#'   
#'   The nodes can be plotted proportional to the sizes of the communities
#'   (\code{node.size.method = "proportional"}). The node representing the largest community
#'   is then plotted with the size specified in \code{max.node.size}. All other nodes
#'   are plotted relative to this largest node. Alternatively, all nodes can have the
#'   same size (\code{node.size.method = "normal"}).
#'   
#'   For the plotting, all isolated nodes will be ignored. If there are less than two
#'   communities in the list, plotting the network is useless. Therefore, an error
#'   is printed in this case.
#'   
#' @examples
#' ## Example with fictitious data
#' 
#' # create qgraph object
#' W <- matrix(c(0,1,1,0,0,0,0,
#'               0,0,1,0,0,0,0,
#'               0,0,0,1,1,1,0,
#'               0,0,0,0,1,1,0,
#'               0,0,0,0,0,1,0,
#'               0,0,0,0,0,0,1,
#'               0,0,0,0,0,0,0), nrow = 7, ncol = 7, byrow = TRUE)
#' W <- Matrix::forceSymmetric(W)
#' W <- qgraph::qgraph(W)
#'
#' # run clique percolation for unweighted networks
#' cp.results <- cpAlgorithm(W = W, k = 3, method = "unweighted")
#' 
#' # plot community network; proportional; maximum size is 7
#' cp.network1 <- cpCommunityGraph(cp.results$list.of.communities.numbers,
#'                                 node.size.method = "proportional",
#'                                 max.node.size = 7)
#'                                        
#' # plot community network; proportional; maximum size is 7
#' # change shape of nodes to triangle via qgraph argument
#' cp.network2 <- cpCommunityGraph(cp.results$list.of.communities.numbers,
#'                                 node.size.method = "proportional",
#'                                 max.node.size = 7,
#'                                 shape = "triangle")
#'                                 
#' ## Example with Obama data set (see ?Obama)
#' 
#' # get data
#' data(Obama)
#' 
#' # estimate network
#' net <- qgraph::EBICglasso(qgraph::cor_auto(Obama), n = nrow(Obama))
#' 
#' # run clique percolation algorithm with specific k and I
#' cpk3I.16 <- cpAlgorithm(net, k = 3, I = 0.16, method = "weighted")
#' 
#' # plot community network; normal
#' Obama.network <- cpCommunityGraph(cpk3I.16$list.of.communities.numbers,
#'                                   node.size.method = "proportional",
#'                                   theme = "colorblind")
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com}
#' 
#' @export cpCommunityGraph

cpCommunityGraph <- function(list.of.communities, node.size.method = c("proportional","normal"),
                               max.node.size = 10, ...){
  #if there fewer than two communities...
  #print that plotting is not necessary
  if (length(list.of.communities) < 2) {
    stop("Less than two communities. Thus, no network is plotted.")
  }
  ###error message if node.size.method is not "proportional" or "normal"
  if (node.size.method != "proportional" & node.size.method != "normal") {
    stop("node.size.method must be 'proportional' or 'normal'.")
  }
  
  #if there is more than one community...
  #create weights matrix for the network
  if (length(list.of.communities) > 1) {
    W <- matrix(c(0), nrow = length(list.of.communities), ncol = length(list.of.communities),
                byrow = TRUE)
    for (i in 1:(length(list.of.communities) - 1)) {
      for (j in (i + 1):length(list.of.communities)) {
        overlap <- Reduce(intersect, list(list.of.communities[[i]],list.of.communities[[j]]))
        if (length(overlap) > 0) {
          W[i,j] <- length(overlap)
          W[j,i] <- length(overlap)
        }
      }
    }
    #if node.size.method requested to be proportional...
    #adjust node size according to community; largest community has size max.node.size
    if (node.size.method == "proportional") {
      size_communities <- c()
      for (i in 1:length(list.of.communities)) {
        size_communities[i] <- length(list.of.communities[[i]])
      }
      divider <- max(size_communities) / max.node.size
      qgraph::qgraph(W, vsize = size_communities/divider, ...)
    }
    
    #if node.size.method requested to be normal...
    #just plot
    if (node.size.method == "normal") {
      qgraph::qgraph(W, ...)
    }
    
    invisible(list(community.weights.matrix = W))
  }
  
}
