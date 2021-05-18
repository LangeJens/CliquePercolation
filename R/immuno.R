#' Data: Immunoglobulin interaction network
#' 
#' Unweighted, undirected network of interactions in the immunoglobulin
#' network. The 1,316 nodes represent amino-acids and two nodes are
#' connected by an edge if the shortest distance of their C_alpha atoms
#' is smaller than \eqn{\Theta = 8} Angstrom.
#' 
#' @docType data
#' 
#' @usage data(immuno)
#' 
#' @format An object of class \code{"qgraph"} with 1,316 nodes and
#'    6,300 edges.
#' 
#' @keywords datasets
#' 
#' @references
#' Gfeller, D. (2007). \emph{Simplifying complex networks: From a clustering
#' to a coarse graining strategy}. EPFL. http://library.epfl.ch/theses/?nr=3888
#' 
#' @source \url{https://CRAN.R-project.org/package=igraphdata}
#' 
#' @examples 
#' data(immuno)
"immuno"