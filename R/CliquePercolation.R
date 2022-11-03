#' @details
#' CliquePercolation includes a number of functions for detecting and interpreting
#' overlapping network communities. It is based on research by Palla et al. (2005)
#' and Farkas et al. (2007).
#' 
#' \describe{
#' \item{cpAlgorithm()}{conducts clique percolation for unweighted and weighted 
#'         networks; see \link{cpAlgorithm}}
#' \item{cpColoredGraph()}{plots the original network with nodes colored according
#'         to community partition, taking sets of nodes into account; see
#'         \link{cpColoredGraph}}
#' \item{cpCommunityGraph()}{plots the network of communities; see \link{cpCommunityGraph}} 
#' \item{cpCommunitySizeDistribution()}{plots the frequency distribution of the
#'         sizes of the communities; see \link{cpCommunitySizeDistribution}}
#' \item{cpThreshold()}{optimizing \code{k} and \code{I} via threshold-based selection;
#'         see \link{cpThreshold}}
#' \item{cpPermuteEntropy()}{determines confidence intervals for the entropy threshold
#'         based on random permutations of the network; see \link{cpPermuteEntropy}}
#' \item{FuzzyMod()}{calculates the fuzzy modularity of a (disjoint or non-disjoint
#'         division) of a graph into subgraphs; see \link{FuzzyMod}}
#' \item{SignedFuzzyMod()}{calculates the fuzzy modularity of a (disjoint or non-disjoint
#'         division) of a graph into subgraphs for signed weighted networks;
#'         see \link{SignedFuzzyMod}}
#' }
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com} 
#' 
#' @keywords internal
#' 
#' @references
#' Farkas, I., Abel, D., Palla, G., & Vicsek, T. (2007). Weighted network modules.
#' \emph{New Journal of Physics, 9}, 180-180. http://doi.org/10.1088/1367-2630/9/6/180
#' 
#' Palla, G., Derenyi, I., Farkas, I., & Vicsek, T. (2005). Uncovering the overlapping community 
#' structure of complex networks in nature and society. \emph{Nature, 435}, 
#' 814-818. http://doi.org/10.1038/nature03607
"_PACKAGE"
#> [1] "_PACKAGE"