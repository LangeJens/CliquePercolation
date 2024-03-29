% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cpAlgorithm.R
\name{cpAlgorithm}
\alias{cpAlgorithm}
\title{Clique Percolation Community Detection}
\usage{
cpAlgorithm(W, k, method = c("unweighted", "weighted", "weighted.CFinder"), I)
}
\arguments{
\item{W}{A qgraph object or a symmetric matrix; see also \link[qgraph]{qgraph}}

\item{k}{Clique size (number of nodes that should form a clique)}

\item{method}{A string indicating the method to use 
(\code{"unweighted"}, \code{"weighted"}, or \code{"weighted.CFinder"}); see Details}

\item{I}{Intensity threshold for weighted networks}
}
\value{
A list object with the following elements:
  \describe{
  \item{list.of.communities.numbers}{list of communities with numbers as identifiers 
        of nodes}
  \item{list.of.communities.labels}{list of communities with labels from qgraph object 
        or row or column names of matrix as identifiers of nodes}
  \item{shared.nodes.numbers}{vector with all nodes that belong to multiple communities 
        with numbers as identifiers of nodes}
  \item{shared.nodes.labels}{vector with all nodes that belong to multiple communities 
        with labels from qgraph object or row or column names of matrix  as identifiers
        of nodes}
  \item{isolated.nodes.numbers}{vector with all nodes that belong to no community 
        with numbers as identifiers of nodes}
  \item{isolated.nodes.labels}{vector with all nodes that belong to no community 
        with labels from qgraph object or row or column names of matrix as identifiers
        of nodes}
  \item{k}{user-specified \code{k}}
  \item{method}{user-specified method}
  \item{I}{user-specified \code{I} (if method was \code{"weighted"} 
        or \code{"weighted.CFinder"})}
}
}
\description{
Function for clique percolation community detection algorithms for weighted
and unweighted networks.
}
\details{
\code{method = "unweighted"} conducts clique percolation for unweighted networks as
  described in Palla et al. (2005). \code{method = "weighted"} conducts clique percolation
  for weighted graphs with inclusion of cliques if their Intensity is higher than the
  specified Intensity (\code{I}), which is the method described in Farkas et al. (2007).
  \code{method = "weighted.CFinder"} conducts clique percolation as in the CFinder program.
  The Intensity (\code{I}) threshold is applied twice, namely first to the Intensity of the
  cliques (as before) and then also to their \code{k-1} overlap with other cliques
  (e.g., in the case of \code{k = 3}, it is applied to the edge that two cliques share).

  For weighted networks, the absolute value of the edge weights is taken.
  Therefore, negative edges are treated like positive edges just like in the CFinder program.
  Thus, the Intensity threshold \code{I} can only be positive.

  cpAlgorithm produces a solution for all networks, even if there are no communities 
  or communities have no overlap. The respective output is empty in such cases.
}
\examples{
## Example for unweighted networks

# create qgraph object
W <- matrix(c(0,1,1,1,0,0,0,0,
              0,0,1,1,0,0,0,0,
              0,0,0,0,0,0,0,0,
              0,0,0,0,1,1,1,0,
              0,0,0,0,0,1,1,0,
              0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,1,
              0,0,0,0,0,0,0,0), nrow = 8, ncol = 8, byrow = TRUE)
W <- Matrix::forceSymmetric(W)
W <- qgraph::qgraph(W)

# run clique percolation for unweighted networks
results <- cpAlgorithm(W = W, k = 3, method = "unweighted")

# print results overview
results

# extract more information about communities
summary(results)

## Example for weighted networks

# create qgraph object
W <- matrix(c(0,1,1,1,0,0,0,0,
              0,0,1,1,0,0,0,0,
              0,0,0,0,0,0,0,0,
              0,0,0,0,1,1,1,0,
              0,0,0,0,0,1,1,0,
              0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,1,
              0,0,0,0,0,0,0,0), nrow = 8, ncol = 8, byrow = TRUE)
set.seed(4186)
rand_w <- stats::rnorm(length(which(W == 1)), mean = 0.3, sd = 0.1)
W[which(W == 1)] <- rand_w
W <- Matrix::forceSymmetric(W)
W <- qgraph::qgraph(W)

# run clique percolation for weighted networks
results <- cpAlgorithm(W = W, k = 3, method = "weighted", I = 0.1)

# print results overview
results

# extract more information about communities
summary(results)

## Example with Obama data set (see ?Obama)

# get data
data(Obama)

# estimate network
net <- qgraph::EBICglasso(qgraph::cor_auto(Obama), n = nrow(Obama))

# run clique percolation algorithm with specific k and I
cpk3I.16 <- cpAlgorithm(net, k = 3, I = 0.16, method = "weighted")

# print results overview
cpk3I.16

# extract more information about communities
summary(cpk3I.16)

}
\references{
Farkas, I., Abel, D., Palla, G., & Vicsek, T. (2007). Weighted network modules.
\emph{New Journal of Physics, 9}, 180-180. http://doi.org/10.1088/1367-2630/9/6/180

Palla, G., Derenyi, I., Farkas, I., & Vicsek, T. (2005). Uncovering the overlapping community 
structure of complex networks in nature and society. \emph{Nature, 435}, 
814-818. http://doi.org/10.1038/nature03607
}
\author{
Jens Lange, \email{lange.jens@outlook.com}
}
