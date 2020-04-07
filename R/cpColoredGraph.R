#' Colored Network According To Clique Percolation Communities
#' 
#' Function for plotting the original network with nodes colored according
#' to the community partition identified via the clique percolation community
#' detection algorithm, taking predefined sets of nodes into account.
#' 
#' @param W A qgraph object; see also \link[qgraph]{qgraph}
#' @param list.of.communities List object taken from results of cpAlgorithm
#'    function; see also \link{cpAlgorithm}
#' @param list.of.sets List object specifying predefined groups of nodes
#'    in original network; default is NULL; see Details
#' @param larger.six Integer indicating whether \code{length(list.of.communities)} is
#'    larger six (if \code{list.of.sets = NULL}) or \code{length(list.of.sets)}
#'    is larger six (if \code{list.of.sets} is not NULL); default is FALSE; see Details
#' @param h.cp Vector of integers indicating the range of hue from which
#'    colors should be drawn for elements in \code{list.of.communities} (if
#'    \code{list.of.sets = NULL}) or for elements in \code{list.of.sets} (if
#'    \code{list.of.sets} is not NULL); default is the value specified in
#'    \link[colorspace:hcl_palettes]{colorspace::qualitative_hcl()}; see Details
#' @param c.cp Integer indicating the chroma from which
#'    colors should be drawn for elements in \code{list.of.communities} (if
#'    \code{list.of.sets = NULL}) or for elements in \code{list.of.sets} (if
#'    \code{list.of.sets} is not NULL); default is 80 as specified in
#'    \link[colorspace:hcl_palettes]{colorspace::qualitative_hcl()}; see Details
#' @param l.cp Integer indicating the luminance from which
#'    colors should be drawn for elements in \code{list.of.communities} (if
#'    \code{list.of.sets = NULL}) or for elements in \code{list.of.sets} (if
#'    \code{list.of.sets} is not NULL); default is 60 as specified in
#'    \link[colorspace:hcl_palettes]{colorspace::qualitative_hcl()}; see Details
#' @param set.palettes.size Integer indicating the number of sets for which
#'    smooth gradients of a set color should be generated using
#'    \link[colorspace:hcl_palettes]{colorspace::sequential_hcl()}; default is
#'    the number of pure communities of a set plus one; see Details
#' @param own.colors Vector of hex code colors of length of \code{list.of.communities}
#'    (if \code{list.of.sets = NULL}) or \code{list.of.sets} (if \code{list.of.sets}
#'    is not NULL); if specified, colors are used for coloring the communities and no
#'    other colors are generated; if NULL (default), reasonable colors are generated; see
#'    Details
#' @param avoid.repeated.mixed.colors Logical indicating whether it should be avoided
#'    that multiple mixed communities are assigned the same color; default is FALSE;
#'    see Details
#' @param ... any additional argument from qgraph; see also \link[qgraph]{qgraph}
#' 
#' @return
#'   The function primarily plots the original network and colors the nodes
#'   according to the communities, taking predefined sets into account. 
#'   Additionally, it returns a list with the following elements:
#'   \describe{
#'   \item{basic.colors.sets}{Vector with colors assigned to the elements in
#'         \code{list.of.sets}, if \code{list.of.sets} is not NULL. Otherwise NULL is returned.}
#'   \item{colors.communities}{Vector with colors of the communities, namely assigned
#'         colors if \code{list.of.sets = NULL} or shaded and mixed colors if
#'         \code{list.of.sets} is not NULL.}
#'   \item{colors.nodes}{List with all colors assigned to each node. Isolated nodes
#'         are white. Shared nodes have a vector of colors from each community they
#'         belong to.}
#'   }
#' 
#' @details
#'   The function takes the results of cpAlgorithm (see also \link{cpAlgorithm}),
#'   that is, either the \code{list.of.communities.numbers} or the
#'   \code{list.of.communities.labels} and plots the original network, coloring
#'   the nodes according to the community partition. If there are no predefined
#'   sets of nodes (\code{list.of.sets = NULL}; the default), each community is
#'   assigned a color by using a palette generation algorithm from the package
#'   colorspace, which relies on HCL color space. Specifically, the function
#'   qualitative_hcl (see also
#'   \link[colorspace:hcl_palettes]{colorspace::qualitative_hcl()} is used,
#'   which generates a balanced set of colors over a range of hue values, holding
#'   chroma and luminance constant. This method is preferred over other palette
#'   generating algorithms in other color spaces (Zeileis et al., subm.). The
#'   default values recommended in qualitative_hcl are used, adapted to the
#'   current context in the case of hue. Yet, h.cp, c.cp, and l.cp can be used
#'   to overwrite the default values. Each node gets the color of the community
#'   it belongs to. Shared nodes are split equally in multiple colors, one for
#'   each community they belong to. Isolated nodes are colored white.
#'   
#'   If there are predefined sets of nodes, the qualitatively different colors
#'   are assigned to the sets specified in \code{list.of.sets}. Then, it is
#'   checked whether communities are pure (they contain nodes from only one set)
#'   or they are mixed (they contain nodes from multiple sets). For pure
#'   communities of each set, the assigned color is taken and faded towards white
#'   with another function from colorspace, namely sequential_hcl
#'   (see also \link[colorspace:hcl_palettes]{colorspace::sequential_hcl()}. For
#'   instance, if there are three pure communities with nodes that are only from
#'   Set 1, then the basic color assigned to Set 1 is taken, and faded toward white
#'   in 3 + 1 steps. There is one color generated more than needed (here four colors
#'   for three communities), because the last color in the fading is always white, which
#'   is reserved for isolated nodes. The three non-white colors are then assigned
#'   to each community, with stronger colors being assigned to larger communities.
#'   In that sense, all communities that entail nodes from only one specific set, will
#'   have rather similar colors (only faded towards white). All communities that
#'   entail nodes from only one specific other set, will also have similar colors,
#'   yet they will differ qualitatively from the colors of the communities that
#'   entail items from other sets. For communities that entail items from multiple
#'   sets, the basic colors assigned to these sets are mixed in proportion to the
#'   number of nodes from each set. For instance, if a community entails two nodes
#'   from Set 1 and one node from Set 2, then the colors of Sets 1 and 2 are mixed
#'   2:1.
#'   
#'   The mixing of colors is subtractive (how paint mixes). Subtractive color
#'   mixing is difficult to implement. An algorithm proposed by Scott Burns is used
#'   (see http://scottburns.us/subtractive-color-mixture/ and 
#'   http://scottburns.us/fast-rgb-to-spectrum-conversion-for-reflectances/). Each
#'   color is transformed into a corresponding reflectance curve via the RGBC
#'   algorithm. That is, optimized reflectance curves of red, green, and blue
#'   are adapted according to the RGB values of the respective color. The reflectance
#'   curves of the colors that need to be mixed are averaged via the weighted
#'   geometric mean. The resulting mixed reflectance curve is transformed back to
#'   RGB values by multiplying the curve with a derived matrix. The algorithm
#'   produces rather good color mixing and is computationally efficient. Yet,
#'   results may not always be absolutely precise.
#'   
#'   The mixing of colors for mixed communities can lead to multiple communities
#'   being assigned the same color. For instance, two communities with two nodes
#'   each from Sets 1 and 2 would have the same color, namely the colors assigned to
#'   the sets mixed in the same proportion. This is reasonable, because these
#'   communities are structurally similar. However, it can be confusing to have
#'   two actually different communities with the same color. To avoid this,
#'   set \code{avoid.repeated.mixed.colors = TRUE}. Doing so slightly alters the
#'   ratio with which the color of a mixed community is determined, if the
#'   community would have been assigned a color that was already assigned.
#'   
#'   The fading of pure communities via sequential_hcl is a function of
#'   the number of sets. If there are more pure communities from a specific
#'   set, more faded colors will be generated. This makes coloring results hard
#'   to compare across different networks, if such a comparison is desired.
#'   For instance, if one network has 12 nodes that belong to three communities
#'   sized 6, 3, and 3, all of them pure (having nodes from only one set), then
#'   their colors will be strong, average, and almost white respectively. If the
#'   same 12 nodes belong to two communities size 6 and 6, both of them pure, then
#'   their colors will be strong and average to almost white. Different numbers
#'   of pure communities therefore change the color range. To circumvent that,
#'   one can specify \code{set.palettes.size} to any number larger than the number
#'   of pure communities of a set plus one. For all sets, sequential_hcl
#'   then generates as many shades towards white of a respective color as specified
#'   in \code{set.palettes.size}. Colors for each community are then picked
#'   from the strongest towards whiter colors, with larger communities being
#'   assigned stronger colors. Note that in this situation, the range of colors
#'   is always the same for all sets in a network, making them comparable across
#'   different sets. When there are more pure communities of one set than from another
#'   their luminance will be lower. Moreover, also across networks, the luminance
#'   of different sets of nodes or of the same set can be compared.
#'   
#'   In all cases, qualitatively different colors are assigned to either the elements
#'   in \code{list.of.communities} (when \code{list.of.sets = NULL}) or the elements
#'   in \code{list.of.sets} (when \code{list.of.sets} is not NULL) with qualitative_hcl.
#'   Zeileis et al. (subm.) argue that this function can generate up to six
#'   different colors that people can still distinguish. For a larger number of
#'   qualitative colors, other packages can be used. Specifically, if the argument
#'   \code{larger.six = TRUE} (default is \code{FALSE}), the qualitatively
#'   different colors are generated via the package Polychrome (Coombes et al., 2019)
#'   with the function createPalette (see also \link[Polychrome]{createPalette}).
#'   This function generates maximally different colors in HCL space and can generate a
#'   higher number of distinct colors. With these colors, the rest of the procedure is
#'   identical. The seedcolors specified in Polychrome are general red, green, and
#'   blue. Note that the Polychrome palettes are maximally distinct, thus they are
#'   most likely not as balanced as the palettes generated with colorspace. In general,
#'   the function cpColoredGraph is recommended only for very small networks
#'   anyways, for which \code{larger.six = FALSE} makes sense. For larger networks,
#'   consider plotting the community network instead (see \link{cpCommunityGraph}).
#'   
#'   When own.colors are specified, these colors are assigned to the elements in
#'   \code{list.of.communities} (if \code{list.of.sets = NULL}) or to the elements
#'   in \code{list.of.sets} (if \code{list.of.sets} is not NULL). The rest of the 
#'   procedure is identical.
#'
#' @examples
#' #generate qgraph object with letters as labels
#' W <- matrix(c(0,0.10,0,0,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0.10,0,0,0.10,0.20,0,0,0,0,0.20,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0.10,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0.10,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0.10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0.20,0,0,0,0,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0.20,0,0,0,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0.20,0,0,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0.20,0,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0.20,0.20,0.30,0,0,0,0,0.30,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0.20,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0,0,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,0.30,0.30,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.30,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
#'               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 21, ncol = 21, byrow = TRUE) 
#' 
#' W <- Matrix::forceSymmetric(W)
#' rownames(W) <- letters[seq(from = 1, to = nrow(W))]
#' colnames(W) <- letters[seq(from = 1, to = ncol(W))]
#' 
#' W <- qgraph::qgraph(W, layout = "spring", edge.labels = TRUE)
#' 
#' #run clique percolation algorithm; three communities; two shared nodes, one isolated node
#' cp <- cpAlgorithm(W, k = 3, method = "weighted", I = 0.09)
#' 
#' #color original graph according to community partition
#' #all other arguments are defaults; qgraph arguments used to return same layout
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' #own colors (red, green, and blue) assigned to the communities
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           own.colors = c("#FF0000","#00FF00","#0000FF"),
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' #define sets of nodes; nodes a to o are in Set 1 and letters p to u in Set 2
#' list.of.sets <- list(letters[seq(from = 1, to = 15)],
#'                      letters[seq(from = 16, to = 21)])
#' 
#' #color original graph according to community partition, taking sets of nodes into account
#' #two communities are pure and therefore get shades of set color; smaller community is more white
#' #one community is mixed, so both set colors get mixed
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           list.of.sets = list.of.sets,
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' #graph as before, but specifying the set palette size to 6
#' #from a range of 6 colors, the pure communities get the darker ones
#' #in a different network with also two pure communities, luminance would therefore be equal
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           list.of.sets = list.of.sets, set.palettes.size = 6,
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' #graph as before, but colors sampled only form yellow to blue range, less chroma, more luminance
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           list.of.sets = list.of.sets, set.palettes.size = 6,
#'                           h.cp = c(50, 210), c.cp = 70, l.cp = 70,
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' #own colors (red and green) assigned to the sets
#' #two communities in shades of red and one community is mix of green and red (brown)
#' \donttest{
#' results <- cpColoredGraph(W, list.of.communities = cp$list.of.communities.labels,
#'                           list.of.sets = list.of.sets,
#'                           own.colors = c("#FF0000","#00FF00"),
#'                           layout = "spring", edge.labels = TRUE)
#' }
#' 
#' @references
#' Coombes, K. R., Brock, G., Abrams, Z. B., & Abruzzo, L. V. (2019). Polychrome:
#' Creating and assessing qualitative palettes with many colors.
#' \emph{Journal of Statistical Software, 90}, 1-26. https://doi.org/10.18637/jss.v090.c01
#' 
#' Zeileis, A., Fisher, J. C., Hornik, K., Ihaka, R., McWhite, C. D., Murrell, P.,
#' Stauffer, R., & Wilke, C. O. (subm.). \emph{colorspace: A toolbox for manipulating
#' and assessing colors and palettes}. https://arxiv.org/abs/1903.06490
#' 
#' @author Jens Lange, \email{lange.jens@@outlook.com}  
#' 
#' @export cpColoredGraph

cpColoredGraph <- function(W, list.of.communities, list.of.sets = NULL, larger.six = FALSE,
                           h.cp = c(0, 360 * (length(cplist) - 1)/length(cplist)),
                           c.cp = 80, l.cp = 60,
                           set.palettes.size = NULL,
                           own.colors = NULL,
                           avoid.repeated.mixed.colors = FALSE,
                           ...){
  
  #error, if list.of.sets does not entail as many nodes as the network (if list.of.sets is specified)
  if (!is.null(list.of.sets)) {
    if (length(unlist(list.of.sets)) != nrow(qgraph::getWmat(W))) {
      stop("There are not as many nodes in list.of.sets as there are nodes in W.
           Check for errors.")
    }
  }
  
  #error, if not all elements of list.of.communities are also in list.of.sets
  #necessary only if list.of.sets is specified
  if (!is.null(list.of.sets)) {
    if (!(all(unique(unlist(list.of.communities)) %in% unique(unlist(list.of.sets))))) {
      stop("Not all nodes in list.of.communities are also in list.of.sets.
           Check whether they have the same format (both are labels or both are numbers) and
           whether there are no spelling mistakes.")
    }
  }
  
  #warning, if list.of.communities is larger six but larger.six = FALSE (when list.of.sets = NULL)
  if (is.null(list.of.sets)) {
    if (length(list.of.communities) > 6 & larger.six == FALSE) {
      warning("There are more than 6 communities. The colors might be hard to distinguish.
              Set larger.six = TRUE to get maximally different colors.
              Yet, colors might be visually a little less pleasing.")
    }
  }
  #warning, if list.of.sets is larger six but larger.six = FALSE (when list.of.sets is specified)
  if (!is.null(list.of.sets)) {
    if (length(list.of.sets) > 6  & larger.six == FALSE) {
      warning("There are more than 6 sets. The colors might be hard to distinguish.
              Set larger.six = TRUE to get maximally different colors.
              Yet, colors might be visually a little less pleasing.")
    }
  }
  
  #subtractive color mixing
  #RGBC method to generate reflectance curves
  #see http://scottburns.us/fast-rgb-to-spectrum-conversion-for-reflectances/
  #reflectance curves are then averaged via the weighted geometric mean and transformed to RGB
  #see http://scottburns.us/subtractive-color-mixture/
  subColorMix <- function(colors, ratio){
    
    #optimized reflectance curves of red (rho_r), green (rho_g), and blue (rho_b)
    #taken from http://scottburns.us/wp-content/uploads/2018/09/RGB-components-comma-separated.txt)
    rho_r <- c(0.021592459, 0.020293111, 0.021807906, 0.023803297, 0.025208132, 0.025414957,
               0.024621282, 0.020973705, 0.015752802, 0.01116804, 0.008578277, 0.006581877,
               0.005171723, 0.004545205, 0.00414512, 0.004343112, 0.005238155, 0.007251939,
               0.012543656, 0.028067132, 0.091342277, 0.484081092, 0.870378324, 0.939513128,
               0.960926994, 0.968623763, 0.971263883, 0.972285819, 0.971898742, 0.972691859,
               0.971734812, 0.97234454, 0.97150339, 0.970857997, 0.970553866, 0.969671404)
    rho_g <- c(0.010542406, 0.010878976, 0.011063512, 0.010736566, 0.011681813, 0.012434719,
               0.014986907, 0.020100392, 0.030356263, 0.063388962, 0.173423837, 0.568321142,
               0.827791998, 0.916560468, 0.952002841, 0.964096452, 0.970590861, 0.972502542,
               0.969148203, 0.955344651, 0.892637233, 0.5003641, 0.116236717, 0.047951391,
               0.027873526, 0.020057963, 0.017382174, 0.015429109, 0.01543808, 0.014546826,
               0.015197773, 0.014285896, 0.015069123, 0.015506263, 0.015545797, 0.016302839)
    rho_b <- c(0.967865135, 0.968827912, 0.967128582, 0.965460137, 0.963110055, 0.962150324,
               0.960391811, 0.958925903, 0.953890935, 0.925442998, 0.817997886, 0.42509696,
               0.167036273, 0.078894327, 0.043852038, 0.031560435, 0.024170984, 0.020245519,
               0.01830814, 0.016588218, 0.01602049, 0.015554808, 0.013384959, 0.012535491,
               0.011199484, 0.011318274, 0.011353953, 0.012285073, 0.012663188, 0.012761325,
               0.013067426, 0.013369566, 0.013427487, 0.01363574, 0.013893597, 0.014025757)
    
    #T matrix needed to convert reflectance curves back to RGB
    #taken from http://scottburns.us/wp-content/uploads/2015/03/matrix_T_tab_delimited.txt
    T_matrix <- matrix(c(5.47813e-05,0.000184722,0.000935514,
                         0.003096265,0.009507714,0.017351596,
                         0.022073595,0.016353161,0.002002407,
                         -0.016177731,-0.033929391,-0.046158952,
                         -0.06381706,-0.083911194,-0.091832385,
                         -0.08258148,-0.052950086,-0.012727224,
                         0.037413037,0.091701812,0.147964686,
                         0.181542886,0.210684154,0.210058081,
                         0.181312094,0.132064724,0.093723787,
                         0.057159281,0.033469657,0.018235464,
                         0.009298756,0.004023687,0.002068643,
                         0.00109484,0.000454231,0.000255925,
                         -4.65552e-05,-0.000157894,-0.000806935,
                         -0.002707449,-0.008477628,-0.016058258,
                         -0.02200529,-0.020027434,-0.011137726,
                         0.003784809,0.022138944,0.038965605,
                         0.063361718,0.095981626,0.126280277,
                         0.148575844,0.149044804,0.14239936,
                         0.122084916,0.09544734,0.067421931,
                         0.035691251,0.01313278,-0.002384996,
                         -0.009409573,-0.009888983,-0.008379513,
                         -0.005606153,-0.003444663,-0.001921041,
                         -0.000995333,-0.000435322,-0.000224537,
                         -0.000118838,-4.93038e-05,-2.77789e-05,
                         0.00032594,0.001107914,0.005677477,
                         0.01918448,0.060978641,0.121348231,
                         0.184875618,0.208804428,0.197318551,
                         0.147233899,0.091819086,0.046485543,
                         0.022982618,0.00665036,-0.005816014,
                         -0.012450334,-0.015524259,-0.016712927,
                         -0.01570093,-0.013647887,-0.011317812,
                         -0.008077223,-0.005863171,-0.003943485,
                         -0.002490472,-0.001440876,-0.000852895,
                         -0.000458929,-0.000248389,-0.000129773,
                         -6.41985e-05,-2.71982e-05,-1.38913e-05,
                         -7.35203e-06,-3.05024e-06,-1.71858e-06),
                       nrow = 3, ncol = 36, byrow = TRUE)
    
    #convert hex colors to sRGB and then to RGB (save latter as matrix)
    colors_srgb <- colorspace::hex2RGB(colors)
    colors_rgb <- colorspace::coords(methods::as(colors_srgb, "RGB"))
    
    #determine reflectance curves of all colors and save them in list
    ref_curves <- list()
    for (t in 1:length(colors)) {
      ref_curves[[t]] <- colors_rgb[t, 1]*rho_r + colors_rgb[t, 2]*rho_g + colors_rgb[t, 3]*rho_b
    }
    
    #if one reflectance curve has only zeros (pure black), change their values to 0.02
    #otherwise, this would not be appropriate for the weighted geometric mean
    for (u in 1:length(ref_curves)) {
      if (sum(ref_curves[[u]]) == 0) {
        ref_curves[[u]] <- c(rep(0.02, length(ref_curves[[u]])))
      }
    }
    
    #determine weighted geometric mean of the reflectance curves
    #first, each curve is raised to its value in ratio object
    #second, curves are multiplied
    ref_curves_rtp <- ref_curves
    for (v in 1:length(colors)) {
      ref_curves_rtp[[v]] <- ref_curves_rtp[[v]]^ratio[v]
    }
    mixed_ref_curve <- ref_curves_rtp[[1]]
    for (w in 2:length(ref_curves_rtp)) {
      mixed_ref_curve <- mixed_ref_curve * ref_curves_rtp[[w]]
    }
    
    #determine RGB values by matrix multiplication
    mixed_color_rgb <- T_matrix %*% mixed_ref_curve
    #if values are out of bound, round them to closest value
    #this is not mentioned by Scoot Burns, but otherwise no color is produced
    for (x in 1:nrow(mixed_color_rgb)) {
      if (mixed_color_rgb[x, 1] < 0) {mixed_color_rgb[x, 1] <- 0}
      if (mixed_color_rgb[x, 1] > 1) {mixed_color_rgb[x, 1] <- 1}
    }
    #convert to RGB
    mixed_color_rgb <- colorspace::RGB(R = mixed_color_rgb[1,1],
                                       G = mixed_color_rgb[2,1],
                                       B = mixed_color_rgb[3,1])
    
    return(colorspace::hex(mixed_color_rgb))
  }
  
  #create list of nodes, indicating in which communities each node is
  node_communities <- list()
  for (i in 1:nrow(qgraph::getWmat(W))) {
    communities <- vector()
    for (j in 1:length(list.of.communities)) {
      if (i %in% list.of.communities[[j]] == TRUE | 
          rownames(qgraph::getWmat(W))[i] %in% list.of.communities[[j]] == TRUE) {
        communities <- c(communities,j)
      }
    }
    node_communities[[i]] <- communities
  }
  
  #create list of nodes, indicating how much the nodes need to be split
  #depends on the number of communities they belong to
  #if node is only in one community, split is 1
  #if node is in two communities, split is 0.5 0.5 asf.
  node_split <- list()
  for (i in 1:length(node_communities)) {
    if (length(node_communities[[i]]) > 0) {
      node_split[[i]] <- c(rep(1/length(node_communities[[i]]),length(node_communities[[i]])))
    } else {node_split[[i]] <- 1}
  }
  
  #assigning colors to communities
  #if no list.of.sets is specified and no own.colors are defined...
  #communities get qualitatively different colors as by polychrome or colorspace
  if (is.null(list.of.sets) & is.null(own.colors)) {
    cplist <- list.of.communities
    if (larger.six == TRUE) {
      set.seed(4186)
      colors_communities <- as.vector(Polychrome::createPalette(length(list.of.communities),
                                                                c("#CC0000","#00CC00","#0000CC"),
                                                                target = "normal"))
    }
    if (larger.six == FALSE) {
      colors_communities <- colorspace::qualitative_hcl(length(list.of.communities),
                                                        h = h.cp, c = c.cp, l = l.cp)
    }
    colors_sets <- NULL
  }
  #if list.of.sets is specified...
  #elements in list.of.sets get qualitatively different colors by polychrome or colorspace
  if (!is.null(list.of.sets)) {
    cplist <- list.of.sets
    #if no own.colors are defined...
    if (larger.six == TRUE & is.null(own.colors)) {
      set.seed(4186)
      colors_sets <- as.vector(Polychrome::createPalette(length(list.of.sets),
                                                         c("#CC0000","#00CC00","#0000CC"),
                                                         target = "normal"))
    }
    if (larger.six == FALSE & is.null(own.colors)) {
      colors_sets <- colorspace::qualitative_hcl(length(list.of.sets),
                                                 h = h.cp, c = c.cp, l = l.cp)
    }
    #if own.colors are defined...
    if (!is.null(own.colors)) {
      #error, if there are not as many colors as there are sets
      if (length(own.colors) > length(list.of.sets)) {
        stop("There are more colors than sets. Specify less colors in own.colors.")
      }
      if (length(own.colors) < length(list.of.sets)) {
        stop("There are less colors than sets. Specify more colors in own.colors.")
      }
      colors_sets <- own.colors
    }
    #create vector with slots for each community that needs to be colored
    colors_communities <- c(rep(NA, length(list.of.communities)))
    #determine to which set each node in a community belongs
    community_sets <- list.of.communities
    for (i in 1:length(list.of.communities)) {
      for (j in 1:length(list.of.communities[[i]])) {
        for (k in 1:length(list.of.sets)) {
          if (list.of.communities[[i]][j] %in% list.of.sets[[k]] == TRUE) {
            community_sets[[i]][j] <- k
          }
        }
      }
    }
    #community entails nodes from only one set -> colored proportionally to its size
    #for this, first create palettes of colors, shaded towards white, for each set
    #this is necessary only if there are pure community with nodes from only this set
    #the number of shades is determined based on set.palettes.size
    #per default, refers to the number of pure communities with a respective set that need color + 1
    #this (count_set + 1) ensures non-white pure communities
    #if set.palettes.site is set higher, colors get shaded toward white accordingly 
    set_palettes <- list()
    for (l in 1:(length(list.of.sets))) {
      count_set <- 0
      for (m in 1:length(community_sets)) {
        if (all(c(rep(l,length(community_sets[[m]]))) == community_sets[[m]])) {
          count_set <- count_set + 1
        }
      }
      if (count_set > 0) {
        if (is.null(set.palettes.size)) {
          set.palettes.size.loop <- count_set + 1
        } else {set.palettes.size.loop <- set.palettes.size}
        basic_color <- methods::as(colorspace::hex2RGB(colors_sets[l]), "polarLUV")
        set_palettes[[l]] <- colorspace::sequential_hcl(set.palettes.size.loop,
                                                        h = colorspace::coords(basic_color)[3],
                                                        c = colorspace::coords(basic_color)[2],
                                                        l = c(colorspace::coords(basic_color)[1],
                                                              100))
        #error, if set.palettes.size(.loop) is smaller than count_set + 1
        #this would lead to less colors than required to color all pure communities
        if (set.palettes.size.loop < (count_set + 1)) {
          stop("There are more pure communities of one set than specified in set.palettes.size.
               Thus, there are less colors produced than required. Set set.palettes.size higher.")
        }
      } else {set_palettes[[l]] <- vector()}
    }
    #identify all pure and mixed communities
    pure_communities <- sapply(community_sets, unique)
    pure_communities <- sapply(pure_communities, length)
    pure_communities <- which(pure_communities == 1)
    if (length(pure_communities) == 0) {
      mixed_communities <- c(1:length(community_sets))
    } else {mixed_communities <- c(1:length(community_sets))[-pure_communities]}
    #to each pure community, assign one color from the respective palette
    #stronger colors are assigned to larger communities
    for (n in 1:length(list.of.sets)) {
      selector <- which(sapply(community_sets[pure_communities], unique) == n)
      selected_communities <- pure_communities[selector]
      size_order <- order(sapply(community_sets[selected_communities], length), decreasing = TRUE)
      selected_colors <- set_palettes[[n]][1:length(selected_communities)]
      colors_communities[selected_communities] <- selected_colors[size_order]
    }
    #community entails nodes from multiple sets -> set colors mixed according to ratio of set nodes
    for (o in mixed_communities) {
      set_numbers <- as.numeric(unique(community_sets[[o]]))
      basic_colors <- colors_sets[set_numbers]
      ratio_mix <- c()
      for (p in set_numbers) {
        ratio_mix <- c(ratio_mix, length(which(community_sets[[o]] == p)))
      }
      ratio_mix <- ratio_mix/sum(ratio_mix)
      add_color <- subColorMix(basic_colors, ratio = ratio_mix)
      #if avoid.repeated.mixed.colors = TRUE...
      #mixed communities with the same color need to be disentangled
      #minor random alteration of ratio in which colors are mixed
      #loop continues until color that will be added is not yet part of the vector of colors
      if (avoid.repeated.mixed.colors == TRUE) {
        set.seed(4186)
        while (add_color %in% colors_communities) {
          for (q in 1:length(ratio_mix)) {
            rand_change <- round(stats::runif(1, min = -0.05, max = 0.05), 3)
            ratio_mix[q] <- ratio_mix[q] + rand_change
            if (ratio_mix[q] < 0) {
              ratio_mix[q] <- 0.001
            }
          }
          ratio_mix <- ratio_mix/sum(ratio_mix)
          add_color <- subColorMix(basic_colors, ratio = ratio_mix)
        }
      }
      colors_communities[o] <- add_color
    }
  }
  #if own.colors are defined but no list.of.sets...
  if (!is.null(own.colors) & is.null(list.of.sets)) {
    #error, if there are not as many colors as there are communities
    if (length(own.colors) > length(list.of.communities)) {
      stop("There are more colors than communities. Specify less colors in own.colors.")
    }
    if (length(own.colors) < length(list.of.communities)) {
      stop("There are less colors than communities. Specify more colors in own.colors.")
    }
    colors_communities <- own.colors
    colors_sets <- NULL
  }
  
  #assigning a color to each node
  colors_nodes <- list()
  #each node gets vector of colors depending on the respective communities they are in
  #isolated nodes get white
  for (i in 1:length(node_communities)) {
    if (length(node_communities[[i]]) > 0) {
      colors_nodes[[i]] <- colors_communities[node_communities[[i]]]
    } else {colors_nodes[[i]] <- "#ffffff"}
  }
  
  #plot pie chart network with assigned colors
  #additional qgraph parameters can be specified
  qgraph::qgraph(qgraph::getWmat(W), pie = node_split, pieColor = colors_nodes, pieBorder = 1, ...)
  
  #return basic colors assigned to sets (if there were sets), community colors, and node colors
  return(list(basic.colors.sets = colors_sets,
              colors.communities = colors_communities,
              colors.nodes = colors_nodes))
  
}
