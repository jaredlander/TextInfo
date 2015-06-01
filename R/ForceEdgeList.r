#' @title plotForce
#' @description Given an edgelist plots a network using forceNetwork from networkD3
#' @details Given an edgelist plots a network using forceNetwork from networkD3
#' @author Jared P. Lander
#' @aliases plotForce
#' @export plotForce
#' @importFrom networkD3 forceNetwork
#' @param data An edgelist in data.frame form
#' @param vertices Name of edge columns
#' @return A forceNetwork object
#' @examples 
#' \dontrun{
#' plotForce(textExtract)
#' }
#' 
plotForce <- function(data, vertices=c('One', 'Two'))
{
    # split data into two dfs
    plottable <- convertToNodeLink(data=data, vertices=vertices)
    
    forceNetwork(Links=plottable$Links, Nodes=plottable$Nodes, Source='Source', Target='Target', NodeID='Index', Group='Group')
}
