#' Example weighted graph data frame for Dijkstra's algorithm.
#' Containing three columns: from node `v1`, to node `v2`, and weight `w`.
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{v1}{Integer. The starting node.}
#'   \item{v2}{Integer. The ending node.}
#'   \item{w}{Numeric. The weight associated with the edge.}
#' }
#'
#' @references
#' Wikipedia: Wiki Graph
#' \url{https://en.wikipedia.org/wiki/Graph_(discrete mathematics)}
#'
#' @examples
#' data(wiki_graph)
#'
#' #Example: run Dijkstra starting from node 1
#' dijkstra(wiki_graph, 1)
#'
#' #Example: run Dijkstra starting from node 3
#' dijkstra(wiki_graph, 3)
#'
#' data(wiki_graph)
"wiki_graph"
