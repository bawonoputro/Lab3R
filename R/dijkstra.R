#' Finding closest distances between nodes
#'
#' @description
#' This function computes the closest distances between nodes.
#' The Dijkstra algorithm starts from the first node to the nearest node,
#' and continue to next nearest node until all nodes have been visited.
#'
#' @param graph A data.frame containing at least three columns: `v1`, `v2`,
#' and `w`, where `v1` and `v2` are node names (as characters) and `w`
#' is the edge weight.
#' @param init_node A character string determining the starting node.
#'
#' @references
#' Dijkstra algorithm on Wikipedia:
#' \url{https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm}
#'
#' @examples
#' wiki_graph <-data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#' v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#' w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#'
#' @export

dijkstra <- function(graph, init_node){

  #input validation
  stopifnot(is.data.frame(graph))
  stopifnot(all(c("v1", "v2", "w") %in% names(graph)))
  stopifnot(is.numeric(init_node), is.atomic(init_node))

  #keep only needed column to make it memory efficient
  graph <- graph[, c("v1","v2","w")]
  graph$v1 <- as.character(graph$v1)
  graph$v2 <- as.character(graph$v2)
  init_node <- as.character(init_node)

  nodes_list <- sort(unique(c(graph$v1, graph$v2)))
  stopifnot(init_node %in% nodes_list)

  #initiate the distance for each node to the initial node
  distance <- rep(Inf, length(nodes_list))
  names(distance) <- nodes_list

  #set the distance of initial node to itself
  distance[as.character(init_node)] <- 0

  #ensuring no duplicate
  graph <- aggregate(w ~ v1 + v2, graph, min)

  #initiate the list of visited and unvisited
  visited <- rep(FALSE, length(nodes_list))
  names(visited) <- nodes_list
  unvisited <- names(visited)[!visited]
  current_node <- init_node

  #start the loop
  repeat{
    #update the visited list
    visited[[current_node]] <- TRUE

    #determine edges row
    edges <- graph[graph$v1 == current_node, c("v2", "w"), drop = FALSE]

    #check distances for neighboring nodes
    if(nrow(edges) > 0){
      for(i in seq_len(nrow(edges))){
        neighbor_node <- edges$v2[i]
        total_distance <- distance[[current_node]] + edges$w[i]
        if(!visited[[neighbor_node]] && total_distance < distance[[neighbor_node]]){
          distance[[neighbor_node]] <- total_distance
        }
      }
    }

    #update unvisited list
    unvisited <- names(visited)[!visited]
    if (length(unvisited) == 0) break

    #determine next node
    next_node <- unvisited[which.min(distance[unvisited])]
    if (is.infinite(distance[[next_node]])) break
    current_node <- next_node
  }

  return(unname(distance))
}
