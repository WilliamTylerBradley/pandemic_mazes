maze_to_path <- function(edges) {
  # set up id
  edges[, id := .I]
  setkey(edges, id)

  # set up nodes data set
  nodes <- unique(rbind(edges[, .(x = x1, y = y1)]
                        , edges[, .(x = x2, y = y2)]))
  nodes[, id := .I]
  setkey(nodes, id)

  # add node ids to edges data set
  edges <- merge(edges, nodes,
    by.x = c("x1", "y1"), by.y = c("x", "y"),
    suffixes = c("", "_node_1"), all.x = TRUE
  )
  edges <- merge(edges, nodes,
    by.x = c("x2", "y2"), by.y = c("x", "y"),
    suffixes = c("", "_node_2"), all.x = TRUE
  )

  # nodes to edges look up table
  nodes_edges <- unique(rbind(
    edges[, .(id = id_node_1, edge = id, connecting_node = id_node_2)],
    edges[, .(id = id_node_2, edge = id, connecting_node = id_node_1)]
  ))
  setkey(nodes_edges, id)

  # save spot for path
  path <- vector(mode = "numeric")

  # variables to keep track of progress through the mase
  last_node <- nodes[y == 0 & x == 5, id]
  current_node <- nodes[y == 0 & x == 6, id]

  # update path
  path <- append(path, current_node)
  previous_node <- current_node

  # keep going to unexplored nodes
  current_node <- nodes_edges[.(current_node), 
                              ][connecting_node != previous_node
                                , connecting_node]

  # continue through the whole path
  while (length(current_node) > 0) {
    path <- append(path, current_node)
    future_node <- nodes_edges[.(current_node), 
                               ][connecting_node != previous_node
                                 , connecting_node]
    previous_node <- current_node
    current_node <- future_node
  }

  path <- data.table(
    order = seq(1, length(path)),
    node = path
  )
  path <- merge(path, nodes, by.x = c("node"), by.y = c("id"))
}
