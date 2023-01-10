get_node_coords <- function(dist, lat_dist, cell_width) {
  node1 <- list(dist = dist + cell_width, lat_dist = lat_dist + cell_width, dir = "s")
  node2 <- list(dist = dist + cell_width, lat_dist = lat_dist - cell_width, dir = "s")
  node3 <- list(dist = dist + cell_width, lat_dist = lat_dist, dir = "f")
  list(node1, node2, node3)
}

get_node <- function(df,  node_dist, node_lat_dist){
  map2(node_dist, node_lat_dist, ~ df %>% filter(distance == .x & lat_dist == .y))
}

validate_move <- function(dist, lat_dist, dist_0, lat_dist_0, cell_width){
  (dist - dist_0) ^ 2 + (lat_dist - lat_dist_0) ^ 2 == cell_width ^ 2
}

get_new_node_coord <- function(dist, lat_dist, cell_width){
  new_positions <- list()
  x_moves <- list(-cell_width, cell_width, 0)
  y_moves <- list(cell_width, 0)
  for(move_x in x_moves){
    for(move_y in y_moves){
      new_dist <- dist + move_y
      new_lat_dist <- lat_dist + move_x
      if(validate_move(new_dist, new_lat_dist, dist, lat_dist, cell_width)){
        new_positions[[length(new_positions) + 1]] <- list(new_dist = new_dist, new_lat_dist = new_lat_dist)
      }
    }
  }
  return(new_positions)
}