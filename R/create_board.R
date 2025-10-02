# battleshipR: Battleship game in R
# Author: Mariano Corzo Toscano

#' Create the game board
#' 
#' @param size Size of the board (default 10, max 20).
#' @param land_pct Percentage of land cells (default 0.2, max 0.5).
#' @return A data frame with board coordinates and states.
#' @export
create_board <- function(size = 10, land_pct = 0.2) {
    # Validation
    if(size > 20) {
      size <- 20
      warning("Maximum board size is 20. Adjusted to 20.")
    }
    if(land_pct > 0.5) {
      land_pct <- 0.5
      warning("Maximum land percentage is 50%. Adjusted to 50%.")
    }
    
    n_cells <- size^2
    n_land  <- round(n_cells * land_pct)
    
    # Coordinates
    rows <- LETTERS[1:size]
    cols <- 1:size
    board <- expand.grid(Row = rows, Col = cols, stringsAsFactors = FALSE)
    board$State <- "Water"
    
    # Land areas
    if(n_land > 0) {
      set.seed(Sys.time())
      n_areas <- ifelse(n_land <= 10, 1, 
                        ifelse(n_land <= 15, 2, 3))
      
      land_cells <- c()
      remaining <- n_land
      
      for(area in 1:n_areas) {
        size_area <- ceiling(remaining / (n_areas - area + 1))
        start <- sample(1:n_cells, 1)
        group <- start
        
        while(length(group) < size_area) {
          neighbors <- unique(c(
            group - 1, group + 1, 
            group - size, group + size
          ))
          neighbors <- neighbors[neighbors > 0 & neighbors <= n_cells]
          candidates <- setdiff(neighbors, group)
          if(length(candidates) == 0) break
          group <- unique(c(group, sample(candidates, 1)))
        }
        land_cells <- c(land_cells, group[1:size_area])
        remaining <- remaining - size_area
      }
      board$State[land_cells] <- "Land"
    }
    
    # Mark neighbors to land
    land_idx <- which(board$State == "Land")
    neighbor_idx <- c()
    for(i in land_idx){
      r <- match(board$Row[i], rows)
      c <- board$Col[i]
      coords <- expand.grid(
        Row = max(1, r-1):min(size, r+1),
        Col = max(1, c-1):min(size, c+1)
      )
      idx <- which(board$Row %in% rows[coords$Row] & 
                     board$Col %in% coords$Col)
      neighbor_idx <- c(neighbor_idx, idx)
    }
    neighbor_idx <- setdiff(unique(neighbor_idx), land_idx)
    board$State[neighbor_idx[board$State[neighbor_idx] == "Water"]] <- "Neighbor"
    
    # Plot board
    plot(NULL, xlim = c(0, size), ylim = c(0, size),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", asp = 1)
    rect(rep(0:(size-1), each=size), rep((size-1):0, size),
         rep(1:size, each=size), rep((size-1):0, size) + 1,
         col = ifelse(board$State == "Land", "sienna3", "deepskyblue"),
         border = "white")
    axis(1, at = 0.5:(size-0.5), labels = cols)
    axis(2, at = 0.5:(size-0.5), labels = rev(rows), las=1)
    
    message("Board created: ", size, "x", size, " cells. ",
            n_land, " land cells in ", n_areas, " areas.")
    return(board)
}

