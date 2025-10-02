# battleshipR: Battleship game in R
# Author: Mariano Corzo Toscano

#' Place ships on the board
#' 
#' @param board A board from create_board().
#' @param fleet A fleet from create_fleet().
#' @return Updated board with ships placed.
#' @export
place_ships <- function(board, fleet) {
  if(missing(board) | missing(fleet)) stop("Board and fleet are required.")
  
  board$Ship <- NA
  size <- length(unique(board$Col))
  rows <- unique(board$Row)
  
  for(i in order(-fleet$Size)) {
    ship_size <- fleet$Size[i]
    ship_name <- fleet$Name[i]
    placed <- FALSE
    
    while(!placed) {
      orientation <- sample(c("H","V"),1)
      if(orientation == "H") {
        row <- sample(rows,1)
        col <- sample(1:(size-ship_size+1),1)
        idx <- which(board$Row==row & board$Col %in% col:(col+ship_size-1))
      } else {
        row <- sample(1:(size-ship_size+1),1)
        col <- sample(1:size,1)
        idx <- which(board$Col==col & board$Row %in% rows[row:(row+ship_size-1)])
      }
      
      if(all(board$State[idx]=="Water") & all(is.na(board$Ship[idx]))) {
        board$Ship[idx] <- ship_name
        # Block neighbors
        for(j in idx){
          r <- match(board$Row[j], rows)
          c <- board$Col[j]
          coords <- expand.grid(
            Row = max(1, r-1):min(size, r+1),
            Col = max(1, c-1):min(size, c+1)
          )
          idxv <- which(board$Row %in% rows[coords$Row] & 
                          board$Col %in% coords$Col)
          board$State[idxv][board$State[idxv]=="Water"] <- "Neighbor"
        }
        placed <- TRUE
      }
    }
  }
  message("Ships placed successfully.")
  return(board)
}