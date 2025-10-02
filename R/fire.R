# battleshipR: Battleship game in R
# Author: Mariano Corzo Toscano

#' Fire at coordinates
#'
#' Shoot at a given coordinate and update the game state.
#'
#' @param board The current game board (data frame).
#' @param row Row identifier (letter).
#' @param col Column identifier (number).
#' @param history Optional data frame with previous shots (keeps track of hits/misses).
#'
#' @return A list with the updated board, updated history, and a message.
#' @export
fire <- function(board, row, col, history = NULL) {
  if(missing(board)) stop("Board must exist.")
  idx <- which(board$Row==row & board$Col==col)
  if(length(idx)==0) stop("Coordinate outside the board.")
  
  if(is.na(board$Ship[idx])) {
    hit <- FALSE
    board$State[idx] <- "Miss"
    symbols(col-0.5, match(row, rev(unique(board$Row)))-0.5,
            squares=1, inches=FALSE, add=TRUE,
            fg="black")
  } else {
    hit <- TRUE
    board$State[idx] <- "Hit"
    rect(col-1, match(row, rev(unique(board$Row)))-1,
         col, match(row, rev(unique(board$Row))),
         col="red", border="white")
  }
  
  history <- rbind(history,
                   data.frame(Row=row, Col=col, Hit=hit))
  
  hits <- sum(history$Hit)
  misses <- nrow(history) - hits
  message("Shot at ", row, col, ": ",
          ifelse(hit, "HIT!", "Miss..."),
          " | Hits: ", hits, " | Misses: ", misses)
  
  # Check if all ships are destroyed
  if(all(board$State[!is.na(board$Ship)]=="Hit")){
    message("All ships have been destroyed! Game over.")
  }
  
  return(list(board=board, history=history))
}