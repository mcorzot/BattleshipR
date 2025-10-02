# battleshipR: Battleship game in R
# Author: Mariano Corzo Toscano

#' Create the fleet
#' 
#' @param board A board created with create_board().
#' @param carrier Number of carriers (size 5).
#' @param destroyer Number of destroyers (size 4).
#' @param frigate Number of frigates (size 3).
#' @param patrol Number of patrol boats (size 2).
#' @return A data frame describing the fleet.
#' @export
create_fleet <- function(board,
                         carrier = 1, destroyer = 1, frigate = 2, patrol = 3){
  if(missing(board)) stop("A board must be created first.")
  
  # Define fleet
  fleet <- data.frame(
    Name = c(rep("Carrier", carrier),
             rep("Destroyer", destroyer),
             rep("Frigate", frigate),
             rep("Patrol", patrol)),
    Size = c(rep(5, carrier), rep(4, destroyer), rep(3, frigate), rep(2, patrol))
  )
  
  water_available <- sum(board$State == "Water")
  
  if(sum(fleet$Size) >= water_available) {
    stop("The fleet is too large for the available water cells.")
  }
  
  message("Fleet created with ", nrow(fleet), " ships.")
  return(fleet)
}
