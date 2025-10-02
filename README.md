Battleship R game created using chatgpt IA

# Install remotes package if necessary:
install.packages("remotes")

# Load remotes library
library(remotes)

# Install BattleshipR library
remotes::install_github("mcorzot/BattleshipR")

# Load BattleshipR library
library(battleshipR)

# Create board
board <- create_board()

# Create fleet
fleet <- create_fleet(board)

# Place ships on board
board <- place_ships(board, fleet)

# Fire
res <- fire(board, "B", 5)

# Enjoy!
