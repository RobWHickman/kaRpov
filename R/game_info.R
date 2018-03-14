#' @title Process Chess Notation
#' @description Functions to process the chess notation- e.g. to split up a position into row and column
#' @param game_id A numeric string of the game ID from the url of that game (e.g. 187146250)
#' @param pgn A pgn string in the form "1. MOVE MOVE 2. MOVE MOVE 3. ...."
#' @param move_colour The colour of the piece about to move (W or B)
#' @param pgn_round A number signifying the round of the pgn. proceeds as 1 1 2 2 3 3 4 4....end
#'
#' @import rvest

#a function to download the pgn of a game from chess.com
get_pgn <- function(game_id){

}

#a function to get the total_moves of a game
total_move_nos <- function(pgn){
  total_moves <- length(unlist(strsplit(gsub("[0-9]\\.", "", pgn), " ")))
  return(total_moves)
}

#a function to find the current move
get_move <- function(pgn, move_colour, pgn_round){
  ifelse(move_colour == "W",
         str_split(str_split(pgn, "[0-9]*\\.")[[1]][pgn_round+1], " ")[[1]][1],
         str_split(str_split(pgn, "[0-9]*\\.")[[1]][pgn_round+1], " ")[[1]][2])
  return(current_move)
}
