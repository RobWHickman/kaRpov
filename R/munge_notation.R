#' @title Process Chess Notation
#' @description Functions to process the chess notation- e.g. to split up a position into row and column
#' @param position A position on the chess board (e.g. A1, B6, H2)
#' @param move A move in algebraic chess notation (e.g. Bxc5, O-O, Qd2#)
#' @param current_move_df The data frame containing the information on current pieces and their position
#' @param pgn A pgn string in the form "1. MOVE MOVE 2. MOVE MOVE 3. ...."
#' @param move_colour The colour of the piece about to move (W or B)
#' @param pgn_round A number signifying the round of the pgn. proceeds as 1 1 2 2 3 3 4 4....end
#'

#splits up a position into a row and a col
split_position <- function(position){
  row <- gsub("[0-9]", "", position)
  col <- gsub("[a-z]", "", position)
  return(c(row, col))
}

#removes any positions which are invalid
check_valid_square <- function(position){
  chess_board <- create_board()
  valid_positions <- position[position %in% paste0(chess_board$row, chess_board$col)]
  return(valid_positions)
}

#function returns only the final position of the piece in question
clean_move <- function(move){
  #remove checks/checkmates
  removed_checks <- gsub("\\+|#|=", "", move)
  #remove takes x
  removed_capture <- gsub("x", "", removed_checks)
  #remove first letter indicating piece
  removed_piece <- gsub("R|N|B|K|Q", "", removed_capture)
  #if length > 2 means there is a position before signifier
  removed_before <- removed_piece
  while(nchar(removed_before) > 2){
    removed_before <- gsub("^.", "", removed_before)
  }

  if(length(check_valid_square(removed_before)) != 1){
    warning("possible cleaning error- check returned 1 good square")
  }
  return(removed_before)
}

#function to remove taken pieces from a df containing the pieces from the previous move
remove_taken_pieces <- function(move, current_move_df){
  keep_rows <- which(current_move_df$piece_position_before != gsub(".*x", "", gsub("\\+|#|=.", "", move)))
  current_move_df <- current_move_df[keep_rows,]
  return(current_move_df)
}

#a function to get the total_moves of a game
total_move_nos <- function(pgn){
  total_moves <- length(unlist(strsplit(gsub("[0-9]\\.", "", pgn), " ")))
  return(total_moves)
}

#a function to find the current move
get_move <- function(pgn, move_colour, pgn_round){
  current_move <- ifelse(move_colour == "W",
                         strsplit(strsplit(pgn, "[0-9]*\\.")[[1]][pgn_round+1], " ")[[1]][1],
                         strsplit(strsplit(pgn, "[0-9]*\\.")[[1]][pgn_round+1], " ")[[1]][2])

  return(current_move)
}

