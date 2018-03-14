#' @title Find Moving Pieces
#' @description Functions to match a pieces initial position to its row in the df of the previous move
#' @param starting_position The starting position of the piece about to move (e.g. A1)
#' @param piece The abbreviation of the piece about to move (e.g. Q, p, R...)
#' @param move_colour The colour of the piece about to move (W or B)
#' @param current_move_df The data frame containing the information on current pieces and their position
#' @param move A move in algebraic chess notation (e.g. Bxc5, O-O, Qd2#)
#'

#find any pieces which match a lazy search based on possible start position
find_matching_pieces <- function(starting_position, piece, move_colour, current_move_df){
  matching_piece_index <- which(current_move_df$piece_colour == move_colour &
                                  current_move_df$piece == piece &
                                  current_move_df$piece_position_before %in% starting_position)
  return(matching_piece_index)
}

#when there are multiple lazy matches, find the exact match
find_exact_matching_pieces <- function(starting_position, piece, move_colour, current_move_df, move){
  start_indicator <-  gsub(clean_move(move), "", gsub(paste0("^", piece), "", gsub("x", "", gsub("\\+|#", "", move))))
  matching_piece_index <- which(current_move_df$piece_colour == move_colour &
                                  current_move_df$piece == piece &
                                  current_move_df$piece_position_before %in% starting_position &
                                  grepl(start_indicator, current_move_df$piece_position_before))
  return(matching_piece_index)
}
