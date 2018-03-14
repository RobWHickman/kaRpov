#' @title Find Starting Location
#' @description Functions to find the starting location of a piece that is moving
#' @param row The row that the piece will end up on
#' @param col The col that the piece will end up on
#' @param current_move_df The data frame containing the information on current pieces and their position
#'

#functions to limit the seatch space based upon pieces in a cross from a rook/queen
check_ranks <- function(row, col, current_move_df){
  chess_board <- create_board()
  col_positions <- gsub(row, "", current_move_df$piece_position_before[grep(row, current_move_df$piece_position_before)])
  row_positions <- gsub(col, "", current_move_df$piece_position_before[grep(col, current_move_df$piece_position_before)])
  if(length(row_positions) == 0){
    row_positions <- row
  } else{
    row_positions <- c(max(row_positions[row > row_positions]), min(row_positions[row < row_positions]))
  }
  if(length(col_positions) == 0){
    col_positions <- col
  } else{
    col_positions <- c(max(col_positions[col > col_positions]), min(col_positions[col < col_positions]))
  }
  possible_starting_positions <- c(paste0(row, col_positions), paste0(row_positions, col))
  possible_starting_positions <- unique(check_valid_square(possible_starting_positions))
  return(possible_starting_positions)
}
#functions to limit the seatch space based upon pieces in a diaganol cross from a bishop/queen
check_diaganols <- function(row, col, current_move_df){
  chess_board <- create_board()
  #find the diagonal constant
  bishop_rightwards_diagonal <- match(row, letters) - col
  #find squares
  rightwards_squares <- paste0(chess_board$row, chess_board$col)[
    #on the same diagonal
    which(match(chess_board$row, letters) - as.numeric(chess_board$col) == bishop_rightwards_diagonal)]
  #seq the diaganol in the perpendicular direction
  perpendicular_squares <- unique(c(rev(paste0(letters[seq(match(row, letters), 8, by = 1)],
                                               seq(col, 1, by = -1)[1:length(seq(match(row, letters), 8, by = 1))])),
                                    paste0(letters[seq(match(row, letters), 1, by = -1)],
                                           seq(col, 8, by = 1)[1:length(seq(match(row, letters), 1, by = -1))])))
  perpendicular_squares <- check_valid_square(perpendicular_squares)
  #concatenate these two and find unique
  possible_starting_positions <- unique(c(rightwards_squares, perpendicular_squares))

  taken_squares <- current_move_df$piece_position_before[current_move_df$piece_position_before %in% possible_starting_positions]

  taken_perpendicular <- as.numeric(gsub("[a-z]", "", taken_squares[taken_squares %in% perpendicular_squares]))
  min_perpendicular <- max(taken_perpendicular[which(taken_perpendicular < col)])
  max_perpendicular <- min(taken_perpendicular[which(taken_perpendicular > col)])
  range_perpendicular <- ifelse(is.finite(max_perpendicular), max_perpendicular, 1):
    ifelse(is.finite(min_perpendicular), min_perpendicular, 8)
  possible_perpendicular <- perpendicular_squares[unlist(lapply(range_perpendicular, grep, perpendicular_squares))]

  taken_rightwards <- as.numeric(gsub("[a-z]", "", taken_squares[taken_squares %in% rightwards_squares]))
  min_rightwards <- min(taken_rightwards[which(taken_rightwards < match(row, letters))])
  max_rightwards <- max(taken_rightwards[which(taken_rightwards > match(row, letters))])
  range_rightwards <- ifelse(is.finite(max_rightwards), max_rightwards, 8):ifelse(is.finite(min_rightwards), min_rightwards, 1)
  possible_rightwards <- rightwards_squares[unlist(lapply(range_rightwards, grep, rightwards_squares))]

  possible_starting_positions <- unique(c(possible_perpendicular, possible_rightwards))
  return(possible_starting_positions)
}

#taken from https://stackoverflow.com/questions/49028617/subtract-1-letter-and-number-from-multiple-strings-of-letters-and-numbers
find_knight_position <- function(move, move_colour, current_move_df){
  #clean up the move
  final_position <- split_position(clean_move(move))
  row <- final_position[1]
  col <- as.numeric(final_position[2])

  #find all possible positions a knight could have moved from

  letters_forwards <- c(paste0(letters[match(row, letters)+1], col-2),
                        paste0(letters[match(row, letters)+1], col+2),
                        paste0(letters[match(row, letters)+2], col-1),
                        paste0(letters[match(row, letters)+2], col+1))
  if(row > "a"){
    letters_backwards <- c(paste0(letters[match(row, letters)-1], col-2),
                           paste0(letters[match(row, letters)-1], col+2),
                           paste0(letters[match(row, letters)-2], col-1),
                           paste0(letters[match(row, letters)-2], col+1))
  } else {
    letters_backwards <- c()
  }

  possible_starting_positions <- c(letters_forwards, letters_backwards)
  possible_starting_positions <- check_valid_square(possible_starting_positions)

  #find any knights which could have started there
  matching_knights <- find_matching_pieces(possible_starting_positions, "N", move_colour, current_move_df)

  #if only one possible match its nice and easy
  if(length(matching_knights) == 1){
    matching_knight <- matching_knights
  } else {
    #more than 1 possible knight
    exact_matching_knights <- find_exact_matching_pieces(starting_position, "N", move_colour, current_move_df, final_position)
    matching_knight <- exact_matching_knights
  }
  return(matching_knight)
}
find_rook_position <- function(move, move_colour, current_move_df){
  #clean up the move
  final_position <- split_position(clean_move(move))
  row <- final_position[1]
  col <- as.numeric(final_position[2])

  #rooks that can move to this square must be in a bounding cross between all other pieces
  possible_starting_positions <- check_ranks(row, col, current_move_df)

  matching_rooks <- find_matching_pieces(possible_starting_positions, "R", move_colour, current_move_df)

  #if only one possible match its nice and easy
  if(length(matching_rooks) == 1){
    matching_rook <- matching_rooks
  } else {
    #more than 1 possible knight
    exact_matching_rooks <- find_exact_matching_pieces(starting_position, "R", move_colour, current_move_df, final_position)
    matching_rook <- exact_matching_rooks
  }
  return(matching_rook)
}
find_bishop_position <- function(move, move_colour, current_move_df){
  #clean up the move
  final_position <- split_position(clean_move(move))
  row <- final_position[1]
  col <- as.numeric(final_position[2])

  possible_starting_positions <- check_diaganols(row, col, current_move_df)

  matching_bishops <- find_matching_pieces(possible_starting_positions, "B", move_colour, current_move_df)
  #if only one possible match its nice and easy
  if(length(matching_bishops) == 1){
    matching_bishop <- matching_bishops
  } else {
    #more than 1 possible knight
    exact_matching_bishops <- find_exact_matching_pieces(starting_position, "B", move_colour, current_move_df, final_position)
    matching_bishop <- exact_matching_bishops
  }
  return(matching_bishop)
}
find_queen_position <- function(move, move_colour, current_move_df){
  #clean up the move
  final_position <- split_position(clean_move(move))
  row <- final_position[1]
  col <- as.numeric(final_position[2])

  possible_starting_positions_diag <- check_diaganols(row, col, current_move_df)
  possible_starting_positions_rank <- check_ranks(row, col, current_move_df)
  possible_starting_positions <- unique(c(possible_starting_positions_diag, possible_starting_positions_rank))

  matching_queens <- find_matching_pieces(possible_starting_positions, "Q", move_colour, current_move_df)
  #if only one possible match its nice and easy
  if(length(matching_queens) == 1){
    matching_queen <- matching_queens
  } else {
    #more than 1 possible knight
    matching_queen <- find_exact_matching_pieces(starting_position, "Q", move_colour, current_move_df, final_position)
  }
  return(matching_queen)
}
find_pawn_position <- function(move, move_colour, current_move_df){
  #clean up the move
  final_position <- split_position(clean_move(move))
  row <- final_position[1]
  col <- as.numeric(final_position[2])

  if(!grepl("x", move)){
    matching_pawns <- which(current_move_df$piece_colour == move_colour &
                              current_move_df$piece == "p" &
                              current_move_df$piece_position_before %in% paste0(row, 1:8))
    if(length(matching_pawns) > 1){
      possible_pawns <- current_move_df$piece_position_before[matching_pawns]
      pawn_ranks <- as.numeric(gsub("[a-z]", "", possible_pawns))
      matching_pawns <- possible_pawns[which(abs(col-pawn_ranks)==min(abs(col-pawn_ranks)))]
      matching_pawns <- which(current_move_df$piece_position_before == matching_pawns)
    }
    matching_pawn <- matching_pawns

  } else if(grepl("^x", move)){
    pawn_rows <- letters[c(match(row, letters) + 1, match(row, letters) - 1)]
    pawn_cols <- ifelse(move_colour == "W", col - 1, col + 1)
    possible_starting_positions <- paste0(pawn_rows, pawn_cols)

    matching_pawn <- find_matching_pieces(possible_starting_positions, "p", move_colour, current_move_df)
  } else if(grepl("^[a-z]x", move)){
    pawn_rows <- gsub("(.)(.*)", "\\1", move)
    pawn_cols <- ifelse(move_colour == "W", col - 1, col + 1)
    matching_pawn <- find_matching_pieces(paste0(pawn_rows, pawn_cols), "p", move_colour, current_move_df)
  }
  return(matching_pawn)
}
find_king_position <- function(move, move_colour, current_move_df){
  king_index <- which(current_move_df$piece_colour == move_colour &
                        current_move_df$piece == "K")
  return(king_index)
}
