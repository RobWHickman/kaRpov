#' @title Find Starting Location
#' @description Functions to find the starting location of a piece that is moving
#' @param current_current_move_df The data frame containing the information on current pieces and their position
#' @param move A move in algebraic chess notation (e.g. Bxc5, O-O, Qd2#)
#' @param move_colour The colour of the piece about to move (W or B)
#' @param pgn A pgn string in the form "1. MOVE MOVE 2. MOVE MOVE 3. ...."
#' @param initial_board the intial board set up (see setup_board())
#' @param move_cutoff should all moves be processed or only up to a certain n. Defaults to NULL
#'

#a function to take the move and update a df to reflect the ending positions of the pieces
add_next_move <- function(move, move_colour, current_move_df){
  #if a pawn has been promoted on the previous move change the piece ids to be updated
  piece_mismatch <- which(current_move_df$piece != gsub("^[W|B]", "", gsub("\\..*", "", current_move_df$piece_id)))
  if(length(piece_mismatch) > 0){
    mismatch_df <- current_move_df[piece_mismatch,]
    current_move_df <- current_move_df[-piece_mismatch,]

    mismatch_df$piece_id <- paste0(mismatch_df$piece_colour, mismatch_df$piece)
    current_move_df <- rbind(current_move_df, mismatch_df)
    current_move_df$piece_id <- make.unique(as.character(current_move_df$piece_id))
  }

  #remove pieces that are about to be taken
  current_move_df <- remove_taken_pieces(move, current_move_df)

  #the final position of the moving piece
  if(!grepl("O-O", move)){
    final_position <- clean_move(move)

  #find the index of the moving piece
  if(grepl("^[a-z]", move)){
    #pawn move
    moving_piece_index <- find_pawn_position(move, move_colour, current_move_df)
  } else if(grepl("K", move)){
    #king move
    moving_piece_index <- find_king_position(move, move_colour, current_move_df)
  } else if(grepl("Q", move)){
    #queen move
    moving_piece_index <- find_queen_position(move, move_colour, current_move_df)
  } else if(grepl("B", move)){
    #bishop move
    moving_piece_index <- find_bishop_position(move, move_colour, current_move_df)
  } else if(grepl("R", move)){
    #rook move
    moving_piece_index <- find_rook_position(move, move_colour, current_move_df)
  } else if(grepl("N", move)){
    #knight move
    moving_piece_index <- find_knight_position(move, move_colour, current_move_df)
    #castling
  }
    #move the piece
    current_move_df$piece_position_after[moving_piece_index] <- final_position
  }

  king_row <- which(current_move_df$piece_colour == move_colour & current_move_df$piece == "K")
  rook_rows <- which(current_move_df$piece_colour == move_colour & current_move_df$piece == "R")
  if(move == "O-O"){
    rook_row <- rook_rows[grep("h", current_move_df$piece_position_before[rook_rows])]
    current_move_df$piece_position_after[king_row] <- gsub("e", "g", current_move_df$piece_position_before[king_row])
    current_move_df$piece_position_after[rook_row] <- gsub("h", "f", current_move_df$piece_position_before[rook_row])
  } else if(move == "O-O-O"){
    rook_row <- rook_rows[which(current_move_df$piece_position_before[rook_rows] == "a8")]
    current_move_df$piece_position_after[king_row] <- gsub("e", "c", current_move_df$piece_position_before[king_row])
    current_move_df$piece_position_after[rook_row] <- gsub("a", "d", current_move_df$piece_position_before[rook_row])
  }

  #if a pawn is promoted change it to the piece it will become
  if(grepl("=", move)){
    current_move_df$piece[moving_piece_index] <- gsub(".*=", "", move)
  }

  return(current_move_df)
}

add_all_moves <- function(pgn, initial_board, move_cutoff = NULL){
  all_moves_df <- initial_board

  #how many moves to process
  if(is.null(move_cutoff)){
    total_moves <- total_move_nos(pgn)
  } else {
    total_moves <- move_cutoff
  }

  for(move_number in 1:total_moves){
    #pgn lists moves as 1. W B 2. W B ... so need to find 'round' first
    pgn_round <- ceiling(move_number/2)
    #then find which colour is taking the move
    move_colours <- c("B", "W")
    move_colour <- move_colours[(move_number %% 2) + 1]

    current_move <- get_move(pgn, move_colour, pgn_round)

    #subset the last move
    current_move_df <- all_moves_df[which(all_moves_df$move == (move_number - 1)),]

    #set the cols
    current_move_df$move <- move_number
    current_move_df$player <- as.character(move_colour)
    current_move_df$piece_position_before <- as.character(current_move_df$piece_position_after)
    current_move_df$piece_position_after <- as.character(current_move_df$piece_position_after)
    current_move_df$movement <- current_move

    #update with positions after the move
    current_move_df <- add_next_move(current_move, move_colour, current_move_df)

    all_moves_df <- rbind(all_moves_df, current_move_df)
  }

  return(all_moves_df)
}
