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
  #remove pieces that are about to be taken
  current_move_df <- remove_taken_pieces(move, current_move_df)

  #the final position of the moving piece
  if(!grepl("O-O", move)){
    final_position <- clean_move(move)
  }

  #find the index of the moving piece
  if(grepl("^[a-z]", move)){
    #pawn move
    moving_piece_index <- find_pawn_position(move, current_move_df)
  } else if(grepl("K", move)){
    #king move
    moving_piece_index <- find_king_position(move, current_move_df)
  } else if(grepl("Q", move)){
    #queen move
    moving_piece_index <- find_queen_position(move, current_move_df)
  } else if(grepl("B", move)){
    #bishop move
    moving_piece_index <- find_bishop_position(move, current_move_df)
  } else if(grepl("R", move)){
    #rook move
    moving_piece_index <- find_rook_position(move, current_move_df)
  } else if(grepl("N", move)){
    #knight move
    moving_piece_index <- find_knight_position(move, current_move_df)
    #castling
  }
  #move the piece
  current_move_df$piece_position_after[moving_piece_index] <- final_position

  if(move == "O-O"){
    current_move_df %>%
      .[c(piece_colour == move_colour & piece == "K"),
        piece_position_after := gsub("^e", "g", piece_position_before)]
    current_move_df %>%
      .[c(piece_colour == move_colour & piece == "R" & grepl("h", piece_position_before)),
        piece_position_after := gsub("^h", "f", piece_position_before)]
  } else if(move == "O-O-O"){
    current_move_df %>%
      .[c(piece_colour == move_colour & piece == "K"),
        piece_position_after := gsub("^e", "c", piece_position_before)]
    current_move_df %>%
      .[c(piece_colour == move_colour & piece == "R" & grepl("h", piece_position_before)),
        piece_position_after := gsub("^a", "d", piece_position_before)]
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
    next_move_no <- length(unique(paste0(all_moves_df$move, all_moves_df$player)))
    #pgn lists moves as 1. W B 2. W B ... so need to find 'round' first
    pgn_round <- ceiling(next_move_no/2)
    #then find which colour is taking the move
    move_colours <- c("B", "W")
    move_colour <- move_colours[(next_move_no %% 2) + 1]

    current_move <- get_move(pgn, move_colour, pgn_round)

    #create a df to hold the move coming up
    current_move_df <- all_moves_df %>%
      #subset the last move
      .[move == next_move_no - 1] %>%
      #set the cols
      .[, move := next_move_no] %>%
      .[, player := as.character(move_colour)] %>%
      .[, piece_position_before := as.character(piece_position_after)] %>%
      .[, piece_position_after := as.character(piece_position_after)] %>%
      .[, movement := current_move]

    #remove taken pieces
    current_move_df <- remove_taken_pieces(current_move, current_move_df)

    #update with positions after the move
    current_move_df <- add_next_move(move, move_colour, current_move_df)

    all_moves_df <- rbind(all_moves_df, current_move_df)
  }

  return(all_moves_df)
}
