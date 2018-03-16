#' @title Plot Chess PGN
#' @description Single function to take a pgn and convert it into a .gif file
#' @param pgn The pgn of the game to be plotted- in the form "1. MOVE MOVE 2. MOVE MOVE 3. ...."
#' @param light_col The colour of the 'white' squares of the board. Defaults to a beige-y colour
#' @param dark_col The colour of the 'black' squares of the board. Defaults to a mid blue colour
#' @param square_labels Whether or not the board squares should be labelled. Defaults to no
#' @param plot Whether or not to plot the board. Deafults to no
#' @param move_cutoff Should all moves be processed or only up to a certain n. Defaults to NULL
#' @param frames The number of frames the output gif should have. Affects the 'smoothness' of the gif. Defaults to 100. If NULL will return the number of moves * 5
#' @param speed The speed that the gif cycles through still images. Each plot will last 1/speed seconds. Defaults to 20
#' @param pause_end Whether to add extra frames at the end to pause in the final position. Defaults to yes and adds 1s to end of the gif
#' @param black_shift Whether to interleave the black and white moves. This isn't working yet so defaults to NULL
#' @param name The name of the file of the png image for each piece

plot_pgn <- function(pgn,
                     #plotting the empty board
                     light_col = "#f5f5dc", dark_col = "#00688b", square_labels = FALSE, plot = FALSE,
                     #calculating moves
                     move_cutoff = NULL, frames = 100,
                     #producing the gif
                     speed = 20, pause_end = TRUE, black_shift = NULL,
                     name = "chess_plot.gif"){

  #first lets set up a board and the pieces
  board_plot <- plot_board(light_col, dark_col, square_labels, plot)
  initial_board <- setup_board()

  #produce a df with the board positions of every piece after every move
  all_moves_df <- add_all_moves(pgn, initial_board, move_cutoff)

  #tween these positions to create smooth animations
  #if frames argument is null use total number of moves * 5
  if(is.null(frames)){
    frames <- total_move_nos(pgn) * 5
  }
  tween_moves_df <- tween_moves(all_moves_df, frames)

  #create the gif
  create_chess_gif(tween_moves_df, speed = 20, pause_end = TRUE, black_shift = NULL, name)
}
