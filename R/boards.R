#' @title Create and Plot Chess Boards
#' @description Functions to create a plain chess board and to plot it. The only arguments are the colours of the board plots
#' @param light_col The colour of the 'white' squares of the board. Defaults to a beige-y colour
#' @param dark_col The colour of the 'black' squares of the board. Defaults to a mid blue colour
#' @param square_labels Whether or not the board squares should be labelled. Defaults to no
#' @param plot whether or not to plot the board. Deafults to yes
#'
#' @import ggplot2

create_board <- function(){
  #create a df of combinations of letters and numbers for each chess square (A1, A2.. B1...H8)
  chess_board <- data.frame(col = rep(1:8, each = 8),
                            row = rep(letters[1:8], 8),
                            row_nums = rep(1:8, 8),
                            colour = rep(c(rep(c("light", "dark"), 4), rep(c("dark", "light"), 4)), 4))
  return(chess_board)
}

plot_board <- function(light_col = "#f5f5dc", dark_col = "#00688b", square_labels = FALSE, plot = yes){
  #create the board df
  chess_board <- create_board()

  p_board <- ggplot() +
    geom_tile(data = chess_board, aes(x = row_nums, y = col, fill = colour)) +
    #inherits from the colour arguments
    scale_fill_manual(values = c(light_col, dark_col), guide = FALSE) +
    theme_minimal() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
  #add position labels to each square or not
  if(square_labels){
    p_board <- p_board +
      geom_text(data = chess_board, aes(x = row_nums, y = col, label = paste0(row, col)),
                position = position_nudge(y = -0.35, x = 0.35), size = 3)
  }

  #whether or not to plot the board
  if(plot){
    p_board
  }

  return(p_board)
}

