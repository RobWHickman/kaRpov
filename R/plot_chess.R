#' @title Plot Chess Games
#' @description Functions to modify a df game moves into a plot
#' @param name The name of the file of the png image for each piece
#' @param row The row the piece is positioned on
#' @param row The col the piece is positioned on
#' @param piece_id The id of each piece (colour + piece abbreviation- e.g. WR, BQ...)
#' @param size The size to plot each piece (0.5 is good)
#' @param tween_moves_df A df of all the moves to plot interpolated between positions
#' @param frames The number of frames the output gif should have. Affects the 'smoothness' of the gif. Defaults to 200
#'
#' @importFrom stringr str_split_fixed
#' @import tweenr
#' @import animation
#' @import grid
#' @import png

tween_moves <- function(all_moves_df, frames = 200){
  all_moves_df <- cbind(all_moves_df, stringr::str_split_fixed(all_moves_df$piece_position_after, "", 2))
  names(all_moves_df)[(ncol(all_moves_df)-1):ncol(all_moves_df)] <- c("piece_row", "piece_col")
  all_moves_df$piece_row_numeric <- match(all_moves_df$piece_row, letters)
  all_moves_df$piece_col <- as.numeric(all_moves_df$piece_col)
  all_moves_df$ease <- "linear"

  #select only the columns to be tweeened
  all_moves_df <- all_moves_df[which(names(all_moves_df) %in% c("move", "piece_id", "piece_row_numeric", "piece_col", "ease"))]

  #tween the df
  tween_moves_df <- tween_elements(all_moves_df, "move", "piece_id", "ease", nframes = frames)
  tween_moves_df$move <- floor(tween_moves_df$move)

  return(tween_moves_df)
}

#functions to get the png images of the chess pieces to be plotted via ggplot2
get_piece_image <- function(name) {
  rasterGrob(readPNG(paste0(find.package("kaRpov"), "/data/", piece, ".png")))
}
add_pieces <- function(row, col, piece_id, size) {
  piece <- gsub("\\.[0-9]", "", piece_id)
  annotation_custom(get_piece_image(piece),
                    xmin = x - size, xmax = x + size,
                    ymin = y - size, ymax = y + size)
}


#plot the chess board- creates a list of plots of each frame
plot_chess_board <- function(frame, tween_moves_df){
  chess_board_plot <- plot_board()
  move_data <- tween_moves_df[which(tween_moves_df$.frame == frame),]
  move_plot <- chess_board_plot +
    mapply(add_pieces,
           row = move_data$piece_row_numeric,
           col = move_data$piece_col,
           piece_id = move_data$.group,
           size = 0.5)

  return(move_plot)
}

#creates a gif of the chess game
create_chess_gif <- function(all_moves_df, speed = 20, name){
  tween_moves_df <- tween_moves(all_moves_df, frames = 200)
  saveGIF({
    for (frame in min(tween_moves_df$.frame):max(tween_moves_df$.frame)){
      plot <- plot_chess_board(frame, tween_moves_df)
      print(plot)}
    }, interval = 1/speed, movie.name = paste0(name, ".gif"))
}




