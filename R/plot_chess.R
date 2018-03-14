#' @title Plot Chess Games
#' @description Functions to modify a df game moves into a plot
#' @param name The name of the file of the png image for each piece
#' @param row The row the piece is positioned on
#' @param row The col the piece is positioned on
#' @param piece_id The id of each piece (colour + piece abbreviation- e.g. WR, BQ...)
#' @param size The size to plot each piece (0.5 is good)
#' @param tween_moves_df A df of all the moves to plot
#'

tween_moves <- function(){

}

#functions to get the png images of the chess pieces to be plotted via ggplot2
get_piece_image <- function(name) {
  rasterGrob(readPNG(paste0("pieces/", piece, ".png")))
}
add_pieces <- function(row, col, piece_id, size) {
  piece <- gsub("[0-9]", "", piece_id)
  annotation_custom(get_piece_image(piece),
                    xmin = x - size, xmax = x + size,
                    ymin = y - size, ymax = y + size)
}


#plot the chess board- creates a list of plots of each frame
plot_chess_board <- function(frame, all_moves_df){
  tween_moves_df <- tween_moves()
  move_data <- tween_moves_df[which(tween_moves_df$.frame == frame)]
  move_plot <- p_board +
    mapply(add_pieces,
           row = move_data$piece_row_numeric,
           col = move_data$piece_col,
           piece_id = move_data$.group,
           size = 0.5)
}

#creates a gif of the chess game
create_chess_gif <- function(all_moves_df, speed, name){
  tween_moves_df <- tween_moves()
  saveGIF({
    for (frame in min(tween_moves_df$.frame):max(tween_moves_df$.frame)){
      plot <- plot_chess_board(frame, tween_moves_df)
      print(plot)}
    }, interval = 1/speed, movie.name = paste0(name, ".gif"))
}




