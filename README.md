---
title: "Animated Chess Game Plots"
author: "Robert Hickman"
date: "14 March 2018"
output: html_document
---

#v0.1

A small collection of functions to plot chess games given the pgn

Currently incomplete:
  - need to add in pawn promotion
  - surely some bugs in the functions
  - code generally needs cleaning

```{r, plot chess game}
pgn <- "1.d4 Nf6 2.c4 e6 3.Nc3 Bb4 4.e3 O-O 5.Bd3 d5 6.Nf3 b6 7.a3 Bxc3+ 8.bxc3 c5 9.O-O Bb7 10.cxd5 exd5 11.a4 Nc6 12.Qb3 c4 13.Bxc4 dxc4 14.Qxc4 Re8 15.Re1 Na5 16.Qd3 Rc8 17.Ng5 Ng4 18.Qxh7+ Kf8 19.Ba3+ Rc5 20.dxc5 Nc6 21.cxb6+ Ne7 22.Qh8#"

pgn2 <- "1.e4 e5 2.Nf3 Nc6 3.Bb5 g6 4.Bxc6 bxc6 5.Nxe5 Bg7 6.d4 Qe7 7.f4 c5 8.O-O c4 9.b3 cxb3 10.Nc3 bxa2 11.Rb1 axb1=Q 12.Bd2 a6 13.Be3 a5 14.Bf2 a4 15.Bg3 a3 16.Bh4 a2 17.Bg3 a1=R 18.Bf2 c6 19.Bg3 c5 20.Bh4"

#create the initial df
initial_board <- setup_board()

#add in all the moves
all_moves_df <- add_all_moves(pgn, initial_board, move_cutoff = NULL)

#create the gif
filename <- "C:/Users/Robert/Desktop/New Coding/chess_gif"
create_chess_gif(all_moves_df, speed = 20, filename)
```

