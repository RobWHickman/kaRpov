---
title: "Animated Chess Game Plots"
author: "Robert Hickman"
date: "14 March 2018"
output: html_document
---

#kaRpov - R based chess game animations

A fairly messy, fairly simple package to turn the pgn of chess games into an animated plot of all the moves

There's definitely a lot that could be refined/commented better/etc. which I'll try and clean up a bit but will probably never fully finish. The only real function you need is plot_pgn in plot_pgn.R which will take a pgn in the form below and turn it into a gif.

Package named after Russian grandmaster [Anatoly Karpov](https://en.wikipedia.org/wiki/Anatoly_Karpov).

Very loosely inspired by (me failing to get to work) JKunst's [rchess package](http://jkunst.com/r/rchess-a-chess-package-for-r/), which will surely also be of interest.

Example below. Uses the pgn for the [immortal game](https://en.wikipedia.org/wiki/Immortal_Game) between Adolf Anderssen and Lionel Kieseritzky in 1851.

```{r plot chess game, message=FALSE,warning=FALSE}

#the pgn for the immortal game
immortal_pgn <- "1.e4 e5 2.f4 exf4 3.Bc4 Qh4+ 4.Kf1 b5 5.Bxb5 Nf6 6.Nf3 Qh6 7.d3 Nh5 8.Nh4 Qg5 9.Nf5 c6 10.g4 Nf6 11.Rg1 cxb5 12.h4 Qg6 13.h5 Qg5 14.Qf3 Ng8 15.Bxf4 Qf6 16.Nc3 Bc5 17.Nd5 Qxb2 18.Bd6 Bxg1 19.e5 Qxa1+ 20.Ke2 Na6 21.Nxg7+ Kd8 22.Qf6+ Nxf6 23.Be7#"

filename <- "C:/Users/MagnusCarlsen/Desktop/chess_gif"

library(tweenr)
library(animation)
library(ggplot2)
library(grid)
library(png)

#create the gif
plot_pgn (immortal_pgn, 
          light_col = "#f5f5dc", dark_col = "#00688b", square_labels = FALSE, plot = FALSE,
          move_cutoff = NULL, frames = 100,
          speed = 10, pause_end = TRUE, black_shift = NULL,
          name = filename)
          
```

