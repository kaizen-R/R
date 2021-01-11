# Shiny 2048 game
# Author: Nico
# Date: 2021/01/11
# v0.3: Goal: Can we do a 2048 game with a working Shiny interface
# This time around, we need to keep score.
# More hours went into this, for more conditions and all.
# But still well below 6h total.
# Still Missing:
#   - I definitely think I can reduce some more the code without making it completely unreadable...
#   - A more beautiful interface wouldn't kill me...
#   - Could I make the machine learn how to rock this game? Maybe...

library(reshape2)
library(ggplot2)
library(shiny)

initialize_2048 <- function(side = 4) {
  m2048 <- matrix(rep(0, side^2), nrow = side)
  m2048[sample(1:side^2, 1)] <- 2
  list(board = m2048, score = 0, finished = 0)
}

# Does not affect score, so kept as-is
add_new_tile <- function(m2048) {
  available_empty <- length(m2048[m2048 == 0])
  if(available_empty > 0) {
    m2048[m2048 == 0][sample(1:length(m2048[m2048 == 0]), 1)] <- sample(c(2, 4), 1)  
  }
  m2048
}

# No change either
simply_move_down <- function(temp_col) {
  non_zero <- temp_col[temp_col != 0]
  c(rep(0, length(temp_col) - length(non_zero)), non_zero) # Implicit return
}

# Now we keep track of score upon merging tiles:
complete_move_down <- function(game_status) {
  t_score <- game_status$score
  game_status$board <- as.matrix(apply(game_status$board, MARGIN = 2, function(temp_col) {
    # Move all down first.
    temp_col <- simply_move_down(temp_col)
    # "consolidate down", in bottom-up order:
    for(i in length(temp_col):2) {
      if(temp_col[i] == temp_col[i-1]) {
        temp_col[i] <- temp_col[i]*2
        t_score <<- t_score + temp_col[i] # Only change that was needed to keep score...
        temp_col[i-1] <- 0
      }
    }
    # Once consolidated, we can then "move down" & return
    simply_move_down(temp_col) 
  }) # apply
  )
  game_status$score <- t_score
  game_status
}

# In less written lines of code, less repeated code, that is:
# Recursive rotation:
rotate_board_x <- function(m2048, n) {
  if((n %% 4) < 1) return(m2048) # Can only rotate so many times...
  t(apply(rotate_board_x(m2048, n-1), 2, rev)) # Recursive call
}

# Closure:
play_directed <- function(n) {
  function(game_status) {
    game_status$board <- rotate_board_x(game_status$board, n)
    game_status <- complete_move_down(game_status)
    game_status$board <- rotate_board_x(game_status$board, 4-n)
    game_status
  }
} 

# Exception
play_down <- function(game_status) {
  complete_move_down(game_status)
}
# For the rest:
play_right <- play_directed(1)
play_up <- play_directed(2)
play_left <- play_directed(3)
# Now to play down, call with n = 0
# To play right, call with n == 1
# (...)

game_finished <- function(game_status) {
  temp_board <- game_status$board
  
  available_empty <- length(temp_board[temp_board == 0])
  if(available_empty == 0) { # Simulated all possible next moves
    possibility1 <- play_down(game_status)$board
    possibility2 <- play_right(game_status)$board
    possibility3 <- play_up(game_status)$board
    possibility4 <- play_left(game_status)$board
    
    if(all(possibility1 == temp_board) &&
       all(play_right(game_status)$board == temp_board) &&
       all(play_up(game_status)$board == temp_board) &&
       all(play_left(game_status)$board == temp_board)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

## ----
# Shiny UI - Would look better with some CSS/JS, but for now...
## ----
ui <- fluidPage(
  fluidRow(h4("Shiny Demo: Playing my own 2048"),
           textOutput("game_score")),
  hr(),
  fluidRow(column(12,
                  align="center",
                  actionButton("play_up", "^")
  )
  ),
  fluidRow(
    column(3, align = "right", actionButton("play_left", "<")),
    column(6, plotOutput("board", height = "250px")),
    column(3, align = "left", actionButton("play_right", ">"))
  ),
  fluidRow(column(12, align="center",
                  actionButton("play_down", "v"),
                  verbatimTextOutput("finished_text")
  )
  )
)

## ----
# Shiny Server
## ----
server <- function(input, output, session) {
  
  # Readable version:  
  m2048 <- reactiveVal(initialize_2048())
  
  # Much repeated code here... I might want to look into this:
  observeEvent(input$play_right, {
    former_board <- m2048()$board
    m2048 <- play_right(m2048())
    if(!all(m2048$board == former_board)) {
      m2048$board <- add_new_tile(m2048$board)
      m2048(m2048) # Set Board
    }
  })
  observeEvent(input$play_left, {
    former_board <- m2048()$board
    m2048 <- play_left(m2048())
    if(!all(m2048$board == former_board)) {
      m2048$board <- add_new_tile(m2048$board)
      m2048(m2048) # Set Board
    }
  })
  observeEvent(input$play_down, {
    former_board <- m2048()$board
    m2048 <- play_down(m2048())
    if(!all(m2048$board == former_board)) {
      m2048$board <- add_new_tile(m2048$board)
      m2048(m2048) # Set Board
    }
  })
  observeEvent(input$play_up, {
    former_board <- m2048()$board
    m2048 <- play_up(m2048())
    if(!all(m2048$board == former_board)) {
      m2048$board <- add_new_tile(m2048$board)
      m2048(m2048) # Set Board
    }
  })
  
  output$board <- renderPlot({
    melted_m2048 <- melt(m2048()$board) # for graph
    
    ggplot(melted_m2048, aes(x = Var2, y = -Var1)) + # Negative y for correct up/down
      geom_raster(aes(fill=ifelse(value != 0, log2(value), -1))) + 
      geom_text(aes(label=ifelse(value != 0, value, ""))) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none") + 
      scale_fill_gradient(low="grey90", high="red") # far from perfect colors
  })
  
  output$finished_text <- renderText({ ifelse(game_finished(m2048()), "No more moves!", "") })
  output$game_score <- renderText( paste("Current Score: ", m2048()$score) )
}

shinyApp(ui = ui, server = server)
