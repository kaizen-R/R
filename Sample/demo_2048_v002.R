# Shiny 2048 game
# Author: Nico
# Date: 2021/01/10
# v0.2: Goal: Can we do a 2048 game with a working Shiny interface
# Still Missing:
#   - Counting points, 
#   - Issue: If you can't move left (for example), will let you and add a tile.
#         I'm not sure this is what it should be. Would then need to "simulate"
#         move first see if it changes anything, and only add tile if it does.
#   - End condition (repeating matrix & no empty slots)
#   - A more beautiful interface wouldn't kill me...

library(reshape2)
library(ggplot2)
library(shiny)

initialize_2048 <- function(m2048 = matrix(rep(0, 16), nrow = 4)) {
  m2048[sample(1:16, 1)] <- 2
  m2048
}
#Changing to a 5x5 works fine if I just do:
# initialize_2048 <- function(m2048 = matrix(rep(0, 25), nrow = 5)) {
#   m2048[sample(1:25, 1)] <- 2
#   m2048
# }

add_new_tile <- function(m2048) {
  available_empty <- length(m2048[m2048 == 0])
  if(available_empty > 0) {
    m2048[m2048 == 0][sample(1:length(m2048[m2048 == 0]), 1)] <- sample(c(2, 4), 1)  
  }
  m2048
}

simply_move_down <- function(temp_col) {
  non_zero <- temp_col[temp_col != 0]
  c(rep(0, length(temp_col) - length(non_zero)), non_zero) # Implicit return
}

complete_move_down <- function(m2048) {
  as.matrix(apply(m2048, MARGIN = 2, function(temp_col) {
    # Move all down first.
    temp_col <- simply_move_down(temp_col)
    # "consolidate down", in bottom-up order:
    for(i in length(temp_col):2) {
      if(temp_col[i] == temp_col[i-1]) {
        temp_col[i] <- temp_col[i]*2
        temp_col[i-1] <- 0
      }
    }
    # Once consolidated, we can then "move down" & return
    simply_move_down(temp_col) 
  }) # lapply
  ) # as.matrix, Implicit return
}

# Seems about right, but now we need to be able to move in any direction...
# Why did I only care about moving down?
# Because moving in any other direction is just about turning, moving down, 
# and turning back. I'm pretty sure I'm not the first one to come up with this...
# Closure:
closure_rotate_right_n <- function(n) {
  function(m2048) {
    for(i in 1:n) {
      m2048 <- t(apply(m2048, 2, rev))
    }
    m2048
  }
}
rotate_right_1 <- closure_rotate_right_n(1)
rotate_right_2 <- closure_rotate_right_n(2)
rotate_right_3 <- closure_rotate_right_n(3)
# Rotate Right 4 would be like no-op.

play_down <- function(m2048) {
  complete_move_down(m2048)
}
play_right <- function(m2048) {
  m2048 <- rotate_right_1(m2048)
  m2048 <- complete_move_down(m2048)
  m2048 <- rotate_right_3(m2048)
  m2048
}
play_left <- function(m2048) {
  m2048 <- rotate_right_3(m2048)
  m2048 <- complete_move_down(m2048)
  m2048 <- rotate_right_1(m2048)
  m2048
}
play_up <- function(m2048) {
  m2048 <- rotate_right_2(m2048)
  m2048 <- complete_move_down(m2048)
  m2048 <- rotate_right_2(m2048)
  m2048
}

## ----
# Shiny UI - Would look better with some CSS/JS, but for now...
## ----
ui <- fluidPage(
  fluidRow(h4("Shiny Demo: Playing my own 2048")),
  hr(),
  fluidRow(column(12,
                  align="center",
                  actionButton("play_up", "^")
                  )
  ),
  fluidRow(
    column(3, actionButton("play_left", "<")),
    column(6, plotOutput("board", height = "250px")),
    column(3, actionButton("play_right", ">"))
  ),
  fluidRow(column(12, align="center",
                  actionButton("play_down", "v"),
                  verbatimTextOutput("board_text")
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
    m2048 <- play_right(m2048())
    m2048 <- add_new_tile(m2048)
    m2048(m2048) # Set Board
  })
  observeEvent(input$play_left, {
    m2048 <- play_left(m2048())
    m2048 <- add_new_tile(m2048)
    m2048(m2048) # Set Board
  })
  observeEvent(input$play_down, {
    m2048 <- play_down(m2048())
    m2048 <- add_new_tile(m2048)
    m2048(m2048) # Set Board
  })
  observeEvent(input$play_up, {
    m2048 <- play_up(m2048())
    m2048 <- add_new_tile(m2048)
    m2048(m2048) # Set Board
  })
  
  output$board <- renderPlot({
    melted_m2048 <- melt(m2048()) # for graph

    ggplot(melted_m2048, aes(x = Var2, y = -Var1)) + # Negative y for correct up/down
      geom_raster(aes(fill=value)) + 
      geom_text(aes(label=ifelse(value != 0, value, ""))) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) + 
      scale_fill_gradient(low="grey90", high="red") # far from perfect colors
  })
  
  output$board_text <- renderText({ m2048() })
}

shinyApp(ui = ui, server = server)
