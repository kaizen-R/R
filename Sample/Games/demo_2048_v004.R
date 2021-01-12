# Shiny 2048 game
# Author: Nico
# Date: 2021/01/12
# v0.4: This time around it's all about playing automatically
# In total hours, I must be at 8-10h into this. (I couldn't sleep this morning)
# Future versions:
#   - Current algorithms are very "simplistic", and no ML involved... Maybe a ReLU?

library(reshape2)
library(ggplot2)

initialize_2048 <- function(side = 4) {
  m2048 <- matrix(rep(0, side^2), nrow = side)
  m2048[sample(1:side^2, 1)] <- 2
  list(board = m2048, score = 0, finished = 0, n_moves = 0)
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


play_random_game <- function() {
  one_game <- initialize_2048()
  
  for(i in 0:1000) {
    # print(paste("Generation", i))
    # print(one_game)
    # #print(class(one_game))
    # flush.console()
    temp_board <- one_game$board
    # Simulate all moves:
    possibility1 <- play_down(one_game)
    possibility2 <- play_right(one_game)
    possibility3 <- play_left(one_game)
    possibility4 <- play_up(one_game)
    
    # Keep only possible next moves
    next_move <- list()
    available_positions <- c()
    if(!all(possibility1$board == temp_board)) {
      next_move[[1]] <- possibility1
      available_positions <- c(available_positions, 1)
    }
    if(!all(possibility2$board == temp_board)) {
      next_move[[2]] <- possibility2
      available_positions <- c(available_positions, 2)
    }
    if(!all(possibility3$board == temp_board)) {
      next_move[[3]] <- possibility3
      available_positions <- c(available_positions, 3)
    }
    if(!all(possibility4$board == temp_board)) {
      next_move[[4]] <- possibility4
      available_positions <- c(available_positions, 4)
    }
    # print("Available Next Moves:")
    # print(next_move)
    # flush.console()
    if(length(available_positions) >= 1) {
      one_game <- next_move[[sample(available_positions, 1)]]
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
    } else {
      one_game$finished <- 1
      return(one_game)
    }

    
  }
}

# Based on real-world experience, it's best to choose one direction over the others
# Then a second direction
# Then a third
# Only use the fourth direction of movement when no other option is available...
play_drlu_game <- function() {
  one_game <- initialize_2048()
  
  for(i in 0:1000) {
    # print(paste("Generation", i))
    # print(one_game)
    # #print(class(one_game))
    # flush.console()
    temp_board <- one_game$board
    # Simulate all moves:
    possibility1 <- play_down(one_game)
    possibility2 <- play_right(one_game)
    possibility3 <- play_left(one_game)
    possibility4 <- play_up(one_game)
    
    # Keep only possible next moves
    next_move <- list()
    available_positions <- c()
    if(!all(possibility1$board == temp_board)) {
      one_game <- possibility1
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
      next
    }
    
    if(!all(possibility2$board == temp_board)) {
      one_game <- possibility2
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
      next
    }
    if(!all(possibility3$board == temp_board)) {
      one_game <- possibility3
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
      next
    }
    if(!all(possibility4$board == temp_board)) {
      one_game <- possibility4
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
      next
    }

    one_game$finished <- 1
    return(one_game)
  }
}

# Similar to the above, but less strict
play_40_30_20_10_game <- function() {
  one_game <- initialize_2048()
  
  for(i in 0:1000) {
    # print(paste("Generation", i))
    # print(one_game)
    # #print(class(one_game))
    # flush.console()
    temp_board <- one_game$board
    # Simulate all moves:
    possibility1 <- play_down(one_game)
    possibility2 <- play_right(one_game)
    possibility3 <- play_left(one_game)
    possibility4 <- play_up(one_game)
    
    # Keep only possible next moves
    next_move <- list()
    available_positions <- c()
    if(!all(possibility1$board == temp_board)) {
      next_move[[1]] <- possibility1
      available_positions <- c(available_positions, rep(1, 40))
    }
    if(!all(possibility2$board == temp_board)) {
      next_move[[2]] <- possibility2
      available_positions <- c(available_positions, rep(2, 30))
    }
    if(!all(possibility3$board == temp_board)) {
      next_move[[3]] <- possibility3
      available_positions <- c(available_positions, rep(3, 20))
    }
    if(!all(possibility4$board == temp_board)) {
      next_move[[4]] <- possibility4
      available_positions <- c(available_positions, rep(4, 10))
    }
    # print("Available Next Moves:")
    # print(next_move)
    # flush.console()
    if(length(available_positions) >= 1) {
      one_game <- next_move[[sample(available_positions, 1)]]
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
    } else {
      one_game$finished <- 1
      return(one_game)
    }
    
    
  }
}

# Choose best option based on movement's score
# Careful here: This game is not a Markov Chain.
# There is some randomness (new tile position and value)
# So this here is not exhaustive: We would have to simulate all POSSIBLE next moves...
play_bruteforce1_game <- function() {
  one_game <- initialize_2048()
  
  for(i in 0:1000) {
    # print(paste("Generation", i))
    # print(one_game)
    # #print(class(one_game))
    # flush.console()
    temp_board <- one_game$board
    # Simulate all moves:
    possibility1 <- play_down(one_game)
    possibility2 <- play_right(one_game)
    possibility3 <- play_left(one_game)
    possibility4 <- play_up(one_game)
    
    # Keep only possible next moves
    next_move <- list()
    position_score <- c(-1, -1, -1, -1)
    if(!all(possibility1$board == temp_board)) {
      next_move[[1]] <- possibility1
      position_score[1] <- possibility1$score
    }
    if(!all(possibility2$board == temp_board)) {
      next_move[[2]] <- possibility2
      position_score[2] <- possibility2$score
    }
    if(!all(possibility3$board == temp_board)) {
      next_move[[3]] <- possibility3
      position_score[3] <- possibility3$score
    }
    if(!all(possibility4$board == temp_board)) {
      next_move[[4]] <- possibility4
      position_score[4] <- possibility4$score
    }
    
    # print("Available Next Moves:")
    # print(next_move)
    # flush.console()
    if(length(position_score[position_score >= 0]) >= 1) {
      one_game <- next_move[[which.max(position_score)]]
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
    } else {
      one_game$finished <- 1
      return(one_game)
    }
  }
}

# Choose best option over overall next movement's possible derived scores
# Careful here: This game is not a Markov Chain.
# There is some randomness (new tile position and value)
# So this here is not exhaustive: We would have to simulate all POSSIBLE next moves...
play_bruteforceSquare_game <- function() {
  one_game <- initialize_2048()
  
  for(i in 0:1000) {
    # print(paste("Generation", i))
    # print(one_game)
    # #print(class(one_game))
    # flush.console()
    temp_board <- one_game$board
    # Simulate all moves:
    possibility1 <- play_down(one_game)
    possibility2 <- play_right(one_game)
    possibility3 <- play_left(one_game)
    possibility4 <- play_up(one_game)
    
    # Keep only possible next moves
    next_move <- list()
    position_score <- c(-1, -1, -1, -1)
    if(!all(possibility1$board == temp_board)) {
      next_move[[1]] <- possibility1
      position_score[1] <- sum(play_down(possibility1)$score,
                               play_right(possibility1)$score,
                               play_left(possibility1)$score,
                               play_up(possibility1)$score)
    }
    if(!all(possibility2$board == temp_board)) {
      next_move[[2]] <- possibility2
      position_score[2] <- sum(play_down(possibility2)$score,
                               play_right(possibility2)$score,
                               play_left(possibility2)$score,
                               play_up(possibility2)$score)
    }
    if(!all(possibility3$board == temp_board)) {
      next_move[[3]] <- possibility3
      position_score[3] <- sum(play_down(possibility2)$score,
                               play_right(possibility2)$score,
                               play_left(possibility2)$score,
                               play_up(possibility2)$score)
    }
    if(!all(possibility4$board == temp_board)) {
      next_move[[4]] <- possibility4
      position_score[4] <- sum(play_down(possibility2)$score,
                               play_right(possibility2)$score,
                               play_left(possibility2)$score,
                               play_up(possibility2)$score)
    }
    
    # print("Available Next Moves:")
    # print(next_move)
    # flush.console()
    if(length(position_score[position_score >= 0]) >= 1) {
      one_game <- next_move[[which.max(position_score)]]
      one_game$board <- add_new_tile(one_game$board)
      one_game$n_moves <- one_game$n_moves + 1
    } else {
      one_game$finished <- 1
      return(one_game)
    }
  }
}

# Messy code but controlled/readable
strategy1 <- list()
strategy2 <- list()
strategy3 <- list()
strategy4 <- list()
strategy5 <- list()

length_games <-50

for(i in 1:length_games) {
  print(paste("Playing Game:", i))
  flush.console()
  strategy1[[i]] <- play_random_game()
  strategy2[[i]] <- play_drlu_game()
  strategy3[[i]] <- play_40_30_20_10_game()
  strategy4[[i]] <- play_bruteforce1_game()
  strategy5[[i]] <- play_bruteforceSquare_game()
}

df1 <- data.frame(game = 1:length_games,
                  strategy = rep("random", length_games),
                  scores = unlist(lapply(strategy1, function(x) {x$score})),
                  max_tile = unlist(lapply(strategy1, function(x) {max(x$board)})),
                  n_moves = unlist(lapply(strategy1, function(x) {x$n_moves})))
df2 <- data.frame(game = 1:length_games,
                  strategy = rep("drlu strict", length_games),
                  scores = unlist(lapply(strategy2, function(x) {x$score})),
                  max_tile = unlist(lapply(strategy2, function(x) {max(x$board)})),
                  n_moves = unlist(lapply(strategy2, function(x) {x$n_moves})))
df3 <- data.frame(game = 1:length_games,
                  strategy = rep("drlu preferred 40-30-20-10", length_games),
                  scores = unlist(lapply(strategy3, function(x) {x$score})),
                  max_tile = unlist(lapply(strategy3, function(x) {max(x$board)})),
                  n_moves = unlist(lapply(strategy3, function(x) {x$n_moves})))
df4 <- data.frame(game = 1:length_games,
                  strategy = rep("brute_force_1", length_games),
                  scores = unlist(lapply(strategy4, function(x) {x$score})),
                  max_tile = unlist(lapply(strategy4, function(x) {max(x$board)})),
                  n_moves = unlist(lapply(strategy4, function(x) {x$n_moves})))
df5 <- data.frame(game = 1:length_games,
                  strategy = rep("brute_force_Square", length_games),
                  scores = unlist(lapply(strategy5, function(x) {x$score})),
                  max_tile = unlist(lapply(strategy5, function(x) {max(x$board)})),
                  n_moves = unlist(lapply(strategy5, function(x) {x$n_moves})))
df_for_plot <- rbind(df1, df2, df3, df4, df5)


ggplot(df_for_plot, aes(x = game, y = scores, group = strategy)) + facet_wrap(~ as.factor(strategy), dir = "v") + geom_line(aes(colour = as.factor(strategy)))
#ggplot(df_for_plot, aes(x = game, y = scores, group = strategy)) + geom_boxplot(aes(colour = as.factor(strategy)))
ggplot(df_for_plot, aes(x = game, y = n_moves, group = strategy)) + facet_wrap(~ as.factor(strategy), dir = "v") + geom_line(aes(colour = as.factor(strategy)))
#ggplot(df_for_plot, aes(x = game, y = n_moves, group = strategy)) + geom_boxplot(aes(colour = as.factor(strategy)))
ggplot(df_for_plot, aes(x = game, y = max_tile, group = strategy)) + geom_line(aes(colour = as.factor(strategy)))
#ggplot(df_for_plot, aes(x = game, y = max_tile, group = strategy)) + geom_boxplot(aes(colour = as.factor(strategy)))
ggplot(df_for_plot, aes(y = max_tile)) + facet_wrap( ~ as.factor(strategy)) + geom_bar(aes(fill = as.factor(strategy)))
