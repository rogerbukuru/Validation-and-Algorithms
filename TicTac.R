rm(list = ls())

#===============================================================================
# Populate the state matrix
#===============================================================================
S = matrix(0,9,8)
# Row Sums:
S[1:3,1] = 1
S[4:6,2] = 1
S[7:9,3] = 1
# Col Sums:
S[c(1:3)*3-2,4] = 1
S[c(1:3)*3-1,5] = 1
S[c(1:3)*3-0,6] = 1
# Diag Sums
S[c(1,5,9),7] = 1
S[c(3,5,7),8] = 1
S

#===============================================================================
# Evaluate the Game State rho(m,S)
#===============================================================================
rho = function(m,S)
{
  m = matrix(m,ncol = 1)
  player1  = any(t(m)%*%S==-3) # X
  player2  = any(t(m)%*%S==+3) # O
  m_1      = (m==-1) #m_
  m_2      = (m==+1) #m+
  tie      = sum((t(m_1)%*%S>0)*(t(m_2)%*%S>0))==8
  winner   = c(-1,0,1)[c(player1,tie,player2)]
  terminal = player1|tie|player2
  ret      = list(terminal = terminal,winner = winner)
  return(ret)
}

m = as.matrix(c(1,1,-1,0,0,0,0,0,-1))
matrix(m,3,3,byrow = TRUE)
rho(m,S)

m = as.matrix(c(1,1,-1,0,0,-1,0,0,-1)) 
matrix(m,3,3,byrow = TRUE)
rho(m,S)

#===============================================================================
# game_tree(m,k) returns a vector of -1, 0, and +1s delineating all terminal 
# game states.
#===============================================================================
game_tree = function(m, k, n_moves = 0, max_moves = 8)
{
  g = c()
  game_state = rho(m, S)
  
  # If we've reached the maximum number of moves or a terminal state, return the result
  if (game_state$terminal || n_moves >= max_moves) {
    g = c(g, game_state$winner)
    return(g)
  } else {
    # If the number of moves is still below the maximum, keep searching
    Index = which(m == 0)  # Find empty cells to make a move
    for (i in 1:length(Index)) {
      x = m
      x[Index[i]] = k  # Place the current player's token
      g = c(g, game_tree(x, -k, n_moves + 1, max_moves))  # Recurse with the next player
    }
    return(g)
  }
}
#m = as.matrix(c(0,0,0,0,0,0,0,0,0))
#m = as.matrix(c(1,1,-1,0,0,0,0,0,-1))



#res = game_tree(m,-1)
#n_g  = 	length(res)
#Xwins = sum(res==-1)
#Draws = sum(res== 0)
#Owins = sum(res==+1)
#c(n_g,Xwins,Draws,Owins)



# Now, use the function to answer the specific questions:

#m = as.matrix(c(0,0,0,0,0,0,0,0,0))  # Open Board
#m = as.matrix(c(1, 1, 0, 0, 0, 0, 0, 0, -1)) #2b
m = as.matrix(c(1, 1, -1, 0, 0, 0, 0, 0, -1)) 
max_moves = 8
result = game_tree(m, -1, 0, max_moves)
n_g  = 	length(result)
# Count results for wins and draws
Xwins = sum(result == -1)  # Count X wins
Draws = sum(result == 0)   # Count draws
Owins = sum(result == 1)   # Count O wins

c(n_g,Xwins, Draws, Owins)

#===============================================================================
# Write a function that draws the game tree.
#===============================================================================
draw_game_tree = function(m,k,x0 = 0,d = 0, lims = c(-1,1))
{
  g = c()
  game_state = rho(m,S)
  if(d == 0) # Initialise plot region
  {
    plot(1,1,type = 'n',xlim = lims, ylim = c(0,sum(m==0)+2),axes = FALSE)
    axis(2)
  }
  if(game_state$terminal)
  {
    text(x0,d,labels = c('X','-','O')[game_state$winner+2],cex = 0.5) # Placing terminal nodes
    g = c(g,game_state$winner)
    return(g)
  }else{
    Index = which(m == 0)
    
    x_seq = seq(lims[1],lims[2],length = length(Index)+1)
    x_mids = x_seq[-1]-0.5*diff(x_seq)
    for(i in 1:length(Index))
    {
      segments(x0,d,x_mids,d+1,col = 'grey')
      x = m 
      x[Index[i]]=k
      g = c(g,draw_game_tree(x,-1*k,x_mids[i],d+1,lims = c(x_seq[i],x_seq[i+1])))
    }
    return(g)
  }
}

#m = as.matrix(c(1,1,0,0,0,0,0,0,-1))
m = as.matrix(c(0,0,0,0,0,0,0,0,0))
matrix(m,3,3,byrow = TRUE)
res = draw_game_tree(m,-1)
n_g  = 	length(res)
Xwins = sum(res==-1)
Draws = sum(res== 0)
Owins = sum(res==+1)
c(n_g,Xwins,Draws,Owins)



#===============================================================================
# MCTS Function
#===============================================================================

mcts = function(m, k, alpha = 0.95, n_simulations = 100) {
  outcomes = matrix(0, nrow = n_simulations, ncol = 3)
  colnames(outcomes) = c("Xwins", "Draws", "Owins")
  
  for (sim in 1:n_simulations) {
    current_board = m
    current_player = k
    
    # Run a simulation of the game with random moves based on the given probability alpha
    while (TRUE) {
      game_state = rho(current_board, S)
      
      # Check if the game has reached a terminal state
      if (game_state$terminal) {
        if (game_state$winner == -1) {  # X wins
          outcomes[sim, "Xwins"] = 1
        } else if (game_state$winner == 1) {  # O wins
          outcomes[sim, "Owins"] = 1
        } else {  # Draw
          outcomes[sim, "Draws"] = 1
        }
        break
      }
      
      # Get empty spots
      empty_spots = which(current_board == 0)
      
      # If there are empty spots, choose the next move with probability alpha
      if (length(empty_spots) > 0) {
        if (runif(1) < alpha) {
          # Explore: Choose a random move
          move = sample(empty_spots, 1)
        } else {
          # Exploit: Choose a deterministic move (for simplicity, we still randomly choose here)
          move = sample(empty_spots, 1)
        }
        
        # Make the move
        current_board[move] = current_player
        current_player = -current_player  # Switch player
      }
    }
  }
  
  # Return the relative frequencies of each outcome
  outcome_frequencies = colMeans(outcomes)
  return(outcome_frequencies)
}

#===============================================================================
# Simulate the MCTS with the given board state
#===============================================================================
m = as.matrix(c(1, 1, 0, 0, 0, 0, 0, 0, -1))  # Board state [1, 1, 0, 0, 0, 0, 0, 0, -1]
alpha = 0.95
n_simulations = 1000

# Run MCTS simulation
relative_frequencies = mcts(m, -1, alpha, n_simulations)

relative_frequencies


library(ggplot2)

# Define the board state and set parameters
m = as.matrix(c(1, 1, -1, 0, 0, 0, 0, 0, -1))  # Board state [1, 1, -1, 0, 0, 0, 0, 0, -1]
n_simulations = 100
n_games = 100

# Run simulations for alpha = 0.9
relative_frequencies_alpha_09 = replicate(n_games, mcts(m, -1, 0.9, n_simulations = n_simulations))

# Run simulations for alpha = 0.7
relative_frequencies_alpha_07 = replicate(n_games, mcts(m, -1, 0.7, n_simulations = n_simulations))

# Convert the results to data frames for easier plotting
alpha_09_df = data.frame(t(relative_frequencies_alpha_09))
alpha_07_df = data.frame(t(relative_frequencies_alpha_07))

# True values for comparison
true_values = c(Xwins = 53/73, Draws = 12/73, Owins = 8/73)

# Create a new data frame for ggplot
df = data.frame(
  Xwins = c(alpha_09_df$Xwins, alpha_07_df$Xwins),
  Draws = c(alpha_09_df$Draws, alpha_07_df$Draws),
  Owins = c(alpha_09_df$Owins, alpha_07_df$Owins),
  alpha = factor(rep(c("Alpha = 0.9", "Alpha = 0.7"), each = n_simulations))
)

# Reshape the data for ggplot
df_long = reshape(df, 
                  varying = c("Xwins", "Draws", "Owins"), 
                  v.names = "value", 
                  timevar = "outcome", 
                  times = c("Xwins", "Draws", "Owins"), 
                  direction = "long")

# Labels for true values
true_labels = data.frame(
  outcome = c("Xwins", "Draws", "Owins"),
  true_value = c(true_values[1], true_values[2], true_values[3]),
  label = c("Xwins = 0.7260", "Draws = 0.1644", "Owins = 0.1096")
)

# Draw the boxplots
ggplot(df_long, aes(x = outcome, y = value, fill = as.factor(alpha))) +
  geom_boxplot() +
  scale_fill_manual(values = c("Alpha = 0.7" = "red", "Alpha = 0.9" = "turquoise")) +  # Explicit color mapping
  geom_hline(data = true_labels, aes(yintercept = true_value), color = "red", linetype = "dashed") +  # True value lines
  geom_text(data = true_labels, aes(x = outcome, y = true_value, label = label), 
            color = "black", vjust = -0.5, size = 4) +  # Adjusted text placement for clarity
  labs(title = "Boxplots of MCTS Relative Frequencies (Alpha = 0.9 vs Alpha = 0.7)", 
       x = "Outcome", 
       y = "Relative Frequency", 
       fill = "Alpha") +  # Adjusted legend title
  theme_minimal()
