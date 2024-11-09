rm(list = ls())
#========================================================================
#                 1. Set up a game universe (Encoding)
#========================================================================

draw_objects = function(J)
{  
  r_o       =  runif(J,0.3,1)
  pi_vals   =  runif(J,-1,1)*pi
  o1        =  r_o*cos(pi_vals)     # x-coordinates of objects
  o2        =  r_o*sin(pi_vals)
  return(cbind(o1,o2))
}
draw_starts = function(N = 1)
{
  r_x      =  runif(N,0,0.25)
  pi_x     =  runif(N,-1,1)*pi
  xt       =  matrix(cbind(r_x*cos(pi_x),r_x*sin(pi_x)),nrow = N,byrow = TRUE)
}

set.seed(2024)
J       =  50  
objects = draw_objects(J)
xt      = draw_starts()
delt    = 0.025             # How big are the steps you can take?
rad     = 0.05              # How close to an object before you crash?

# Just a function to plot the game:
plot_game = function(xt,objects,rad,cols = 1)
{
  plot(objects[,2]~objects[,1],type = 'n', ylim = c(-1,1), xlim = c(-1,1))
  symbols(objects[,1],objects[,2],circles = rep(rad,J),ylim = c(-1,1),xlim = c(-1,1),inches = FALSE,add = TRUE,bg = 'seagreen')
  points(xt[,2]~xt[,1], pch = 16, cex = 1,col = cols)
  pi_v = seq(-1,1,1/100)*pi
  y_edge = sin(pi_v)
  x_edge = cos(pi_v)
  lines(y_edge~x_edge)
  lines(c(0.25*y_edge)~c(0.25*x_edge),lty = 2)
}

plot_game(xt,objects,rad,'black')
#========================================================================
#                  2. Evaluating the game state
#========================================================================

game_status=function(xt, objects, rad) {
  # Initialize the status and minimum distance vectors
  status    = rep(0, dim(xt)[1])
  min_dists = rep(0, dim(xt)[1])
  
  # Number of objects (obstacles)
  J = dim(objects)[1]
  
  # Create a matrix of ones for broadcasting in distance calculations
  ones = matrix(1, J, 1)
  
  # Calculate minimum distance to objects for each drone position
  for (i in 1:dim(xt)[1]) {
    min_dists[i] = min(sqrt(rowSums((objects - ones %*% xt[i,])^2)))
  }
  
  # Check for NA values in min_dists
  if (any(is.na(min_dists))) {
    cat("Found NA in min_dists:", min_dists, "\n")
  }
  
  # Update status: 1 for escape, -1 for crash, 0 otherwise
  status = 1 * (sqrt(rowSums(xt^2)) > 1) - 1 * (min_dists < rad)
  
  # Check for NA values in status
  if (any(is.na(status))) {
    cat("Found NA in status:", status, "\n")
  }
  
  # Compute 1 / (d_min - r) for each drone
  proximity_term = ifelse(min_dists > rad, 1 / (min_dists - rad), 100)
  
  
  # Return the status and minimum distances
  ret = list(status = status, dists = min_dists, proximity = proximity_term)
  return(ret)
}


# Just randomly move pieces for now:
#control = function(xt,theta)
#{
#  N_games = dim(xt)[1]
#  return(cbind(runif(N_games,-1,1),1))
#}

play = function(x0, delt, objects, rad, theta, plt = FALSE, trace = FALSE) {
  k = 0                      # Initialize step counter
  xt = x0                    # Set the initial positions of drones
  trajectories = NULL        # To store trajectories if needed
  
  # Check the initial game status
  res_status = game_status(xt, objects, rad)
  status = res_status$status
  terminal = status != 0     # Indicates if a game has ended
  
  # Initialize trajectories storage if tracing is enabled
  if (trace) {
    trajectories = array(dim = c(dim(xt)[1], dim(xt)[2], 101))
    trajectories[,,1] = xt
  }
  
  # Plot the initial game setup if plotting is enabled
  if (plt) { plot_game(xt, objects, rad, 'black') }
  
  while ((any(status == 0)) & (k < 100)) {
    k = k + 1
    
    # Use the control function to get movement direction
    ct = control(xt, theta)
    xt = xt + ct * delt * cbind(1 - terminal, 1 - terminal)  # Update positions
    
    # Update the game status after movement
    res_status = game_status(xt, objects, rad)
    status = res_status$status
    terminal = status != 0  # Update terminal status
    
    # Record trajectory if tracing is enabled
    if (trace) { trajectories[,,k] = xt }
  }
  
  # Plot final state if plotting is enabled
  if (plt) { plot_game(xt, objects, rad, c('red', 'black', 'green')[status + 2]) }
  
  # Return the results of the game
  return(list(k = k, status = status, xt = xt, trajectories = trajectories))
}



#========================================================================
#                  3. Control (Giving a Model Agency.)
#========================================================================

model = function(X, theta, nodes) {
  # Infer dimensions
  N = dim(X)[1]
  p = dim(X)[2]
  q = 2
  dims = c(p, nodes, q)
  
  # Populate weight and bias matrices
  index = 1:(dims[1] * dims[2])
  W1 = matrix(theta[index], dims[1], dims[2])
  index = max(index) + 1:(dims[2] * dims[3])
  W2 = matrix(theta[index], dims[2], dims[3])
  index = max(index) + 1:(dims[3] * dims[4])
  W3 = matrix(theta[index], dims[3], dims[4])
  
  index = max(index) + 1:(dims[2])
  b1 = matrix(theta[index], dims[2], 1)
  index = max(index) + 1:(dims[3])
  b2 = matrix(theta[index], dims[3], 1)
  index = max(index) + 1:(dims[4])
  b3 = matrix(theta[index], dims[4], 1)
  
  ones = matrix(1, 1, N)
  a0 = t(X)
  
  # Forward pass through network layers
  a1 = tanh(t(W1) %*% a0 + b1 %*% ones)
  a2 = tanh(t(W2) %*% a1 + b2 %*% ones)
  a3 = tanh(t(W3) %*% a2 + b3 %*% ones)
  
  # Return the output
  return(list(a3 = t(a3)))
}

p     = 3
q     = 2
nodes = 5
npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
npars
theta_rand = runif(npars,-1,1)

control = function(xt, theta) {
  
  res_status = game_status(xt, objects, rad)
  proximity_term = res_status$proximity
  X = cbind(xt, proximity_term)
  res_model = model(X, theta, rep(nodes,2))
  return(res_model$a3)  # The control output as movement directions
  
}

#========================================================================
#                  4. Objectives and Fitness
#========================================================================

play_a_game = function(theta) {
  # Draw a random starting position
  xt = draw_starts(1)
  # Play the game
  res = play(xt, delt, objects, rad, theta, plt = TRUE, trace = FALSE)
  # Return the final status
  score = res$status == 1  # Successful escape if status is 1
  return(score)
}

play_a_game(theta_rand)


#========================================================================
#                  5. Evolutionary Learning
#========================================================================

library('GA')

# Objective function for GA optimization
obj = play_a_game

# Run the GA
GA <- ga(type = 'real-valued', 
         fitness = obj, 
         lower = rep(-10, npars), 
         upper = rep(+10, npars), 
         popSize = 100, 
         maxiter = 200, 
         keepBest = TRUE)

# Plot the GA results
plot(GA)

# Get the optimized parameters
theta_hat = GA@solution[1,]

set.seed(2024)
# Draw 100 starting positions
xt_try = draw_starts(100)

# Run the game with optimized parameters
res_final = play(xt_try, delt, objects, rad, theta_hat, plt = TRUE, trace = TRUE)

# Calculate success rate
success_rate = mean(res_final$status == 1)

# Print the success rate
cat("Proportion of successful escapes:", success_rate, "\n")

# Plot trajectories
for (i in 1:dim(xt_try)[1]) {
  lines(res_final$trajectories[i,2,] ~ res_final$trajectories[i,1,], col = 'lightgrey')
}


set.seed(2024)
J = 50  
objects = draw_objects(J)

# Draw 100 random starting positions
xt_try = draw_starts(100)

# Use optimized theta_hat from the previous GA result
theta_hat = GA@solution[1,]

# Plot the new game setup with obstacles and starting points
plot_game(xt_try, objects, rad, 'black')

# Run the simulation for 100 games with trace enabled
res_final = play(xt_try, delt, objects, rad, theta_hat, plt = TRUE, trace = TRUE)

# Calculate and report the success rate (proportion of successful escapes)
success_rate = mean(res_final$status == 1)
cat("Proportion of successful escapes with new obstacles:", success_rate, "\n")

# Draw trace plots for each trajectory
for (i in 1:dim(xt_try)[1]) {
  lines(res_final$trajectories[i,2,] ~ res_final$trajectories[i,1,], col = 'lightgrey')
}




