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

game_status <- function(xt, objects, rad) {
  status <- rep(0, nrow(xt))         # Initialize status as 0 (ongoing) for each drone
  min_dists <- numeric(nrow(xt))      # Array to store minimum distances to any object
  forest_radius <- 1                  # Radius of the forest boundary
  J <- nrow(objects)                  # Number of objects (trees)
  ones <- rep(1, J)                   # Vector of ones for easy distance calculations
  
  for (i in 1:nrow(xt)) {
    # Calculate the distance to the center to check if the drone has escaped
    distance_to_center <- sqrt(sum(xt[i, ]^2))
    if (distance_to_center > forest_radius) {
      status[i] <- 1                  # Set status to 1 if the drone has escaped
      min_dists[i] <- distance_to_center
    } else {
      # Calculate distances to each object
      distances_to_objects <- sqrt(rowSums((objects - xt[i, ])^2))
      min_dists[i] <- min(distances_to_objects)  # Store the minimum distance to an object
      # Check if the drone has crashed into any object
      if (any(distances_to_objects <= rad)) {
        status[i] <- -1               # Set status to -1 if a crash is detected
      }
    }
  }
  
  # Return the game status and minimum distances
  ret <- list(status = status, dists = min_dists) # You can return min dists
  return(ret)
}


# Just randomly move pieces for now:
#control = function(xt,theta)
#{
#  N_games = dim(xt)[1]
#  return(cbind(runif(N_games,-1,1),1))
#}

play = function(x0,delt,objects,rad,theta,plt = FALSE,trace = FALSE)
{
  k           =  0    # Count how many steps
  xt          =  x0  # Set the initial coordinate(s) for the drone.
  trajectories = NULL
  # Check the game status:
  res_status  = game_status(xt, objects, rad)
  status      = res_status$status
  # Check which games are still active:
  terminal      = (status !=0)
  if(trace)
  {
     trajectories = array(dim = c(nrow(xt),2, 100))
  	 trajectories[,,1] = xt
  }
  if(plt){plot_game(xt,objects,rad,'black')}
  while((any(status==0))&(k<100))
  {
    k = k + 1
    
    # Now, let the model update the position of the pieces:
    
    ct = control(xt, theta)
    xt = xt+ct*delt*cbind(1-terminal,1-terminal)


    # Checkk the game status after the positions are updates:
    res_status  = game_status(xt, objects, rad)
    status      = res_status$status
    terminal    = (status != 0)
    if(trace){trajectories[,,k] = xt}
  }
  if(trace && k < 100) {
    for(j in (k+1):100) {
      trajectories[,,j] <- xt
    }
  }
  if(plt){plot_game(xt,objects,rad,c('red','black','green')[status+2])}
  return(list(k = k, status = status,xt= xt,trajectories = trajectories))
}


#========================================================================
#                  3. Control (Giving a Model Agency.)
#========================================================================

model = function(X,theta,nodes)
{
  # Infer dimensions:
   N = dim(X)[1]
   p = dim(X)[2]
   q = 2
   dims = c(p,nodes,q)
  
   # Populate weight and bias matrices:
   index = 1:(dims[1]*dims[2])
   W1    = matrix(theta[index],dims[1],dims[2])
   index = max(index)+1:(dims[2]*dims[3])
   W2    = matrix(theta[index],dims[2],dims[3])
   #index = max(index)+1:(dims[3]*dims[4])
   #W3    = matrix(theta[index],dims[3],dims[4])

   index = max(index)+1:(dims[2])
   b1    = matrix(theta[index],dims[2],1)
   index = max(index)+1:(dims[3])
   b2    = matrix(theta[index],dims[3],1)
   #index = max(index)+1:(dims[4])
   #b3    = matrix(theta[index],dims[4],1)

   ones    = matrix(1, N, 1)
   a0      = X
   
   # Evaluate the updating equation in matrix form
   a1 = tanh(a0 %*% W1 + ones %*% t(b1))
   a2 = tanh(a1 %*% W2 + ones %*% t(b2))
   #a3 = tanh(a2 %*% W3 + ones %*% t(b3))

   # Return a list of relevant objects:
   return(a2)
}

p     = 2
q     = 2
nodes = 3
npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
npars
theta_rand = runif(npars,-1,1)

control = function(xt,theta)
{
  model_output = model(xt, theta, nodes = 3)
  return(model_output)
}



#========================================================================
#                  4. Objectives and Fitness
#========================================================================

play_a_game <- function(theta) {
  xt <- draw_starts(1)            # Initial starting position
  delta <- 0.025                  # Step size
  max_steps <- 100                # Maximum allowed steps
  score <- 0                      # Initialize score
  status <- 0                     # Game status (0 = ongoing, 1 = escaped, -1 = crashed)
  
  # Game loop
  for (k in 1:max_steps) {
    if (status != 0) break        # Stop if escaped or crashed
    
    # Get control output from the model
    control_move <- control(xt, theta)
    
    # Update position
    xt <- xt + control_move * delta
    
    # Check the game status after moving
    game_res <- game_status(xt, objects, rad)
    status <- game_res$status
    
    # Update score: +1 for escape, -1 for crash, 0 otherwise
    if (status == 1) {
      score <- 1
      break
    } else if (status == -1) {
      score <- -1
      break
    }
  }
  
  return(score)
}

play_a_game(theta_rand)


#========================================================================
#                  5. Evolutionary Learning
#========================================================================

library('GA')
obj = function(theta) {
  return(play_a_game(theta))  # Maximize score
}
npars = 2 * 3 + 3 * 2 + 3 + 2  # Number of parameters for (3,3)-network

GA = ga(
  type = "real-valued",
  fitness = obj,
  min = rep(-1, npars),  # Lower bound for theta
  max = rep(1, npars),   # Upper bound for theta
  popSize = 100,
  maxiter = 200
)

plot(GA)


theta_hat = GA@solution[1,]
theta_hat

xt_try = draw_starts(100)

successful_escapes = 0
trajectories       = array(dim=c(100, 100,2))


for( i in 1:100){
  x0 <- matrix(xt_try[i, ], nrow = 1, ncol = 2)
  game_result = play(x0, delt, objects, rad, theta_hat, trace = TRUE) 
  trajectories[i,,] = game_result$trajectories[,,]
  # Count successful escapes
  if (game_result$status == 1) {
    successful_escapes = successful_escapes + 1
  }
  
}

# Calculate the proportion of successful navigations
success_rate <- successful_escapes / 100
cat("Proportion of successful navigations:", success_rate, "\n")

# Plot the initial game setup
plot_game(draw_starts(1), objects, rad, 'black')

# Loop over each trajectory and plot only up to the recorded number of steps
for (i in 1:100) {
  # Get the number of steps taken in the i-th game
  steps_taken <- sum(!is.na(trajectories[i, 1, ]))
  
  # Get the final status of the game from the last recorded position
  final_status <- game_status(matrix(trajectories[i, , steps_taken], nrow = 1), objects, rad)$status
  color <- ifelse(final_status == 1, "green", "red")  # Green for escape, red for crash
  
  # Plot the trajectory up to the last recorded step
  lines(trajectories[i, 1, 1:steps_taken], trajectories[i, 2, 1:steps_taken], col = color)
}



 


