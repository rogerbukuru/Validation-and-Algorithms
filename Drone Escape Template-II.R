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

game_status=function(xt,objects, rad)
{
  status    = 
  min_dists = 
  J         = 
  ones      = 
  for(i in 1:dim(xt)[1])
  {
    min_dists[i] = 
  }
  status = 
  ret = list(status = status,dists = min_dists) # You can return min dists
  return(ret)
}

# Just randomly move pieces for now:
control = function(xt,theta)
{
  N_games = dim(xt)[1]
  return(cbind(runif(N_games,-1,1),1))
}

play = function(x0,delt,objects,rad,theta,plt = FALSE,trace = FALSE)
{
  k           =    # Count how many steps
  xt          =    # Set the initial coordinate(s) for the drone.
  trajectories = NULL
  # Check the game status:
  res_status  = 
  status      = 
  # Check which games are still active:
  terminal      = 
  if(trace)
  {
     trajectories = array(dim = c())
  	 trajectories[,,1] = xt
  }
  if(plt){plot_game(xt,objects,rad,'black')}
  while((any(status==0))&(k<100))
  {
    k = 
    
    # Now, let the model update the position of the pieces:
    
    ct = 
    xt = xt+ct*delt*cbind(1-terminal,1-terminal)

    # Checkk the game status after the positions are updates:
    res_status  = 
    status      = 
    terminal    = 
    if(trace){trajectories[,,k] = xt}
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
   index = max(index)+1:(dims[3]*dims[4])
   W3    = matrix(theta[index],dims[3],dims[4])

   index = max(index)+1:(dims[2])
   b1    = matrix(theta[index],dims[2],1)
   index = max(index)+1:(dims[3])
   b2    = matrix(theta[index],dims[3],1)
   index = max(index)+1:(dims[4])
   b3    = matrix(theta[index],dims[4],1)

   ones    = 
   a0      = 
   
   # Evaluate the updating equation in matrix form
   a1 = tanh()
   a2 = tanh()
   a3 = tanh()

   # Return a list of relevant objects:
   return(list(a3 = t(a3)))
}

p     = 2
q     = 2
nodes = 3
npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
npars
theta_rand = runif(npars,-1,1)

control = function(xt,pars)
{

  return()
}



#========================================================================
#                  4. Objectives and Fitness
#========================================================================

play_a_game = function(theta)
{

  return(score)
}
play_a_game(theta_rand)


#========================================================================
#                  5. Evolutionary Learning
#========================================================================

library('GA')
obj = play_a_games
GA  = 
plot(GA)


theta_hat = GA@solution[1,]
theta_hat
 


 


