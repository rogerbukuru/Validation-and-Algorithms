rm(list = ls())
#========================================================================
#                 1. Set up a game universe (Encoding)
#========================================================================

set.seed(2023)
J       =  40                # J Objects/trees
o1      =  runif(J,-1,1)     # x-coordinates of objects
o2      =  runif(J,-0.5,1)   # y-coordinates of objects
objects =  cbind(o1,o2)
xt      =  cbind(runif(10,-0.8,0.8),-1)# 10 random starting positions
delt    = 0.05              # How big are the steps you can take?
rad     = 0.05              # How close to an object before you crash?

# Just a function to plot the game:
plot_game = function(xt,objects,rad,cols = 1)
{
  plot(objects[,2]~objects[,1], pch = 13, cex = 2, col = 'darkgreen', ylim = c(-1,1), xlim = c(-1,1),xlab ='South',ylab = 'West')
  mtext('North',3);mtext('East',4)
  points(xt[,2]~xt[,1], pch = 16, cex = 2,col = cols)
  abline(v = c(-1,1), lty = 3,col = 'black',lwd = 2)
  abline(h = c(-1,1), lty = 3,col = c('black','black'), lwd = 2)
}

plot_game(xt,objects,rad,'black')

#========================================================================
#                  2. Evaluating the game state
#========================================================================

game_status=function(xt,objects, rad)
{
  status    = rep(0,dim(xt)[1])
  min_dists = rep(0,dim(xt)[1])
  J         = dim(objects)[1]
  ones      = matrix(1,J,1)
  for(i in 1:dim(xt)[1])
  {
    min_dists[i] = min(sqrt(rowSums((objects-ones%*%xt[i,])^2)))
  }
  status = 1*(xt[,2]>1)-1*((xt[,1]< -1)|(xt[,1]> +1)|(xt[,2]< -1)|(min_dists<rad))
  ret = list(status = status)
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
  k           = 0    # Count how many steps
  xt          = x0   # Set the initial coordinate(s) for the drone.
  trajectories   = NULL
  # Check the game status:
  res_status  = game_status(xt,objects,rad)
  status      = res_status$status
  
  # Check which games are still active:
  terminal      = status!=0
  if(trace)
  {
     trajectories = array(dim = c(dim(xt)[1],dim(xt)[2],101))
  	 trajectories[,,1] = xt
  }
  if(plt){plot_game(xt,objects,rad,'black')}
  while((any(status==0))&(k<100))
  {
    k = k+1
    
    # Now, let the model update the position of the pieces:
    ct = control(xt,theta)
    xt = xt+ct*delt*cbind(1-terminal,1-terminal)

    # Checkk the game status after the positions are updates:
    res_status  = game_status(xt,objects,rad)
    status      = res_status$status
    terminal    = status!=0
    if(trace){trajectories[,,k] = xt}
    if(plt){plot_game(xt,objects,rad,c('red','black','green')[status+2])}
  }
  return(list(k = k, status = status,xt= xt,trajectories = trajectories))
}
theta_rand = runif(1)
#play(xt,delt,objects,rad,theta_rand,plt = TRUE,trace = FALSE)

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

   ones    = matrix(1,1,N)
   a0      = t(X)
   
   # Evaluate the updating equation in matrix form
   a1 = tanh(t(W1)%*%a0+b1%*%ones)
   a2 = tanh(t(W2)%*%a1+b2%*%ones)
   a3 = tanh(t(W3)%*%a2+b3%*%ones)

   # Return a list of relevant objects:
   return(list(a3 = t(a3)))
}

p     = 2
q     = 2
nodes = 5
npars = p*nodes+nodes*nodes+nodes*q+nodes+nodes+q
npars
theta_rand = runif(npars,-1,1)
res_net = model(xt,theta_rand,rep(nodes,2))
res_net

control = function(xt,pars)
{
  res_model = model(xt,pars,rep(nodes,2))
  return(res_model$a3)
}
control(xt,theta_rand)

# Now controled by NN with par vector theta_rand
#play(xt,delt,objects,rad,theta_rand,plt = TRUE,trace = FALSE)

#========================================================================
#                  4. Objectives and Fitness
#========================================================================

T_games = 10

play_T_games = function(theta)
{
  xt    = cbind(runif(T_games,-0.8,0.8),-1) 
  res   = play(xt,delt,objects,rad,theta)
  score = mean(res$status==1)
  return(score)
}
play_T_games(theta_rand)


#========================================================================
#                  5. Evolutionary Learning
#========================================================================

library('GA')
obj = play_T_games
GA <- ga(type= 'real-valued',obj,lower = rep(-10,npars),upper = rep(+10,npars),popSize = 100,maxiter = 100, keepBest = TRUE)
plot(GA)


theta_hat = GA@solution[1,]
theta_hat
 

xt_try    = cbind(runif(100,-0.8,0.8),-1)
play(xt_try,delt,objects,rad,theta_hat,TRUE,TRUE)

 
# Does it work?
xt_try    = cbind(runif(100,-0.8,0.8),-1)
res_final = play(xt_try,delt,objects,rad,theta_hat,TRUE,TRUE)
for(i in 1:dim(xt_try)[1])
{
  lines(res_final$trajectories[i,2,]~res_final$trajectories[i,1,], col = 'lightgrey')
}
