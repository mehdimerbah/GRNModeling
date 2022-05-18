#Get dimensions for the weight matrices by defining the number of neurons in every layer

#Let's make a fucntion to get us layer sizes for a network


getLayerSize <- function(X, y, hidden_neurons, train=TRUE){
  # Number of units in the input layer
  n_x <- dim(X)[1]
  # Number of hidden neurons
  n_h <- hidden_neurons
  # Number of output neurons
  n_y <- dim(y)[1]
  
  
  size <- list("n_x" = n_x,
               "n_h" = n_h,
               "n_y" = n_y)
  
  return(size)
}

layer_size <- getLayerSize(X_train, y_train, hidden_neurons = 4)

#Initializing the parameters randomly before the optimization
#Let's make a function to give a random set of parameters to start with and then run a forward and backpropagation to otpimize

initializeParameters <- function(X, list_layer_size){
  
  m <- dim(data.matrix(X))[2]
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  ## Initializing the parameters to random values before we start propagation
  ## runif is used to generate uniformly distributed random deviations
  ## rep function basically replicates elements of the matrix
  W1 <- matrix(runif(n_h * n_x), nrow = n_h, ncol = n_x, byrow = TRUE)  
  b1 <- matrix(rep(0, n_h), nrow = n_h)
  W2 <- matrix(runif(n_y * n_h), nrow = n_y, ncol = n_h, byrow = TRUE) 
  b2 <- matrix(rep(0, n_y), nrow = n_y)
  
  params <- list("W1" = W1,
                 "b1" = b1, 
                 "W2" = W2,
                 "b2" = b2)
  
  return (params)
  
}

init_params <- initializeParameters(X_train, layer_size)
lapply(init_params, function(x) dim(x))

init_params$b1

# Defining the activation function, we will use sigmoid
sigmoid <- function(x){
  return(1 / (1 + exp(-x)))
}









