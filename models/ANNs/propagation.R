
forwardPropagation <- function(X, params, list_layer_size){
  
  m <- dim(X)[2] # nbr of training examples
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  
  W1 <- params$W1
  b1 <- params$b1
  W2 <- params$W2
  b2 <- params$b2
  
  # Parameter update should be simultaneous with each propagation
  ## This is where we compute the resulting terms (weights and bias) as we go from layer to the next
  
  b1_new <- matrix(rep(b1, m), nrow = n_h)
  b2_new <- matrix(rep(b2, m), nrow = n_y)
  
  ## (m * n) x (n * k) => m*k 
  ## INPUT ===> HIDDEN LAYER [ 2 ==> 4 ]
  ## Z1 = input for first layer activation
  ## Z1: matrix_mult product of weight matrix and feature matrix + bias
  ## Vectorized implementation defines bias matrix for matrix addition
  Z1 <- W1 %*% X + b1_new
  A1 <- sigmoid(Z1)
  
  ## HIDDEN LAYER ===> OUTPUT [ 4 ==> 1 ]
  ## A2 is the output of the hypothesis function
  Z2 <- W2 %*% A1 + b2_new
  A2 <- sigmoid(Z2)
  
  ##NOTE: Sigmoid does not affect the dimensions, used to introduce non-linearity
  ## TBD: Try mix of tanh and sigmoid
  
  ## We return all of the calculated values to use them for backPropagation
  activation_values <- list("Z1" = Z1,
                     "A1" = A1, 
                     "Z2" = Z2,
                     "A2" = A2)
  
  return (activation_values)
}

backPropagation <- function(X, y, activations, params, list_layer_size){
  
  m <- dim(X)[2]
  
  n_x <- list_layer_size$n_x
  n_h <- list_layer_size$n_h
  n_y <- list_layer_size$n_y
  ### We start from the end and work back
  A2 <- activations$A2
  A1 <- activations$A1 
  W2 <- params$W2
  
  dZ2 <- A2 - y
  dW2 <- 1/m * (dZ2 %*% t(A1)) 
  db2 <- matrix(1/m * sum(dZ2), nrow = n_y)
  db2_new <- matrix(rep(db2, m), nrow = n_y)
  
  dZ1 <- (t(W2) %*% dZ2) * (1 - A1^2)
  dW1 <- 1/m * (dZ1 %*% t(X))
  db1 <- matrix(1/m * sum(dZ1), nrow = n_h)
  db1_new <- matrix(rep(db1, m), nrow = n_h)
  
  
  grads <- list("dW1" = dW1, 
                "db1" = db1,
                "dW2" = dW2,
                "db2" = db2)
  
  return(grads)
}
