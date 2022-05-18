trainModel <- function(X, y, num_iteration, hidden_neurons, lr){
  
  layer_size <- getLayerSize(X, y, hidden_neurons)
  init_params <- initializeParameters(X, layer_size)
  cost_history <- c()
  
  for (i in 1:num_iteration) {
  # First forward propagate with the random set of parameters and get initial output
    fwd_prop <- forwardPropagation(X, init_params, layer_size)
    # compute the cost of the forward propagation
    cost <- computeCost(X, y, fwd_prop)
  # Run a backpropagation to better estimate parameters 
    back_prop <- backwardPropagation(X, y, fwd_prop, init_params, layer_size)
  # update the parameters using the list of gradients  
    update_params <- updateParameters(back_prop, init_params, learning_rate = lr)
    init_params <- update_params
    # variable to track the change in cost (loss difference with every epoch)
    cost_history <- c(cost_history, cost)
    # print the cost every 10000th epoch
    if (i %% 10000 == 0) cat("Iteration", i, " | Cost: ", cost, "\n")
  }
  
  # return the final list of parameters and list of costs changing with every epoch
  model_out <- list("updated_params" = update_params,
                    "cost_hist" = cost_history)
  return (model_out)
}