# This function
computeCost <- function(X, y, params) {
  # we get the number of training examples since dim X = nbr of Features x nbr of Examples
  m <- dim(X)[2]
  #extract the final output of the hypothesis function (second layer activation)
  A2 <- params$A2
  
  loss <- (log(A2) * y) + (log(1-A2) * (1-y))
  cost <- -sum(loss/m)
  
  return (cost)
}
cost <- computeCost(X_train, y_train, fwd_propagate)
