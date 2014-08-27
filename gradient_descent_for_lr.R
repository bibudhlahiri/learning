classify <- function()
{
  set.seed(1)
  x1 <- rnorm(100)
  x2 <- runif(100)
  df = data.frame(x1 = x1, x2 = x2)
  df$y = (df$x1 > 0 & df$x2 > 0.5)
  X <- as.matrix(df[, 1:2])
  X <- cbind(1, X)
  y <- as.vector(df$y)
  theta <- rep(0, 3)
  cost <- cost_function(X, theta, y)
  for (i in 1:100)
  {
    theta_old <- theta
    theta <- theta - gradient(X, theta_old, y)
    cost <- cost_function(X, theta, y)
    cat(paste("after i = ", i, ", theta = ", theta, "\n", sep = "")) 
    cat(paste("cost = ", cost, "\n", sep = ""))
  }
}


h_theta_x <- function(theta, x)
{
  return(1/(1 + exp(-(t(x) %*% theta))))
}

sigmoid <- function(alpha)
{
  cat(paste("length(alpha) = ", length(alpha), "\n", sep = ""))  
  ret_vector <- (1/(1 + exp(-alpha)))
  return(ret_vector)
}


cost_function <- function(X, theta, y)
{
  m <- length(y)
  X_theta <- X %*% theta
  sigmoid_X_theta <- c()
  for (i in 1:length(X_theta)
  {
    sigmoid_X_theta <- sigmoid(X_theta[i])
  }
  print(sigmoid_X_theta)
  #J = (-1/m)*(t(y) %*% log(sigmoid(X %*% theta)) + t(rep(1, m) - y) %*% log(rep(1, m) - sigmoid(X %*% theta)))
}

#Compute the gradient for the j-th component of theta
gradient <- function(X, theta, y)
{
  m <- length(y)
  grad = (1/m)*(t(sigmoid(X %*% theta) - y) %*% X)
}
