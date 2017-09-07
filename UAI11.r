#Implementation of independence test from UAI11
unconditional_independence <- function(x, y, n)
{
  library(Hmisc)
  #y = x half of the time and y = -x half of the time
  print(hoeffd(x,y)) #D ranges from -0.5 to 1.0. 
  #The higher the value of D, the more dependent are x and y. 
  library(energy)
  print(dcov.test(x, y, R = 199))
  cat(paste("Distance correlation = ", dcor(x, y, index = 1.0),  "\n", sep = ""))
  library(kernlab)
  rbf <- rbfdot(sigma = 0.5)
  K_X <- kernelMatrix(rbf, x)
  K_Y <- kernelMatrix(rbf, y)
  H <- diag(n) - (1/n)*rep(1,n)%*%t(rep(1,n))
  K_X_tilde <- H%*%K_X%*%H
  K_Y_tilde <- H%*%K_Y%*%H
  library(psych)
  T_UI <- (1/n)*tr(K_X_tilde%*%K_Y_tilde)
  cat(paste("T_UI = ", T_UI, "\n", sep = ""))
  lambda_x <- sort(eigen(K_X_tilde)$values, decreasing = TRUE)
  lambda_y <- sort(eigen(K_Y_tilde)$values, decreasing = TRUE)
  sample_size_asymp_distn <- 500
  sample_points <- c()
  for (k in 1:sample_size_asymp_distn)
  {
   sample_value <- 0
   for (i in 1:n)
   {
    for (j in 1:n)
	{
	  z_i_j <- rnorm(1)
	  sample_value <- sample_value + lambda_x[i]*lambda_y[j]*(z_i_j^2)
	}
   }
   sample_value <- (1/(n^2))*sample_value
   #cat(paste("sample_value = ", sample_value, "\n", sep = ""))
   sample_points <- c(sample_points, sample_value)
  }
  Fn <- ecdf(sample_points)
  1 - Fn(T_UI)
}


example_1 <- function()
{
  set.seed(1)
  n <- 200
  x <- seq(-10, 10, length= n)
  y <- x*sign(runif(n, -1, 1))
  #y is generated from x, however, D = 0.06, which is very low.
  plot(x,y)
  p_value <- unconditional_independence(x, y, n)
  cat(paste("p_value = ", p_value, "\n", sep = "")) #T_UI = 6.76810937921045, p_value = 0 for UAI11 test,  
  #p-value = 0.005 for distance-covariance test of independence.
  #Distance correlation = 0.264881064513878.
  #The D-value is 0.06, which is very low. 
}

example_2 <- function()
{
  set.seed(1)
  n <- 200
  x <- seq(-10, 10, length= n)
  y <- sign(runif(n, -1, 1))
  #x and y are generated independently, D = 0.
  plot(x,y)
  p_value <- unconditional_independence(x, y, n)
  cat(paste("p_value = ", p_value, "\n", sep = "")) #T_UI = 0.309492064468975, p_value = 0.582 for UAI11 test,
  #p-value = 0.39 for distance-covariance test of independence.
  #Distance correlation = 0.0834150745278777
}

example_2()

#source("C:\\Users\\blahiri\\learning\\UAI11.r")