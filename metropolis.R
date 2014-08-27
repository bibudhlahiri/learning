mh_sim <- function(std_dev = 0.05)
{
  set.seed(1)
  n <- 1000
  x <- rep(0, n)
  x[1] <- 0.5
  n_jumps <- 0
  for (i in 1:(n-1))
  {
    x_c <- rnorm(1, mean = x[i], sd = std_dev)
    r <- runif(1)
    numerator <- dnorm(x_c, mean = 0, sd = 1)
    denom <- dnorm(x[i], mean = 0, sd = 1)
    ratio <- numerator/denom
    cat(paste("x[i] = ", x[i], ", x_c = ", x_c, ", numerator = ", numerator, ", denom = ", denom, ", r = ", r, ", ratio = ", ratio, "\n", sep = ""))
      
    if (r < min(1, ratio))
    {
      x[i+1] <- x_c
      cat("Jumping to x_c\n")
      n_jumps <- n_jumps + 1
    }
    else
    {
      x[i+1] <- x[i]
      cat("Staying at x_i\n")
    }
  }
  cat(paste("Fraction of cases where jumped = ", n_jumps/(n-1), "\n", sep = ""))
  x  
}
