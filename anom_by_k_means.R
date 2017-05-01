generate_data <- function()
{
  #Generate two clusters with centers at 
  library(MASS)
  Sigma <- matrix(c(0.02,0,0,0.02),2,2)
  cluster1 <- mvrnorm(n = 1000, rep(0, 2), Sigma)
  cluster2 <- mvrnorm(n = 1000, rep(2, 2), Sigma)
  normal <- rbind(cluster1, cluster2)
  #Pick some random outliers with X between 0.5 and 1.5 and Y between 0.5 and 1.5
  n_outliers1 <- 7
  outliers1 <- cbind(runif(n_outliers1, 0.5, 1.5), runif(n_outliers1, 0.5, 1.5))
  #Pick some random outliers with X between -0.5 and 0.5 but Y between 0.5 and 2.5
  n_outliers2 <- 7
  outliers2 <- cbind(runif(n_outliers2, -0.5, 0.5), runif(n_outliers2, 0.5, 2.5))
  #Pick some random outliers with X between 0.5 and 2.5 but Y between -0.5 and 1.5
  n_outliers3 <- 7
  outliers3 <- cbind(runif(n_outliers3, 0.5, 2.5), runif(n_outliers3, -0.5, 1.5))
  all_data <- rbind(normal, outliers1, outliers2, outliers3)
  plot(all_data)
  all_data
}

cluster_and_find_outliers <- function()
{
  all_data <- generate_data()
  n_all_data <- nrow(all_data)
  training <- sample(n_all_data, 0.7*n_all_data)
  trg_data <- all_data[training,]
  test_data <- all_data[-training,]
  clusters <- kmeans(trg_data, centers = 2)
}
