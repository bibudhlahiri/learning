generate_data <- function()
{
  #Generate two clusters with centers at 
  library(MASS)
  Sigma <- matrix(c(0.02,0,0,0.02),2,2)
  cluster1 <- mvrnorm(n = 1000, rep(0, 2), Sigma)
  cluster2 <- mvrnorm(n = 1000, rep(2, 2), Sigma)
  normal <- data.frame(rbind(cluster1, cluster2))
  colnames(normal) <- c("col1", "col2")
  normal$type <- "normal"
  
  #Pick some random outliers with X between 0.5 and 1.5 and Y between 0.5 and 1.5
  n_outliers1 <- 7
  outliers1 <- cbind(runif(n_outliers1, 0.5, 1.5), runif(n_outliers1, 0.5, 1.5))
  #Pick some random outliers with X between -0.5 and 0.5 but Y between 0.5 and 2.5
  n_outliers2 <- 7
  outliers2 <- cbind(runif(n_outliers2, -0.5, 0.5), runif(n_outliers2, 0.5, 2.5))
  #Pick some random outliers with X between 0.5 and 2.5 but Y between -0.5 and 1.5
  n_outliers3 <- 7
  outliers3 <- cbind(runif(n_outliers3, 0.5, 2.5), runif(n_outliers3, -0.5, 1.5))
  outliers <- data.frame(rbind(outliers1, outliers2, outliers3))
  colnames(outliers) <- c("col1", "col2")
  outliers$type <- "outlier"
  
  all_data <- rbind(normal, outliers)
}

cluster_and_find_outliers <- function()
{
  all_data <- generate_data()
  n_all_data <- nrow(all_data)
  training <- sample(n_all_data, 0.7*n_all_data)
  trg_data <- all_data[training,]
  test_data <- all_data[-training,]
  #print(head(test_data))
  clusters <- kmeans(trg_data[, c("col1", "col2")], centers = 2)
  #For each point in test_data, check which cluster center it is nearest to and assign it to that cluster
  test_data$dist1 <- apply(test_data, 1, function(row)distance_to_center(c(as.numeric(row["col1"]), as.numeric(row["col2"])), clusters$centers[1,])) 
  test_data$dist2 <- apply(test_data, 1, function(row)distance_to_center(c(as.numeric(row["col1"]), as.numeric(row["col2"])), clusters$centers[2,]))
  test_data$cluster <- ifelse((test_data$dist1 <= test_data$dist2), 1, 2) 
  test_data$distance_to_centroid <- with(test_data, pmin(dist1, dist2))
  #test_data$cluster <- apply(test_data, 1, function(row)assign_to_cluster(c(as.numeric(row["col1"]), as.numeric(row["col2"])), clusters$centers)) 
  #Print the top N points whose distances to their nearest cluster centers are the highest
  test_data <- (test_data[order(-test_data$distance_to_centroid),])
}

distance_to_center <- function(point, cluster_center)
{
  sqrt(sum(as.numeric(point) - as.numeric(cluster_center))^2)
}

assign_to_cluster <- function(point, cluster_centers)
{
  dist1 <- sqrt(sum(as.numeric(point) - as.numeric(cluster_centers[1, ]))^2)
  dist2 <- sqrt(sum(as.numeric(point) - as.numeric(cluster_centers[2, ]))^2)
  ifelse((dist1 <= dist2), 1, 2)
}
