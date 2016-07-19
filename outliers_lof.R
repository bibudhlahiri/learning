library(data.table)
library(cluster)

elem_idx <- 1 
row_idx <- 1
curr_row <- 1
curr_idx_among_outliers <- 1

load_avalon_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\From_Gopal\\Avalon_Alignment_Data_60_MO_LO_20_MO_Settlement.csv"
  avalon_data <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                      colClasses = c("Date", "character", "character", "Date", "character",
                                     "numeric", "character", "numeric", "character", "numeric", 
                                     "numeric", "character", "character", "character", "character",
                                     "character", "character", "character", "numeric", "character",
                                     "character", "character", "character", "character", "character", 
                                     "character", "character", "character", "character", "character",
                                     "numeric", "character", "numeric", "numeric", "numeric",
                                     "character", "character", "character", "character", "numeric",
                                     "character", "character", "numeric", "character", "character",
                                     "character"),
                      data.table = TRUE)
  cols <- c("REPAIR STATE CODE NAME", "REG STATE CODE NAME", "MILEAGE (mile)", "WAIS CODE NAME", "PARTS NO. NAME", 
            "T1 CODE NAME", "T2 CODE NAME", "FUNCTION CODE NAME", "F.DIST", "COUNTRY/DIST CODE NAME",
            "DIST SETTLE. AMT - G.TOT (USD)", "DIST SETTLE. AMT - PARTS TOT (USD)", 
            "TIRE MAKER", "ENG PLANT CODE NAME", "ENG TYPE")
  avalon_data <- avalon_data[, .SD, .SDcols = cols]
  cols <- c("REPAIR STATE CODE NAME", "REG STATE CODE NAME", "WAIS CODE NAME", "PARTS NO. NAME", 
            "T1 CODE NAME", "T2 CODE NAME", "FUNCTION CODE NAME", "COUNTRY/DIST CODE NAME",
            "TIRE MAKER", "ENG PLANT CODE NAME", "ENG TYPE")
  avalon_data[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
  #With space in column names, refer to them as 
  #avalon_data[['REPAIR STATE CODE NAME']] or avalon_data[, `REPAIR STATE CODE NAME`]
  avalon_data
}

find_lof_outliers <- function()
{
  avalon_data <- load_avalon_data()
  pairwise_dist <- as.matrix(daisy(avalon_data, "gower"))
  
  k <- 10
  nearest_neighbors <- t(apply(t(pairwise_dist),2,sort))
  
  k_distances <- nearest_neighbors[, (k + 1)] #Holds the distance to the k-th NN for each data point
  print(names(sort(k_distances, decreasing = TRUE))[1:15]) #"3"   "92"  "548" "835" "401" "5"   "118" "930" "6"   "261" "2"   "977" "904" "982" "9"
  
  reachability_distances <- matrix(mapply(function(x, j) max(k_distances[j], x), pairwise_dist, col(pairwise_dist)),
                                   nrow = nrow(pairwise_dist)) 

  #Find the number of neighbors for each point whose distance from the given point is at most k_distance for that point.
  #May be more than k if there are ties.
  k_neighborhood_sizes <- apply(pairwise_dist, 1, 
                                function(x) find_k_neighborhood_size(x, k_distances))
  local_reachability_densities <- apply(pairwise_dist, 1, 
                                        function(x) find_local_reachability_density(x, k_distances, 
                                                                                    reachability_distances, 
                                                                                    k_neighborhood_sizes))
  local_outlier_factors <- apply(pairwise_dist, 1, 
                                 function(x) find_local_outlier_factor(x, k_distances, local_reachability_densities, 
                                                                       k_neighborhood_sizes))
  cat("Fivenum of local_outlier_factors is\n")
  print(fivenum(local_outlier_factors))
  rd_from_kNNs_for_outliers <- analyze_outliers(local_outlier_factors, k_distances, pairwise_dist, reachability_distances)
}

find_k_neighborhood_size <- function(all_neighbors, k_distances)
{
  ret_value <- length(all_neighbors[(all_neighbors <= k_distances[elem_idx])]) - 1
  elem_idx <<- elem_idx + 1
  ret_value
}

find_local_reachability_density <- function(all_neighbors, k_distances, reachability_distances, k_neighborhood_sizes)
{
  #Find the indices of the k nearest neighbors of the current data point, and pull the reachability distance of the 
  #current element from those neighbors.
  kNN_indices <- which(all_neighbors <= k_distances[row_idx])
  kNN_indices <- kNN_indices[kNN_indices != row_idx]
  kNN_indices <- as.numeric(kNN_indices)
  
  sum_reachability_distances <- sum(reachability_distances[row_idx, kNN_indices])
  ret_value <- k_neighborhood_sizes[row_idx]/sum_reachability_distances
  row_idx <<- row_idx + 1
  ret_value
}

find_local_outlier_factor <- function(all_neighbors, k_distances, local_reachability_densities, k_neighborhood_sizes)
{
  kNN_indices <- which(all_neighbors <= k_distances[curr_row])
  kNN_indices <- kNN_indices[kNN_indices != curr_row]
  kNN_indices <- as.numeric(kNN_indices)
  
  sum_lrd <- sum(local_reachability_densities[kNN_indices])
  lrd_curr_point <- local_reachability_densities[curr_row]
  ret_value <- sum_lrd/(k_neighborhood_sizes[curr_row]*lrd_curr_point)
  curr_row <<- curr_row + 1
  ret_value
}

#Take the outliers (LOF value above a threshold) and get an explanation of why they are outliers. 
#Pick their reachability distance values from their k nearest neighbors, and these should be 
#in general higher than the average/median values since the outliers are on average "far" from their 
#nearest neighbors compared to normal points. Confirm that the reachability distance values are high because 
#the outliers are far away from their neighbors (rd_k(A,B) = max(k_distance(B), d(A, B))) and check 
#what makes the outliers far from their k nearest neighbors.
analyze_outliers <- function(local_outlier_factors, k_distances, pairwise_dist, reachability_distances)
{
  lof_threshold <- 1.8
  outlier_indices <- as.numeric(which(local_outlier_factors > lof_threshold))
  
  cat("outlier_indices\n")
  print(outlier_indices) #With lof_threshold = 1.8, outlier_indices: 1   2   9  28 118 194 261 311 548 594 645 835 930 977 982
   
  #Get the distance to the k-th NN for all outliers
  k_distances_for_outliers <- k_distances[outlier_indices]
  
  print(fivenum(k_distances))
  print(fivenum(k_distances_for_outliers))
  
  #Get the indices of the k-th NNs for all outliers from pairwise_dist matrix
  pairwise_dist_for_outliers <- pairwise_dist[outlier_indices,]
  reachability_distances_for_outliers <- reachability_distances[outlier_indices,]
  
  rd_from_kNNs_for_outliers <- apply(pairwise_dist_for_outliers, 1, 
                                function(x) get_rd_from_kNNs_for_outliers(x, outlier_indices, k_distances_for_outliers, reachability_distances_for_outliers))
    
  cat("fivenum for rd_from_kNNs_for_outliers is\n")
  print(fivenum(rd_from_kNNs_for_outliers))
  cat("fivenum for general reachability_distances is\n")
  print(fivenum(reachability_distances))
  rd_from_kNNs_for_outliers
}

#For a given outlier, gets its reachability distance values from its k NNs. So returns k numbers for an outlier.
get_rd_from_kNNs_for_outliers <- function(outliers_neighbors, outlier_indices, k_distances_for_outliers, reachability_distances_for_outliers)
{
  kNN_indices <- which(outliers_neighbors <= k_distances_for_outliers[curr_idx_among_outliers])
  
  #The original index of the given outlier should be removed from kNN_indices. As we need the original index, we need to 
  #pull it from outlier_indices.
  kNN_indices <- kNN_indices[kNN_indices != outlier_indices[curr_idx_among_outliers]]
  kNN_indices <- as.numeric(kNN_indices)  
  
  ret_vector <- reachability_distances_for_outliers[curr_idx_among_outliers, kNN_indices]
  curr_idx_among_outliers <<- curr_idx_among_outliers + 1
  ret_vector
}

#fivenum(pairwise_dist) 0.0000000 0.3393090 0.4186019 0.5001053 0.8609580 - Bimodal distribution
rd_from_kNNs_for_outliers <- load_avalon_data()
