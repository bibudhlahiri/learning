library(data.table)
library(cluster)

elem_idx <- 1 
row_idx <- 1
curr_row <- 1

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
  pairwise_dist <- as.matrix(daisy(avalon_data, "gower"))
  
  k <- 10
  nearest_neighbors <- t(apply(t(pairwise_dist),2,sort))
  
  k_distances <- nearest_neighbors[, (k + 1)] #Holds the distance to the k-th NN for each data point
  
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
  cat(paste("curr_row = ", curr_row, ", sum_lrd = ", sum_lrd, 
            ", k_neighborhood_sizes[curr_row] = ", k_neighborhood_sizes[curr_row], 
            ", lrd_curr_point = ", lrd_curr_point, "\n", sep = ""))
  curr_row <<- curr_row + 1
  ret_value
}


local_outlier_factors <- load_avalon_data()