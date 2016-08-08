library(data.table)
library(cluster)
library(ggplot2)

elem_idx <- 1 
row_idx <- 1
curr_row <- 1
curr_idx_among_outliers <- 1
curr_idx_general_pop <- 1

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
  print(fivenum(local_outlier_factors)) #0.8879776 1.0153629 1.0947816 1.2082687 2.5167702
  #rd_from_kNNs <- analyze_outliers(local_outlier_factors, k_distances, pairwise_dist, reachability_distances, 
  #                                             local_reachability_densities)
  
  rd_from_kNNs <- apply(pairwise_dist, 1, 
                        function(x) get_rd_from_kNNs(x, k_distances, reachability_distances))
  rd_from_kNNs <- t(rd_from_kNNs)
  draw_plots(local_outlier_factors, local_reachability_densities, rd_from_kNNs)
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
analyze_outliers <- function(local_outlier_factors, k_distances, pairwise_dist, reachability_distances, 
                             local_reachability_densities)
{
  lof_threshold <- 1.87
  outlier_indices <- as.numeric(which(local_outlier_factors > lof_threshold))
  
  cat("outlier_indices\n")
  print(outlier_indices) #With lof_threshold = 1.87, outlier_indices: 1, 2, 28, 118, 194, 261, 311, 548, 645, 835, 930, 982
   
  #Get the distance to the k-th NN for all outliers
  k_distances_for_outliers <- k_distances[outlier_indices]
  
  print(fivenum(k_distances)) #0.06754083 0.13570965 0.14203925 0.20070854 0.39313761
  print(fivenum(k_distances_for_outliers)) #0.2051604 0.2162725 0.2945539 0.3137104 0.3570943: Look at Q1, median and Q3.
  #These distance values for the outliers are higher than the corresponding distance values for the general population. 
  #These are distances to the 10th NN.
  
  #Get the indices of the k-th NNs for all outliers from pairwise_dist matrix
  pairwise_dist_for_outliers <- pairwise_dist[outlier_indices,]
  reachability_distances_for_outliers <- reachability_distances[outlier_indices,]
  
  rd_from_kNNs <- apply(pairwise_dist, 1, 
                        function(x) get_rd_from_kNNs(x, k_distances, reachability_distances))
  rd_from_kNNs <- t(rd_from_kNNs) #Without transpose, was giving 10 x 1200 matrix instead of 1200 x 10
  rd_from_kNNs_for_outliers <- rd_from_kNNs[outlier_indices,]
      
  cat("fivenum for rd_from_kNNs_for_outliers is\n")
  print(fivenum(rd_from_kNNs_for_outliers)) #0.1356421 0.2095692 0.2869193 0.3035375 0.3931376: reachability distances from 
  #k NNs for outliers is about twice that of the general population
  cat("fivenum for reachability_distances of general population from their k NNs is\n") #0.06754083 0.13523596 0.14073088 0.15538691 0.39313761
  print(fivenum(rd_from_kNNs))
  
  cat("fivenum for local_reachability_densities is\n")
  print(fivenum(local_reachability_densities)) #2.704293  6.024992  7.004358  7.991096 14.218325
  cat("fivenum for local_reachability_densities of outliers is\n")
  print(fivenum(local_reachability_densities[outlier_indices])) #2.902675 3.233046 3.507296 5.051305 5.281766: The local
  #reachability densities for outliers are on average much lower (about half) of the general population.
  rd_from_kNNs
}

get_rd_from_kNNs <- function(neighbors, k_distances, reachability_distances)
{
  kNN_indices <- as.numeric(which(neighbors <= k_distances[curr_idx_general_pop]))
  kNN_indices <- kNN_indices[kNN_indices != curr_idx_general_pop]
  ret_vector <- reachability_distances[curr_idx_general_pop, kNN_indices]
  curr_idx_general_pop <<- curr_idx_general_pop + 1
  ret_vector
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

draw_plots <- function(local_outlier_factors, local_reachability_densities, rd_from_kNNs)
{
  #Variation of outlier score with reachability density
  analysis_data <- data.frame("local_reachability_densities" = local_reachability_densities, "local_outlier_factors" = local_outlier_factors)
  image_file <- "C:\\Users\\blahiri\\learning\\figures\\all_plots.png"
  png(image_file, width = 800, height = 800)
  p1 <- ggplot(analysis_data, aes(x = local_reachability_densities, y = local_outlier_factors, group = 1)) + geom_line(colour="red") + geom_smooth(method=lm)
    
  #Distribution of outlier score
  p2 <- ggplot(analysis_data, aes(x = local_outlier_factors)) + geom_histogram(aes(y = ..density..)) + geom_density()
   
  #How does reachability distance from the k-th NN vary with outlier score?
  df <- data.frame(rd_from_kNNs)
  df <- cbind(df, local_outlier_factors)
  colnames <- paste("X", 1:10, sep = "")
  df_long <- melt(df, id.vars = c("local_outlier_factors"), measure.vars = colnames, variable.name= "k_value", 
                  value.name = "dist_with_kNN")
  p3 <- ggplot(data = df_long, aes(x = dist_with_kNN, y = local_outlier_factors, group = k_value, colour = k_value)) + geom_line() + geom_point()
  multiplot(p1, p2, p3, cols=2)
  aux <- dev.off()
}

#fivenum(pairwise_dist) 0.0000000 0.3393090 0.4186019 0.5001053 0.8609580 - Bimodal distribution
find_lof_outliers()



