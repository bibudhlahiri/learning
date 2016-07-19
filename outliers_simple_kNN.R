library(data.table)
library(cluster)

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

outliers_by_knn <- c()

#Find the top-ranking elements whose distances from their k-th NNs are the highest
find_knn_outliers <- function()
{
  avalon_data <- load_avalon_data()
  pairwise_dist <- as.matrix(daisy(avalon_data, "gower"))  
  nearest_neighbors <- t(apply(t(pairwise_dist),2,sort))
  max_k <- 20
  matr_kNN_distances <- nearest_neighbors[, 2:(max_k + 1)]
  indices_by_knn_distances <- apply(matr_kNN_distances, 2, 
                                    function(x) get_indices_by_knn_distances(x))
  index_threshold <- 100 #Till what index do we go?
  #Take the top index_threshold elements from each column, i.e., if index_threshold = 100, 
  #then we are taking the top 100 elements whose distances with their 1st NN are the highest, 
  #then the top 100 elements whose distances with their 2nd NN are the highest, and so on. 
  #Take the intersection of all such elements.
  indices_by_knn_distances <- indices_by_knn_distances[1:index_threshold,]
  outliers_by_knn <<- indices_by_knn_distances[,1]
  apply(indices_by_knn_distances[, -1], 2, function(x) combine_columns_for_outliers(x))
  outliers_by_knn
}

get_indices_by_knn_distances <- function(distances_with_kNN)
{
  sort(distances_with_kNN, decreasing = TRUE, index.return=TRUE)$ix
}

combine_columns_for_outliers <- function(index_column)
{
  outliers_by_knn <<- intersect(outliers_by_knn, index_column)
}

outliers_by_knn <- find_knn_outliers()
