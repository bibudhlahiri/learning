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
  
  #Take the outliers and get an explanation of why they are outliers. Get their distances with the k-th NNs 
  #for k from 1 to max_k.
  kNN_distances_for_outliers <- matr_kNN_distances[outliers_by_knn, ]
  cat("Fivenum of distances to 20 NNs for general population is\n")
  print(fivenum(matr_kNN_distances)) #25th, 50th and 75th %iles are 0.134,0.141 and 0.2008 respectively.
  cat("Fivenum of distances to 20 NNs for outliers is\n")
  print(fivenum(kNN_distances_for_outliers)) #25th, 50th and 75th %iles are 0.2278, 0.275046 and 0.2926258 respectively.
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

#pairwise_dist <- find_knn_outliers()

analyze_outliers <- function()
{
  #Row 2 is an outlier as detected by this algorithm. 
  nearest_neighbors[2, 1:11]
  #Distances with its top 10 NNs are:
  #0.1517397 0.2896674 0.2898763 0.2922456 0.2931135 0.2958721 0.2960444 0.3003956 0.3005631 0.3018497
  
  fivenum(nearest_neighbors[, 2]) 
  #5-num summary of distances to 1st NN for the general population is
  #9.262861e-06 9.080478e-03 6.987182e-02 8.781732e-02 2.865031e-01. So the median is 0.0698 whereas for row 2, it is 0.1517397
  
  fivenum(nearest_neighbors[, 3]) 
  #5-num summary of distances to 2nd NN for the general population is
  #0.0003575465 0.0681581676 0.0761668884 0.1362878953 0.3426311251. So the median is 0.0761 whereas for row 2, it is 0.2896674
  
  #Which row is the 1st NN to row 2?
  which(as.numeric(rank(pairwise_dist[2, ], ties.method = "random")) == 2)
  #The 6th element (rank 2) is the 1st NN with a distace of 0.1517397.
  
  avalon_data[2, ]
  #Row 2 is as follows: REPAIR STATE CODE NAME = MARYLAND (1.91%), REG STATE CODE NAME = NA (3.33%), 
  #MILEAGE (mile) = 25285 (99.66%ile), 
  #WAIS CODE NAME = STEERING- MISCELLANEOUS-MISC. (35%), PARTS NO. NAME = "CLIP, STEERING TELESCOPIC LEVER" (0.41%), 
  #T1 CODE NAME = "UNSTABLE STEERING (VEHICLE WANDERS)" (26.4%), T2 CODE NAME = "BROKEN,SPLIT,TORN/DENT" (0.166%), 
  #FUNCTION CODE NAME = "STEERING & OTHERS" (37%), F.DIST = 91041, COUNTRY/DIST CODE NAME = "USA", 
  #DIST SETTLE. AMT - G.TOT (USD) = 84.29 (3%ile), DIST SETTLE. AMT - PARTS TOT (USD) = 9.83 (99.67%ile), TIRE MAKER = BS, 
  #PLANT CODE NAME = TMMK, ENG TYPE = 2GR-FE
  
  #For percentiles for numeric values, do 
  #column <- "MILEAGE (mile)"; f <- ecdf(avalon_data[[column]]); f(25285)
  
  avalon_data[6, ]
  #Row 6 is as follows: REPAIR STATE CODE NAME = NORTH CAROLINA, REG STATE CODE NAME = NORTH CAROLINA, MILEAGE (mile) = 35050, 
  #WAIS CODE NAME = STEERING- MISCELLANEOUS-MISC., PARTS NO. NAME = "CLIP, STEERING TELESCOPIC LEVER", 
  #T1 CODE NAME = "UNSTABLE STEERING (VEHICLE WANDERS)", T2 CODE NAME = "BROKEN,SPLIT,TORN/DENT", 
  #FUNCTION CODE NAME = "STEERING & OTHERS", F.DIST = 91041, COUNTRY/DIST CODE NAME = "USA", 
  #DIST SETTLE. AMT - G.TOT (USD) = 78.81, DIST SETTLE. AMT - PARTS TOT (USD) = 9.83, TIRE MAKER = BS, 
  #PLANT CODE NAME = TMMK, ENG TYPE = 2GR-FE
  
  #Take a typical pair where a point is around at a median distance (0.0698) from its 1st NN.
  #Row 17 has a distance of 0.071936292 with its 1st NN, which is row 1074.
  which(as.numeric(rank(pairwise_dist[17, ], ties.method = "random")) == 2)
  
  avalon_data[17, ]
  #Row 17 is as follows: REPAIR STATE CODE NAME = TEXAS (9.16%), REG STATE CODE NAME = TEXAS (8.66%), 
  #MILEAGE (mile) = 13336 (94%ile), WAIS CODE NAME = STEERING- MISCELLANEOUS-MISC. (35%), 
  #PARTS NO. NAME = "VEHICLE PULLING (PRELIMINARY CHECK AND ROAD TEST)_INSP (34.25%)", 
  #T1 CODE NAME = "VEHICLE PULLS OR DRIFTS TO THE RIGHT (30.7%)", T2 CODE NAME = "OUT OF BALANCE" (8.75%), 
  #FUNCTION CODE NAME = "STEERING & OTHERS" (37%), F.DIST = 91041, COUNTRY/DIST CODE NAME = "USA", 
  #DIST SETTLE. AMT - G.TOT (USD) = 435.6 (95.67%), DIST SETTLE. AMT - PARTS TOT (USD) = 0, TIRE MAKER = MN, 
  #PLANT CODE NAME = TMMK, ENG TYPE = 2GR-FE
  
  avalon_data[1074, ]
  #Row 1074 is as follows: REPAIR STATE CODE NAME = TEXAS, REG STATE CODE NAME = TEXAS, MILEAGE (mile) = 11497, 
  #WAIS CODE NAME = STEERING- MISCELLANEOUS-MISC., PARTS NO. NAME = "VEHICLE PULLING (PRELIMINARY CHECK AND ROAD TEST)_INSP", 
  #T1 CODE NAME = "VEHICLE PULLS OR DRIFTS TO THE RIGHT", T2 CODE NAME = "POOR FIT,POOR ASSEMBLING", 
  #FUNCTION CODE NAME = "STEERING & OTHERS", F.DIST = 91041, COUNTRY/DIST CODE NAME = "USA", 
  #DIST SETTLE. AMT - G.TOT (USD) = 403.3, DIST SETTLE. AMT - PARTS TOT (USD) = 0, TIRE MAKER = MN, 
  #PLANT CODE NAME = TMMK, ENG TYPE = 2GR-FE
}

#Check the modal values of the categorical variables and the distributions of the numeric ones
general_patterns <- function()
{
  avalon_data <- load_avalon_data()
  columns <- names(avalon_data)
  n_rows <- nrow(avalon_data)
  for (column in columns)
  {
    cat(paste("column = ", column, "\n", sep = ""))
    if (is.factor(avalon_data[[column]]))
    {
      t <- sort(table(avalon_data[[column]]), decreasing = TRUE)
      df <- data.frame(values = names(t), frequencies = as.numeric(t))
      df$percentages <- df$frequencies/n_rows
      print(df)
    }
    else
    {
      print(fivenum(avalon_data[[column]]))
    }
  }
}

general_patterns()
#REPAIR STATE CODE NAME: TEXAS: 9.16%, CALIFORNIA: 9%, FLORIDA: 8.25%
#REG STATE CODE NAME: CALIFORNIA: 9%, TEXAS: 8.66%, FLORIDA: 7.9%
#MILEAGE (mile): 2  3872  5729 10002 35988
#WAIS CODE NAME: FR WHEEL ALIGNMENT VEHICLE TENDS TO MOVE ONE SIDE: 40%, STEERING- MISCELLANEOUS-MISC.: 35%, 
#                FR WHEEL ALIGNMENT OTHERS: 23%
#PARTS NO. NAME: WHEEL ALIGNMENT_INSP: 63%, VEHICLE PULLING (PRELIMINARY CHECK AND ROAD TEST)_INSP: 34.25%,
#                STEERING WHEEL OFF-CENTER (TIE RODS) _ADJST: 0.66%
#T1 CODE NAME: VEHICLE PULLS OR DRIFTS TO THE RIGHT: 30.7%, VEHICLE PULLS OR DRIFTS TO THE LEFT: 27.9%
#T2 CODE NAME: OTHERS: 39%, POOR FIT,POOR ASSEMBLING: 16.67%, IMPROPER FREE PLAY: 11.16%
#FUNCTION CODE NAME: SUSPENTION & AXLE: 63%, STEERING & OTHERS: 37%
#F.DIST: 91025 91041 91041 91041 91041
#COUNTRY/DIST CODE NAME: USA: 98%, CANADA: 2%
#DIST SETTLE. AMT - G.TOT (USD): 16.600  132.000  174.445  280.190 1172.600
#DIST SETTLE. AMT - PARTS TOT (USD): 0.0   0.0   0.0   0.0 661.7
#TIRE MAKER: BS: 54%, MN: 46%
#ENG PLANT CODE NAME: TMMK: 95.5%, KAMIGO: 4.5%
#ENG TYPE: 2GR-FE: 76%, 2AR-FXE: 24%


