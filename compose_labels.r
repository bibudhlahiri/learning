library(rjson)

fill_grid_with_labels <- function(cnn_results, cutoff = 15, serial_no = 1)
{
   cat(paste("Image file is ", cnn_results[["results"]][[serial_no]][["img_name"]], "\n", sep = ""))
   cat("The characters to be matched from are\n")
   print(cnn_results[["results"]][[serial_no]][["captions"]][1:cutoff])
   cell_width <- 40
   cell_height <- 55
   n_columns <- 800/cell_width
   n_rows <- 550/cell_height
   m_grid <- matrix(nrow = n_rows, ncol = n_columns)
   for (i in 1:n_rows) 
   {
     for (j in 1:n_columns)
	 {
	   m_grid[i, j] <- find_best_match(cnn_results, i, j, cutoff, serial_no)
	 }
   }  
   m_grid   
}

find_best_match <- function(cnn_results, i, j, cutoff = 15, serial_no = 1, cell_width = 40, cell_height = 55)
{
  #Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the grid cell
  cell_boundaries <- c((j-1)*cell_width, (i-1)*cell_height, j*cell_width - 1, (i-1)*cell_height, 
	                     (j-1)*cell_width, i*cell_height - 1, j*cell_width - 1, i*cell_height - 1)
						 
  best_score <- Inf
  best_match <- "*" 
  cat(paste("i = ", i, ", j = ", j, "\n", sep = ""))
  cat("cell_boundaries\n")
  print(cell_boundaries)
  #cat(paste("cutoff = ", cutoff, "\n", sep = ""))
  for (k in 1:cutoff)
  {
    #cat(paste("k = ", k, "\n", sep = ""))
    box_params <- unlist(cnn_results[["results"]][[serial_no]][["boxes"]][k])
	#print(box_params)
	#Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the k-th box
	box_boundaries <- c(box_params[1], box_params[2], box_params[1] + box_params[3], box_params[2],
	                    box_params[1], box_params[2] + box_params[4], box_params[1] + box_params[3], box_params[2] + box_params[4])
	score_for_box <- sum(abs(cell_boundaries - box_boundaries))
	
	if (score_for_box < best_score)
	{
	  best_score <- score_for_box
	  best_match <- cnn_results[["results"]][[serial_no]][["captions"]][k]
	  #cat(paste("best_score = ", best_score, ", best_match = ", best_match, "\n", sep = ""))
	  boundaries_for_best_match <- box_boundaries
	}
  }
  cat("boundaries_for_best_match\n")
  print(round(boundaries_for_best_match))
  best_match
}

#source("C:\\Users\\blahiri\\Chevron\\compose_labels.r")
cnn_results <- fromJSON(file = "C:\\Users\\blahiri\\Chevron\\vis\\data\\results.json")
m_grid <- fill_grid_with_labels(cnn_results, cutoff = 15, serial_no = 1)








