library(rjson)
library(stringdist)

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

#Extracts all distinct horizontal n-grams from a matrix 
extract_n_grams <- function(m_grid, n = 3)
{
  n_grams <- c()
  n_rows <- nrow(m_grid)
  n_columns <- ncol(m_grid)
  for (i in 1:n_rows)
  {
    for (j in 1:(n_columns - n + 1))
	{
	  string <- paste(m_grid[i, j:(j+n-1)], collapse = "")
	  if (!(string %in% n_grams))
	  {
	    n_grams <- c(n_grams, string)
	  }
	}
  }
  n_grams
}

remove_junk_chars <- function(m_grid)
{
  #Scan the n-grams (n = 3, 4 and 5) from rows. Have a dictionary. Do fuzzy lookup.
  candidates <- c(extract_n_grams(m_grid, 3), extract_n_grams(m_grid, 4), extract_n_grams(m_grid, 5))
  dictionary <- c("108", "NOTE", "1VN-998")
  cand_dic <- expand.grid(candidate = candidates, dict_entry = dictionary)
  #Note: string distance is case-sensitive, so converting to uppercase for now
  cand_dic$candidate <- toupper(cand_dic$candidate)
  cand_dic$distance <- apply(cand_dic, 1, function(row) stringdist(as.character(row["candidate"]), 
                                                                   as.character(row["dict_entry"]), method = "dl"))
  #What fraction of characters in the dictionary string match the candidate in the right order?
  cand_dic$dict_entry_length <- apply(cand_dic, 1, function(row) nchar(as.character(row["dict_entry"])))
  cand_dic$fuzzy_match_score <- cand_dic$distance/cand_dic$dict_entry_length
  cand_dic <- cand_dic[with(cand_dic, order(fuzzy_match_score)),]
}

#source("C:\\Users\\blahiri\\Chevron\\compose_labels.r")
cnn_results <- fromJSON(file = "C:\\Users\\blahiri\\Chevron\\vis\\data\\results.json")
m_grid <- fill_grid_with_labels(cnn_results, cutoff = 15, serial_no = 1)
cand_dic <- remove_junk_chars(m_grid)
print(head(cand_dic, 20))








