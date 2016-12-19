library(rjson)
library(stringdist)

#The matched characters are given. 
fill_grid_with_labels <- function(tess_results, image_name)
{
   #Get the subset for the given image only
   data_for_this_image <- subset(tess_results, (FileName == image_name))
   
   cell_width <- 20
   cell_height <- 25
   n_columns <- 400/cell_width
   n_rows <- 275/cell_height
   m_grid <- matrix(nrow = n_rows, ncol = n_columns)
   for (i in 1:n_rows) 
   {
     for (j in 1:n_columns)
	 {
	   m_grid[i, j] <- find_best_match(data_for_this_image, i, j)
	 }
   }  
   m_grid   
}

find_best_match <- function(data_for_this_image, i, j, cell_width = 20, cell_height = 25)
{
  #Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the grid cell
  cell_boundaries <- c((j-1)*cell_width, (i-1)*cell_height, j*cell_width - 1, (i-1)*cell_height, 
	                     (j-1)*cell_width, i*cell_height - 1, j*cell_width - 1, i*cell_height - 1)
						 
  best_score <- Inf
  best_match <- "*" 
  n_matched_chars <- nrow(data_for_this_image)
  for (k in 1:n_matched_chars)
  {
    #Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the k-th matched character
	box_boundaries <- c(data_for_this_image[k, "LHS_X"], data_for_this_image[k, "LHS_Y"], 
	                    data_for_this_image[k, "RHS_X"], data_for_this_image[k, "LHS_Y"],
						data_for_this_image[k, "LHS_X"], data_for_this_image[k, "RHS_Y"],
						data_for_this_image[k, "RHS_X"], data_for_this_image[k, "RHS_Y"])
	score_for_box <- sum(abs(cell_boundaries - box_boundaries))
	
	if (score_for_box < best_score)
	{
	  best_score <- score_for_box
	  best_match <- data_for_this_image[k, "CHARACTER_TEXT"]
	  boundaries_for_best_match <- box_boundaries
	}
  }
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

remove_junk_chars <- function(m_grid, dictionary)
{
  #Scan the n-grams (n = 3, 4 and 5) from rows. Have a dictionary. Do fuzzy lookup.
  candidates <- c(extract_n_grams(m_grid, 3), extract_n_grams(m_grid, 4), extract_n_grams(m_grid, 5))
  cand_dic <- expand.grid(candidate = candidates, dict_entry = dictionary)
  #Note: string distance is case-sensitive, so converting to uppercase for now
  cand_dic$candidate <- toupper(cand_dic$candidate)
  cand_dic$distance <- apply(cand_dic, 1, function(row) stringdist(as.character(row["candidate"]), 
                                                                   as.character(row["dict_entry"]), method = "dl"))
  #What fraction of characters in the dictionary string match the candidate in the right order?
  cand_dic$dict_entry_length <- apply(cand_dic, 1, function(row) nchar(as.character(row["dict_entry"])))
  cand_dic$fuzzy_match_score <- 1 - cand_dic$distance/cand_dic$dict_entry_length
  cand_dic <- cand_dic[with(cand_dic, order(-fuzzy_match_score)),]
}

display_output_of_tess <- function(tess_results, image_name)
{
   #Get the subset for the given image only
   data_for_this_image <- subset(tess_results, (FileName == image_name))
   cat(paste("Image file is ", image_name, "\n", sep = ""))
   n_matched_chars <- nrow(data_for_this_image)
   
   library(ggplot2)
   df <- data.frame()
   image_file <- paste("C:\\Users\\blahiri\\Chevron\\vis\\from_jay\\op_of_bb_tess\\20161219\\", 
                       substr(image_name, 1, nchar(image_name) - 4), ".png", sep = "")
   png(image_file)
   p <- ggplot(df) + geom_point() + xlim(0, 400) + ylim(0, 275) + scale_y_reverse()
   
   for (i in 1:n_matched_chars)
   {
     p <- p + annotate("rect", xmin = data_for_this_image[i, "LHS_X"], xmax = data_for_this_image[i, "RHS_X"],
                          ymin = data_for_this_image[i, "LHS_Y"], ymax = data_for_this_image[i, "RHS_Y"], alpha = .2)
	 p <- p + annotate("text", x = (data_for_this_image[i, "LHS_X"] + data_for_this_image[i, "RHS_X"])/2, 
	                           y = (data_for_this_image[i, "LHS_Y"] + data_for_this_image[i, "RHS_Y"])/2, 
							   label = data_for_this_image[i, "CHARACTER_TEXT"])
   }
   print(p)
   aux <- dev.off()
}

display_all_outputs_of_tess <- function(tess_results)
{
  image_names <- unique(tess_results$FileName)
  print(image_names)
  n_images <- length(image_names)
  for (i in 1:n_images)
  {
    display_output_of_tess(tess_results, image_name = image_names[i])
  }
}

measure_recall_precision <- function(global_dictionary, tess_results, threshold = 0.35)
{
  image_names <- unique(tess_results$FileName)
  n_images <- length(image_names)
  sum_recall <- 0
  sum_precision <- 0
  for (i in 1:n_images)
  {
    cat(paste("\n\nImage = ", image_names[i], "\n", sep = ""))
    m_grid <- fill_grid_with_labels(tess_results, image_name = image_names[i])
	#Pass the global_dictionary to remove_junk_chars() because we want to see if 
	#an image is falsely reporting strings not present in that image
    cand_dic <- remove_junk_chars(m_grid, unlist(global_dictionary))
	flagged <- subset(cand_dic, (fuzzy_match_score >= threshold))
	reported_matches <- unique(flagged$dict_entry)
	cat("The reported matches from the dictionary and their fuzzy matching scores are\n")
	entries_scores <- aggregate(flagged$fuzzy_match_score, by = list(flagged$dict_entry), FUN = max)
	colnames(entries_scores) <- c("dict_entry", "fuzzy_match_score")
	entries_scores <- merge(entries_scores, flagged)
	entries_scores <- entries_scores[with(entries_scores, order(-fuzzy_match_score)),]
	print(entries_scores)
	
	recall <- length(intersect(global_dictionary[[i]], reported_matches))/length(global_dictionary[[i]])
	sum_recall <- sum_recall + recall
	precision <- length(intersect(global_dictionary[[i]], reported_matches))/length(reported_matches)
	sum_precision <- sum_precision + precision
    cat(paste("Recall = ", recall, ", precision = ", precision, "\n", sep = ""))
  }
  cat(paste("With threshold = ", threshold, ", avg recall = ", sum_recall/n_images, ", avg precision = ", sum_precision/n_images, "\n", sep = ""))
}

#source("C:\\Users\\blahiri\\Chevron\\compose_labels_from_tess_op.r")

filename <- "C:\\Users\\blahiri\\Chevron\\data\\output_20161219.csv"
tess_results <- read.csv(filename, header = F, stringsAsFactors = F)
colnames(tess_results) <- c("FileName", "LHS_X", "LHS_Y", "RHS_X", "RHS_Y", "CHARACTER_TEXT")

#display_all_outputs_of_tess(tess_results)

global_dictionary <- list(c("6-P-C5-7513", "(MINIMUM FLOW)"), c("IP-PCP-14", "PAHH 8425A1"), c("6VB-75G"), c("2-BD-C5-2587"),
                          c("CONTRACTOR"), c("1-P-C2A-1838"), c("SP101"), c("6VB-75G"), c("FSV 8427A", "FO 8427", "6VC-460", "VN-998"), 
						  c("PI 6305B", "PSLL 6305B"))
measure_recall_precision(global_dictionary, tess_results, threshold = 0.4)
#With threshold = 0.35, avg recall = 0.9, avg precision = 0.68
#With threshold = 0.39, avg recall = 0.85, avg precision = 0.7
#With threshold = 0.4, avg recall = 0.85, avg precision = 0.7






