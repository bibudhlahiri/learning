library(rjson)
library(stringdist)

curr_row <<- 1
scores_for_grid_cells <<- data.frame(matrix(ncol = 5, nrow = 1)) #Just junk initialization of global variable 

#The matched characters are given. 
fill_grid_with_labels <- function(tess_results, image_name, flipped)
{
   #Get the subset for the given image only
   data_for_this_image <- subset(tess_results, ((FileName == image_name) & (FLIPPED == as.numeric(flipped))))
   #Fill up the grid only if there are horizontal or vertical characters to be placed
   if (nrow(data_for_this_image) == 0)
   {
     return(NA)
   }
   #Assigning an ID to each character so that if same character occur multiple times in 
   #the same image, the occurrences can be distinguished.
   data_for_this_image$char_id <- 1:nrow(data_for_this_image)
   cell_width <- ifelse(!flipped, 20, 25)
   cell_height <- ifelse(!flipped, 25, 20)
   n_columns <- 400/cell_width
   n_rows <- round(275/cell_height)
   m_grid <- matrix(nrow = n_rows, ncol = n_columns)
   #scores_for_grid_cells is being created to improve precision, and will be used later
   scores_for_grid_cells <<- data.frame(matrix(ncol = 5, nrow = n_rows*n_columns)) 
   colnames(scores_for_grid_cells) <<- c("row_number", "col_number", "char_placed", "id_of_char_placed", "score")
   curr_row <<- 1
   for (i in 1:n_rows) 
   {
     for (j in 1:n_columns)
	 {
	   m_grid[i, j] <- find_best_match(data_for_this_image, i, j, cell_width, cell_height)
	 }
   } 
   m_grid <- remove_junk_chars(m_grid, data_for_this_image, scores_for_grid_cells, n_rows, n_columns)   
   print(m_grid)
   return(m_grid)
}

find_best_match <- function(data_for_this_image, i, j, cell_width, cell_height)
{
  #Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the grid cell
  cell_boundaries <- c((j-1)*cell_width, (i-1)*cell_height, j*cell_width - 1, (i-1)*cell_height, 
	                     (j-1)*cell_width, i*cell_height - 1, j*cell_width - 1, i*cell_height - 1)
						 
  best_score <- Inf
  best_match <- "*" 
  n_matched_chars <- nrow(data_for_this_image)
  id_of_best_match <- 0
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
	  id_of_best_match <- data_for_this_image[k, "char_id"]
	}
  }
  scores_for_grid_cells[curr_row, "row_number"] <<- i
  scores_for_grid_cells[curr_row, "col_number"] <<- j
  scores_for_grid_cells[curr_row, "char_placed"] <<- best_match
  scores_for_grid_cells[curr_row, "id_of_char_placed"] <<- id_of_best_match
  scores_for_grid_cells[curr_row, "score"] <<- best_score
  curr_row <<- curr_row + 1
  best_match
}

#We assigned one character to each cell of the grid originally. That
#is creating lots of false positives, especially with the shorter tokens.
#We will remove the unnecessary characters with the following logic: for each
#matched character, if it has been allocated to multiple cells, we will keep
#it only in that cell where its score (based on differences of coordinates) is highest, 
#and remove it from everywhere else.
remove_junk_chars <- function(m_grid, data_for_this_image, scores_for_grid_cells, n_rows, n_columns)   
{
  #Find the lowest (best) score for each char_id
  best_scores_for_chars <- aggregate(scores_for_grid_cells$score, by = list(scores_for_grid_cells$id_of_char_placed), FUN = min)
  colnames(best_scores_for_chars) <- c("id_of_char_placed", "score")
  best_scores_for_chars <- merge(best_scores_for_chars, scores_for_grid_cells)
  
  for (i in 1:(n_rows*n_columns))
  {
    #Go through each cell of m_grid (using scores_for_grid_cells), and find out the best position for the char placed in the cell 
    #from best_scores_for_chars. If the position of the current cell does not match the best position for the placed character,
    #fill the cell with a special character like "@".
    record_for_best_position <- subset(best_scores_for_chars, (best_scores_for_chars$id_of_char_placed == scores_for_grid_cells[i, "id_of_char_placed"]))
    if (!((record_for_best_position[1, "row_number"] == scores_for_grid_cells[i, "row_number"])
             && (record_for_best_position[1, "col_number"] == scores_for_grid_cells[i, "col_number"])))
    {
      m_grid[scores_for_grid_cells[i, "row_number"], scores_for_grid_cells[i, "col_number"]] <- "@"
    }
  }
  m_grid
}


#Extract tokens and not n-grams of fixed length. A token is a seuqence of non-empty characters in the grid. 
#Can be of any length.
extract_tokens_horizontally <- function(m_grid)
{
  tokens <- c()
  n_rows <- nrow(m_grid)
  n_columns <- ncol(m_grid)
  
  for (i in 1:n_rows)
  {
    string <- paste(m_grid[i, 1:n_columns], collapse = "")
	raw_tokens <- unlist(strsplit(string, "@"))
	tokens <- c(tokens, raw_tokens[raw_tokens != ""])
  }
  tokens
}

extract_tokens_vertically <- function(m_grid)
{
  tokens <- c()
  n_rows <- nrow(m_grid)
  n_columns <- ncol(m_grid)
  
  for (i in 1:n_columns)
  {
    string <- paste(m_grid[n_rows:1, i], collapse = "")
	raw_tokens <- unlist(strsplit(string, "@"))
	tokens <- c(tokens, raw_tokens[raw_tokens != ""])
  }
  tokens
}

#TBD: Some numeric tokens are part of larger tokens, e.g., 84251 is extracted from 37.jpg which is actually 
#a part of "PAHH 8425A1". In such cases, we will check if the number-only string is part of some bigger string 
#in the global dictionary. If it is, we will see if the first part of the string from the dictionary (e.g., "PAHH")
#also appears as a token (at least partly). If it does, we will combine the alphabetic and numeric parts, e.g. PAHH
#and 84251, and keep that as a candidate string and deleting the tokens like "PAHH" and "84251" from the set of candidates.
#This needs to be done since strings like "PAHH 8425A1" are appearing over multiple rows in the grid. 

#Extracts all distinct horizontal n-grams from a matrix 
extract_n_grams_horizontally <- function(m_grid, n = 3)
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

#Extracts all distinct vertical n-grams from a matrix, going from bottom to top.
extract_n_grams_vertically <- function(m_grid, n = 3)
{
  n_grams <- c()
  n_rows <- nrow(m_grid)
  n_columns <- ncol(m_grid)
  for (i in 1:n_columns)
  {
    for (j in n_rows:n)
	{
	  string <- paste(m_grid[j:(j-n+1), i], collapse = "")
	  if (!(string %in% n_grams))
	  {
	    n_grams <- c(n_grams, string)
	  }
	}
  }
  n_grams
}


rowwise_fuzzy_score <- function(candidate, dict_entry)
{
  #If the dictionary string has only 3 characters, it should match on all. Otherwise, set score to 0.
  #if ((nchar(dict_entry) == 3) &&  (!(dict_entry == candidate)))
  #{
  #  return(0)
  #}
  distance <- stringdist(candidate, dict_entry, method = "dl")
  dict_entry_length <- nchar(dict_entry)
  fuzzy_match_score <- 1 - distance/dict_entry_length
}

calc_fuzzy_score <- function(m_grid, dictionary, flipped)
{
  dictionary <- unique(dictionary) #Remove duplicates from dictionary as same symbol may be present in multiple sliding windows
  #Scan the n-grams (n = 3, 4 and 5) from rows/columns, depending on flipped. Have a dictionary. Do fuzzy lookup.
  if (!flipped)
  {
    #candidates <- c(extract_n_grams_horizontally(m_grid, 3), extract_n_grams_horizontally(m_grid, 4), extract_n_grams_horizontally(m_grid, 5))
	candidates <- extract_tokens_horizontally(m_grid)
  }
  else
  {
    #candidates <- c(extract_n_grams_vertically(m_grid, 3), extract_n_grams_vertically(m_grid, 4), extract_n_grams_vertically(m_grid, 5))
	candidates <- extract_tokens_vertically(m_grid)
  }
  cand_dic <- expand.grid(candidate = candidates, dict_entry = dictionary)
  #Note: string distance is case-sensitive, so converting to uppercase for now
  cand_dic$candidate <- toupper(cand_dic$candidate)
  #cand_dic$distance <- apply(cand_dic, 1, function(row) stringdist(as.character(row["candidate"]), 
  #                                                                 as.character(row["dict_entry"]), method = "dl"))
  #What fraction of characters in the dictionary string match the candidate in the right order?
  #cand_dic$dict_entry_length <- apply(cand_dic, 1, function(row) nchar(as.character(row["dict_entry"])))
  #cand_dic$fuzzy_match_score <- 1 - cand_dic$distance/cand_dic$dict_entry_length
  cand_dic$fuzzy_match_score <- apply(cand_dic, 1, function(row) rowwise_fuzzy_score(as.character(row["candidate"]), 
                                                                                     as.character(row["dict_entry"])))
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
   image_file <- paste("C:\\Users\\blahiri\\Chevron\\vis\\from_jay\\op_of_bb_tess\\20161220\\", 
                       substr(image_name, 1, nchar(image_name) - 4), ".png", sep = "")
   png(image_file)
   p <- ggplot(df) + geom_point() + xlim(0, 400) + ylim(0, 275) + scale_y_reverse()
   
   for (i in 1:n_matched_chars)
   {
     p <- p + annotate("rect", xmin = data_for_this_image[i, "LHS_X"], xmax = data_for_this_image[i, "RHS_X"],
                          ymin = data_for_this_image[i, "LHS_Y"], ymax = data_for_this_image[i, "RHS_Y"], alpha = .2)
	 if (data_for_this_image[i, "FLIPPED"])
	 {
	   p <- p + annotate("text", x = (data_for_this_image[i, "LHS_X"] + data_for_this_image[i, "RHS_X"])/2, 
	                           y = (data_for_this_image[i, "LHS_Y"] + data_for_this_image[i, "RHS_Y"])/2, 
							   label = data_for_this_image[i, "CHARACTER_TEXT"], angle = 90)
	 }
	 else
	 {
	   p <- p + annotate("text", x = (data_for_this_image[i, "LHS_X"] + data_for_this_image[i, "RHS_X"])/2, 
	                           y = (data_for_this_image[i, "LHS_Y"] + data_for_this_image[i, "RHS_Y"])/2, 
							   label = data_for_this_image[i, "CHARACTER_TEXT"])
	 }
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
  print(image_names)
  n_images <- length(image_names)
  sum_recall <- 0
  sum_precision <- 0
  for (i in 1:n_images)
  {
    cat(paste("\n\nImage = ", image_names[i], "\n", sep = ""))
	
	#Extract the horizontal strings first
    m_grid_horizontal <- fill_grid_with_labels(tess_results, image_name = image_names[i], FALSE)
	#Pass the global_dictionary to calc_fuzzy_score() because we want to see if 
	#an image is falsely reporting strings not present in that image
	if (!is.na(m_grid_horizontal))
	{
      cand_dic_horizontal <- calc_fuzzy_score(m_grid_horizontal, unlist(global_dictionary, use.names = FALSE), FALSE)
	}
	else
	{
	  cand_dic_horizontal <- NA
	}
	
	#Extract the vertical strings next
	m_grid_vertical <- fill_grid_with_labels(tess_results, image_name = image_names[i], TRUE)
	#Pass the global_dictionary to calc_fuzzy_score() because we want to see if 
	#an image is falsely reporting strings not present in that image
	if (!is.na(m_grid_vertical))
	{
      cand_dic_vertical <- calc_fuzzy_score(m_grid_vertical, unlist(global_dictionary, use.names = FALSE), TRUE)
	}
	else
	{
	  cand_dic_vertical <- NA
	}
	if ((!is.na(cand_dic_horizontal)) && (!is.na(cand_dic_vertical)))
	{
	  cand_dic <- rbind(cand_dic_horizontal, cand_dic_vertical)
	}
	else if (!is.na(cand_dic_horizontal))
	{
	  cand_dic <- cand_dic_horizontal
	}
	else
	{
	  cand_dic <- cand_dic_vertical
	}
	flagged <- subset(cand_dic, (fuzzy_match_score >= threshold))
	reported_matches <- unique(flagged$dict_entry)
	
	if (length(reported_matches) == 0)
	{
	  recall <- 0
	  precision <- 0
	}
	else
	{
	  cat("The reported matches from the dictionary and their fuzzy matching scores are\n")
	  entries_scores <- aggregate(flagged$fuzzy_match_score, by = list(flagged$dict_entry), FUN = max)
	  colnames(entries_scores) <- c("dict_entry", "fuzzy_match_score")
	  entries_scores <- merge(entries_scores, flagged)
	  entries_scores <- entries_scores[with(entries_scores, order(-fuzzy_match_score)),]
	  print(entries_scores)
	
	  recall <- length(intersect(global_dictionary[[image_names[i]]], reported_matches))/length(global_dictionary[[image_names[i]]])
	  sum_recall <- sum_recall + recall
	  precision <- length(intersect(global_dictionary[[image_names[i]]], reported_matches))/length(reported_matches)
	  sum_precision <- sum_precision + precision
	}
    cat(paste("Recall = ", recall, ", precision = ", precision, "\n", sep = ""))
  }
  cat(paste("With threshold = ", threshold, ", avg recall = ", sum_recall/n_images, ", avg precision = ", sum_precision/n_images, "\n", sep = ""))
}

#source("C:\\Users\\blahiri\\Chevron\\compose_labels_from_tess_op.r")

filename <- "C:\\Users\\blahiri\\Chevron\\data\\output_20161220.csv"
tess_results <- read.csv(filename, header = F, stringsAsFactors = F)
colnames(tess_results) <- c("FileName", "LHS_X", "LHS_Y", "RHS_X", "RHS_Y", "CHARACTER_TEXT", "FLIPPED")

#display_all_outputs_of_tess(tess_results)
#Creating this as a named list so that lookup can be performed by name,
#and not by index. That way, the order of filenames in output.csv 
#does not matter.
global_dictionary <- list("35.jpg" = c("6-P-C5-7513", "(MINIMUM FLOW)"),
                          "37.jpg" = c("IP-PCP-14", "PAHH 8425A1"), 
						  "86.jpg" = c("6VB-75G", "C5-1826"), 
						  "92.jpg" = c("2-BD-C5-2587"),
                          "93.jpg" = c("CONTRACTOR"), 
						  "99.jpg" = c("1-P-C2A-1838", "A2A"),  
						  "112.jpg" = c("SP101"), 
						  "116.jpg" = c("XA8427A1", "PALL8427A", "IP-PCP-14", "P-C5-1827"), 
						  "126.jpg" = c("6VB-75G"), 
						  "127.jpg" = c("FSV 8427A", "FO 8427", "6VC-460", "1VN-998", "VB-75M"), 
						  "128.jpg" = c("FV8427", "4FO", "6-P-C5-1813"), 
						  "129.jpg" = c("P-C5-1827"), 
						  "145.jpg" = c("PI 6305B", "PSLL 6305B"), 
						  "310.jpg" = c("1-FL-C2A-2874", "1VN-998", "1VB-71", "C2A", "HPV"), 
						  "312.jpg" = c("2-DW-A1-1360", "HS8425", "XL8425"), 
						  "810.jpg" = c("1VN-998", "FSV8426", "FIT8426", "VC-460"), 
						  "910.jpg" = c("10VB-75GCSO", "1VN-998S", "C2A", "HPV"), 
						  "1010.jpg" = c("1FL-C2A-2876", "1VB-71", "C2A", "VENDOR") 
						  )
sink("C:\\Users\\blahiri\\Chevron\\log.txt")
measure_recall_precision(global_dictionary, tess_results, threshold = 0.35)
sink()
#Before implementing flipped characters, 
#With threshold = 0.35, avg recall = 0.9, avg precision = 0.68
#With threshold = 0.39, avg recall = 0.85, avg precision = 0.7
#With threshold = 0.4, avg recall = 0.85, avg precision = 0.7

#After implementing flipped characters, 
#With threshold = 0.4, avg recall = 0.9083, avg precision = 0.373
#With threshold = 0.5, avg recall = 0.621296296296296, avg precision = 0.37781774325892

#After implementing remove_junk_chars,
#With threshold = 0.35, avg recall = 0.893518518518518, avg precision = 0.423535415202082
#After adding rowwise_fuzzy_score (rule about 3-digit strings),
#With threshold = 0.35, avg recall = 0.831481481481481, avg precision = 0.422987749742136
#After adding extract_tokens_horizontally and extract_tokens_vertically instead of n-grams,
#With threshold = 0.35, avg recall = 0.742592592592592, avg precision = 0.537037037037037
#After we took off the change about 3-digit strings in rowwise_fuzzy_score, 
#With threshold = 0.35, avg recall = 0.80462962962963, avg precision = 0.525295075295075
#With threshold = 0.4, avg recall = 0.747222222222222, avg precision = 0.560678210678211






