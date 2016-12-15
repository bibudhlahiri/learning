library(rjson)

initial_exploration <- function()
{
    cnn_results <- fromJSON(file = "C:\\Users\\blahiri\\Chevron\\vis\\data\\results.json")
    class(cnn_results) #list
    names(cnn_results) #"opt"     "results" #First and second key in the hash
    class(cnn_results[["results"]]) #list
    length(cnn_results[["results"]]) #100: one entry for each image
    cnn_results[["results"]][[1]]
	#$img_name
	##[1] "211.jpg"
	#$scores
	##[1]   7.74330282   6.59691000...
	#$captions
	##[1] "o" "8" "v"
	##$boxes
	##$boxes[[1]]
	##[1] 661.16870  26.39072  35.64502  52.37738
	##$boxes[[2]]
	##[1] 682.02197  23.54664  40.87756  53.51899
	#...
	##$boxes[[262]]
	##[1] 772.57904 110.29961  25.55835  61.87128
	cnn_results[["results"]][[1]][["img_name"]] #"211.jpg"
	length(cnn_results[["results"]][[1]][["scores"]]) #262, NLL values in decreasing order
	cnn_results[["results"]][[1]][["captions"]] #"o" "8" "v" "1" "9" "8" "n"
	#The strings are "108", "NOTE", "VN-998" and "1''". Total 13 characters. In captions, 1 appears 43 times. 
	#We check the top 13 captions as there are actually 13 characters. #"o" "8" "v" "1" "9" "8" "n" "9" "o" "n" "o" "8" "v"
	cnn_results[["results"]][[1]][["boxes"]][1:13]
	#The boxes for these top 13 are
	##$boxes[#[1]]
	##[1] 661.16870  26.39072  35.64502  52.37738
	##$boxes[[2]]
	##[1] 682.02197  23.54664  40.87756  53.51899
	##$boxes[[3]]
	##[1] 276.69736 331.49875  41.30530  62.25323
	##$boxes[[4]]
	##[1] 635.70288  32.20738  33.28064  50.63665
	##$boxes[[5]]
	##[1] 421.57812 325.83612  39.77216  58.55115

	#$boxes[[6]]
	#[1] 440.98630 324.93417  46.08063  52.96216

	#$boxes[[7]]
	#[1] 631.10535 258.01935  40.65039  61.87280

	#$boxes[[8]]
	#[1] 382.71091 343.10162  54.23322  41.93671

	#$boxes[[9]]
	#[1] 651.18536 265.32910  61.91016  46.32239

	#$boxes[[10]]
	#[1] 303.40945 336.74216  47.86957  47.61005

	#$boxes[[11]]
	#[1] 688.22791 246.27599  33.63110  62.28615

	#$boxes[[12]]
	#[1] 692.068665   7.008644  34.270508  36.703430

	#$boxes[[13]]
	#[1] 253.66951 338.42499  38.15904  58.97894
}

fill_grid_with_labels <- function(cnn_results)
{
   cell_width <- 40
   cell_height <- 55
   n_columns <- 800/cell_width
   n_rows <- 550/cell_height
   m_grid <- matrix(nrow = n_rows, ncol = n_columns)
   for (i in 1:n_rows) 
   {
     for (j in 1:n_columns)
	 {
	   m_grid[i, j] <- find_best_match(cnn_results, i, j)
	 }
   }  
   m_grid   
}

find_best_match <- function(cnn_results, i, j, cell_width = 40, cell_height = 55)
{
  #Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the grid cell
  cell_boundaries <- c((j-1)*cell_width, (i-1)*cell_height, j*cell_width - 1, (i-1)*cell_height, 
	                     (j-1)*cell_width, i*cell_height - 1, j*cell_width - 1, i*cell_height - 1)
						 
  cutoff <- 15
  best_score <- Inf
  best_match <- "*" 
  for (k in 1:cutoff)
  {
    box_params <- unlist(cnn_results[["results"]][[1]][["boxes"]][k])
	#Get x_tl, y_tl, x_tr, y_tr, x_bl, y_bl, x_br, y_br for the k-th box
	box_boundaries <- c(box_params[1], box_params[2], box_params[1] + box_params[3], box_params[2],
	                    box_params[1], box_params[2] + box_params[4], box_params[1] + box_params[3], box_params[2] + box_params[4])
	score_for_box <- sum(abs(cell_boundaries - box_boundaries))
	if (score_for_box < best_score)
	{
	  best_score <- score_for_box
	  best_match <- cnn_results[["results"]][[1]][["captions"]][k]
	}
  }
  best_match
}

#source("C:\\Users\\blahiri\\Chevron\\compose_labels.r")
cnn_results <- fromJSON(file = "C:\\Users\\blahiri\\Chevron\\vis\\data\\results.json")
m_grid <- fill_grid_with_labels(cnn_results)








