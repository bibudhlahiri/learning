library(rpart)
library(data.table)

load_comet_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\COMET_DATA_2016_1.TXT"
  #Read 2,476,484 rows in 30 seconds
  comet_data <- fread(filename, header = FALSE, sep = "|", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("character", "character", "character", "numeric", "character",
					               "Date", "Date", "character", "numeric", "Date", 
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "Date", "Date",
								   "Date", "numeric", "Date", "numeric", "character",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "character", "character", "character", "character", "Date",
								   "character", "character", "character", "character", "character",
								   "numeric", "character", "numeric", "numeric", "numeric",
								   "character", "numeric", "character", "character", "numeric",
					               "Date", "numeric", "numeric", "character", "character",
								   "character", "Date", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "character", "character", "character", "character"),
                    data.table = TRUE)
  lines <- readLines("C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\Schema.txt")
  from_schema_file <- unlist(strsplit(lines, " "))
  is_column_name <- unlist(lapply(from_schema_file, check_if_column_name))
  column_names <- from_schema_file[which(is_column_name == TRUE)]
  setnames(comet_data, names(comet_data), column_names)
  
  comet_data[, MODEL_DECILE := ifelse((MODEL_DECILE == -1), 9, MODEL_DECILE)]
  
  sample_size <- 60000
  sampled_comet_data <- comet_data[sample(nrow(comet_data), sample_size), ]
  sample_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\SAMPLED_COMET_DATA_2016.TXT"
  write.table(sampled_comet_data, sample_filename, sep = "|", row.names = FALSE, col.names = TRUE, quote = FALSE)
  comet_data
}

analyze_wire_centers <- function(comet_data)
{
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(ACCTSK)), by = list(STATE, CLLI8)]
  
  #There are wire centers which are in multiple states. For now, take the state in which the wire center has maximum accounts,
  #and for all accounts in that wire center, update the state values to that state.
  if (FALSE)
  {
    #There are 7498 distinct values of CLLI8 but 8783 rows in by_state_wc
    tx <- table(by_state_wc[, CLLI8])
    tx <- tx[order(-tx)]
    wc_in_multiple_states <- as.data.table(tx)
    names(wc_in_multiple_states) <- c("CLLI8", "n_states")
    wc_in_multiple_states <- wc_in_multiple_states[(n_states > 1), ]
  }
  setkey(by_state_wc, CLLI8, n_accounts)
  by_state_wc <- by_state_wc[order(CLLI8, -n_accounts)]
  wc_largest_state <- by_state_wc[, .SD[1], by = CLLI8] #Back to 7498 states
}

update_states <- function(comet_data)
{
  wc_largest_state <- analyze_wire_centers(comet_data)
  setkey(wc_largest_state, CLLI8)
  comet_data[, STATE := apply(comet_data, 1, function(row)get_state_for_wc(as.character(row["CLLI8"]), wc_largest_state))]
  #Alternative: join comet_data and wc_largest_state by CLLI8
  
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(ACCTSK)), by = list(STATE, CLLI8)]
  cat(paste("nrow(by_state_wc) = ", nrow(by_state_wc), "\n", sep = ""))
  
  comet_data
}

get_state_for_wc <- function(wirecenter, wc_largest_state)
{
  wc_largest_state[(CLLI8 == wirecenter), STATE]
}

load_comet_sample <- function()
{
  sample_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\SAMPLED_COMET_DATA_2016.TXT"
  sampled_comet_data <- fread(sample_filename, header = TRUE, sep = "|", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("character", "character", "character", "numeric", "character",
					               "Date", "Date", "character", "numeric", "Date", 
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "Date", "Date",
								   "Date", "numeric", "Date", "numeric", "character",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "character", "character", "character", "character", "Date",
								   "character", "character", "character", "character", "character",
								   "numeric", "character", "numeric", "numeric", "numeric",
								   "character", "numeric", "character", "character", "numeric",
					               "Date", "numeric", "numeric", "character", "character",
								   "character", "Date", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "character", "character", "character", "character"),
                    data.table = TRUE)
}

check_if_column_name <- function(input)
{
  if ((input == "") || (substr(input, 1, nchar("VARCHAR")) == "VARCHAR") || (substr(input, 1, nchar("SMALLINT")) == "SMALLINT") ||
      (substr(input, 1, nchar("INTDATE")) == "INTDATE") || (substr(input, 1, nchar("DECIMAL")) == "DECIMAL") || 
	  (substr(input, 1, nchar("INTEGER")) == "INTEGER") || (substr(input, 1, nchar("CHAR")) == "CHAR"))
	 return(FALSE)
  return(TRUE)
} 

prepare_for_contract_renewal <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  #minus_90_date <- Sys.Date() - 90
  #plus_90_date <- Sys.Date() + 90
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  #for_today <- comet_data[((TRACKER_DT >= as.character(minus_90_date)) & (TRACKER_DT <= as.character(plus_90_date))),]
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #Before dropping dates, generate corresponding factor variables
  for_today[, renewed := (RENEW_DATE != "")]
  for_today[, AIO_offered := (AIO_OFFER_DT != "")]
  for_today[, CRM_offered := (CRM_OFFER_DT != "")]
  for_today[, video_disconnected := (VIDEO_DISCONNECT_DT != "")]
  for_today[, data_disconnected := (DATA_DISCONNECT_DT != "")]
  for_today[, account_disconnected := (ACCT_DISCONNECT_DT != "")] #0.97% of for_today is account_disconnected
  
  cols <- c("STATE", "ACCT_SERVICE_TYPE", "CONTROL_GROUP", "TRACKER_LOCK", "VIDEO_CONTROLLABLE", "BUNDLE_NAME", "NEW_BUNDLE_NAME", 
            "MDU_FLAG", "AIO_OFFER_NAME", "VIDEO_NOT_CONTROLLABLE", "DATA_NOT_CONTROLLABLE", "DATA_CONTROLLABLE", "AIO_CHANNEL_NAME",
			"PromoOfferMix", "IONT_OFFER", "PromoOfferMix_Before", "BUNDLE_NAME_DATA", "NEW_BUNDLE_NAME_DATA", "CLLI8", "DATA_MOVES",
			"VIDEO_MOVES", "PUP_WR97646", "PUP_WR98238", "PUP_WR102621", "PUP_WR102596", "VIDEO_DSICONNECT_RSN_CD", "DATA_DSICONNECT_RSN_CD",
			"renewed", "AIO_offered", "CRM_offered", "video_disconnected", "data_disconnected", "account_disconnected")
  for_today[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
  for_today[ ,c("ACCTSK", "TRACKER_DT", "TRACKER_EXEC_DT", "RENEW_DATE", "AIO_OFFER_DT", 
                "CRM_OFFER_DT", "VIDEO_DISCONNECT_DT", "DATA_DISCONNECT_DT", "NEW_CONTRACT_END_DATE", 
				"ACCT_STRT_DT", "ACCT_DISCONNECT_DT") := NULL]
  for_today
}

analyze_contract_renewal <- function(comet_data)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  #W/o any optimization, the decision tree on all 1,136,189 rows splits only based on AIO_OFFER_NAME. One group has 
  #93.6% FALSE and 6.35% TRUE, and the other node has 96.6% TRUE and 3.3% FALSE. Together, these two nodes have 
  #999318 + 136871 = 1,136,189 points. But one reason for this may be decision tree favors categorical variables with too many (274)
  #distinct values: can we reduce the number of distinct values by grouping values together?
  
  #With the reduction in the number of distinct values for the categorical features, AIO_offered became the only feature used.
  #93.67% of people who were not offered AIO did not renew, 96.6% of people who were offered AIO renewed.
  for_today <- reduce_number_of_distinct_values(for_today)
  dtree <- rpart(renewed ~ ., data = for_today)
}

analyze_factor_variables <- function(comet_data)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  columns <- names(for_today)
  for (column in columns)
  {
    if (is.factor(for_today[, get(column)]))
	{
	  #CLLI8 has 7086 unique values, AIO_OFFER_NAME has 203, PromoOfferMix_Before has 35, PromoOfferMix has 34, ACCT_SERVICE_TYPE has 15,
	  #BUNDLE_NAME has 30, NEW_BUNDLE_NAME has 30.
	  cat(paste("column = ", column, ", no. of unique values = ", length(unique(for_today[, get(column)])), "\n", sep = ""))
	}
  }
}

#Since decision tree favors categorical variables with too many distinct values, reduce the number of 
#distinct values of such variables by merging the categories after top (k-1).
reduce_number_of_distinct_values <- function(for_today)
{
  k <- 5
  columns <- names(for_today)
  columns <- columns[(columns != "STATE")]
  for (column in columns)
  {
    if (is.factor(for_today[, get(column)]))
	{
	  tx <- table(for_today[, get(column)])
      names_in_order <- names(tx[order(-tx)])
	  if (length(names_in_order) > k)
	  {
        top_names <- names_in_order[1:(k - 1)]	 
        if ("" %in% top_names)
        {
         top_names <- names_in_order[1:k]
		 top_names <- top_names[top_names != ""]
        }
		set(for_today, j = column, value = ifelse(for_today[[column]] %in% top_names, as.character(for_today[[column]]), "Other"))
	  }
	}
  }
  #Among states, put CA, TX, FL in "Other" bucket
  for_today[, STATE := ifelse((STATE %in% c("CA", "TX", "FL")), "Other", as.character(for_today[, STATE]))]
  for_today
}

#Create a forest of decision trees with sampled feature sets.
el_yunque_sample_features_with_equal_prob <- function(comet_data, F = 5, T = 50)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  for_today <- reduce_number_of_distinct_values(for_today)
  
  for (i in 1:T)
  {
    features <- names(for_today)
	features <- features[features != "renewed"]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("renewed ~ ", paste(sampled_features, collapse = " + "), sep = "")
    dtree <- rpart(as.formula(formula_str), data = for_today)
	if (!is.null(dtree$splits))
	{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	  print_dtree(dtree)
	}
  }
  dtree
}

el_yunque_sample_features_with_input_prob <- function(comet_data, F = 5, T = 50)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  for_today <- reduce_number_of_distinct_values(for_today)
  filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\COMET_Data_Weight_for_Impetus_POC_processed.csv"
  column_weights <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, colClasses = c("character", "numeric"), data.table = TRUE)
  sampling_probs <- column_weights$Weight/sum(column_weights$Weight)
  
  for (i in 1:T)
  {
	sampled_features <- column_weights$ColumnName[sample(nrow(column_weights), F, prob = sampling_probs)]
	formula_str <- paste("renewed ~ ", paste(sampled_features, collapse = " + "), sep = "")
    dtree <- rpart(as.formula(formula_str), data = for_today)
	if (!is.null(dtree$splits))
	{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	  print_dtree(dtree)
	}
  }
  dtree
}

el_yunque_sample_features_from_business_provided_features <- function(comet_data, F = 5, T = 50)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  for_today <- reduce_number_of_distinct_values(for_today)
  cols_to_retain <- c("MODEL_DECILE", "MDU_FLAG", "STATE", "CLLI8", 
                      "BUNDLE_NAME", "NEW_BUNDLE_NAME", "BUNDLE_NAME_DATA", "NEW_BUNDLE_NAME_DATA",
                      "PUP_WR102621", "PUP_WR102596", "PromoOfferMix", "PromoAmt", "PromoOfferMix_Before", "PromoAmt_Before", 
					  "CSSC_CALLS_AFTER30", "VENDOR_CALLS_AFTER30", "renewed")
  for_today <- for_today[, .SD, .SDcols = cols_to_retain]
  
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\Tree_output_", timestr, ".txt", sep = "")
  
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- cols_to_retain
	features <- features[features != "renewed"]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("renewed ~ ", paste(sampled_features, collapse = " + "), sep = "")
    dtree <- rpart(as.formula(formula_str), data = for_today)
	if (!is.null(dtree$splits))
	{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	  print_dtree(dtree)
	}
  }
  sink()
  dtree
}

get_path_to_node <- function(node_id)
{
  j <- node_id
  path <- c()
  while (j > 1)
  {
    path <- c(path, j)
	j <- floor(j/2)
  }
  sort(path)
}

#thr_majority_size_in_node tells how much the size of the majority group in a node should be,
#as a fraction of the original population. thr_majority tells at least how much the probability of the 
#majority class in a node should be.
print_dtree <- function(dtree, thr_majority_size_in_node = 0.05, thr_majority = 0.6)
{
  tree_data <- dtree$frame
  population_size <- tree_data[1, "n"]
  n_nodes <- nrow(tree_data)
  tree_data$node_id <- rownames(tree_data)
  cat("\n\n")
  for (i in 2:n_nodes) #Don't print root
  {
    majority_prob <- max(tree_data[i, "yval2"][4], tree_data[i, "yval2"][5])
	majority_size_in_node <- tree_data[i, "n"] - tree_data[i, "dev"]
	#cat(paste("From node ", tree_data[i, "node_id"], ", majority_size as a fraction of population = ",
	#          majority_size_in_node/population_size, "\n", sep = ""))
	if (majority_size_in_node >= (thr_majority_size_in_node*population_size) && majority_prob >= thr_majority)
	{
	  path <- paste(get_path_to_node(as.numeric(tree_data[i, "node_id"])), collapse = " -> ")
	  cat(paste("Along path ", path, ", ", 
	            majority_size_in_node, " (", round(100*majority_size_in_node/population_size, 2), "% of population) ", 	
				ifelse((tree_data[i, "yval"] == 2), "renewed", "did not renew"), "\n", sep = ""))
	}
  }
}

comet_data <- load_comet_data()
#comet_data <- load_comet_sample()
#for_today <- prepare_for_contract_renewal(comet_data)
#reduce_number_of_distinct_values(for_today)
#dtree <- analyze_contract_renewal(comet_data)
#dtree <- el_yunque(comet_data)
#dtree <- el_yunque_sample_features_from_business_provided_features(comet_data)
wc_largest_state <- analyze_wire_centers(comet_data)
comet_data <- update_states(comet_data)



