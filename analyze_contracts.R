library(rpart)
library(data.table)

#The following method loads COMET extract from a local folder, defined by <filename>, and 
#saves to local machine an output sample of a given size.
load_comet_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_3\\COMET_DATA_2016_3.TXT"
  #Read 2,476,484 rows in 30 seconds
  comet_data <- fread(filename, header = FALSE, sep = "|", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("character", "numeric", "Date", "character", "character", #1-5
					               "character", "numeric", "numeric", "numeric", "numeric", #6-10
						           "numeric", "numeric", "numeric", "numeric", "numeric", #11-15
								   "numeric", "numeric", "numeric", "numeric", "numeric", #16-20
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "Date", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "Date", "numeric", "numeric", "numeric", "numeric",
								   "Date", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "Date", "Date", "character", "character", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "numeric", "numeric", "numeric", "numeric",
								   "numeric", "character", "character", "character", "character",
								   "character", "character", "numeric", "character", "character", #91-95
								   "numeric", "character", "character", "character", "character", #96-100
								   "character", "Date", "character", "character", "character"     #101-104, last one for COMPETR1_NM
								   ), 
                    data.table = TRUE)
					
  #Read the column names from the provided file Schema.txt. We do not need the variable types from there.
  lines <- readLines("C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_3\\Schema.txt")
  #Split the input lines from Schema.txt, so that column names and column types show up as different elements in a vector
  #named from_schema_file
  from_schema_file <- unlist(strsplit(lines, " "))
  #The vector from_schema_file needs to be "cleaned", to remove the data types, tabs and spaces. The function check_if_column_name()
  #is called to check which entries in from_schema_file are valid values for column names.
  is_column_name <- unlist(lapply(from_schema_file, check_if_column_name))
  #Just keep the valid column names.
  column_names <- from_schema_file[which(is_column_name == TRUE)]
  setnames(comet_data, names(comet_data), column_names)
  
  #Update the MODEL_DECILE values so that all -1 (absent) values are replaced by 9.
  comet_data[, MODEL_DECILE := ifelse((MODEL_DECILE == -1), 9, MODEL_DECILE)]
  
  #We found an issue in the data that many wire centers correspond to more than one states. For such wire centers,
  #through analyze_wire_centers() and update_states(), we are updating the states as follows: take the state which has
  #maximum accounts with that wire center. Replace all other values of states by the majority state.
  wc_largest_state <- analyze_wire_centers(comet_data)
  comet_data <- update_states(comet_data)
  
  #We are creating "regions", an entity at a level of granularity between state and wire center. If a wire center has 
  #5000 (configurable parameter) or more accounts, it forms a region by itself. For all other wire centers in the SAME 
  #state, we put them in a single bucket, and name it as "<state_name>_Other".
  comet_data <- create_regions(comet_data)
  
  #We create a sample of size 60,000, save it to disk, and use it for constructing multiple decision trees to make things faster.
  sample_size <- 60000
  sampled_comet_data <- comet_data[sample(nrow(comet_data), sample_size), ]
  sample_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_3\\SAMPLED_COMET_DATA_2016.TXT"
  write.table(sampled_comet_data, sample_filename, sep = "|", row.names = FALSE, col.names = TRUE, quote = FALSE)
  comet_data
}

#The vector from_schema_file needs to be "cleaned", to remove the data types, tabs and spaces. The following function
#is called to check which entries in from_schema_file are valid values for column names. It is written to work on one input 
#string at a time. Sometimes the column names and types were separated by both space and tab, so we have a leading "\t" in the 
#descriptions of some column types in this method, e.g., "\tBYTEINT".
check_if_column_name <- function(input)
{
  if ((input == "") || (substr(input, 1, nchar("VARCHAR")) == "VARCHAR") || (substr(input, 1, nchar("SMALLINT")) == "SMALLINT") ||
      (substr(input, 1, nchar("INTDATE")) == "INTDATE") || (substr(input, 1, nchar("DECIMAL")) == "DECIMAL") || 
	  (substr(input, 1, nchar("INTEGER")) == "INTEGER") || (substr(input, 1, nchar("CHAR")) == "CHAR") ||
	  #New dataset has bunch of tabs
	  (input == "\t") || (substr(input, 1, nchar("\tVARCHAR")) == "\tVARCHAR") || (substr(input, 1, nchar("\tSMALLINT")) == "\tSMALLINT") ||
      (substr(input, 1, nchar("\tINTDATE")) == "\tINTDATE") || (substr(input, 1, nchar("\tDECIMAL")) == "\tDECIMAL") || 
	  (substr(input, 1, nchar("\tINTEGER")) == "\tINTEGER") || (substr(input, 1, nchar("\tCHAR")) == "\tCHAR") ||
	  (substr(input, 1, nchar("\tINT")) == "\tINT") || (substr(input, 1, nchar("\tDATE")) == "\tDATE") ||
	  (substr(input, 1, nchar("DATE")) == "DATE") || (substr(input, 1, nchar("\tBYTEINT")) == "\tBYTEINT"))
	 return(FALSE)
  return(TRUE)
} 

#This method returns wc_largest_state, a data.table with the name of the largest state (state with maximum number of accounts) 
#for each wire center.
analyze_wire_centers <- function(comet_data)
{
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(AcctSK)), by = list(STATE, CLLI8)]
  
  #There are wire centers which are in multiple states. For now, take the state in which the wire center has maximum accounts,
  #and for all accounts in that wire center, update the state values to that state.
  setkey(by_state_wc, CLLI8, n_accounts)
  by_state_wc <- by_state_wc[order(CLLI8, -n_accounts)]
  #At this point, by_state_wc will have all the states for a given wire center placed consecutively, and  
  #ordered by the number of accounts that belong to that combination of state and wire center. For examle, if 
  #RCPKNJ01PS0 has accounts in multiple states, but most accounts in NJ, then NJ will be at number 1, and so on.
  #So, we are just taking the first state for each wire center in the next step.
  wc_largest_state <- by_state_wc[, .SD[1], by = CLLI8] #Back to 7498 states
  wc_largest_state <- wc_largest_state[, n_accounts := NULL]
}

update_states <- function(comet_data)
{
  wc_largest_state <- analyze_wire_centers(comet_data)
  #Join comet_data and wc_largest_state by CLLI8 so that we have the largest state for a 
  #given wire center in comet_data.
  names(wc_largest_state) <- c("CLLI8", "largest_state")
  setkey(wc_largest_state, CLLI8)
  setkey(comet_data, CLLI8)
  comet_data <- comet_data[wc_largest_state, nomatch = 0]
  comet_data[, STATE := NULL]
  setnames(comet_data, "largest_state", "STATE")
  #Check back after replacement
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(AcctSK)), by = list(STATE, CLLI8)]
  cat(paste("nrow(by_state_wc) = ", nrow(by_state_wc), "\n", sep = ""))
  comet_data
}

#Create regions at a level of granularity between state and CLLI8. Let the wire centers with 5000 or more accounts in 
#a state remain as-is, put all other wire centers in a single bucket.
create_regions <- function(comet_data, thr_wc = 5000)
{
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(AcctSK)), by = list(STATE, CLLI8)]
  by_state_wc <- by_state_wc[order(STATE, -n_accounts)]
  by_state_wc[, region := ifelse((n_accounts >= thr_wc), as.character(by_state_wc[, CLLI8]), 
             paste(as.character(by_state_wc[, STATE]), "Other", sep = "_"))]
  by_state_wc <- by_state_wc[, c("n_accounts", "STATE") := NULL]		 
  
  setkey(comet_data, CLLI8)
  setkey(by_state_wc, CLLI8)
  comet_data <- comet_data[by_state_wc, nomatch = 0]
  
  #Check results in updated comet_data
  setkey(comet_data, STATE, region)
  by_state_region <- comet_data[, list(n_accounts = length(AcctSK)), by = list(STATE, region)]
  setkey(by_state_region, STATE, n_accounts)
  by_state_region <- by_state_region[order(STATE, -n_accounts)]
  print(by_state_region) #71 regions created with thr_wc = 5000

  comet_data
}

#The following method just loads the sample from the local machine that was written in load_comet_data().
load_comet_sample <- function()
{
  sample_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_3\\SAMPLED_COMET_DATA_2016.TXT"
  sampled_comet_data <- fread(sample_filename, header = TRUE, sep = "|", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("character", "numeric", "Date", "character", "character", #1-5 (A-E)
					               "numeric", "numeric", "numeric", "numeric", "numeric", #6-10 (F-J)
						           "numeric", "numeric", "numeric", "numeric", "numeric", #11-15 (K-O)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #16-20 (P-T)
								   "numeric", "numeric", "numeric", "numeric", "Date", #21-25 (U-Y)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #26-30 (Z-AD)
								   "numeric", "numeric", "numeric", "numeric", "Date", #31-35 (AE-AI)
								   "numeric", "numeric", "numeric", "numeric", "Date", #36-40 (AJ-AN)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #41-45 (AO-AS)
								   "numeric", "numeric", "numeric", "numeric", "Date", #46-50 (AT-AX)
								   "Date", "character", "character", "numeric", "numeric", #51-55 (AY-BC)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #56-60 (BD-BH)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #61-65 (BI-BM)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #66-70 (BN-BR)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #71-75 (BS-BW)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #76-80 (BX-CB)
								   "numeric", "numeric", "numeric", "numeric", "numeric", #81-85 (CC-CG)
								   "character", "character", "character", "character", "character", #86-90 (CH-CL)
								   "character", "numeric", "character", "character", "numeric", #91-95 (CM-CQ)
								   "character", "character", "character", "character", "character", #96-100 (CR-CV)
								   "Date", "character", "character", 
								   "character", #Added for COMPETR1_NM
								   "character", "character" #101-106 (CW-DB)
								   ), data.table = TRUE) #The last two "character" fields are added for STATE and region in reading sample
}

#The following method takes the "base dataset" that we work on from the sample or the entire dataset, 
#whichever is provided as the input argument comet_data. The base dataset is all the accounts which have a 
#contract renewal date (TRACKER_DT) within last 180 days. 
prepare_for_contract_renewal <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #The date fields will be dropped because they are not to be used in modeling exercise. 
  #But before dropping dates, generate corresponding factor variables
  for_today[, renewed := (RENEW_DATE != "")]
  for_today[, AIO_offered := (AIO_OFFER_DT != "")]
  for_today[, CRM_offered := (CRM_OFFER_DT != "")]
  for_today[, video_disconnected := (VIDEO_DISCONNECT_DT != "")]
  for_today[, data_disconnected := (DATA_DISCONNECT_DT != "")]
  for_today[, account_disconnected := (ACCT_DISCONNECT_DT != "")] #0.97% of for_today is account_disconnected
  for_today[, has_competitor := (COMPETR1_NM != "")]
  
  #List the columns whose type should be "factor", i.e., the categorical variables
  cols <- c("STATE", "region", "ACCT_SERVICE_TYPE", "VIDEO_CONTROLLABLE",  
            "MDU_FLAG", "VIDEO_NOT_CONTROLLABLE", "DATA_NOT_CONTROLLABLE", "DATA_CONTROLLABLE", 
			"PromoOfferMix", "IONT_OFFER", "PromoOfferMix_Before", "CLLI8", 
			"PUP_WR102621", "PUP_WR102596", 
			"renewed", "video_disconnected", "data_disconnected", "account_disconnected",
			"Upgarde_Video", "Upgrade_Data", "Downgrade_Video", "Downgrade_Data", "Cust_PBA_M30", "Cust_PBA_30",
			"COMPETR1_NM", "has_competitor")
  #Use lapply() to set all of them to factor at once
  for_today[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
  #Dropping the date fields and AcctSK
  for_today[ ,c("AcctSK", "TRACKER_DT", "RENEW_DATE", "AIO_OFFER_DT", 
                "CRM_OFFER_DT", "VIDEO_DISCONNECT_DT", "DATA_DISCONNECT_DT",
				"ACCT_DISCONNECT_DT") := NULL]
  for_today
}


#Since decision tree favors categorical variables with too many distinct values, reduce the number of 
#distinct values of such variables (except STATE and region) by merging the categories after top (k-1).
reduce_number_of_distinct_values <- function(for_today)
{
  k <- 5
  columns <- names(for_today)
  columns <- columns[!(columns %in% c("STATE", "region"))]
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
  for_today
}

#The following is the core method that creates T decision trees, each with (F + 2) features, where the 
#F features are sampled at random from the featurset mentioned as "cols_to_retain".
el_yunque_sample_features_from_business_provided_features <- function(comet_data, F = 5, T = 50)
{
  for_today <- prepare_for_contract_renewal(comet_data)
  for_today <- reduce_number_of_distinct_values(for_today)
  #The following features were provided as input by the business users: business wants to see how these impact renewal/non-renewal
  cols_to_retain <- c("MODEL_DECILE", "MDU_FLAG", "STATE", "region", #Replacing CLLI8 by region 
                      "PUP_WR102621", "PUP_WR102596", "PromoOfferMix", "PromoAmt", "PromoOfferMix_Before", "PromoAmt_Before", 
					  "CSSC_CALLS_AFTER30", "VENDOR_CALLS_AFTER30", "CSSC_CALLS_BEFORE30", "VENDOR_CALLS_BEFORE30", 
					  "CSSC_CALLS_BEFORE60", "VENDOR_CALLS_BEFORE60", "renewed",
					  "Upgarde_Video", "Upgrade_Data", "Downgrade_Video", "Downgrade_Data", "Cust_PBA_M30", "Cust_PBA_30",
					  "PBA_M30_AMT", "PBA_30_AMT", "COMPETR1_NM", "has_competitor")
  for_today <- for_today[, .SD, .SDcols = cols_to_retain]
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_3\\renewal_results\\Tree_output_", timestr, ".txt", sep = "")
  
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- cols_to_retain
	features <- features[features != "renewed"]
	sampled_features <- features[sample(length(features), F)]
	#Force the usage of state and region
	if (!("STATE" %in% sampled_features))
	  sampled_features <- c(sampled_features, "STATE")
	if (!("region" %in% sampled_features))
	  sampled_features <- c(sampled_features, "region")
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

#The following method is a quick way to generate the path to a node 
#in the decision tree from root
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

#comet_data <- load_comet_data()
comet_data <- load_comet_sample()
dtree <- el_yunque_sample_features_from_business_provided_features(comet_data) #Neither COMPETR1_NM nor has_competitor gets picked up




