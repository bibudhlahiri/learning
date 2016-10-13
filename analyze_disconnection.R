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
  sample_size <- 20000
  sampled_comet_data <- comet_data[sample(nrow(comet_data), sample_size), ]
  sample_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016\\SAMPLED_COMET_DATA_2016.TXT"
  write.table(sampled_comet_data, sample_filename, sep = "|", row.names = FALSE, col.names = TRUE, quote = FALSE)
  comet_data
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

prepare_for_disconnection <- function(comet_data)
{
  minus_90_date <- Sys.Date() - 90
  plus_90_date <- Sys.Date() + 90
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_90_date)) & (TRACKER_DT <= as.character(plus_90_date))),] #Do we do this 
  #analysis also for the same [-90, +90] window?
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #Before dropping dates, generate corresponding factor variables
  for_today[, renewed := (RENEW_DATE != "")] #Are we interested in seeing whether renewal/non-renewal influences disconnection?
  for_today[, AIO_offered := (AIO_OFFER_DT != "")]
  for_today[, CRM_offered := (CRM_OFFER_DT != "")]
  for_today[, video_disconnected := (VIDEO_DISCONNECT_DT != "")]
  for_today[, data_disconnected := (DATA_DISCONNECT_DT != "")]
  for_today[, account_disconnected := (ACCT_DISCONNECT_DT != "")] #We consider only account-level disconnection for this analyis, not
  #video or data disconnection
  
  cols <- c("STATE", "ACCT_SERVICE_TYPE", "CONTROL_GROUP", "TRACKER_LOCK", "VIDEO_CONTROLLABLE", "BUNDLE_NAME", "NEW_BUNDLE_NAME", 
            "MDU_FLAG", "AIO_OFFER_NAME", "VIDEO_NOT_CONTROLLABLE", "DATA_NOT_CONTROLLABLE", "DATA_CONTROLLABLE", "AIO_CHANNEL_NAME",
			"PromoOfferMix", "IONT_OFFER", "PromoOfferMix_Before", "BUNDLE_NAME_DATA", "NEW_BUNDLE_NAME_DATA", "CLLI8", "DATA_MOVES",
			"VIDEO_MOVES", "PUP_WR97646", "PUP_WR98238", "PUP_WR102621", "PUP_WR102596",
			"renewed", "AIO_offered", "CRM_offered", "video_disconnected", "data_disconnected", "account_disconnected")
  for_today[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
  for_today[ ,c("ACCTSK", "TRACKER_DT", "TRACKER_EXEC_DT", "RENEW_DATE", "AIO_OFFER_DT", 
                "CRM_OFFER_DT", "VIDEO_DISCONNECT_DT", "DATA_DISCONNECT_DT", "NEW_CONTRACT_END_DATE", 
				"ACCT_STRT_DT", "ACCT_DISCONNECT_DT", "CLLI8") := NULL]
  for_today
}

analyze_disconnection <- function(comet_data)
{
  for_today <- prepare_for_disconnection(comet_data)
  #The decision tree on all 1,137,251 rows is a rather deep decision tree with depth 5.
  for_today <- reduce_number_of_distinct_values(for_today)
  dtree <- rpart(account_disconnected ~ ., data = for_today)
}


#Since decision tree favors categorical variables with too many distinct values, reduce the number of 
#distinct values of such variables by merging the categories after top (k-1).
reduce_number_of_distinct_values <- function(for_today)
{
  k <- 5
  columns <- names(for_today)
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

#Create a forest of decision trees with sampled feature sets.
el_yunque <- function(comet_data, F = 5, T = 50)
{
  for_today <- prepare_for_disconnection(comet_data)
  for_today <- reduce_number_of_distinct_values(for_today)
  
  for (i in 1:T)
  {
    features <- names(for_today)
	features <- features[features != "account_disconnected"]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("account_disconnected ~ ", paste(sampled_features, collapse = " + "), sep = "")
    dtree <- rpart(as.formula(formula_str), data = for_today)
	if (!is.null(dtree$splits))
	{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	}
  }
  dtree
}

#comet_data <- load_comet_data()
comet_data <- load_comet_sample()
#for_today <- prepare_for_contract_renewal(comet_data)
#reduce_number_of_distinct_values(for_today)
#dtree <- analyze_disconnection(comet_data)
dtree <- el_yunque(comet_data)


