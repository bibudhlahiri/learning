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
  sample_size <- 5000
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
  minus_90_date <- Sys.Date() - 90
  plus_90_date <- Sys.Date() + 90
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_90_date)) & (TRACKER_DT <= as.character(plus_90_date))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #Before dropping dates, generate corresponding factor variables
  for_today[, renewed := (RENEW_DATE != "")]
  for_today[, AIO_offered := (AIO_OFFER_DT != "")]
  for_today[, CRM_offered := (CRM_OFFER_DT != "")]
  for_today[, video_disconnected := (VIDEO_DISCONNECT_DT != "")]
  for_today[, data_disconnected := (DATA_DISCONNECT_DT != "")]
  for_today[, account_disconnected := (ACCT_DISCONNECT_DT != "")]
  
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
  for (column in columns)
  {
    if (is.factor(for_today[, get(column)]))
	{
	  tx <- table(for_today[, get(column)])
      names_in_order <- names(tx[order(-tx)])
	  if (length(names_in_order) > k)
	  {
	    #cat(paste("\n\ncolumn = ", column, "\n", sep = ""))
		#print(tx[order(-tx)])
        top_names <- names_in_order[1:(k - 1)]	 
        if ("" %in% top_names)
        {
         top_names <- names_in_order[1:k]
		 top_names <- top_names[top_names != ""]
        }
	    #print(top_names)
		set(for_today, j = column, value = ifelse(for_today[[column]] %in% top_names, as.character(for_today[[column]]), "Other"))
	    #print(table(for_today[, get(column)]))
	  }
	}
  }
  for_today
}

#comet_data <- load_comet_data()
comet_data <- load_comet_sample()
#for_today <- prepare_for_contract_renewal(comet_data)
#reduce_number_of_distinct_values(for_today)
dtree <- analyze_contract_renewal(comet_data)



