library(data.table)

load_comet_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\COMET_DATA_2016_2.TXT"
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
								   "character", "Date", "character", "character"), #101-104
                    data.table = TRUE)
  lines <- readLines("C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\Schema.txt")
  from_schema_file <- unlist(strsplit(lines, " "))
  is_column_name <- unlist(lapply(from_schema_file, check_if_column_name))
  column_names <- from_schema_file[which(is_column_name == TRUE)]
  setnames(comet_data, names(comet_data), column_names)
  
  comet_data[, MODEL_DECILE := ifelse((MODEL_DECILE == -1), 9, MODEL_DECILE)]
  wc_largest_state <- analyze_wire_centers(comet_data)
  comet_data <- update_states(comet_data)
  comet_data <- create_regions(comet_data)
}

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
	  (substr(input, 1, nchar("DATE")) == "DATE"))
	 return(FALSE)
  return(TRUE)
}

analyze_wire_centers <- function(comet_data)
{
  setkey(comet_data, STATE, CLLI8)
  by_state_wc <- comet_data[, list(n_accounts = length(AcctSK)), by = list(STATE, CLLI8)]
  
  #There are wire centers which are in multiple states. For now, take the state in which the wire center has maximum accounts,
  #and for all accounts in that wire center, update the state values to that state.
  setkey(by_state_wc, CLLI8, n_accounts)
  by_state_wc <- by_state_wc[order(CLLI8, -n_accounts)]
  wc_largest_state <- by_state_wc[, .SD[1], by = CLLI8] #Back to 7498 states
  wc_largest_state <- wc_largest_state[, n_accounts := NULL]
}

update_states <- function(comet_data)
{
  wc_largest_state <- analyze_wire_centers(comet_data)
  #Join comet_data and wc_largest_state by CLLI8
  names(wc_largest_state) <- c("CLLI8", "largest_state")
  setkey(wc_largest_state, CLLI8)
  setkey(comet_data, CLLI8)
  comet_data <- comet_data[wc_largest_state, nomatch = 0]
  comet_data[, STATE := NULL]
  setnames(comet_data, "largest_state", "STATE")
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
}

analyze_renewal_rates_by_state <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  #For renewal, we are defining the base as accounts which have contract expiry date starting from 6 months before to today. 
  #Thus, we are not including accounts whose contract expiry dates are in future, because people typically renew mostly in 
  #a [-30,+30] window around the contract expiry date. If we take accounts whose contract expiry dates are in future, 
  #the renewal rates will drop.
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #Before dropping dates, generate corresponding factor variables
  for_today[, renewed := as.numeric(RENEW_DATE != "")]
  setkey(for_today, STATE)
  by_state <- for_today[, list(n_accounts = length(AcctSK), n_renewed = sum(renewed)), by = STATE]
  by_state[, renewal_rate := n_renewed/n_accounts]
  by_state <- by_state[order(-renewal_rate)]
  op_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\renewal_results\\renewal_rate_by_state.csv"
  write.table(by_state, op_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  by_state
}

analyze_renewal_rates_by_region <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),] 
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  #Before dropping dates, generate corresponding factor variables
  for_today[, renewed := as.numeric(RENEW_DATE != "")]
  setkey(for_today, region)
  by_region <- for_today[, list(n_accounts = length(AcctSK), n_renewed = sum(renewed)), by = region]
  by_region[, renewal_rate := n_renewed/n_accounts]
  
  #Add state column for ease of reading
  state_region <- unique(comet_data[, list(STATE, region)])
  setkey(by_region, region)
  setkey(state_region, region)
  by_region <- by_region[state_region, nomatch = 0]
  
  by_region <- by_region[order(-renewal_rate)]
  op_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\renewal_results\\renewal_rate_by_region.csv"
  write.table(by_region, op_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  by_region
}

analyze_disconnection_rates_by_state <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  comet_data[, ACCT_DISCONNECT_DT := gsub("/", "-", comet_data$ACCT_DISCONNECT_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  for_today[, account_disconnected := as.numeric((ACCT_DISCONNECT_DT >= as.character(minus_180_date)) 
                                                  & (ACCT_DISCONNECT_DT <= as.character(Sys.Date())))]
  setkey(for_today, STATE)
  by_state <- for_today[, list(n_accounts = length(AcctSK), n_disconnected = sum(account_disconnected)), by = STATE]
  print(by_state)
  by_state[, disconnection_rate := n_disconnected/n_accounts]
  by_state <- by_state[order(-disconnection_rate)]
  op_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\disconnection_results\\disconnection_rate_by_state.csv"
  cat(paste("op_filename = ", op_filename, "\n", sep = ""))
  write.table(by_state, op_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  by_state
}

analyze_disconnection_rates_by_region <- function(comet_data)
{
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  comet_data[, ACCT_DISCONNECT_DT := gsub("/", "-", comet_data$ACCT_DISCONNECT_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  for_today[, account_disconnected := as.numeric((ACCT_DISCONNECT_DT >= as.character(minus_180_date)) 
                                                  & (ACCT_DISCONNECT_DT <= as.character(Sys.Date())))]
  setkey(for_today, region)
  by_region <- for_today[, list(n_accounts = length(AcctSK), n_disconnected = sum(account_disconnected)), by = region]
  by_region[, disconnection_rate := n_disconnected/n_accounts]
  
  #Add state column for ease of reading
  state_region <- unique(comet_data[, list(STATE, region)])
  setkey(by_region, region)
  setkey(state_region, region)
  by_region <- by_region[state_region, nomatch = 0]
  
  by_region <- by_region[order(-disconnection_rate)]
  op_filename <- "C:\\Users\\blahiri\\Verizon\\COMET_DATA_2016_2\\disconnection_results\\disconnection_rate_by_region.csv"
  write.table(by_region, op_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  by_region
}

analyze_high_low_disconnection_regions <- function(comet_data)
{
  setkey(comet_data, region)
  minus_180_date <- Sys.Date() - 180
  comet_data[, TRACKER_DT := gsub("/", "-", comet_data$TRACKER_DT)]
  comet_data[, ACCT_DISCONNECT_DT := gsub("/", "-", comet_data$ACCT_DISCONNECT_DT)]
  setkey(comet_data, TRACKER_DT)
  for_today <- comet_data[((TRACKER_DT >= as.character(minus_180_date)) & (TRACKER_DT <= as.character(Sys.Date()))),]
  cat(paste("nrow(for_today) = ", nrow(for_today), "\n", sep = ""))
  
  for_today[, account_disconnected := as.numeric((ACCT_DISCONNECT_DT >= as.character(minus_180_date)) 
                                                  & (ACCT_DISCONNECT_DT <= as.character(Sys.Date())))]
  high_disc <- for_today[(region %in% c("RCPKNJ01PS0", "NBWKNJNBPS1", "NWTNMAWAPS0", 
                                      "ANNPMDANPS0", "NYCKNYWMPS0", "NYCMNYPSPS0", "PHLAPAMKPS1")),]
  low_disc <- for_today[(!(region %in% c("RCPKNJ01PS0", "NBWKNJNBPS1", "NWTNMAWAPS0", 
                                      "ANNPMDANPS0", "NYCKNYWMPS0", "NYCMNYPSPS0", "PHLAPAMKPS1"))),]
}

comet_data <- load_comet_data()
#by_state <- analyze_renewal_rates_by_state(comet_data)
#by_region <- analyze_renewal_rates_by_region(comet_data)
by_state <- analyze_disconnection_rates_by_state(comet_data)
by_region <- analyze_disconnection_rates_by_region(comet_data)

