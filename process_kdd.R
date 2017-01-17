library(data.table)

load_kdd_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.corrected"
  data_corr <- fread(filename, header = FALSE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                                colClasses = c("numeric", "character", "character", "character", "numeric", 
					                           "numeric", "character", "numeric", "numeric", "numeric",
											   "numeric", "character", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "character", "character", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "character"), 
                                data.table = TRUE)
  lines <- readLines("C:\\Users\\blahiri\\kdd_cup_for_SAx\\Schema.txt")
  from_schema_file <- unlist(strsplit(lines, ": "))
  column_names <- from_schema_file[c(TRUE, FALSE)]
  column_names <- c(column_names, "connection_label")
  setnames(data_corr, names(data_corr), column_names)
  
  #data_corr[, src_bytes := (data_corr$src_bytes - mean(data_corr$src_bytes))/sd(data_corr$src_bytes)]
  #Checked: mean(data_corr$src_bytes) = -1.852102e-17 and sd(data_corr$src_bytes) = 1
  #data_corr[, dst_bytes := (data_corr$dst_bytes - mean(data_corr$dst_bytes))/sd(data_corr$dst_bytes)]
  #Checked: mean(data_corr$dst_bytes) = -1.864818e-17 and sd(data_corr$dst_bytes) = 1
  sc <- c("src_bytes", "dst_bytes", "count", "srv_count", "dst_host_count", "dst_host_srv_count") 
  data_corr <- copy(data_corr)[ , (sc) := lapply(.SD, scale), .SDcols = sc]
  #standardized_filename <- "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.standardized"
  #write.table(data_corr, standardized_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  standardized_sample_filename <- "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.standardized.sampled"
  sample_size <- 24800 #Makes it a < 5 MB file
  sampled_kdd_data <- data_corr[sample(nrow(data_corr), sample_size), ]
  write.table(sampled_kdd_data, standardized_sample_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  data_corr
}
  
#source("C:\\Users\\blahiri\\kdd_cup_for_SAx\\process_kdd.R")
data_corr <- load_kdd_data() #4,898,431 rows; 972781 (19.85%) normal






