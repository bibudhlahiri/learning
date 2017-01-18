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

map_to_five_classes <- function(input_data)
{
  input_data[, parent_label := apply(input_data, 1, function(row) lookup_parent_label(as.character(row["connection_label"])))]
}

lookup_parent_label <- function(connection_label)
{
  if (connection_label %in% c("buffer_overflow.", "loadmodule.", "perl.", "rootkit."))
  {
    return("u2r")
  }
  if (connection_label %in% c("ftp_write.", "guess_passwd.", "imap.", "multihop.", "phf.", "spy.", "warezclient.", "warezmaster."))
  {
    return("r2l")
  }
  if (connection_label %in% c("back.", "land.", "neptune.", "pod.", "smurf.", "teardrop."))
  {
    return("dos")
  }
  if (connection_label == "normal.")
  {
    return("normal")
  }
  return("probe")
}

cluster_kdd <- function()
{
  filename <- "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.standardized.sampled"
  kdd_sample <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
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
  kdd_sample <- map_to_five_classes(kdd_sample)
  kdd_sample[, final_label := ifelse((kdd_sample$parent_label == "normal"), "normal", "attack")]
  clusters <- kmeans(kdd_sample[, .SD, .SDcols = sapply(kdd_sample, is.numeric)],  #cluster with the numeric columns only
                     centers = 2)
  #Check fraction of normal and attack packets in two clusters.
  kdd_sample[, assigned_cluster := clusters$cluster]
  cont_tab <- table(kdd_sample$final_label, kdd_sample$assigned_cluster) #Average purity of clusters is 0.8364662
  #We took the definition of average purity from here: http://www.siam.org/meetings/sdm06/proceedings/030caof.pdf
  #62 points in cluster 1, 24738 in cluster 2. But interestingly, 54 (87%) of points in cluster 1 are normal, whereas
  #19839 (80%) of points in cluster 2 are attack traffic.
  cont_tab
}
  
#source("C:\\Users\\blahiri\\kdd_cup_for_SAx\\process_kdd.R")
#data_corr <- load_kdd_data() #4,898,431 rows; 972781 (19.85%) normal
#data_corr <- map_to_five_classes(data_corr)
#print(table(data_corr$parent_label)) #dos 3883370 (79%), probe 41102 (0.8%), r2l 1126 (0.02%), u2r 52
cont_tab <- cluster_kdd()






