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
  
  data_corr[ , num_outbound_cmds := NULL] #All values are 0
  
  
  #List the columns whose type should be "factor", i.e., the categorical variables. 
  cols <- c("protocol_type", "service", "flag")
  #Use lapply() to set all of them to factor at once
  data_corr[,(cols) := lapply(.SD, as.factor), .SDcols = cols]
  
  #Since the columns land, logged_in, is_host_login, is_guest_login are already binary, we make them numeric.
  #They were originally read as character.
  cols <- c("land", "logged_in", "is_host_login", "is_guest_login")
  data_corr[,(cols) := lapply(.SD, as.numeric), .SDcols = cols]
  
  data_corr <- reduce_number_of_distinct_values(data_corr)
  
  #Create dummy variables corresponding to the factor variables protocol_type, service and flag
  
  data_corr[, protocol_type_icmp := as.numeric(data_corr$protocol_type == "icmp")]
  data_corr[, protocol_type_tcp := as.numeric(data_corr$protocol_type == "tcp")]
  
  data_corr[, service_ecr_i := as.numeric(data_corr$service == "ecr_i")]
  data_corr[, service_private := as.numeric(data_corr$service == "private")]
  
  data_corr[, flag_S0 := as.numeric(data_corr$flag == "S0")]
  data_corr[, flag_SF := as.numeric(data_corr$flag == "SF")]
  
  data_corr[ , c("protocol_type", "service", "flag") := NULL]
  
  #Scale the columns which are really numeric (were not made numeric from categorical) and have a wide range of values.
  #Making these numeric make sense from the semantic point of view.
  sc <- c("duration", "src_bytes", "dst_bytes", "wrong_fragment", "urgent", "hot", "num_failed_logins", 
          "num_compromised", "num_root", "num_file_creations", "num_shells", "num_access_files",
		  "count", "serror_rate", "rerror_rate", "same_srv_rate", "diff_srv_rate", 
		  "srv_count", "srv_serror_rate", "srv_rerror_rate", "srv_diff_host_rate", 
		  "dst_host_count", "dst_host_srv_count") 
  data_corr <- copy(data_corr)[ , (sc) := lapply(.SD, scale), .SDcols = sc]
  #standardized_filename <- "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.standardized"
  #write.table(data_corr, standardized_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  #Round off numeric columns to reduce filesize
  cols_to_round <- names(data_corr)
  cols_to_round <- cols_to_round[(cols_to_round != "connection_label")]
  for (j in cols_to_round) set(data_corr, j = j, value = round(data_corr[[j]], 4))
  
  standardized_sample_filename <- "C:\\Users\\blahiri\\kdd_cup_for_SAx\\kddcup.data.standardized.sampled"
  sample_size <- 25000 #Makes it a ~5 MB file
  sampled_kdd_data <- data_corr[sample(nrow(data_corr), sample_size), ]
  write.table(sampled_kdd_data, standardized_sample_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  data_corr
}


#Reduce the number of distinct values of categorical variables (except connection_label) by merging the categories after top (k-1).
reduce_number_of_distinct_values <- function(input_data)
{
  k <- 3
  columns <- names(input_data)
  columns <- columns[(columns != "connection_label")]
  for (column in columns)
  {
    if (is.factor(input_data[, get(column)]))
	{
	  tx <- table(input_data[, get(column)])
      names_in_order <- names(tx[order(-tx)])
	  if (length(names_in_order) > k)
	  {
        top_names <- names_in_order[1:(k - 1)]	 
        if ("" %in% top_names)
        {
         top_names <- names_in_order[1:k]
		 top_names <- top_names[top_names != ""]
        }
		set(input_data, j = column, value = ifelse(input_data[[column]] %in% top_names, as.character(input_data[[column]]), "Other"))
	  }
	}
  }
  input_data
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
                                colClasses = c("numeric", "numeric", "numeric", "numeric",  #Taking 1 off for num_outbound_cmds
					                           "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "numeric",
											   "numeric", "numeric", "numeric", "numeric", "character",
											   "numeric", "numeric", "numeric", "numeric", "numeric"), 
                                data.table = TRUE)
  kdd_sample <- map_to_five_classes(kdd_sample)
  kdd_sample[, final_label := ifelse((kdd_sample$parent_label == "normal"), "normal", "attack")]
  to_cluster <- kdd_sample[, .SD, .SDcols = sapply(kdd_sample, is.numeric)] #cluster with the numeric columns only
  clusters <- kmeans(to_cluster, centers = 200)
  #Check fraction of normal and attack packets in two clusters.
  to_cluster[, assigned_cluster := clusters$cluster]
  
  #Get an idea of the distance of points from their nearest cluster centers. This will help set the threshold in SAx.
  #to_cluster
  to_cluster[, distance_to_centroid := apply(to_cluster, 1, function(row) find_distance_to_centroid( 
  c(row["duration"], row["src_bytes"], row["dst_bytes"], row["land"], row["wrong_fragment"],
    row["urgent"], row["hot"], row["num_failed_logins"], row["logged_in"], row["num_compromised"],
	row["root_shell"], row["su_attempted"], row["num_root"], row["num_file_creations"], row["num_shells"],
	row["num_access_files"], row["is_host_login"], row["is_guest_login"], row["count"], row["srv_count"],
	row["serror_rate"], row["srv_serror_rate"], row["rerror_rate"], row["srv_rerror_rate"], row["same_srv_rate"],
	row["diff_srv_rate"], row["srv_diff_host_rate"], row["dst_host_count"], row["dst_host_srv_count"], row["dst_host_same_srv_rate"],
	row["dst_host_diff_srv_rate"], row["dst_host_same_src_port_rate"], row["dst_host_srv_diff_host_rate"], row["dst_host_serror_rate"], row["dst_host_srv_serror_rate"],
    row["dst_host_rerror_rate"], row["dst_host_srv_rerror_rate"], row["protocol_type_tcp"], row["service_ecr_i"], row["service_private"],
	row["flag_S0"], row["flag_SF"]),
  row["assigned_cluster"], clusters$centers))]
  
  print(fivenum(to_cluster$distance_to_centroid)) 
  #With k = 2, 0.00621319   0.03545544   0.03545762   3.23626149  114.51476117
  #With k = 3, 0.00207569   0.02620893   0.02621193   1.92907525  114.46398929
  #With k = 4, 1.726433e-03 0.0256287    0.02563178   1.593763    114.2546
  #With k =30, 0.002375915  0.022761839  0.022765315  0.274212546 19.959214687
  #With k =100,0.002936039  0.021564915  0.021568586  0.105381863 13.784577548
  #With k =150,0.00000000   0.02116488   0.02116862   0.08667574  13.78457755
  #With k =200,0.000000000  0.001751310  0.004750433  0.069927011 13.784577548
  percentile <- ecdf(to_cluster$distance_to_centroid)
  print(percentile(10)) #0.99076
  
  to_cluster
}
  
find_distance_to_centroid <- function(current_vec, assigned_cluster, centers)
{
  #print(current_vec)
  sqrt(sum((as.numeric(current_vec) - as.numeric(centers[assigned_cluster, ]))^2))
}
#source("C:\\Users\\blahiri\\kdd_cup_for_SAx\\process_kdd.R")
#data_corr <- load_kdd_data() #4,898,431 rows; 972781 (19.85%) normal
#data_corr <- map_to_five_classes(data_corr)
#print(table(data_corr$parent_label)) #dos 3883370 (79%), probe 41102 (0.8%), r2l 1126 (0.02%), u2r 52
to_cluster <- cluster_kdd()

#euclidean_dist <- test_normalization()






