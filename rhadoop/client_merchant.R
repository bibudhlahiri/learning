Sys.setenv(HADOOP_STREAMING = "/Users/blahiri/hadoop/hadoop-2.7.0/share/hadoop/tools/lib/hadoop-streaming-2.7.0.jar")
Sys.setenv(HADOOP_CMD = "/Users/blahiri/hadoop/hadoop-2.7.0/bin/hadoop")
#Sys.setenv(HADOOP_COMMON_LIB_NATIVE_DIR = "/Users/blahiri/hadoop/hadoop-2.7.0/lib/native")
#Sys.setenv(HADOOP_OPTS = "-Djava.library.path = /Users/blahiri/hadoop/hadoop-2.7.0/lib")

library(rmr2)
set.seed(1)

rmr.options(backend = "hadoop")  
hdfs.data.root = "client_merchant"
hdfs.data = file.path(hdfs.data.root, "input")
hdfs.out.root = hdfs.data.root
hdfs.out = file.path(hdfs.out.root, "output")

n_clients <- 100
n_merchants <- 500
k <- 10
p <- 20
high_vol_lower_limit <- 50
high_vol_upper_limit <- 100

#Assign each client to one of k clusters
clients <- data.frame(id = 1:n_clients, cluster = sample(1:k, n_clients, replace = TRUE))
print(clients)
#Pick the subset of p high-value merchants for each cluster and store it somewhere
high_val_merchants <- lapply(as.list(1:k), function(x) sample(1:n_merchants, p))
print(high_val_merchants)
#Get the high-value merchants for each client
out = to.dfs(keyval(rep(clients$id, each = p), unlist(high_val_merchants[clients$cluster])))
client_high_val_merchant = as.data.frame(from.dfs(out))
colnames(client_high_val_merchant) <- c("client_id", "merchant_id")
#Assign high value to these merchants for these clients
client_high_val_merchant = as.data.frame(keyval(client_high_val_merchant, sample(high_vol_lower_limit:high_vol_upper_limit, nrow(client_high_val_merchant), replace = TRUE)))
colnames(client_high_val_merchant) <- c("client_id", "merchant_id", "purchase_vol")
print(client_high_val_merchant)
client_high_val_merchant = to.dfs(client_high_val_merchant)

#Compute the cosine similarity between all pairs of clients/merchants(?)
#First, compute the dot products between pairs of (column) vectors representing merchants
map1 = function(., client_merchant) {
          keyval(client_merchant$client_id, paste(client_merchant$merchant_id, "_", client_merchant$purchase_vol, sep = ""))
       }
reduce1 = function(client, merchant_purchase) {
  #Initialize a collection data structure to store all tuples for a given client
  clients_bag = c()
  for (other1 in merchant_purchase)
   {
     for (other2 in merchant_purchase)
     {   
       split1 = unlist(strsplit(other1, "_"))
       merchant1 = split1[1]; purchase1 = split1[2]  
       split2 = unlist(strsplit(other2, "_"))
       merchant2 = split2[1]; purchase2 = split2[2]
       if (as.numeric(merchant1) < as.numeric(merchant2))  # < ensures one pair of merchants appears exactly once for a client
       {
         str_key = paste(merchant1, "_", merchant2, sep = "")
         num_val = as.numeric(purchase1)*as.numeric(purchase2)
         merchant_pair_product <- paste(str_key, "_", num_val, sep = "")
         clients_bag <- c(clients_bag, merchant_pair_product)
       }
     }
   }
   keyval(client, clients_bag)
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_products")
mrjob1 = mapreduce(input = client_high_val_merchant, output = "/Users/blahiri/learning/rhadoop/merchant_pair_products/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map1, reduce = reduce1)


csv.input.format1 = make.input.format(format='csv', mode='text', streaming.format = NULL, sep=',',
										 col.names = c('client', 'merchant_pair_product'),
										 stringsAsFactors=F)
library(stringr)
map2 = function(., merchant_pair_products) {
          #Input value to mapper function (merchant_pair_products) is a data frame
          df <- as.data.frame(str_split_fixed(merchant_pair_products$merchant_pair_product, "_", 3))
          colnames(df) <- c("merchant1", "merchant2", "product")
          keyval(paste(df$merchant1, "_", df$merchant2, sep = ""), df$product)
       }
reduce2 = function(merchant_pair, product) {
  #Since product was originally a vector of factor variables, had to cast to characters before converting to numeric.
  keyval(merchant_pair, sum(as.numeric(as.character(product))))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_dot_product")
mrjob2 = mapreduce(input = "/Users/blahiri/learning/rhadoop/merchant_pair_products/",  
                   input.format = csv.input.format1,
                   output = "/Users/blahiri/learning/rhadoop/merchant_pair_dot_product/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","),
                   map = map2, reduce = reduce2)
                   
                   
map3 = function(., client_merchant) {
          keyval(client_merchant$merchant_id, client_merchant$purchase_vol)
       }
reduce3 = function(merchant_id, purchase_vol) {
  num_purchase_vol <- as.numeric(as.character(purchase_vol))
  purchase_vol_square <- num_purchase_vol^2
  keyval(merchant_id, round(sqrt(sum(purchase_vol_square)), 2))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_l2_norms")
mrjob3 = mapreduce(input = client_high_val_merchant, output = "/Users/blahiri/learning/rhadoop/merchant_l2_norms/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map3, reduce = reduce3)
                   
csv.input.format2 = make.input.format(format='csv', mode='text', streaming.format = NULL, sep=',',
									  col.names = c('merchant_id', 'l2_norm'),
									  stringsAsFactors=F)

map4 = function(., merchant_l2_norms){
  #cat(paste("from map4, class(merchant_l2_norms$merchant_id) = ", class(merchant_l2_norms$merchant_id), "\n", sep = ""), file = stderr())
  copy <- merchant_l2_norms
  colnames(copy) <- c("merchant_id1", "l2_norm1")
  cartesian_product <- merge(merchant_l2_norms, copy)
  pairs <- subset(cartesian_product, (merchant_id < merchant_id1))
  pairs$merchant_pair <- paste(pairs$merchant_id, "_", pairs$merchant_id1, sep = "")
  pairs <- pairs[, c("merchant_pair", "l2_norm", "l2_norm1")]
  keyval(pairs$merchant_pair, paste(pairs$l2_norm, "_", pairs$l2_norm1, sep = ""))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_l2_norms")
mrjob4 = mapreduce(input = "/Users/blahiri/learning/rhadoop/merchant_l2_norms/", 
                   input.format = csv.input.format2,
                   output = "/Users/blahiri/learning/rhadoop/merchant_pair_l2_norms/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map4)
                   
#Copy the dot products and the L2 norms for merchant pairs to a common location. Tag them to indicate the filetype. 
#They will be taken from this location for computing the final cosine similarity.
merchant_pair_dot_products <- read.csv("/Users/blahiri/learning/rhadoop/merchant_pair_dot_product/part-00000", header = FALSE)
colnames(merchant_pair_dot_products) <- c("merchant_pair", "dot_product")
merchant_pair_dot_products$dot_product <- paste("d_", merchant_pair_dot_products$dot_product, sep = "")
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_all_inputs")
system("mkdir /Users/blahiri/learning/rhadoop/merchant_pair_all_inputs")
write.table(merchant_pair_dot_products, "/Users/blahiri/learning/rhadoop/merchant_pair_all_inputs/merchant_pair_dot_products", row.names = FALSE, col.names = FALSE, sep = ",")

merchant_pair_l2_norms <- read.csv("/Users/blahiri/learning/rhadoop/merchant_pair_l2_norms/part-00000", header = FALSE)
colnames(merchant_pair_l2_norms) <- c("merchant_pair", "l2_norms")
merchant_pair_l2_norms$l2_norms <- paste("l_", merchant_pair_l2_norms$l2_norms, sep = "")
write.table(merchant_pair_l2_norms, "/Users/blahiri/learning/rhadoop/merchant_pair_all_inputs/merchant_pair_l2_norms", row.names = FALSE, col.names = FALSE, sep = ",")

csv.input.format3 = make.input.format(format='csv', mode='text', streaming.format = NULL, sep=',',
									  col.names = c('merchant_pair', 'other_info'),
									  stringsAsFactors=F)
map5 = function(., merchant_pair_all_inputs){
  keyval(merchant_pair_all_inputs$merchant_pair, merchant_pair_all_inputs$other_info)
}
reduce5 = function(merchant_pair, other_info){
  #Identify the type of info (dot product or L2 norms) based on the prefix
  dot_product <- other_info[(substr(other_info, 1, 2) == "d_")]
  dot_product <- substr(dot_product, 3, nchar(dot_product))
  if (length(dot_product) == 0)  #Not all pairs of merchants will have a dot product because the clients purchasing from them may be disjoint
     dot_product <- 0
  l2_norms <- other_info[(substr(other_info, 1, 2) == "l_")]
  split = unlist(strsplit(l2_norms, "_"))
  l2_norm1 = split[2]
  l2_norm2 = split[3]
  cosine_sim = as.numeric(dot_product)/(as.numeric(l2_norm1)*as.numeric(l2_norm2))
  keyval(merchant_pair, cosine_sim)
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_cosine_sim")
mrjob5 = mapreduce(input = "/Users/blahiri/learning/rhadoop/merchant_pair_all_inputs/", 
                   input.format = csv.input.format3,
                   output = "/Users/blahiri/learning/rhadoop/merchant_pair_cosine_sim/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map5, reduce = reduce5)
                   
csv.input.format4 = make.input.format(format='csv', mode='text', streaming.format = NULL, sep=',',
									  col.names = c('merchant_pair', 'cosine_sim'),
									  stringsAsFactors=F)
map6 = function(., merchant_pair_cosine_sim){
  merchant_pair_cosine_sim <- subset(merchant_pair_cosine_sim, (as.numeric(as.character(cosine_sim)) > 0))
  df <- as.data.frame(str_split_fixed(merchant_pair_cosine_sim$merchant_pair, "_", 2))
  colnames(df) <- c("merchant1", "merchant2")
  df$merchant1 <- sprintf("%03s", df$merchant1)
  df$merchant2 <- sprintf("%03s", df$merchant2)
  distances <- 1 - as.numeric(as.character(merchant_pair_cosine_sim$cosine_sim)) 
  keyval(df$merchant1, paste(distances, "_", df$merchant2, sep = ""))
}
reduce6 = function(merchant1, distance_merchant2) {
  cat(paste("merchant1 = ", merchant1, ", length(distance_merchant2) = ", length(distance_merchant2), "\n", sep = ""), file = stderr())
  keyval(merchant1, sort(distance_merchant2))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_distance_ordered")
mrjob6 = mapreduce(input = "/Users/blahiri/learning/rhadoop/merchant_pair_cosine_sim/", 
                   input.format = csv.input.format4,
                   output = "/Users/blahiri/learning/rhadoop/merchant_pair_distance_ordered/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map6, reduce = reduce6)

