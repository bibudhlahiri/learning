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
#Pick the subset of p high-value merchants for each cluster and store it somewhere
high_val_merchants <- lapply(as.list(1:k), function(x) sample(1:n_merchants, p))
#Get the high-value merchants for each client
out = to.dfs(keyval(rep(clients$id, each = p), unlist(high_val_merchants[clients$cluster])))
client_high_val_merchant = as.data.frame(from.dfs(out))
colnames(client_high_val_merchant) <- c("client_id", "merchant_id")
#Assign high value to these merchants for these clients
#client_high_val_merchant$purchase_vol <- sample(high_vol_lower_limit:high_vol_upper_limit, nrow(client_high_val_merchant), replace = TRUE)
client_high_val_merchant = as.data.frame(keyval(client_high_val_merchant, sample(high_vol_lower_limit:high_vol_upper_limit, nrow(client_high_val_merchant), replace = TRUE)))
colnames(client_high_val_merchant) <- c("client_id", "merchant_id", "purchase_vol")
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
       if (merchant1 < merchant2)  # < ensures one pair of merchants appears exactly once for a client
       {
         str_key = paste(merchant1, "_", merchant2, sep = "")
         num_val = as.numeric(purchase1)*as.numeric(purchase2)
         merchant_pair_product <- paste(str_key, "_", num_val, sep = "")
         clients_bag <- c(clients_bag, merchant_pair_product)
       }
     }
   }
   return(keyval(client, clients_bag))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_products")
mrjob1 = mapreduce(input = client_high_val_merchant, output = "/Users/blahiri/learning/rhadoop/merchant_pair_products/", 
                   output.format = make.output.format(format = "csv", mode = "text", sep = ","), 
                   map = map1, reduce = reduce1)
#results <- from.dfs(input = "/Users/blahiri/learning/rhadoop/merchant_pair_products/")

asa.csv.input.format = make.input.format(format='csv', mode='text', streaming.format = NULL, sep=',',
										 col.names = c('client', 'merchant_pair_product'),
										 stringsAsFactors=F)
library(stringr)
map2 = function(., merchant_pair_products) {
          #Input value to mapper function is a data frame
          df <- as.data.frame(str_split_fixed(merchant_pair_products$merchant_pair_product, "_", 3))
          colnames(df) <- c("merchant1", "merchant2", "product")
          keyval(paste(df$merchant1, "_", df$merchant2, sep = ""), df$product)
       }
reduce2 = function(merchant_pair, product) {
  #Since product was originally a vector of factor variables, had to cast to characters before converting to numeric.
  product <- as.numeric(as.character(product))
  n <- length(product)
  dot_product <- 0
  for (i in 1:n)
  {
    dot_product <- dot_product + product[i]
    cat(paste("merchant_pair = ", merchant_pair, ", product[i] = ", product[i], ", dot_product = ", dot_product, "\n", sep = ""), file = stderr())
  }
  #dot_product = sum(as.numeric(product))
  cat(paste("merchant_pair = ", merchant_pair, ", dot_product = ", dot_product, "\n", sep = ""), file = stderr())
  return(keyval(merchant_pair, dot_product))
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_dot_product")
mrjob2 = mapreduce(input = "/Users/blahiri/learning/rhadoop/merchant_pair_products/",  
                   input.format = asa.csv.input.format,
                   output = "/Users/blahiri/learning/rhadoop/merchant_pair_dot_product/", 
                   output.format = "csv", map = map2, reduce = reduce2)
