Sys.setenv(HADOOP_STREAMING = "/Users/blahiri/hadoop/hadoop-2.7.0/share/hadoop/tools/lib/hadoop-streaming-2.7.0.jar")
Sys.setenv(HADOOP_CMD = "/Users/blahiri/hadoop/hadoop-2.7.0/bin/hadoop")
#Sys.setenv(HADOOP_COMMON_LIB_NATIVE_DIR = "/Users/blahiri/hadoop/hadoop-2.7.0/lib/native")
#Sys.setenv(HADOOP_OPTS = "-Djava.library.path = /Users/blahiri/hadoop/hadoop-2.7.0/lib")

library(rmr2)

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
  for (other1 in merchant_purchase)
   {
     for (other2 in merchant_purchase)
     {
       cat(paste("client = ", client, ", other1 = ", other1, ", other2 = ", other2, "\n", sep = ""), file = stderr())
       split1 = unlist(strsplit(other1, "_"))
       merchant1 = split1[1]; purchase1 = split1[2]
       cat(paste("merchant1 = ", merchant1, ", purchase1 = ", purchase1, "\n", sep = ""), file = stderr())
       split2 = unlist(strsplit(other2, "_"))
       merchant2 = split2[1]; purchase2 = split2[2]
       cat(paste("merchant2 = ", merchant2, ", purchase2 = ", purchase2, "\n", sep = ""), file = stderr())
       if (merchant1 != merchant2)
       {
         str_key = paste(merchant1, "_", merchant2, sep = "")
         num_val = as.numeric(purchase1)*as.numeric(purchase2)
         cat(paste("str_key = ", str_key, ", num_val = ", num_val, "\n", sep = ""), file = stderr())
         return(keyval(str_key, num_val))
       }
     }
   }
}
system("rm -r /Users/blahiri/learning/rhadoop/merchant_pair_products")
mrjob1 = mapreduce(input = client_high_val_merchant, output = "/Users/blahiri/learning/rhadoop/merchant_pair_products/", output.format = "csv", map = map1, reduce = reduce1)
results <- from.dfs(input = "/Users/blahiri/learning/rhadoop/merchant_pair_products/")
