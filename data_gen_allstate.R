library(data.table)
library(e1071)
library(rpart)

gen_driving_data <- function()
{
  N <- 100000
  g_force <- rnorm(N, 0.08, 0.12)
  driving_data <- data.table(g_force = g_force)
  setkey(driving_data, g_force)
  
  risky <- driving_data[abs(g_force) >= 0.2]
  non_risky <- driving_data[abs(g_force) < 0.2]
  cat(paste("nrow(risky) = ", nrow(risky), ", nrow(non_risky) = ", nrow(non_risky),
            ", total = ", sum(nrow(risky) + nrow(non_risky)), "\n", sep = ""))
  
  risky[, time_of_day := sample(c("night", "day"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, raining := sample(c("true", "false"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, accident_prone_area := sample(c("true", "false"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, risky := "true"]
  
  non_risky[, time_of_day := sample(c("night", "day"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, raining := sample(c("true", "false"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, accident_prone_area := sample(c("true", "false"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, risky := "false"]
  
  driving_data <- rbindlist(list(risky, non_risky))
}

classify_driving_data_dtree <- function()
{
  driving_data <- gen_driving_data()
  cols <- c("risky", "accident_prone_area", "raining", "time_of_day")
  driving_data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]
  train = sample(1:nrow(driving_data), 0.7*nrow(driving_data))
  test = (-train)
  cat(paste("Size of training data = ", length(train), ", size of test data = ", (nrow(driving_data) - length(train)), "\n", sep = ""))
  
  training_data <- driving_data[train, ]
  #training_data <- create_bs_by_over_and_undersampling(training_data)
  test_data <- driving_data[test, ]
  
  print(table(training_data$risky)) 
  print(table(test_data$risky))
  
  #cols <- c("session_id", "flow_id")            
  
  tune.out <- tune.rpart(risky ~ ., 
                         data = training_data,
                         #data = training_data[, .SD, .SDcols = -cols], 
                         minsplit = c(5, 10, 15, 20), maxdepth = seq(5, 30, 5))
  print(summary(tune.out))
  
  bestmod <- tune.out$best.model
  test_data[, predicted_risky := as.character(predict(bestmod, newdata = test_data, type = "class"))]
  
  prec_recall <- table(test_data[, risky], test_data[, predicted_risky], dnn = list('actual', 'predicted'))
  print(prec_recall)
  
  #Measure overall accuracy
  setkey(test_data, risky, predicted_risky)
  accuracy <- nrow(test_data[(risky == predicted_risky),])/nrow(test_data)
  recall <- prec_recall[2,2]/sum(prec_recall[2,])
  precision <- prec_recall[2,2]/sum(prec_recall[,2])
  cat(paste("Overall accuracy = ", accuracy, ", recall = ", recall, ", precision = ", precision, "\n\n", sep = "")) #0.9273255
  
  print(bestmod)
  print(bestmod$variable.importance/sum(bestmod$variable.importance))
}

classify_driving_data_dtree()
