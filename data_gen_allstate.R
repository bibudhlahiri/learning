library(data.table)
library(e1071)
library(rpart)
set.seed(1)

gen_driving_data <- function()
{
  N <- 100000
    
  n_risky <- 0.5*N
  risky <- data.table(time_of_day = sample(c("night", "day"), n_risky, replace = TRUE, prob = c(0.95, 0.05)))
  risky[, raining := sample(c("true", "false"), n_risky, replace = TRUE, prob = c(0.95, 0.05))]
  risky[, accident_prone_area := sample(c("true", "false"), n_risky, replace = TRUE, prob = c(0.7, 0.3))]
  risky[, g_force := c(rnorm(n_risky/2, 0.185, 0.1), rnorm(n_risky/2, -0.185, 0.1))]
  risky[, n_prev_instances := rbinom(n_risky, 4, 0.6)]
  risky[, risky := sample(c("true", "false"), n_risky, replace = TRUE, prob = c(0.9, 0.1))]
  
  n_non_risky <- N - n_risky
  non_risky <- data.table(time_of_day = sample(c("night", "day"), n_non_risky, replace = TRUE, prob = c(0.5, 0.5)))
  non_risky[, raining := sample(c("true", "false"), n_non_risky, replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, accident_prone_area := sample(c("true", "false"), n_non_risky, replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, g_force := c(rnorm(n_non_risky/2, 0.08, 0.04), rnorm(n_non_risky/2, -0.08, 0.04))]
  non_risky[, n_prev_instances := rbinom(n_non_risky, 2, 0.6)]
  non_risky[, risky := sample(c("true", "false"), n_non_risky, replace = TRUE, prob = c(0.1, 0.9))]
  
  driving_data <- rbindlist(list(risky, non_risky))
  print(head(driving_data))
  driving_data
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
  test_data <- driving_data[test, ]
  
  print(table(training_data$risky)) 
  print(table(test_data$risky))
  
  #cols <- c("session_id", "flow_id")            
  
  bestmod <- rpart(risky ~ ., 
                   data = training_data,
                   #data = training_data[, .SD, .SDcols = -cols]
                   #control = rpart.control(maxdepth = 10)
                   )
  test_data[, predicted_risky := as.character(predict(bestmod, newdata = test_data, type = "class"))]
  
  prec_recall <- table(test_data[, risky], test_data[, predicted_risky], dnn = list('actual', 'predicted'))
  print(prec_recall)
  
  #Measure overall accuracy
  setkey(test_data, risky, predicted_risky)
  accuracy <- nrow(test_data[(risky == predicted_risky),])/nrow(test_data)
  recall <- prec_recall[2,2]/sum(prec_recall[2,])
  precision <- prec_recall[2,2]/sum(prec_recall[,2])
  cat(paste("Overall accuracy = ", accuracy, ", recall = ", recall, ", precision = ", precision, "\n\n", sep = "")) 
  
  print(bestmod)
  print(bestmod$variable.importance/sum(bestmod$variable.importance))
}

classify_driving_data_dtree()
