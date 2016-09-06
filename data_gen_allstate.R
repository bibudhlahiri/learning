library(data.table)
library(e1071)
library(rpart)
set.seed(1)

gen_driving_data <- function()
{
  N <- 100000
    
  n_high_risk <- 0.33*N
  high_risk <- data.table(time_of_day = sample(c("night", "day"), n_high_risk, replace = TRUE, prob = c(0.95, 0.05)))
  high_risk[, raining := sample(c("true", "false"), n_high_risk, replace = TRUE, prob = c(0.95, 0.05))]
  high_risk[, accident_prone_area := sample(c("true", "false"), n_high_risk, replace = TRUE, prob = c(0.7, 0.3))]
  high_risk[, g_force := c(runif(n_high_risk/2, 0.4, 5), runif(n_high_risk/2, -5, -0.4))]
  high_risk[, n_prev_instances := rbinom(n_high_risk, 4, 0.6)]
  high_risk[, risky := sample(c("high", "medium", "low"), n_high_risk, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  
  n_medium_risk <- 0.33*N
  medium_risk <- data.table(time_of_day = sample(c("night", "day"), n_medium_risk, replace = TRUE, prob = c(0.8, 0.2)))
  medium_risk[, raining := sample(c("true", "false"), n_medium_risk, replace = TRUE, prob = c(0.8, 0.2))]
  medium_risk[, accident_prone_area := sample(c("true", "false"), n_medium_risk, replace = TRUE, prob = c(0.6, 0.4))]
  medium_risk[, g_force := c(runif(n_medium_risk/2, 0.2, 0.4), runif(n_medium_risk/2, -0.4, -0.2))]
  medium_risk[, n_prev_instances := rbinom(n_medium_risk, 3, 0.6)]
  medium_risk[, risky := sample(c("high", "medium", "low"), n_medium_risk, replace = TRUE, prob = c(0.05, 0.9, 0.05))]
  
  n_low_risk <- N - n_high_risk - n_medium_risk 
  low_risk <- data.table(time_of_day = sample(c("night", "day"), n_low_risk, replace = TRUE, prob = c(0.5, 0.5)))
  low_risk[, raining := sample(c("true", "false"), n_low_risk, replace = TRUE, prob = c(0.2, 0.8))]
  low_risk[, accident_prone_area := sample(c("true", "false"), n_low_risk, replace = TRUE, prob = c(0.2, 0.8))]
  low_risk[, g_force := runif(n_low_risk, -0.2, 0.2)]
  low_risk[, n_prev_instances := rbinom(n_low_risk, 2, 0.6)]
  low_risk[, risky := sample(c("high", "medium", "low"), n_low_risk, replace = TRUE, prob = c(0.05, 0.05, 0.9))]
  
  driving_data <- rbindlist(list(high_risk, medium_risk, low_risk))
  id_start <- 217654
  driving_data[, id := sample(id_start:(id_start+N-1), replace = FALSE)]
  
  #filename <- "C:\\Users\\blahiri\\AllState\\driving_data.csv"
  #write.table(driving_data, filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
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
  
  #If there are more ID-type fields that are not used in modelling, they should be added here
  cols <- c("id")            
  
  bestmod <- rpart(risky ~ ., 
                   #data = training_data,
                   data = training_data[, .SD, .SDcols = -cols]
                   #control = rpart.control(maxdepth = 10)
                   )
  test_data[, predicted_risky := as.character(predict(bestmod, newdata = test_data, type = "class"))]
  
  prec_recall <- table(test_data[, risky], test_data[, predicted_risky], dnn = list('actual', 'predicted'))
  print(prec_recall)
  
  #Measure overall accuracy
  setkey(test_data, risky, predicted_risky)
  accuracy <- nrow(test_data[(risky == predicted_risky),])/nrow(test_data)
  cat(paste("Overall accuracy = ", accuracy, "\n\n", sep = "")) 
  
  print(bestmod)
  print(bestmod$variable.importance/sum(bestmod$variable.importance))
  bestmod
}

#gen_driving_data()
bestmod <- classify_driving_data_dtree()

