library(data.table)
library(e1071)
library(rpart)
set.seed(1)

add_junk_fields <- function(driving_data, N)
{
  #Add fields that are not needed for modelling
  event_id_start <- 217654
  driving_data[, event_id := sample(event_id_start:(event_id_start+N-1), N, replace = FALSE)]
  driver_id_start <- 182542
  driving_data[, driver_id := sample(driver_id_start:(driver_id_start+N-1), N, replace = FALSE)]
  driving_data[, start_latitude := runif(N, 33.95235, 34.08308)]
  driving_data[, start_longitude := runif(N, -118.38511, -118.3659)]
  driving_data[, end_latitude := runif(N, 34.08308, 34.2134)]
  driving_data[, end_longitude := runif(N, -118.3659, -118.3472)]
  
  beginning_of_time <- as.POSIXlt("2015-01-01 00:00:00")
  driving_data[, started_at := beginning_of_time + runif(N, 0, 600*24*3600)]
  driving_data[, ended_at := started_at + runif(N, 0, 2*3600)]
  
  
  driving_data[, start_address_city := "Los Angeles"]
  driving_data[, end_address_city := "Los Angeles"]
  driving_data[, start_address_state := "CA"]
  driving_data[, end_address_state := "CA"]
  driving_data[, start_address_zip := sample(c(90001:93591), N, replace = TRUE)]
  driving_data[, end_address_zip := start_address_zip + round(runif(N, 0, 3))]

  filename <- "C:\\Users\\blahiri\\healthcare\\data\\cloudera_challenge\\Medicare_Provider_Charge_Inpatient_DRG100_FY2011.csv"
  address_data <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                      colClasses = c("character", "numeric", "character", "character", "character",
                                     "character", "numeric", "character", "numeric", "numeric", "numeric", "numeric"),
                      data.table = TRUE)
  sampled_addresses <- sample(1:nrow(address_data), N, replace = FALSE)
  driving_data[, start_address_street := address_data[['Provider Street Address']][sampled_addresses]]
  sampled_addresses <- sample(1:nrow(address_data), N, replace = FALSE)
  driving_data[, end_address_street := address_data[['Provider Street Address']][sampled_addresses]]
  
  driving_data[, fuel_cost_usd := runif(N, 1.0, 5.0)]
  driving_data[, fuel_volume_l := runif(N, 1.0, 5.0)]
  driving_data[, average_kmpl := runif(N, 6, 14)]
  driving_data[, average_from_epa_kmpl := runif(N, 7.7, 15.7)]
  driving_data[, score_events := runif(N, 30, 70)]
  driving_data[, score_speeding := round(runif(N, 30, 70))]
  driving_data[, hard_brakes := sample(c(0, 1, 2), N, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  driving_data[, hard_accels := sample(c(0, 1, 2), N, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  driving_data[, duration_over_70_s := sample(c(0, 1, 2), N, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  driving_data[, duration_over_75_s := sample(c(0, 1, 2), N, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  driving_data[, duration_over_80_s := sample(c(0, 1, 2), N, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  driving_data[, start_timezone := "America/Los Angeles"]
  driving_data[, end_timezone := "America/Los Angeles"]
  driving_data[, city_fraction := 1]
  driving_data[, highway_fraction := 0]
  driving_data[, idling_time_s := round(runif(N, 10, 70))]
  driving_data[, tags := sample(c("business", "personal"), N, replace = TRUE, prob = c(0.5, 0.5))]
  driving_data
}

gen_driving_data <- function()
{
  N <- 100000
    
  n_high_risk <- 0.33*N
  high_risk <- data.table(time_of_day = sample(c("night", "day"), n_high_risk, replace = TRUE, prob = c(0.95, 0.05)))
  high_risk[, raining := sample(c("true", "false"), n_high_risk, replace = TRUE, prob = c(0.95, 0.05))]
  high_risk[, accident_prone_area := sample(c("true", "false"), n_high_risk, replace = TRUE, prob = c(0.95, 0.05))]
  high_risk[, g_force := c(runif(n_high_risk/2, 0.4, 5), runif(n_high_risk/2, -5, -0.4))]
  high_risk[, n_prev_instances := rbinom(n_high_risk, 4, 0.6)]
  high_risk[, risky := sample(c("high", "medium", "low"), n_high_risk, replace = TRUE, prob = c(0.9, 0.05, 0.05))]
  
  n_medium_risk <- 0.33*N
  medium_risk <- data.table(time_of_day = sample(c("night", "day"), n_medium_risk, replace = TRUE, prob = c(0.8, 0.2)))
  medium_risk[, raining := sample(c("true", "false"), n_medium_risk, replace = TRUE, prob = c(0.8, 0.2))]
  medium_risk[, accident_prone_area := sample(c("true", "false"), n_medium_risk, replace = TRUE, prob = c(0.8, 0.2))]
  medium_risk[, g_force := c(runif(n_medium_risk/2, 0.2, 0.4), runif(n_medium_risk/2, -0.4, -0.2))]
  medium_risk[, n_prev_instances := rbinom(n_medium_risk, 3, 0.6)]
  medium_risk[, risky := sample(c("high", "medium", "low"), n_medium_risk, replace = TRUE, prob = c(0.05, 0.9, 0.05))]
  
  n_low_risk <- N - n_high_risk - n_medium_risk 
  low_risk <- data.table(time_of_day = sample(c("night", "day"), n_low_risk, replace = TRUE, prob = c(0.2, 0.8)))
  low_risk[, raining := sample(c("true", "false"), n_low_risk, replace = TRUE, prob = c(0.2, 0.8))]
  low_risk[, accident_prone_area := sample(c("true", "false"), n_low_risk, replace = TRUE, prob = c(0.2, 0.8))]
  low_risk[, g_force := runif(n_low_risk, -0.2, 0.2)]
  low_risk[, n_prev_instances := rbinom(n_low_risk, 2, 0.6)]
  low_risk[, risky := sample(c("high", "medium", "low"), n_low_risk, replace = TRUE, prob = c(0.05, 0.05, 0.9))]
  
  driving_data <- rbindlist(list(high_risk, medium_risk, low_risk))
  driving_data <- add_junk_fields(driving_data, N)
  filename <- "C:\\Users\\blahiri\\AllState\\driving_data.csv"
  write.table(driving_data, filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
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
  cols <- c("event_id", "driver_id", "start_latitude", "start_longitude", "end_latitude", "end_longitude", 
            "started_at", "ended_at", "start_address_street", "end_address_street", 
            "start_address_city", "end_address_city",
            "start_address_state", "end_address_state",
            "start_address_zip", "end_address_zip", "fuel_cost_usd", "fuel_volume_l", "average_kmpl", 
            "average_from_epa_kmpl", "score_events", "score_speeding", "hard_brakes", "hard_accels",
            "duration_over_70_s", "duration_over_75_s", "duration_over_80_s", "start_timezone",
            "end_timezone", "city_fraction", "highway_fraction", "idling_time_s", "tags")
  
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

