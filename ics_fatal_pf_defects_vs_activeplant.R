#This analysis is the one that started after we got the feedback from Tara on 29/11/2016 (about REPAIR_TERMINAL , 2-level grid etc).
library(plyr)
library(dplyr)
library(ggplot2)
library(rpart)
library(party)

dtree <- NA

load_ics_activeplant_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\ICSDataReplacedNA.csv"
  ics_activeplant <- read.csv(filename, header = T, stringsAsFactors = F)
}

load_activeplant_hourly_avg_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\FinalAP_rf.csv"
  GIAPJ_AllM_New <- read.csv(filename, header = T, stringsAsFactors = F)
}

compute_dpvs <- function(ics_activeplant)
{
  ics_activeplant$record_hour <- substr(ics_activeplant$RecordTime, 1, 13)
  dpv_by_hour <- ics_activeplant %>% group_by(record_hour) %>% summarise(dpv = sum(tdefect)/length(RecordTime),
                                                                         mean_BP1_1_Robot_Air_Motor = mean(BP1_1_Robot_Air_Motor),
                                                                         mean_BP2_1_Robot_Air_Motor = mean(BP2_1_Robot_Air_Motor),
																		 mean_CP1_1_Robot_Air_Motor = mean(CP1_1_Robot_Air_Motor),
																		 mean_CP1_3_Robot_HV = mean(CP1_3_Robot_HV),
																		 mean_CP2_1_Robot_Air_Motor = mean(CP2_1_Robot_Air_Motor),
																		 mean_CBTH_Air_House_1_Humidity = mean(CBTH_Air_House_1_Humidity),
																		 mean_CBTH_Air_House_2_Humidity = mean(CBTH_Air_House_2_Humidity),
																		 mean_CBTH_Air_House_3_Humidity = mean(CBTH_Air_House_3_Humidity),
																		 mean_CBTH_Air_House_5_Humidity = mean(CBTH_Air_House_5_Humidity),
																		 mean_CBTH_Air_House_2_Temp = mean(CBTH_Air_House_2_Temp),
																		 mean_CBTH_Air_House_5_Temp = mean(CBTH_Air_House_5_Temp)
                                                                         )
}
  
build_dtree <- function(dpv_by_hour)
{
  features <- colnames(dpv_by_hour)
  features <- features[!(features %in% c("record_hour", "dpv"))]
  formula_str <- paste("dpv ~ ", paste(features, collapse = " + "), sep = "")
  #rpart(as.formula(formula_str), data = dpv_by_hour)
  ctree(as.formula(formula_str), data = dpv_by_hour)
}

build_rf <- function(GIAPJ_AllM_New)
{
  library(randomForest)
  #To handle Missing Value in data, using imputation from "X", "RecordTime", "tdefect", "tveh",
  rfImputed <- rfImpute(dpv~. , GIAPJ_AllM_New[,-c(1:4)])
  rf.fitN <- randomForest(dpv~. , rfImputed)
  file_path <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code"
  save(rf.fitN, file = paste(file_path, "\\rf.fitN.rda", sep = ""))
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\rfImputed.csv"
  write.table(rfImputed, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  rfImputed
}

custom_fitness <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
{
  newdata <- data.frame(BP2_1_Robot_Air_Motor = x1, CBTH_Air_House_1_Humidity = x2,
                        CBTH_Air_House_2_Humidity = x3, CBTH_Air_House_3_Humidity = x4,
						CBTH_Air_House_5_Humidity = x5, CBTH_Air_House_2_Temp = x6,
						CBTH_Air_House_5_Temp = x7, CBTH_Air_House_4_Humidity = x8,
						CP1_4_Robot_Air_Motor = x9, CP2_4_Robot_Air_Motor = x10)
  #Note: ga() maximizes the fitness function but we want to minimize DPV, hence return negative
  library(randomForest)
  raw_prediction <- predict(rf_from_file, newdata)
  #cat(paste("x1 = ", x1, ", x2 = ", x2, ", raw_prediction = ", raw_prediction, "\n", sep = ""))
  -raw_prediction
}

optimize_ap_params <- function()
{
  library(GA)
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\rfImputed.csv"
  input_data <- read.csv(filename, header = T, stringsAsFactors = F)
  features <- colnames(input_data)
  features <- features[(features != "dpv")]

  min_values <- as.numeric(sapply(features, function(feature) min(input_data[, feature])))
  max_values <- as.numeric(sapply(features, function(feature) max(input_data[, feature])))
  print(min_values)
  print(max_values)
  
  GA <- ga(type = "real-valued", 
           fitness = function(x) custom_fitness(x[1], x[2], x[3], x[4], 
		                                        x[5], x[6], x[7], x[8], 
												x[9], x[10]), 
           min = min_values, max = max_values, maxiter = 5,
           optim = TRUE, keepBest = TRUE)
  print(summary(GA))
  cat(paste("Optimal solution is ", paste(paste(features, as.numeric(GA@solution), sep = " = "), collapse = ", "), sep = ""))
  GA
}

#Obs: temp and humidity variables have reasonably low CVs.
check_hourly_CVs <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Coefficient\\Coefficient.csv"
  CVs <- read.csv(filename, header = T, stringsAsFactors = F)
  columns <- colnames(CVs)
  columns <- columns[!(columns %in% c("X", "recordtime"))]
  for (column in columns)
  {
    
	vec <- CVs[, column]
	vec <- vec[!is.na(vec)]
	med_vc <- median(vec)
	if (med_vc <= 0.3)
	{
	 cat(paste("column = ", column, ", med_vc = ", med_vc, "\n", sep = ""))
	 print(fivenum(vec))
	}
  }
}

jump_of_air_motor_within_an_hour <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\BP1RobotAM1.csv"
  BP1_1_Robot_AM <- read.csv(filename, header = T, stringsAsFactors = F)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\jump_within_an_hour.png"
  png(image_file, width = 800, height = 400)
  BP1_1_Robot_AM$timestamp <- strptime(BP1_1_Robot_AM$X_c0, "%Y-%m-%d %H:%M:%S")
  p <- ggplot(BP1_1_Robot_AM, aes(x = timestamp, y = value)) + geom_line() + scale_x_datetime(date_labels = "%H:%M:%S") + xlab("Time") + 
       ylab("BP1 1 Robot Air Motor Speed") + theme(axis.text.x = element_text(size=12,color='black',face='bold'),
	                                               axis.text.y = element_text(size=12,color='black',face='bold'))
  print(p)
  aux <- dev.off()
  BP1_1_Robot_AM
}

#Analyze the variance per variables per process per car and take means, SD and CV.
#Do not analyze by hour. 
variable_analysis_per_car_process_variable <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\avg_date_galc_join_activePlant_SelectedVins\\avg_date_galc_join_activePlant_SelectedVins.csv"
  selectedVINs <- read.csv(filename, header = T, stringsAsFactors = F)
  selectedVINs <- selectedVINs %>% filter(substr(variablename, nchar(variablename) - 4, nchar(variablename)) != "Color")
  variables_by_vehicles <- selectedVINs %>% select(vin, eventname, variablename, value) %>% group_by(vin, eventname, variablename) %>% summarise(average = mean(value), std_dev = sd(value))
  variables_by_vehicles$coeff_of_var <- ifelse((variables_by_vehicles$average == 0), 0, 
                                               variables_by_vehicles$std_dev/variables_by_vehicles$average)
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\avg_date_galc_join_activePlant_SelectedVins\\variables_by_vehicles.csv"
  write.table(variables_by_vehicles, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  
  #Take the median CVs per process and variable now
  median_CV_by_var <- variables_by_vehicles %>% ungroup() %>% select(eventname, variablename, coeff_of_var) %>% group_by(eventname, variablename) %>% summarise(median_CV = median(coeff_of_var))
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\avg_date_galc_join_activePlant_SelectedVins\\median_CV_by_var.csv"
  write.table(median_CV_by_var, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_vs_activeplant.R")
#ics_activeplant <- load_ics_activeplant_data()
#dpv_by_hour <- compute_dpvs(ics_activeplant)
#dtree <<- build_dtree(dpv_by_hour)
#GA <- optimize_ap_params(dpv_by_hour)
#check_hourly_CVs()
#BP1_1_Robot_AM <- jump_of_air_motor_within_an_hour()
#GIAPJ_AllM_New <- load_activeplant_hourly_avg_data()
#rfImputed <- build_rf(GIAPJ_AllM_New)
#rf_from_file <<- NA
#file_path <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code"
#load(paste(file_path, "\\rf.fitN.rda", sep = ""))
#rf_from_file <<- rf.fitN
#GA <- optimize_ap_params()
median_CV_by_var <- variable_analysis_per_car_process_variable() 

