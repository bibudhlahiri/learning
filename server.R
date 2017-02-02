library(shiny)

rf_from_file <<- NA
file_path <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code"
load(paste(file_path, "\\rf.fitN.rda", sep = ""))
rf_from_file <<- rf.fitN

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
  -raw_prediction
}

optimize_ap_params <- function(input_ranges)
{
  library(GA)
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\rfImputed.csv"
  input_data <- read.csv(filename, header = T, stringsAsFactors = F)
  features <- colnames(input_data)
  features <- features[(features != "dpv")]
  
  #min_values <- as.numeric(sapply(features, function(feature) min(input_data[, feature])))
  #max_values <- as.numeric(sapply(features, function(feature) max(input_data[, feature])))
  min_values <- c(input_ranges[["BP2_1_Robot_AM_min"]], input_ranges[["AH1_min_hum"]], input_ranges[["AH2_min_hum"]], 
                  input_ranges[["AH3_min_hum"]], input_ranges[["AH5_min_hum"]], input_ranges[["AH2_min_temp"]],
				  input_ranges[["AH5_min_temp"]], input_ranges[["AH4_min_hum"]], input_ranges[["CP1_4_Robot_AM_min"]], 
				  input_ranges[["CP2_4_Robot_AM_min"]])
  max_values <- c(input_ranges[["BP2_1_Robot_AM_max"]], input_ranges[["AH1_max_hum"]], input_ranges[["AH2_max_hum"]], 
                  input_ranges[["AH3_max_hum"]], input_ranges[["AH5_max_hum"]], input_ranges[["AH2_max_temp"]],
				  input_ranges[["AH5_max_temp"]], input_ranges[["AH4_max_hum"]], input_ranges[["CP1_4_Robot_AM_max"]], 
				  input_ranges[["CP2_4_Robot_AM_max"]])
  print(min_values)
  print(max_values)
  
  classes <- sapply(features, function(feature) class(input_data[, feature]))
  print(classes)
  
  GA <- ga(type = "real-valued", 
           fitness = function(x) custom_fitness(x[1], x[2], x[3], x[4], 
		                                        x[5], x[6], x[7], x[8], 
												x[9], x[10]), 
           min = min_values, max = max_values, maxiter = 5,
           optim = TRUE)
  print(summary(GA))
  paste("Optimal solution is ", paste(paste(features, round(as.numeric(GA@solution), 3), sep = " = "), collapse = ", "), sep = "")
}

shinyServer(function(input, output) {

rangeInput <- reactive({

    AH1_min_hum <- input$AH1_min_hum
	AH1_max_hum <- input$AH1_max_hum
	BP2_1_Robot_AM_min <- input$BP2_1_Robot_AM_min
	BP2_1_Robot_AM_max <- input$BP2_1_Robot_AM_max
	AH2_min_hum <- input$AH2_min_hum
	AH2_max_hum <- input$AH2_max_hum
	AH2_min_temp <- input$AH2_min_temp
	AH2_max_temp <- input$AH2_max_temp
	CP1_4_Robot_AM_min <- input$CP1_4_Robot_AM_min
    CP1_4_Robot_AM_max <- input$CP1_4_Robot_AM_max
	AH3_min_hum <- input$AH3_min_hum
	AH3_max_hum <- input$AH3_max_hum
	CP2_4_Robot_AM_min <- input$CP2_4_Robot_AM_min
	CP2_4_Robot_AM_max <- input$CP2_4_Robot_AM_max
    AH4_min_hum <- input$AH4_min_hum
	AH4_max_hum <- input$AH4_max_hum
	AH5_min_hum <- input$AH5_min_hum
	AH5_max_hum <- input$AH5_max_hum
	AH5_min_temp <- input$AH5_min_temp
	AH5_max_temp <- input$AH5_max_temp
    
    list("AH1_min_hum" = AH1_min_hum, "AH1_max_hum" = AH1_max_hum, "BP2_1_Robot_AM_min" = BP2_1_Robot_AM_min,
	     "BP2_1_Robot_AM_max" = BP2_1_Robot_AM_max, "AH2_min_hum" = AH2_min_hum, "AH2_max_hum" = AH2_max_hum,
		 "AH2_min_temp" = AH2_min_temp, "AH2_max_temp" = AH2_max_temp, "CP1_4_Robot_AM_min" = CP1_4_Robot_AM_min,
		 "CP1_4_Robot_AM_max" = CP1_4_Robot_AM_max, "AH3_min_hum" = AH3_min_hum, "AH3_max_hum" = AH3_max_hum,
		 "CP2_4_Robot_AM_min" = CP2_4_Robot_AM_min, "CP2_4_Robot_AM_max" = CP2_4_Robot_AM_max, 
		 "AH4_min_hum" = AH4_min_hum, "AH4_max_hum" = AH4_max_hum, "AH5_min_hum" = AH5_min_hum,
		 "AH5_max_hum" = AH5_max_hum, "AH5_min_temp" = AH5_min_temp, "AH5_max_temp" = AH5_max_temp) 
  })
  
  output$optimal_soln <- renderText({ 
      input_ranges <- rangeInput()
      optimize_ap_params(input_ranges)          
     })
 }
)