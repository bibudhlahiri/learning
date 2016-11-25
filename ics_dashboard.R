#This analysis is for the visuals selected for the dashboard by Walt. The focus will be on fatal Lexus Paint Finish defects.
library(data.table)
library(ggplot2)

load_ics_fatal_defects_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\NOV_23\\ICS_DEFECT_HISTORY_VIEW\\ICS_DEFECT_HISTORY_VIEW.csv"
  ics_history <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "numeric", "Date", "Date", "character", #A-E
					               "numeric", "character", "character", "character", "character", #F-J
								   "numeric", "character", "character", "character", "character", #K-O
								   "character", "character", "character", "character", "character", #P-T
								   "character", "character", "character", "character", "character", #U-Y
								   "Date", "character", "character", "character", "Date", #Z-AD
								   "character", "numeric", "character", "character", "character", #AE-AI
								   "character", "character", "numeric", "numeric", "numeric", #AJ-AN
								   "character", "character", "character", "character", "character", #AO-AS
								   "numeric", "character", "character", "character", "character", #AT-AX
								   "character", "character", "character", "character", "character", #AY-BC
								   "character", "character", "character", "character", "numeric", #BD-BH
								   "numeric", "character", "character", "character", "numeric", #BI-BM
								   "numeric", "numeric", "numeric", "numeric", "numeric", #BN-BR
								   "numeric", "numeric", "numeric", "numeric", "character", #BS-BW
								   "character", "character", "character", "character", "character", #BX-CB
								   "character", "character", "character", "Date", "numeric", #CC-CG
								   "character", "character", "character", "character", "character", #CH-CL
								   "character", "character", "Date", "character", "character", #CM-CQ
								   "character", "character", "character", "character", "character", #CR-CV
								   "numeric", "character", "numeric", "character", "numeric" #CW-DA
								   ),  
                    data.table = TRUE)
  setkey(ics_history, RESPONSIBLE_SHOP, MODEL_NUM)  
  ics_history <- ics_history[((RESPONSIBLE_SHOP == "PAINT L3") & (MODEL_NUM == "643W")),] #256,958 rows
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_arc_vehicle_info table. ICS+ history has only MODEL_NUM and not Katashiki.
  
  #Drop the defect data corresponding to weekends as they are causing too many short-term spikes
  ics_history[, day_of_week := weekdays(as.Date(ics_history$CREATION_TIME, "%d-%b-%y"))]
  setkey(ics_history, day_of_week)
  ics_history <- ics_history[(!(day_of_week %in% c("Saturday", "Sunday"))),]
  ics_history$day_of_week <- factor(ics_history$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  #Start from 1 Oct 2015 as that was the time when ICS+ became operational/stable
  ics_history <- ics_history[(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01")),]
  
  #Drop data points with REPAIR_TERMINAL == RH_TL_INPUT
  setkey(ics_history, REPAIR_TERMINAL)
  ics_history <- ics_history[(REPAIR_TERMINAL != 'RH_TL_INPUT'),]
  
  #Keeping manuf_date for joining with ICS vehicle info table. 
  ics_history[, manuf_date := strftime(strptime(ics_history$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]
  ics_history
}

load_ics_fatal_paint_finish_data <- function(fatal_defects)
{
  setkey(fatal_defects, SECTION_NUM)
  pf_defects <- fatal_defects[(SECTION_NUM == "PF"),]
}

load_arc_vehicle_info <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\NOV_23\\ICS_DEFECT_HISTORY_VIEW\\ICS_ARC_VEHICLE_INFO.CSV"
  #Each VEHICLE_ID appears only once in ICS_arc_vehicle_info. Same applies for each VIN_NO.
  arc_vehicle_info <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "character", "numeric", "character", "character", #A-E
					               "character", "character", "character", "character", "Date", #F-J
								   "Date", "character", "numeric", "character", "character", #K-O
								   "numeric", "character", "numeric", "character", "character", #P-T
								   "character", "character", "character" #U-W
								   ), data.table = TRUE) #38,545 rows
								   
  #Keeping manuf_date for joining with ICS history
  arc_vehicle_info[, manuf_date := strftime(strptime(arc_vehicle_info$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_arc_vehicle_info table. ICS+ history has only MODEL_NUM and not Katashiki.
  setkey(arc_vehicle_info, MODEL_NUM)  
  arc_vehicle_info <- arc_vehicle_info[(MODEL_NUM == "643W"),]
  
  arc_vehicle_info[, day_of_week := weekdays(as.Date(arc_vehicle_info$CREATION_TIME, "%d-%b-%y"))]
  setkey(arc_vehicle_info, day_of_week)
  arc_vehicle_info <- arc_vehicle_info[(!(day_of_week %in% c("Saturday", "Sunday"))),]
  arc_vehicle_info$day_of_week <- factor(arc_vehicle_info$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  #Start from 1 Oct 2015 as that was the time when ICS+ became operational/stable
  arc_vehicle_info <- arc_vehicle_info[(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01")),]
}

#Generate the DPV plot for the given time-window: last week, last month or last year
pf_dpv_for_fatal_defects <- function(time_window = "year", pf_defects, arc_vehicle_info)
{
  end_date <- max(pf_defects$manuf_date)
  if (time_window == "week")
  {
    start_date <- as.character(as.Date(end_date) - 6)
	date_break_freq <- "1 day"
  }
  else if (time_window == "month")
  {
    start_date <- as.character(as.Date(end_date) - 29)
	date_break_freq <- "1 week"
  }
  else #if (time_window == "year")
  {
    start_date <- as.character(as.Date(end_date) - 364)
	date_break_freq <- "1 month"
  }
  setkey(pf_defects, manuf_date)
  setkey(arc_vehicle_info, manuf_date)
  pf_defects <- pf_defects[(manuf_date >= start_date),]
  arc_vehicle_info <- arc_vehicle_info[(manuf_date >= start_date),]
  
  #Same DEFECT_NUM can occur for multiple VEHICLE_IDs. Same VEHICLE_ID can have multiple DEFECT_NUMs.
  pf_defects_by_manuf_date <- pf_defects[, list(n_pf_defects = length(DEFECT_NUM)), by = manuf_date]  
  
  #In arc_vehicle_info, each VEHICLE_ID or VIN occurs exactly once
  vehicles_by_manuf_date <- arc_vehicle_info[, list(n_vehicles = length(VEHICLE_ID)), by = manuf_date]
  
  #Combine ICS history and vehicle info data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(pf_defects_by_manuf_date, manuf_date)
  pf_defects_by_manuf_date <- pf_defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  pf_defects_by_manuf_date[, pf_dpv := n_pf_defects/n_vehicles] 
    
  print(fivenum(pf_defects_by_manuf_date$pf_dpv)) #For time_window = "year", 0.5615764 1.1104651 1.4454976 1.9693252 7.5714286: variance is highest
  #For time_window = "month", 0.7488372 1.0939227 1.2135266 1.3181818 1.6865672
  #For time_window = "week", 1.227053 1.236967 1.288136 1.318182 1.349515: variance is lowest
    
  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\PF_DPV_", 
                      time_window, ".png", sep = "")
  png(image_file)
  pf_defects_by_manuf_date[, manuf_date := as.Date(pf_defects_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(pf_defects_by_manuf_date, aes(x = manuf_date, y = pf_dpv)) + geom_line(size = 0.5) + 
       scale_x_date(date_breaks = date_break_freq, date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs for fatal defects") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + 
		 theme(axis.text.x = element_text(angle = 90))
  if (time_window == "week")
  {
    p <- p + geom_point()
  }
  print(p)
  aux <- dev.off()

  pf_defects_by_manuf_date
}

#2D and 3D heatmaps/histograms of number of TBP Step 2 defects based on their X and Y coordinates.
#Observation: with yearly data, 2209 defects (3.31% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
#With monthly data, 221 defects (4.23% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
#With weekly data, 49 defects (4.1% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
#Conclusion: The fatal paint finish defects are consistently concentrated in this particular region.
plot_pf_defects_x_y <- function(input_data, time_window = "year", n_cells_x = 15, n_cells_y = 15)
{
  library(plot3D)
  end_date <- max(input_data$manuf_date)
  if (time_window == "week")
  {
    start_date <- as.character(as.Date(end_date) - 6)
  }
  else if (time_window == "month")
  {
    start_date <- as.character(as.Date(end_date) - 29)
  }
  else #if (time_window == "year")
  {
    start_date <- as.character(as.Date(end_date) - 364)
  }
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  #cut divides the range of x into intervals and codes the values in x according to which interval they fall.
  #Range is hard-coded to make sure all subsets of data have the same grid boundaries for a given grid size.
  #Otherwise, it becomes data-dependent and the boundaries for different time-slices do not match exactly.
  x_c <- cut(input_data$X_COOR, breaks = seq(0, 12000, length.out = n_cells_x + 1)) #The output of cut() is a factor
  y_c <- cut(input_data$Y_COOR, breaks = seq(0, 12000, length.out = n_cells_y + 1))

  #Calculate joint counts at cut levels:
  z <- table(x_c, y_c) #The arguments to table can be factors

  #Plot as a 3D histogram:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\", 
	        "\\pf_defects_3D_",
	        time_window, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  hist3D(z = z, border = "black")
  dev.off()

  #Plot as a 2D heatmap:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\",
	       "\\pf_defects_2D_",
	       time_window, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  image2D(z = z, border = "black")
  dev.off()
  
  index_of_max <- as.numeric(which(z == max(z), arr.ind = TRUE))
  x_boundaries_of_max <- rownames(z)[index_of_max[1]]
  y_boundaries_of_max <- colnames(z)[index_of_max[2]]
  x_min <- as.numeric(gsub("\\(", "", strsplit(x_boundaries_of_max, ",")[[1]][1]))
  x_max <- as.numeric(gsub("\\]", "", strsplit(x_boundaries_of_max, ",")[[1]][2]))
  y_min <- as.numeric(gsub("\\(", "", strsplit(y_boundaries_of_max, ",")[[1]][1]))
  y_max <- as.numeric(gsub("\\]", "", strsplit(y_boundaries_of_max, ",")[[1]][2]))
  cat(paste("Max: ", max(z), " defects (", round(100*max(z)/nrow(input_data), 2), 
            "% of all fatal PF defects) occurred for x in [", x_min, ", ", x_max, "] and y in [", y_min, ", ", y_max, "]\n", sep = ""))
  
  z
}

#As yearly, monthly and weekly data all point to the same region as the highest-defect region, we are analyzing that 
#region for last one year.
analyze_high_defect_region_by_time <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  setkey(input_data, X_COOR, Y_COOR)
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data[(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max),]
  print(nrow(input_data))
  input_data_by_manuf_date <- input_data[, list(n_pf_defects = length(DEFECT_NUM)), by = manuf_date]

  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\high_defect_region_by_time.png"
  png(image_file, width = 1200, height = 400)
  input_data_by_manuf_date[, manuf_date := as.Date(input_data_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(input_data_by_manuf_date, aes(x = manuf_date, y = n_pf_defects)) + geom_line(size = 0.5) + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs for fatal defects") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + 
		 theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()  
}

analyze_high_defect_region_by_shift <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  setkey(input_data, X_COOR, Y_COOR)
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data[(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max),]
  #In the high-defect region, total 1228 defects from shift A (1.25 times shift B), 981 defects from shift B. 56% from shift A, 44% from shift B.
  #This is more even that the distribution in overall PF fatal defects data, where 65% come from A, 35% from B.
  
  print(table(input_data$RESPONSIBLE_SHIFT))
  setkey(input_data, CREATION_TIME, RESPONSIBLE_SHIFT)
  by_creation_time <- input_data[, list(n_defects = length(DEFECT_ID)), by = list(CREATION_TIME, RESPONSIBLE_SHIFT)] 
  by_creation_time[, manuf_date := as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")]
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\high_defect_region_by_shift.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line(aes(colour = RESPONSIBLE_SHIFT), size = 0.5) + scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("No. of paint defects") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
       theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
}

#Website to order data is: https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd
#Data file is available at: http://www.ncdc.noaa.gov/orders/isd/5620597190494dat.txt
#Order details are at: https://www.ncdc.noaa.gov/orders/isd/CDO5620597190494.html
load_external_weather_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\weather\\cincinnati_weather.txt"
  weather <- fread(filename, header = TRUE, sep = " ", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "numeric", "character", "character", "character",
					               "character", "character", "character", "character", "character",
								   "character", "character", "character", "character", "character",
								   "character", "character", "character", "character", "character",
								   "character", "character", "character", "character", "character",
								   "character", "character", "character", "character", "character",
								   "character", "character", "character"
								   ), data.table = TRUE)
  cols_to_retain <- c("YR--MODAHRMN", "TEMP", "DEWP")
  weather <- weather[, .SD, .SDcols = cols_to_retain]
  weather[, date_captured := substr(weather[['YR--MODAHRMN']], 1, 8)]
  weather <- weather[((TEMP != "****") & (DEWP != "****")),]
  weather$TEMP <- as.numeric(weather$TEMP)
  weather$DEWP <- as.numeric(weather$DEWP)
  weather[, humidity := (DEWP - TEMP)]
  weather[, date_captured := paste(substr(date_captured, 1, 4), "-", substr(date_captured, 5, 6), "-", 
                                           substr(date_captured, 7, 8), sep = "")]
  setkey(weather, date_captured)
  weather_by_date <- weather[, list(avg_temp = mean(TEMP),
                                    avg_humidity = mean(humidity)), by = date_captured]
}

analyze_high_defect_region_by_temperature <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  setkey(input_data, X_COOR, Y_COOR)
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data[(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max),]
  
  setkey(input_data, manuf_date)
  defects_by_date <- input_data[, list(n_defects = length(DEFECT_NUM)), by = manuf_date]
  
  weather_by_date <- load_external_weather_data()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_temp, defects_by_date$n_defects)) #-0.1644567 when we focus on the high-defect region.
  #However, overall fatal Paint Finish defects has a correlation coeff of -0.4033189. That means defects in this region
  #are concentrated for some other reason as well. 
  #lm(n_defects ~ avg_temp, data = defects_by_date) Intercept 11.74982, slope -0.04605
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\high_defect_region_by_temperature.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(defects_by_date, aes(avg_temp, n_defects)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Total fatal paint finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  defects_by_date
}

#Total number of defects vs temperature does not show a very strong trend, so try DPV instead.
analyze_dpv_for_high_defect_region_by_temperature <- function(input_data, arc_vehicle_info, x_min = 9600, x_max = 10400, 
                                                              y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  setkey(input_data, X_COOR, Y_COOR)
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data[(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max),]
  
  setkey(input_data, manuf_date)
  defects_by_date <- input_data[, list(n_defects = length(DEFECT_NUM)), by = manuf_date]
  
  setkey(arc_vehicle_info, manuf_date)
  arc_vehicle_info <- arc_vehicle_info[(manuf_date >= start_date),] 
  
  #In arc_vehicle_info, each VEHICLE_ID or VIN occurs exactly once
  vehicles_by_manuf_date <- arc_vehicle_info[, list(n_vehicles = length(VEHICLE_ID)), by = manuf_date]
  
  #Combine PF fatal defects and vehicle info data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(defects_by_date, manuf_date)
  defects_by_date <- defects_by_date[vehicles_by_manuf_date, nomatch = 0]
  defects_by_date[, pf_dpv := n_defects/n_vehicles] 
  
  weather_by_date <- load_external_weather_data()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_temp, defects_by_date$pf_dpv)) #-0.2268994 when we focus on the high-defect region.
  #However, overall fatal Paint Finish DPV has a correlation coeff of -0.3847753. That means defects in this region
  #are concentrated for some other reason as well. 
  #lm(pf_dpv ~ avg_temp, data = defects_by_date) Intercept 0.0875062, slope -0.0005156
  
  image_file <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\dpv_for_high_defect_region_by_temperature.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(defects_by_date, aes(avg_temp, pf_dpv)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("DPV for fatal paint finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  defects_by_date
}

analyze_high_defect_region_by_humidity <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  setkey(input_data, manuf_date)
  input_data <- input_data[(manuf_date >= start_date),]
  
  setkey(input_data, X_COOR, Y_COOR)
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data[(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max),]
  
  setkey(input_data, manuf_date)
  defects_by_date <- input_data[, list(n_defects = length(DEFECT_NUM)), by = manuf_date]
  
  weather_by_date <- load_external_weather_data()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_humidity, defects_by_date$n_defects)) #-0.04532517 when we focus on the high-defect region.
  #However, overall fatal Paint Finish defects has a correlation coeff of 0.07895822 (note the change of sign). Both correlation 
  #coefficients are negligible.
  print(lm(n_defects ~ avg_humidity, data = defects_by_date)) #Intercept 8.67052, slope -0.04159
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\dashboard\\high_defect_region_by_humidity.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(defects_by_date, aes(avg_temp, n_defects)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily humidity") + ylab("Total fatal paint finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  defects_by_date
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_dashboard.R")
fatal_defects <- load_ics_fatal_defects_data() #227,242 rows. Date range is 2015-10-01 to 2016-11-23.
pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #79,032 rows
arc_vehicle_info <- load_arc_vehicle_info() #43,693 rows
#z <- plot_pf_defects_x_y(pf_defects, time_window = "week", n_cells_x = 15, n_cells_y = 15)
#analyze_high_defect_region_by_time(pf_defects)
#analyze_high_defect_region_by_shift(pf_defects)
#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(time_window = "year", pf_defects, arc_vehicle_info)
#defects_by_date <- analyze_high_defect_region_by_temperature(pf_defects)
#defects_by_date <- analyze_dpv_for_high_defect_region_by_temperature(pf_defects, arc_vehicle_info)
defects_by_date <- analyze_high_defect_region_by_humidity(pf_defects)
