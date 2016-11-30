#This analysis is the one that started after we got the feedback from Tara on 29/11/2016 (about REPAIR_TERMINAL , 2-level grid etc).
library(plyr)
library(dplyr)
library(ggplot2)

load_ics_fatal_defects_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\NOV_23\\ICS_DEFECT_HISTORY_VIEW\\ICS_DEFECT_HISTORY_VIEW.csv"
  ics_history <- read.csv(filename, header = T, stringsAsFactors = F)
  ics_history <- ics_history %>% filter(REPAIR_TERMINAL %in% c("OFFLINE_PREP","OFFLINE_POLISH"))
  
  #After applying these two filters, all MODEL_NUM values are 643W only.
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_arc_vehicle_info table. ICS+ history has only MODEL_NUM and not Katashiki.
  
  #Drop the defect data corresponding to weekends as they are causing too many short-term spikes
  ics_history$day_of_week <- weekdays(as.Date(ics_history$CREATION_TIME, "%d-%b-%y"))
  
  ics_history <- ics_history %>% filter(!(day_of_week %in% c("Saturday", "Sunday")))
  ics_history$day_of_week <- factor(ics_history$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  #Start from 1 Oct 2015 as that was the time when ICS+ became operational/stable
  ics_history <- ics_history %>% filter(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01"))
  
  #Keeping manuf_date for joining with ICS vehicle info table. 
  ics_history$manuf_date <- strftime(strptime(ics_history$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")
  ics_history
}

load_ics_fatal_paint_finish_data <- function(fatal_defects)
{
  pf_defects <- fatal_defects %>% filter(SECTION_NUM == "PF")
}

load_arc_vehicle_info <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\NOV_23\\ICS_DEFECT_HISTORY_VIEW\\ICS_ARC_VEHICLE_INFO.CSV"
  #Each VEHICLE_ID appears only once in ICS_arc_vehicle_info. Same applies for each VIN_NO.
  arc_vehicle_info <- read.csv(filename, header = T, stringsAsFactors = F)
								   
  #Keeping manuf_date for joining with ICS history
  arc_vehicle_info$manuf_date <- strftime(strptime(arc_vehicle_info$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_arc_vehicle_info table. ICS+ history has only MODEL_NUM and not Katashiki.
  arc_vehicle_info <- arc_vehicle_info %>% filter(MODEL_NUM == "643W")
  
  arc_vehicle_info$day_of_week <- weekdays(as.Date(arc_vehicle_info$CREATION_TIME, "%d-%b-%y"))
  arc_vehicle_info <- arc_vehicle_info %>% filter(!(day_of_week %in% c("Saturday", "Sunday")))
  arc_vehicle_info$day_of_week <- factor(arc_vehicle_info$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  #Start from 1 Oct 2015 as that was the time when ICS+ became operational/stable
  arc_vehicle_info <- arc_vehicle_info %>% filter(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01"))
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
  
  pf_defects <- pf_defects %>% filter(manuf_date >= start_date)
  arc_vehicle_info <- arc_vehicle_info %>% filter(manuf_date >= start_date)
  
  #Same DEFECT_NUM can occur for multiple VEHICLE_IDs. Same VEHICLE_ID can have multiple DEFECT_NUMs. 
  pf_defects_by_manuf_date <- pf_defects %>% select(DEFECT_NUM, manuf_date) %>% group_by(manuf_date) %>% summarise(n_pf_defects = length(DEFECT_NUM))
  
  #In arc_vehicle_info, each VEHICLE_ID or VIN occurs exactly once
  vehicles_by_manuf_date <- arc_vehicle_info %>% select(VEHICLE_ID, manuf_date) %>% group_by(manuf_date) %>% summarise(n_vehicles = length(VEHICLE_ID))
  
  #Combine ICS history and vehicle info data to compute DPV
  pf_defects_by_manuf_date <- pf_defects_by_manuf_date %>% inner_join(vehicles_by_manuf_date)
  pf_defects_by_manuf_date$pf_dpv <- pf_defects_by_manuf_date$n_pf_defects/pf_defects_by_manuf_date$n_vehicles
    
  print(fivenum(pf_defects_by_manuf_date$pf_dpv)) #For time_window = "year", 0.04379562 0.76237624 1.01273885 1.28333333 4.21126761: variance is highest
  #For time_window = "month", 0.6790698 1.0093023 1.0791176 1.2171717 1.5582822
  #For time_window = "week", 1.227053 1.236967 1.288136 1.318182 1.349515: variance is lowest
    
  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\PF_DPV_", 
                      time_window, ".png", sep = "")
  png(image_file)
  pf_defects_by_manuf_date$manuf_date <- as.Date(pf_defects_by_manuf_date$manuf_date, "%Y-%m-%d")
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
#Observation: with yearly data, 1440 defects (3.34% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
#With monthly data, 202 defects (4.2% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
#With weekly data, 41 defects (3.87% of all fatal PF defects) occurred for x in [9600, 10400] and y in [3200, 4000].
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
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #cut divides the range of x into intervals and codes the values in x according to which interval they fall.
  #Range is hard-coded to make sure all subsets of data have the same grid boundaries for a given grid size.
  #Otherwise, it becomes data-dependent and the boundaries for different time-slices do not match exactly.
  x_c <- cut(input_data$X_COOR, breaks = seq(0, 12000, length.out = n_cells_x + 1)) #The output of cut() is a factor
  y_c <- cut(input_data$Y_COOR, breaks = seq(0, 12000, length.out = n_cells_y + 1))

  #Calculate joint counts at cut levels:
  z <- table(x_c, y_c) #The arguments to table can be factors

  #Plot as a 2D heatmap:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\",
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

#This method is for digging deeper into the region that showed maximum number of defects in plot_pf_defects_x_y(). 
#We do this for the one-year time-slice only as otherwise we have too few defects.
plot_pf_defects_x_y_second_level <- function(input_data, 
                                             x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000, 
											 n_cells_x = 15, n_cells_y = 15)
{
  library(plot3D)
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)  
  print(nrow(input_data))
  #cut divides the range of x into intervals and codes the values in x according to which interval they fall.
  #Range is hard-coded to make sure all subsets of data have the same grid boundaries for a given grid size.
  #Otherwise, it becomes data-dependent and the boundaries for different time-slices do not match exactly.
  x_c <- cut(input_data$X_COOR, breaks = seq(x_min, x_max, length.out = n_cells_x + 1)) 
  #Note: The arguments to seq() are not hard-coded any more, unlike plot_pf_defects_x_y()
  y_c <- cut(input_data$Y_COOR, breaks = seq(y_min, y_max, length.out = n_cells_y + 1))

  z <- table(x_c, y_c)

  #Plot as a 2D heatmap:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\pf_defects_2D_second_level_",
	       n_cells_x, "x", n_cells_y, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  image2D(z = z, border = "black")
  dev.off()
  
  #Plot as a 2D contour:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\pf_defects_contour_second_level_",
	       n_cells_x, "x", n_cells_y, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  contour2D(z)
  #image2D(z = z, rasterImage = TRUE)
  #image2D(z = z, clab = "m")
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

  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_time.png"
  png(image_file, width = 1200, height = 400)
  input_data_by_manuf_date[, manuf_date := as.Date(input_data_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(input_data_by_manuf_date, aes(x = manuf_date, y = n_pf_defects)) + geom_line(size = 0.5) + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily fatal defects for Paint Finish") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + 
		 theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()  
  
  input_data_by_manuf_date
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
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_shift.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line(aes(colour = RESPONSIBLE_SHIFT), size = 0.5) + scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("No. of paint defects") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
       theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
}


#Extracted selected columns from the raw file in Linux. 
#Applied this twice to get rid of the headers: sed '1d' 5620597190494dat.txt > tmpfile; mv tmpfile 5620597190494dat.txt
#wc -l 5620597190494dat.txt gives 14147 lines
#awk -F ' ' '{print $3, $4, $20,  $22}' 5620597190494dat.txt > weather2.txt
#wc -l  weather2.txt gives 14147 lines
load_external_weather_data2 <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\weather\\weather2.txt"
  weather <- fread(filename, header = FALSE, sep = " ", stringsAsFactors = FALSE, showProgress = TRUE, 
                colClasses = c("character", "character", "numeric", "numeric"), data.table = TRUE)
  setnames(weather, c("date_captured", "HrMn", "TEMP", "DEWP"))
  weather <- weather[((TEMP != 999.9) & (DEWP != 9999.9) & (DEWP != 999.9)),]
  weather[, humidity := (DEWP - TEMP)]
  weather[, date_captured := paste(substr(date_captured, 1, 4), "-", substr(date_captured, 5, 6), "-", 
                                           substr(date_captured, 7, 8), sep = "")]
  setkey(weather, date_captured)
  weather_by_date <- weather[, list(avg_temp = mean(TEMP),
                                    avg_humidity = mean(humidity)), by = date_captured]
  weather_by_date
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
  
  #weather_by_date <- load_external_weather_data()
  weather_by_date <- load_external_weather_data2()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_temp, defects_by_date$n_defects)) #-0.1644567 when we focus on the high-defect region with old weather data,
  #it changes to -0.1793046 for new weather data.
  #However, overall fatal Paint Finish defects has a correlation coeff of -0.4033189 with old weather data, and -0.3856621 for new weather data. 
  #That means defects in this region are concentrated for some other reason as well. 
  print(lm(n_defects ~ avg_temp, data = defects_by_date)) #Intercept 11.74982, slope -0.04605 with old weather data.
  #Intercept 10.39286, slope -0.09481 with new weather data
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_temperature2.png"
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
  
  #weather_by_date <- load_external_weather_data()
  weather_by_date <- load_external_weather_data2()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_temp, defects_by_date$pf_dpv)) #-0.2268994 when we focus on the high-defect region with old weather data,
  #and -0.2292308 for new weather data.
  #However, overall fatal Paint Finish DPV has a correlation coeff of -0.3847753 for old weather data and -0.3632067 for new weather data. 
  #That means defects in this region are concentrated for some other reason as well. 
  print(lm(pf_dpv ~ avg_temp, data = defects_by_date)) #Intercept 0.0875062, slope -0.0005156 for old weather data,
  #Intercept 0.0696688, slope -0.0009482 for new weather data
  
  image_file <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\dpv_for_high_defect_region_by_temperature2.png"
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
  
  #weather_by_date <- load_external_weather_data()
  weather_by_date <- load_external_weather_data2()
  setkey(weather_by_date, date_captured)
  defects_by_date <- defects_by_date[weather_by_date, nomatch = 0]
  
  print(cor(defects_by_date$avg_humidity, defects_by_date$n_defects)) #-0.04532517 when we focus on the high-defect region for old weather
  #data, and -0.03817399 for new weather data. Both correlation coefficients are negligible.
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_humidity2.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(defects_by_date, aes(avg_humidity, n_defects)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily humidity") + ylab("Total fatal paint finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  defects_by_date
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_post_tara_feedback.R")
#fatal_defects <- load_ics_fatal_defects_data() #52,519 rows. Date range is 2015-10-01 to 2016-11-23.
#pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #45,577 rows
#arc_vehicle_info <- load_arc_vehicle_info() #43,693 rows
#z <- plot_pf_defects_x_y(pf_defects, time_window = "week", n_cells_x = 15, n_cells_y = 15)
#input_data_by_manuf_date <- analyze_high_defect_region_by_time(pf_defects)
#analyze_high_defect_region_by_shift(pf_defects)
#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(time_window = "week", pf_defects, arc_vehicle_info)
z <- plot_pf_defects_x_y_second_level(pf_defects, n_cells_x = 10, n_cells_y = 10)
#defects_by_date <- analyze_high_defect_region_by_temperature(pf_defects)
#defects_by_date <- analyze_dpv_for_high_defect_region_by_temperature(pf_defects, arc_vehicle_info)
#defects_by_date <- analyze_high_defect_region_by_humidity(pf_defects)
#weather_by_date <- load_external_weather_data2()
