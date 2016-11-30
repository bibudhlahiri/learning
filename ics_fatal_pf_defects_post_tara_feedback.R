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
#It points out a row of 6 cells for x in [9680, 10160] and y in [3680, 3760] where 264 (18.3% of 1440) defects occur.
#More specifically, 53 defects (3.68% of all fatal PF defects) occurred for x in [9760, 9840] and y in [3680, 3760], i.e., 
#in an 80 x 80 region.
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
            "% of the fatal PF defects in the high-defect region) occurred for x in [", x_min, ", ", x_max, "] and y in [", y_min, ", ", y_max, "]\n", sep = ""))
  
  z
}

#As yearly, monthly and weekly data all point to the same region as the highest-defect region, we are analyzing that 
#region for last one year.
analyze_high_defect_region_by_time <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  print(nrow(input_data))
  input_data_by_manuf_date <- input_data %>% select(DEFECT_NUM, manuf_date) %>% group_by(manuf_date) %>% summarise(n_pf_defects = length(DEFECT_NUM))

  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_time.png"
  png(image_file, width = 1200, height = 400)
  input_data_by_manuf_date$manuf_date <- as.Date(input_data_by_manuf_date$manuf_date, "%Y-%m-%d")
  p <- ggplot(input_data_by_manuf_date, aes(x = manuf_date, y = n_pf_defects)) + geom_line(size = 0.5) + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily fatal defects for Paint Finish") + 
	   theme(axis.text = element_text(colour = 'blue', size = 12, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 12, face = 'bold')) + 
		 theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()  
  
  input_data_by_manuf_date
}

#In the high-defect region, total 814 defects from shift A, 626 defects from shift B. 57% from shift A, 43% from shift B.
#This is more even that the distribution in overall PF fatal defects data, where 64% come from A, 36% from B.
analyze_high_defect_region_by_shift <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  #Take one year's data only to make the number of defects match
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  print(table(input_data$RESPONSIBLE_SHIFT))
  
  by_creation_time <- input_data %>% select(DEFECT_ID, CREATION_TIME, RESPONSIBLE_SHIFT) %>% group_by(CREATION_TIME, RESPONSIBLE_SHIFT) %>% summarise(n_defects = length(DEFECT_ID))
  by_creation_time$manuf_date <- as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")
  
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
load_external_weather_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\weather\\weather2.txt"
  weather <- read.csv(filename, header = F, stringsAsFactors = F)
  colnames(weather) <- c("date_captured", "HrMn", "TEMP", "DEWP")
  weather <- weather %>% filter((TEMP != 999.9) & (DEWP != 9999.9) & (DEWP != 999.9))
  weather$humidity <- (DEWP - TEMP)
  weather$date_captured <- paste(substr(weather$date_captured, 1, 4), "-", substr(weather$date_captured, 5, 6), "-", 
                                           substr(weather$date_captured, 7, 8), sep = "")]
  weather_by_date <- weather %>% select(date_captured, TEMP, humidity) %>% group_by(date_captured) %>% 
                     summarise(avg_temp = mean(TEMP), avg_humidity = mean(humidity))
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_post_tara_feedback.R")
#fatal_defects <- load_ics_fatal_defects_data() #52,519 rows. Date range is 2015-10-01 to 2016-11-23.
#pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #45,577 rows
#arc_vehicle_info <- load_arc_vehicle_info() #43,693 rows
#z <- plot_pf_defects_x_y(pf_defects, time_window = "week", n_cells_x = 15, n_cells_y = 15)
#input_data_by_manuf_date <- analyze_high_defect_region_by_time(pf_defects)
analyze_high_defect_region_by_shift(pf_defects)
#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(time_window = "week", pf_defects, arc_vehicle_info)
#z <- plot_pf_defects_x_y_second_level(pf_defects, n_cells_x = 10, n_cells_y = 10)
#defects_by_date <- analyze_high_defect_region_by_temperature(pf_defects)
#defects_by_date <- analyze_dpv_for_high_defect_region_by_temperature(pf_defects, arc_vehicle_info)
#defects_by_date <- analyze_high_defect_region_by_humidity(pf_defects)
#weather_by_date <- load_external_weather_data2()
