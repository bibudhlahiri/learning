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
  #For time_window = "week", 0.9772727 1.0663507 1.1073446 1.1594203 1.3058252: variance is lowest
    
  
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

#Check which pages have most defects. There are 3 values of PAGE only: 
#096B EXTERIOR SURFACE (51.63%), 096B DOOR OPENINGS (34.77%), 096B FR RR SURFACE (13.59%).
analyze_by_page <- function(input_data)
{
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  by_page <- input_data %>% select(PAGE, DEFECT_ID) %>% group_by(PAGE) %>% summarise(n_defects = length(DEFECT_ID))
  by_page <- by_page[with(by_page, order(-n_defects)),]
  total_defects <- nrow(input_data)
  by_page$percentage <- 100*by_page$n_defects/total_defects
  by_page$PAGE <- factor(by_page$PAGE, 
                              levels = by_page$PAGE,
                              ordered = TRUE)
  print(by_page)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\pf_fatal_defects_by_page.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_page, aes(x = factor(PAGE), y = n_defects)) + geom_bar(stat = "identity") + xlab("Page") + 
       ylab("Number of fatal Paint Finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_page
}

#Takes a PAGE value as input and does the 2D heatmap for defects in that page only.
#For PAGE = 096B EXTERIOR SURFACE, max: 1229 defects (5.52% of all fatal PF defects in the page) occurred for x in [9600, 10400] and y in [3200, 4000].
#For PAGE = 096B DOOR OPENINGS, max: 835 defects (5.57% of all fatal PF defects in the page) occurred for x in [9600, 10400] and y in [2400, 3200]. 
#The range of Y coordinates differ now: shifted by one cell down.
#For PAGE = 096B FR RR SURFACE, max: 392 defects (6.69% of all fatal PF defects in the page) occurred for x in [6400, 7200] and y in [5600, 6400].
#Both X and Y coordinates are very different now. 
plot_pf_defects_x_y_specific_to_page <- function(input_data, time_window = "year", input_page = "096B EXTERIOR SURFACE", 
                                                 n_cells_x = 15, n_cells_y = 15)
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
  input_data <- input_data %>% filter(PAGE == input_page)
  
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
	       time_window, "_", gsub(" ", "_", input_page), ".png", sep = "")
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
  cat(paste("For PAGE = ", input_page, ", max: ", max(z), " defects (", round(100*max(z)/nrow(input_data), 2), 
            "% of all fatal PF defects in the page) occurred for x in [", x_min, ", ", x_max, "] and y in [", y_min, ", ", y_max, "]\n", sep = ""))
  
  z
}

#This method is for digging deeper into the region that showed maximum number of defects in plot_pf_defects_x_y(). 
#We do this for the one-year time-slice only as otherwise we have too few defects.
#It points out a row of 6 cells for x in [9680, 10160] and y in [3680, 3760] where 264 (18.3% of 1440) defects occur.
#More specifically, 53 defects (3.68% of 1440 fatal PF defects) occurred for x in [9760, 9840] and y in [3680, 3760], i.e., 
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
  image2D(z = z, border = "black"
          #,xaxs = "i", yaxs="i", 
		  #,pty = "s"
		  #,axes=F
		  )
  #axis(1,pos=1)
  #axis(2,pos=1)
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
  
  
  #input_data
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
                                           substr(weather$date_captured, 7, 8), sep = "")
  weather_by_date <- weather %>% select(date_captured, TEMP, humidity) %>% group_by(date_captured) %>% 
                     summarise(avg_temp = mean(TEMP), avg_humidity = mean(humidity))
}

#84.86% defects in the high-defect region are from QTR PANEL. There are only 3 values of item: QTR PANEL, DOOR OPENING (14.65%) and C PILLAR.
#So, 99.5% of the defects in this region are coming from these two.
analyze_by_item <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  by_item <- input_data %>% select(ITEM, DEFECT_ID) %>% group_by(ITEM) %>% summarise(n_defects = length(DEFECT_ID))
  by_item <- by_item[with(by_item, order(-n_defects)),]
  
  total_defects <- nrow(input_data)
  by_item$percentage <- 100*(by_item$n_defects/total_defects)
  print(by_item)
  by_item$ITEM <- factor(by_item$ITEM, 
                              levels = by_item$ITEM,
                              ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_item.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_item, aes(x = factor(ITEM), y = n_defects)) + geom_bar(stat = "identity") + xlab("Item") + 
       ylab("#Fatal PF defects in max-defect region") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_item
}

#48% are from LH (A ZONE RR [SO]), 17% are from LH (S ZONE FR [SO]), 16.59% from LH (S ZONE RR [SO]) and 12.64% are 
#from LH DOGLEG (A ZONE[SO]. These comprise of 94.5% defects in the high-defect region. All of them are from LH and not RH.
analyze_by_portion <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  by_portion <- input_data %>% select(PORTION, DEFECT_ID) %>% group_by(PORTION) %>% summarise(n_defects = length(DEFECT_ID))
  by_portion <- by_portion[with(by_portion, order(-n_defects)),]
  
  total_defects <- nrow(input_data)
  by_portion$percentage <- 100*(by_portion$n_defects/total_defects)
  by_portion$PORTION <- factor(by_portion$PORTION, 
                              levels = by_portion$PORTION,
                              ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_portion.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_portion, aes(x = factor(PORTION), y = n_defects)) + geom_bar(stat = "identity") + xlab("Portion") + 
       ylab("#Fatal PF defects in max-defect region") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_portion
}

#945 (66%) from zone A, 495 (34%) from zone S
analyze_by_zone <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  input_data$zone <- apply(input_data, 1, function(row) get_zone_from_portion(as.character(row["PORTION"])))
  by_zone <- input_data %>% select(zone, DEFECT_ID) %>% group_by(zone) %>% summarise(n_defects = length(DEFECT_ID))
  by_zone <- by_zone[with(by_zone, order(-n_defects)),]
  
  total_defects <- nrow(input_data)
  by_zone$percentage <- 100*(by_zone$n_defects/total_defects)
  by_zone$zone <- factor(by_zone$zone, levels = by_zone$zone, ordered = TRUE)
  print(by_zone)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\post_tara_feedback\\high_defect_region_by_zone.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_zone, aes(x = factor(zone), y = n_defects)) + geom_bar(stat = "identity") + xlab("zone") + 
       ylab("#Fatal PF defects in max-defect region") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) 
  print(p)
  dev.off()
  by_zone
}

get_zone_from_portion <- function(portion)
{
  if (!grepl("ZONE", portion))
  {
    return(NA)
  }
  portion_split <- strsplit(portion, " ")[[1]]
  zone_position_in_portion <- which(grepl("ZONE", portion_split)) - 1
  raw_zone <- portion_split[zone_position_in_portion]
  parenth_pos <- which(strsplit(raw_zone, "")[[1]] == "(")
  zone <- substr(raw_zone, parenth_pos + 1, parenth_pos + 1)
}

#Are there coordinates which are appearing multiple times with the same defect? 
analyze_coordinates_for_overlap <- function(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  end_date <- max(input_data$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  input_data <- input_data %>% filter(manuf_date >= start_date)
  
  #Note: The left boundary is not part of the interval, the right boundary is.
  input_data <- input_data %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  by_coord <- input_data %>% select(X_COOR, Y_COOR, DEFECT_ID) %>% group_by(X_COOR, Y_COOR) %>% summarise(n_defects = length(DEFECT_ID))
  by_coord <- by_coord %>% filter(n_defects > 1)
  by_coord <- by_coord[with(by_coord, order(-n_defects)),]
  
  #9 defects at (x = 9840, y = 3675) only. The VEHICLE_ID and DEFECT_NUM values are all unique. The CREATION_TIME values range from 
  #March to August. The portion values are all same (LH (A ZONE FR [SO])). The item values are all QTR PANEL.
  #9 defects at (x = 10095, y = 3675) only. The VEHICLE_ID and DEFECT_NUM values are all unique. The CREATION_TIME values range from 
  #Dec 2015 to August 2016. The portion values are all same (LH (A ZONE RR [SO])). The item values are all QTR PANEL.
  defects_at_single_point <- input_data %>% filter(X_COOR == 10095 & Y_COOR == 3675)
  print(length(unique(defects_at_single_point$VEHICLE_ID)))
  print(length(unique(defects_at_single_point$DEFECT_NUM)))
  by_coord
}

#Are there coordinates, in the high-defect region, in general, that belong to multiple portions?
coordinates_in_overlaps <- function(fatal_defects, pf_defects, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
{
  coords_in_mult_portions <- fatal_defects %>% select(X_COOR, Y_COOR, PORTION) %>% group_by(X_COOR, Y_COOR) %>% summarise(n_portions = length(unique(PORTION)))
  coords_in_mult_portions <- coords_in_mult_portions %>% filter(n_portions > 1)
  coords_in_mult_portions <- coords_in_mult_portions[with(coords_in_mult_portions, order(-n_portions)),]
  
  end_date <- max(pf_defects$manuf_date)
  start_date <- as.character(as.Date(end_date) - 364)
  defects_in_hd_region <- pf_defects %>% filter(manuf_date >= start_date) %>% filter(X_COOR > x_min & X_COOR <= x_max & Y_COOR > y_min & Y_COOR <= y_max)
  
  #Are there defects in the high-defect region that could have been allocated to multiple portions? 218 such defects.
  defects_in_hd_region <- defects_in_hd_region %>% inner_join(coords_in_mult_portions, by = c("X_COOR", "Y_COOR"))
  print(defects_in_hd_region[1:10, c("X_COOR", "Y_COOR", "PORTION")]) #The PORTION comes from defects_in_hd_region, which means the reported portion
  #value in the defect
  
  #One such defect is at x = 10005, y = 3315, portion reported was LH DOGLEG (A ZONE[SO]), but it could also have been LH (S ZONE FR [SO]).
  print(fatal_defects %>% filter(X_COOR ==  10005 & Y_COOR == 3315) %>% select(PORTION))
  #Another such defect is at x = 9675, y = 3345, portion reported was LH (S ZONE FR [SO]), but it could also have been LH DOGLEG (A ZONE[SO]).
  print(fatal_defects %>% filter(X_COOR ==  9675 & Y_COOR == 3345) %>% select(PORTION))
  defects_in_hd_region
}



#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_post_tara_feedback.R")
#fatal_defects <- load_ics_fatal_defects_data() #52,519 rows. Date range is 2015-10-01 to 2016-11-23.
#pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #45,577 rows
#arc_vehicle_info <- load_arc_vehicle_info() #43,693 rows
#z <- plot_pf_defects_x_y(pf_defects, time_window = "week", n_cells_x = 15, n_cells_y = 15)
#input_data_by_manuf_date <- analyze_high_defect_region_by_time(pf_defects)
#analyze_high_defect_region_by_shift(pf_defects)
#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(time_window = "week", pf_defects, arc_vehicle_info)
#plot_pf_defects_x_y_second_level(pf_defects, n_cells_x = 10, n_cells_y = 10)
#by_item <- analyze_by_item(pf_defects, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
#by_portion <- analyze_by_portion(pf_defects, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
#by_zone <- analyze_by_zone(input_data, x_min = 9600, x_max = 10400, y_min = 3200, y_max = 4000)
#by_coord <- analyze_coordinates_for_overlap(pf_defects)
#defects_in_hd_region <- coordinates_in_overlaps(fatal_defects, pf_defects)
by_page <- analyze_by_page(pf_defects)
#z <- plot_pf_defects_x_y_specific_to_page(pf_defects, time_window = "year", input_page = "096B FR RR SURFACE", 
#                                                 n_cells_x = 15, n_cells_y = 15)
