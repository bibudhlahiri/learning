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
#Observation: with yearly data, 2285 defects (3.43% of all fatal PF defects) occurred for x in [9440, 10200] and y in [3140, 3930].
#With monthly data, 234 defects (4.48% of all fatal PF defects) occurred for x in [9380, 10100] and y in [3100, 3870].
#With weekly data, 45 defects (3.77% of all fatal PF defects) occurred for x in [9380, 10100] and y in [3170, 3940].
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
  
  x_c <- cut(input_data$X_COOR, n_cells_x)
  y_c <- cut(input_data$Y_COOR, n_cells_y)

  #Calculate joint counts at cut levels:
  z <- table(x_c, y_c)

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


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_dashboard.R")
#fatal_defects <- load_ics_fatal_defects_data() #227,242 rows. Date range is 2015-10-01 to 2016-11-23.
#pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #79,032 rows
#arc_vehicle_info <- load_arc_vehicle_info() #43,693 rows
z <- plot_pf_defects_x_y(pf_defects, time_window = "week", n_cells_x = 15, n_cells_y = 15)

#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(time_window = "year", pf_defects, arc_vehicle_info)


