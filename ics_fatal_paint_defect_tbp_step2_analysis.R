#This analysis is for the fatal Lexus Paint Finish defects where discrepancy is SEED TC, and ITEM is DOOR OPENING.
#The reason is we are following the same process in TBP Step 2, and these are the dimensions where most of the fatal defects
#have occurred.
library(data.table)
library(ggplot2)

load_ics_fatal_defects_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\ics_T01ICSGP-Source-TMMK-Lexus-All(History_Table_only)\\ics_T01ICSGP.csv"
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
  ics_history <- ics_history[((RESPONSIBLE_SHOP == "PAINT L3") & (MODEL_NUM == "643W")),] #237,551 rows
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_VEHICLE_INFO_MV table. ICS+ history has only MODEL_NUM and not Katashiki.
  
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
  
  #Keeping manuf_date for joining with ICS vehicle info table
  ics_history[, manuf_date := strftime(strptime(ics_history$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]
  ics_history
}

#Trying to understand the X and Y coordinate ranges for various portions.
#Portions can be overlapping with each other, as shown by the following coordinates:
#                     PORTION   min_x max_x min_y max_y
#  1: LH B PLR LWR(A ZONE[SO])     0  7890     0  3825
#  2:        LH FR (A ZONE FR)     0  4830     0  4740
get_x_y_ranges_for_portions <- function(fatal_defects)
{
  setkey(fatal_defects, PORTION)
  portion_boundaries <- fatal_defects[, list(min_x = min(X_COOR), max_x = max(X_COOR), 
                                             min_y = min(Y_COOR), max_y = max(Y_COOR)), by = PORTION]
  portion_boundaries <- portion_boundaries[order(min_x)]
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\portion_boundaries.csv"
  write.table(portion_boundaries, filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  portion_boundaries
}

load_ics_fatal_paint_finish_data <- function(fatal_defects)
{
  setkey(fatal_defects, SECTION_NUM)
  pf_defects <- fatal_defects[(SECTION_NUM == "PF"),]
}

load_seed_tc_door_opening_data <- function(pf_defects)
{
  setkey(pf_defects, DISCREPANCY, ITEM)
  pf_defects[((DISCREPANCY == 'SEED TC') & (ITEM == 'DOOR OPENING')),]
}

load_seed_tc_door_data <- function(pf_defects)
{
  setkey(pf_defects, DISCREPANCY, ITEM)
  pf_defects[((DISCREPANCY == 'SEED TC') & (ITEM == 'DOOR')),]
}

load_vehicle_info_mv <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICS\\ics_T01ICSGP-Source-TMMK-Lexus-All( Vehcile_shop_Table)\\ICS_VEHICLE_INFO_MV.CSV"
  #Each VEHICLE_ID appears only once in ICS_VEHICLE_INFO_MV. Same applies for each VIN_NO.
  vehicle_info_mv <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "character", "numeric", "character", "character", #A-E
					               "character", "character", "character", "character", "Date", #F-J
								   "Date", "character", "numeric", "character", "character", #K-O
								   "numeric", "character", "numeric", "character", "character", #P-T
								   "character", "character", "character" #U-W
								   ), data.table = TRUE) #38,545 rows
								   
  #Keeping manuf_date for joining with ICS history
  vehicle_info_mv[, manuf_date := strftime(strptime(vehicle_info_mv$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]
  #There is a one-to-one mapping between MODEL_NUM 643W and Katashiki value "GSV 60L CETGKA". This is confirmed
  #from the ICS_VEHICLE_INFO_MV table. ICS+ history has only MODEL_NUM and not Katashiki.
  setkey(vehicle_info_mv, MODEL_NUM)  
  vehicle_info_mv <- vehicle_info_mv[(MODEL_NUM == "643W"),]
  
  vehicle_info_mv[, day_of_week := weekdays(as.Date(vehicle_info_mv$CREATION_TIME, "%d-%b-%y"))]
  setkey(vehicle_info_mv, day_of_week)
  vehicle_info_mv <- vehicle_info_mv[(!(day_of_week %in% c("Saturday", "Sunday"))),]
  vehicle_info_mv$day_of_week <- factor(vehicle_info_mv$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  #Start from 1 Oct 2015 as that was the time when ICS+ became operational/stable
  vehicle_info_mv <- vehicle_info_mv[(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01")),]
}


#Normalize by the number of cars with a given type of exterior color code, otherwise more common 
#colors will give rise to more defects.
#Observation: For door opening, highest DPV for 1J4 (0.06320225, 90 defects for 1424 cars), 10th is 1H9 (0.035, 92 defects for 2628 cars)
analyze_tbp_step2_defects_by_exterior_color <- function(seed_tc_door_opening_data, vehicle_info_mv)
{
  setkey(seed_tc_door_opening_data, VEHICLE_ID)
  setkey(vehicle_info_mv, VEHICLE_ID)
  seed_tc_door_opening_data <- seed_tc_door_opening_data[vehicle_info_mv, nomatch = 0]
  
  #Get the number of defects by exterior color
  setkey(seed_tc_door_opening_data, EXTERIOR_COLOR_CODE)
  by_ext_color <- seed_tc_door_opening_data[, list(n_defects = length(DEFECT_NUM)), by = EXTERIOR_COLOR_CODE]
  
  #Get the production volume by exterior color. We do not do it from seed_tc_door_opening_data because it will only have cars with TBP step 2 defects.
  prodn_by_ext_color <- vehicle_info_mv[, list(n_cars = length(VEHICLE_ID)), by = EXTERIOR_COLOR_CODE]
  
  setkey(by_ext_color, EXTERIOR_COLOR_CODE)
  setkey(prodn_by_ext_color, EXTERIOR_COLOR_CODE)
  by_ext_color <- by_ext_color[prodn_by_ext_color, nomatch = 0]
  
  by_ext_color[ , dpv := n_defects/n_cars]

  setkey(by_ext_color, dpv)
  by_ext_color <- by_ext_color[order(-dpv)]
  
  by_ext_color$EXTERIOR_COLOR_CODE <- factor(by_ext_color$EXTERIOR_COLOR_CODE, 
                              levels = by_ext_color$EXTERIOR_COLOR_CODE,
                              ordered = TRUE)
  print(by_ext_color[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\door_opening\\tbp_step2_defects_by_ext_color.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_ext_color[1:10,], aes(x = factor(EXTERIOR_COLOR_CODE), y = dpv)) + geom_bar(stat = "identity") + xlab("Exterior Color Code") + 
       ylab("Number of fatal Paint Finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_ext_color
}

#Normalize by the number of cars with a given type of interior color code, otherwise more common 
#colors will give rise to more defects.
#Observation: For door opening, highest DPV for LC02 (0.2, but 1 defect for 5 cars), 10th is LA20 (0.057, 35 defects for 613 cars)
analyze_tbp_step2_defects_by_interior_color <- function(seed_tc_door_opening_data, vehicle_info_mv)
{
  setkey(seed_tc_door_opening_data, VEHICLE_ID)
  setkey(vehicle_info_mv, VEHICLE_ID)
  seed_tc_door_opening_data <- seed_tc_door_opening_data[vehicle_info_mv, nomatch = 0]
  
  #Get the number of defects by interior color
  setkey(seed_tc_door_opening_data, INTERIOR_COLOR_CODE)
  by_int_color <- seed_tc_door_opening_data[, list(n_defects = length(DEFECT_NUM)), by = INTERIOR_COLOR_CODE]
  
  #Get the production volume by interior color. We do not do it from seed_tc_door_opening_data because it will only have cars with TBP step 2 defects.
  prodn_by_int_color <- vehicle_info_mv[, list(n_cars = length(VEHICLE_ID)), by = INTERIOR_COLOR_CODE]
  
  setkey(by_int_color, INTERIOR_COLOR_CODE)
  setkey(prodn_by_int_color, INTERIOR_COLOR_CODE)
  by_int_color <- by_int_color[prodn_by_int_color, nomatch = 0]
  
  by_int_color[ , dpv := n_defects/n_cars]

  setkey(by_int_color, dpv)
  by_int_color <- by_int_color[order(-dpv)]
  
  by_int_color$INTERIOR_COLOR_CODE <- factor(by_int_color$INTERIOR_COLOR_CODE, 
                              levels = by_int_color$INTERIOR_COLOR_CODE,
                              ordered = TRUE)
  print(by_int_color[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\door_opening\\tbp_step2_defects_by_int_color.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_int_color[1:10,], aes(x = factor(INTERIOR_COLOR_CODE), y = dpv)) + geom_bar(stat = "identity") + xlab("Interior Color Code") + 
       ylab("Number of fatal Paint Finish defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_int_color
}

#A basic scatterplot of X and Y coordinates of all TBP Step 2 defects
plot_all_x_y <- function(seed_tc_door_opening_data)
{
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\fatal_defects_locations.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(seed_tc_door_opening_data, aes(X_COOR, Y_COOR)) + geom_point() + xlab("X-coordinate of defect") + 
       ylab("Y-coordinate of defect") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold'))
  print(p)
  dev.off()
}

#2D and 3D heatmaps/histograms of number of TBP Step 2 defects based on their X and Y coordinates.
#Observation: For door opening, 304 defects occurred for x in [9220, 10300] and y in [2410, 3560]
#For door, 196 defects occurred for x in [5480, 6390] and y in [3460, 4610]
plot_tbp_step2_defects_x_y <- function(seed_tc_door_data, n_cells_x = 10, n_cells_y = 10)
{
  library(plot3D)
  x_c <- cut(seed_tc_door_data$X_COOR, n_cells_x)
  y_c <- cut(seed_tc_door_data$Y_COOR, n_cells_y)

  #Calculate joint counts at cut levels:
  z <- table(x_c, y_c)

  #Plot as a 3D histogram:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\door\\pf_defects_3D_",
	       n_cells_x, "_x_", n_cells_y, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  hist3D(z = z, border = "black")
  dev.off()

  #Plot as a 2D heatmap:
  image_file <- 
     paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\door\\pf_defects_2D_",
	       n_cells_x, "_x_", n_cells_y, ".png", sep = "")
  png(image_file,  width = 600, height = 480, units = "px")
  image2D(z = z, border = "black")
  dev.off()
  
  #Draw one or more boxes on a scatterplot of the ITEM based on rectangles obtained from the 2D histogram where most defects occurr
  index_of_max <- as.numeric(which(z == max(z), arr.ind = TRUE))
  x_boundaries_of_max <- rownames(z)[index_of_max[1]]
  y_boundaries_of_max <- colnames(z)[index_of_max[2]]
  x_min <- as.numeric(gsub("\\(", "", strsplit(x_boundaries_of_max, ",")[[1]][1]))
  x_max <- as.numeric(gsub("\\]", "", strsplit(x_boundaries_of_max, ",")[[1]][2]))
  y_min <- as.numeric(gsub("\\(", "", strsplit(y_boundaries_of_max, ",")[[1]][1]))
  y_max <- as.numeric(gsub("\\]", "", strsplit(y_boundaries_of_max, ",")[[1]][2]))
  cat(paste("Max: ", max(z), " defects occurred for x in [", x_min, ", ", x_max, "] and y in [", y_min, ", ", y_max, "]\n", sep = ""))
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\tbp_step2\\door\\fatal_defects_locations.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(seed_tc_door_opening_data, aes(X_COOR, Y_COOR)) + geom_point() + 
	   annotate("rect", xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, alpha = 0.5) + 
	   xlab("X-coordinate of defect") + ylab("Y-coordinate of defect") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold'))
  print(p)
  dev.off()
  
  z
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_paint_defect_tbp_step2_analysis.R")
fatal_defects <- load_ics_fatal_defects_data() #209,880 rows
#portion_boundaries <- get_x_y_ranges_for_portions(fatal_defects)
pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #72,629 rows
seed_tc_door_opening_data <- load_seed_tc_door_opening_data(pf_defects) #3,145 rows
#seed_tc_door_data <- load_seed_tc_door_data(pf_defects) #2,435 rows
vehicle_info_mv <- load_vehicle_info_mv() #21,488 rows
#by_ext_color <- analyze_tbp_step2_defects_by_exterior_color(seed_tc_door_opening_data, vehicle_info_mv)
by_int_color <- analyze_tbp_step2_defects_by_interior_color(seed_tc_door_opening_data, vehicle_info_mv)
#cluster_goodness <- analyze_pf_defects_by_x_y_coord(pf_defects)
#plot_all_x_y(seed_tc_door_opening_data)
#z <- plot_tbp_step2_defects_x_y(seed_tc_door_data, 10, 10)


