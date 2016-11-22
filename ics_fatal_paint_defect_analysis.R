#This analysis is for all Lexus paint defects from ICS+.
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

load_ics_fatal_paint_finish_data <- function(fatal_defects)
{
  setkey(fatal_defects, SECTION_NUM)
  pf_defects <- fatal_defects[(SECTION_NUM == "PF"),]
}

#35.48% from zone A, 29.86% from zone B, 27.69% from zone S
analyze_by_zone <- function(pf_defects)
{
  system.time(pf_defects[, zone := apply(pf_defects, 1, function(row) get_zone_from_portion(as.character(row["PORTION"])))])
  by_zone <- pf_defects[, list(n_defects = length(DEFECT_ID)), by = zone]
  setkey(by_zone, n_defects)
  by_zone <- by_zone[order(-n_defects)]
  total_defects <- nrow(pf_defects)
  by_zone[, percentage := 100*n_defects/total_defects]
  by_zone$zone <- factor(by_zone$zone, levels = by_zone$zone, ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\paint_finish_fatal_defects_by_zone.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_zone, aes(x = factor(zone), y = n_defects)) + geom_bar(stat = "identity") + xlab("zone") + 
       ylab("Number of Lexus paint finish fatal defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
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

#Analyzing by first two characters of PORTION. 37490 from LH (51.69%), 35027 from RH (48.30%)
analyze_by_location <- function(pf_defects)
{
  pf_defects[, location := substr(pf_defects$PORTION, 1, 2)]
  setkey(pf_defects, location)
  pf_defects <- pf_defects[(location != "(C"),]
  levels(pf_defects$location) <- c('LH', 'RH')
  by_location <- pf_defects[, list(n_defects = length(DEFECT_ID)), by = location]
  setkey(by_location, n_defects)
  by_location <- by_location[order(-n_defects)]
  total_defects <- nrow(pf_defects)
  by_location[, percentage := 100*n_defects/total_defects]
  by_location$location <- factor(by_location$location, levels = by_location$location, ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\paint_finish_fatal_defects_by_location.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_location, aes(x = factor(location), y = n_defects)) + geom_bar(stat = "identity") + xlab("location") + 
       ylab("Number of Lexus paint finish fatal defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  by_location
}

#Analyzing by first four characters of PORTION.
#RH FR: 8405, LH FR: 8031, LH RR: 5934, RH RR: 5785, RH B: 3774 
analyze_by_long_location <- function(pf_defects)
{
  pf_defects[, location := substr(pf_defects$PORTION, 1, 5)]
  pf_defects <- pf_defects[(!(location %in% c("LH (S", "(C", "LH (A", "RH (B", "LH DO", "LH (B", "RH (S"))),]
  setkey(pf_defects, location)
  by_location <- pf_defects[, list(n_defects = length(DEFECT_ID)), by = location]
  setkey(by_location, n_defects)
  by_location <- by_location[order(-n_defects)]
  total_defects <- nrow(pf_defects)
  by_location[, percentage := 100*n_defects/total_defects]
  by_location$location <- factor(by_location$location, levels = by_location$location, ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\paint_finish_fatal_defects_by_long_location.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_location[1:5,], aes(x = factor(location), y = n_defects)) + geom_bar(stat = "identity") + xlab("Long location") + 
       ylab("Number of Lexus paint finish fatal defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  by_location
}

#15.89% SEED TC, 9.72% YARN SEED: this has a long tail
analyze_by_discrepancy <- function(pf_defects)
{
  setkey(pf_defects, DISCREPANCY)
  by_discrepancy <- pf_defects[, list(n_defects = length(DEFECT_ID)), by = DISCREPANCY]
  setkey(by_discrepancy, n_defects)
  by_discrepancy <- by_discrepancy[order(-n_defects)]
  total_defects <- nrow(pf_defects)
  by_discrepancy[, percentage := 100*n_defects/total_defects]
  by_discrepancy$DISCREPANCY <- factor(by_discrepancy$DISCREPANCY, 
                              levels = by_discrepancy$DISCREPANCY,
                              ordered = TRUE)
  print(by_discrepancy[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\lexus_paint_defects_by_discrepancy.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_discrepancy[1:10,], aes(x = factor(DISCREPANCY), y = n_defects)) + geom_bar(stat = "identity") + xlab("Discrepancy") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_discrepancy
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

pf_dpv_for_fatal_defects <- function(pf_defects, vehicle_info_mv, threshold = 10)
{
  setkey(pf_defects, manuf_date)
  #Same DEFECT_NUM can occur for multiple VEHICLE_IDs. Same VEHICLE_ID can have multiple DEFECT_NUMs.
  pf_defects_by_manuf_date <- pf_defects[, list(n_pf_defects = length(DEFECT_NUM)), by = manuf_date]  
  
  setkey(vehicle_info_mv, manuf_date)
  #In vehicle_info_mv, each VEHICLE_ID or VIN occurs exactly once
  vehicles_by_manuf_date <- vehicle_info_mv[, list(n_vehicles = length(VEHICLE_ID)), by = manuf_date]
  
  #Combine ICS history and vehicle info data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(pf_defects_by_manuf_date, manuf_date)
  pf_defects_by_manuf_date <- pf_defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  pf_defects_by_manuf_date[, pf_dpv := n_pf_defects/n_vehicles] 
  
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\DPV_fatal_defects.csv"
  write.table(pf_defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
    
  print(fivenum(pf_defects_by_manuf_date$pf_dpv)) #0.4653465 1.0584795 1.3524664 1.7261146 407.0000000
    
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\PF_DPV.png"
  png(image_file, width = 1200, height = 400)
  pf_defects_by_manuf_date[, manuf_date := as.Date(pf_defects_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(pf_defects_by_manuf_date, aes(x = manuf_date, y = pf_dpv)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs for fatal defects")
  print(p)
  aux <- dev.off()
  
  trunc_pf_defects_by_manuf_date <- pf_defects_by_manuf_date[(pf_dpv <= threshold),]
  cat(paste("Truncating ", 100*(1 - nrow(trunc_pf_defects_by_manuf_date)/nrow(pf_defects_by_manuf_date)), "% of points", "\n", sep = ""))
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\fatal_defects\\trunc_PF_DPV.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(trunc_pf_defects_by_manuf_date, aes(x = manuf_date, y = pf_dpv)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs for fatal defects (truncated plot)")
  print(p)
  aux <- dev.off()

  pf_defects_by_manuf_date
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_paint_defect_analysis.R")
fatal_defects <- load_ics_fatal_defects_data() #209,880 rows
pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #72,629 rows
#by_zone <- analyze_by_zone(pf_defects)
#by_location <- analyze_by_location(pf_defects)
by_discrepancy <- analyze_by_discrepancy(pf_defects)
#vehicle_info_mv <- load_vehicle_info_mv() #21,488 rows
#pf_defects_by_manuf_date <- pf_dpv_for_fatal_defects(pf_defects, vehicle_info_mv, threshold = 10)
#by_location <- analyze_by_long_location(pf_defects)



