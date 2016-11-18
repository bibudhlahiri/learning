#This analysis is for all Lexus paint defects from ICS+.
library(data.table)
library(ggplot2)

load_ics_data <- function()
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
  #setkey(ics_history, as.Date(CREATION_TIME, "%d-%b-%y"))
  ics_history[(as.Date(CREATION_TIME, "%d-%b-%y") >= as.Date("2015-10-01")),]
}

analyze_by_portion <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, PORTION)
  by_portion <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = PORTION]
  setkey(by_portion, n_defects)
  by_portion <- by_portion[order(-n_defects)]
  total_defects <- nrow(lexus_paint_defects)
  by_portion[, percentage := 100*n_defects/total_defects]
  by_portion$PORTION <- factor(by_portion$PORTION, 
                              levels = by_portion$PORTION,
                              ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\lexus_paint_defects_by_portion.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_portion[1:10,], aes(x = factor(PORTION), y = n_defects)) + geom_bar(stat = "identity") + xlab("Portion") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_portion
}


#Items DOOR, DOOR OPENING and DOOR INNER SURFACE contribute to 50% of all paint defects
analyze_by_item <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, ITEM)
  by_item <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = ITEM]
  setkey(by_item, n_defects)
  by_item <- by_item[order(-n_defects)]
  total_defects <- nrow(lexus_paint_defects)
  by_item[, percentage := 100*n_defects/total_defects]
  by_item$ITEM <- factor(by_item$ITEM, 
                              levels = by_item$ITEM,
                              ordered = TRUE)
  print(by_item[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\lexus_paint_defects_by_item.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_item[1:10,], aes(x = factor(ITEM), y = n_defects)) + geom_bar(stat = "identity") + xlab("Item") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_item
}

#33% of paint defects are from Page = "096B EXTERIOR SURFACE"
analyze_by_page <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, PAGE)
  by_page <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = PAGE]
  setkey(by_page, n_defects)
  by_page <- by_page[order(-n_defects)]
  total_defects <- nrow(lexus_paint_defects)
  by_page[, percentage := 100*n_defects/total_defects]
  by_page$PAGE <- factor(by_page$PAGE, 
                              levels = by_page$PAGE,
                              ordered = TRUE)
  print(by_page[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\lexus_paint_defects_by_page.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_page[1:10,], aes(x = factor(PAGE), y = n_defects)) + geom_bar(stat = "identity") + xlab("Page") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_page
}

#26.66% of paint defects are from terminal = RH_TL_INPUT
analyze_by_terminal <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, TERMINAL)
  by_terminal <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = TERMINAL]
  setkey(by_terminal, n_defects)
  by_terminal <- by_terminal[order(-n_defects)]
  total_defects <- nrow(lexus_paint_defects)
  by_terminal[, percentage := 100*n_defects/total_defects]
  by_terminal$TERMINAL <- factor(by_terminal$TERMINAL, 
                              levels = by_terminal$TERMINAL,
                              ordered = TRUE)
  print(by_terminal[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\lexus_paint_defects_by_terminal.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_terminal[1:10,], aes(x = factor(TERMINAL), y = n_defects)) + geom_bar(stat = "identity") + xlab("Terminal") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_terminal
}

analyze_by_discrepancy <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, DISCREPANCY)
  by_discrepancy <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = DISCREPANCY]
  setkey(by_discrepancy, n_defects)
  by_discrepancy <- by_discrepancy[order(-n_defects)]
  total_defects <- nrow(lexus_paint_defects)
  by_discrepancy[, percentage := 100*n_defects/total_defects]
  by_discrepancy$DISCREPANCY <- factor(by_discrepancy$DISCREPANCY, 
                              levels = by_discrepancy$DISCREPANCY,
                              ordered = TRUE)
  print(by_discrepancy[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\lexus_paint_defects_by_discrepancy.png"
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

analyze_by_creation_date <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, CREATION_TIME)
  by_creation_time <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = CREATION_TIME] 
  by_creation_time[, manuf_date := as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")]
  print(fivenum(by_creation_time$n_defects)) #1 420 696 920 1977
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\defects_per_day.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("No. of paint defects") + theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
  
  #Sort by date before returning. Not necessary for the plot, though, as manuf_date is Date.
  by_creation_time <- by_creation_time[order(manuf_date)]
  by_creation_time
}

load_galc_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\GALC\\GALC-DW-TMMK-Lexus-sinceNov2015\\Lexus_Data.csv"
  #Each VEHICLE_ID appears only once in GALC. Same applies for each VIN_NO.
  galc <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("character", "numeric", "numeric", "numeric", "numeric", #A-E
					               "character", "character", "Date", "character", "character", #F-J
								   "numeric", "character", "character", "character", "numeric", #K-O
								   "numeric", "character", "Date", "Date", "Date", #P-T
								   "Date", "Date", "Date", "Date", "Date", #U-Y
								   "Date", "Date", "Date", "Date", "Date", #Z-AD
								   "Date", "Date", "Date", "Date", "Date", #AE-AI
								   "Date", "Date", "Date", "Date", "Date", #AJ-AN
								   "Date", "Date", "Date", "character" #AO-AR
								   ), data.table = TRUE) #38,545 rows
  galc[, manuf_date := strftime(strptime(galc$A0_CDATE, "%m-%d-%y"), "%Y-%m-%d")]
  #For Lexus, GALC data should be filtered on attribute KATASHIKI with value GSV60L-CETGKA. But currently all values are that only.
  galc
}


dpv_from_ics_galc <- function(lexus_paint_defects, galc, threshold = 50)
{
  setkey(lexus_paint_defects, CREATION_TIME)
  defects_by_manuf_date <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID),
                                                      n_pf_defects = sum(as.numeric(SECTION_NUM == "PF"))), by = CREATION_TIME] 
  defects_by_manuf_date[, manuf_date := strftime(strptime(defects_by_manuf_date$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]   
  
  
  #Get the number of Lexus vehicles manufactured each day to compute DPV  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]
  
  #Combine ICS and GALC data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(defects_by_manuf_date, manuf_date)
  defects_by_manuf_date <- defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  defects_by_manuf_date[, total_DPV := n_defects/n_vehicles] 
  defects_by_manuf_date[, pf_DPV := n_pf_defects/n_vehicles]
  
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\DPV.csv"
  write.table(defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  print(fivenum(defects_by_manuf_date$total_DPV)) #1.000000  3.941860 5.189655 11.641026 421.000000
  print(mean(defects_by_manuf_date$total_DPV)) #16.23373
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\DPV.png"
  png(image_file, width = 1200, height = 400)
  defects_by_manuf_date[, manuf_date := as.Date(defects_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(defects_by_manuf_date, aes(x = manuf_date, y = total_DPV)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily DPVs")
  print(p)
  aux <- dev.off()
  
  trunc_defects_by_manuf_date <- defects_by_manuf_date[(total_DPV <= threshold),]
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\trunc_DPV.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(trunc_defects_by_manuf_date, aes(x = manuf_date, y = total_DPV)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily DPVs (truncated plot)")
  print(p)
  aux <- dev.off()
   
  defects_by_manuf_date
}


analyze_by_creation_date_and_shift <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, CREATION_TIME, RESPONSIBLE_SHIFT)
  by_creation_time <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = list(CREATION_TIME, RESPONSIBLE_SHIFT)] 
  by_creation_time[, manuf_date := as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")]
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\defects_per_day_and_shift.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line(aes(colour = RESPONSIBLE_SHIFT)) + scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("No. of paint defects") + theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
  
  #Sort by date before returning. Not necessary for the plot, though, as manuf_date is Date.
  by_creation_time <- by_creation_time[order(manuf_date)]
  by_creation_time
}

analyze_by_day_of_week <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, day_of_week)
  by_day_of_week <- lexus_paint_defects[, list(n_defects = length(DEFECT_ID)), by = day_of_week]
  setkey(by_day_of_week, n_defects)
  by_day_of_week <- by_day_of_week[order(-n_defects)]
  by_day_of_week$day_of_week <- factor(by_day_of_week$day_of_week, 
							  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\defects_by_day_of_week.png"
  png(image_file,  width = 600, height = 480, units = "px")
  p <- ggplot(by_day_of_week, aes(x = factor(day_of_week), y = n_defects)) + geom_bar(stat = "identity") + xlab("Day of Week") + 
       ylab("Number of Lexus paint defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
  by_day_of_week
}

#Analyzing Paint Finish by date since it is the section contributing to most defects.
#Observation: Shift A is a clear winner, which was not visible when we took all defects.
#The shift difference was high initially and fell after if we consider the long-term trend,
#but there are short-term cycles still present.
analyze_paint_finish_by_creation_date_and_shift <- function(lexus_paint_defects)
{
  setkey(lexus_paint_defects, SECTION_NUM)
  pf_defects <- lexus_paint_defects[(SECTION_NUM == "PF"),]
  #In pf_defects, total 50710 defects from shift A (1.8 times shift B), 27843 defects from shift B. 64% from shift A, 36% from shift B.
  setkey(pf_defects, CREATION_TIME, RESPONSIBLE_SHIFT)
  by_creation_time <- pf_defects[, list(n_defects = length(DEFECT_ID)), by = list(CREATION_TIME, RESPONSIBLE_SHIFT)] 
  by_creation_time[, manuf_date := as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")]
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\pf_defects_per_day_and_shift.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line(aes(colour = RESPONSIBLE_SHIFT)) + scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("No. of paint defects") + theme(axis.text.x = element_text(angle = 90))
  print(p)
  aux <- dev.off()
  
  by_creation_time_wide <- dcast(by_creation_time, formula = CREATION_TIME + manuf_date ~ RESPONSIBLE_SHIFT, value.var = "n_defects")
  by_creation_time_wide[, shift_diff := A - B]
  #fivenum(by_creation_time_wide$A) 33.0 141.0 183.5 234.0 586.0
  #fivenum(by_creation_time_wide$B)  1.0  77.0 105.5 142.0 362.0
  #fivenum(by_creation_time_wide$shift_diff) -61.0  32.0  63.0 124.5 506.0
  by_creation_time_long <- melt(by_creation_time_wide[, .SD, .SDcols = c("manuf_date", "A", "B", "shift_diff")], 
                                id = "manuf_date")
								
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\pf_defects_with_shift_diff.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time_long, aes(x = manuf_date, y = value, colour = variable)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("Total defects by shift and their difference")
  print(p)
  aux <- dev.off()
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\pf_shift_diff_only.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time_wide, aes(x = manuf_date, y = shift_diff)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + 
       xlab("Date") + ylab("Difference between number of defects in two shifts")
  print(p)
  aux <- dev.off()
  
  by_creation_time_wide
}

#Check DPVs arising from Paint Finish as most defects come from there. Assumption is that
#all vehicles undergo paint finish, so we can use the same numbers as used for total DPV
#from GALC.
#Observation: Paint finish DPVs have been fairly consistent around 2 since mid-January
dpv_for_paint_finish <- function(lexus_paint_defects, galc, threshold = 20)
{
  setkey(lexus_paint_defects, SECTION_NUM)
  pf_defects <- lexus_paint_defects[(SECTION_NUM == "PF"),]
  
  setkey(pf_defects, CREATION_TIME)
  pf_defects_by_manuf_date <- pf_defects[, list(n_pf_defects = length(DEFECT_ID)), by = CREATION_TIME] 
  pf_defects_by_manuf_date[, manuf_date := strftime(strptime(pf_defects_by_manuf_date$CREATION_TIME, "%d-%b-%y"), "%Y-%m-%d")]   
  
  
  #Get the number of Lexus vehicles manufactured each day to compute DPV  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]
  
  #Combine ICS and GALC data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(pf_defects_by_manuf_date, manuf_date)
  pf_defects_by_manuf_date <- pf_defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  pf_defects_by_manuf_date[, pf_DPV := n_pf_defects/n_vehicles] 
  
  print(fivenum(pf_defects_by_manuf_date$pf_DPV)) #0.7085427  1.4371585  1.9239770  2.7229730 53.3846154
  print(mean(pf_defects_by_manuf_date$pf_DPV)) #3.143101
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\PF_DPV.png"
  png(image_file, width = 1200, height = 400)
  pf_defects_by_manuf_date[, manuf_date := as.Date(pf_defects_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(pf_defects_by_manuf_date, aes(x = manuf_date, y = pf_DPV)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs")
  print(p)
  aux <- dev.off()
  
  trunc_pf_defects_by_manuf_date <- pf_defects_by_manuf_date[(pf_DPV <= threshold),]
  cat(paste("Truncating ", 100*(1 - nrow(trunc_pf_defects_by_manuf_date)/nrow(pf_defects_by_manuf_date)), "% of points", "\n", sep = ""))
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\all_defects\\trunc_PF_DPV.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(trunc_pf_defects_by_manuf_date, aes(x = manuf_date, y = pf_DPV)) + geom_line() + 
       scale_x_date(date_breaks = "1 month", date_labels = "%d-%b-%Y") + xlab("Time") + ylab("Daily Paint Finish DPVs (truncated plot)")
  print(p)
  aux <- dev.off()
   
  pf_defects_by_manuf_date
}

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
  weather[, DefectTime := paste(date_captured, " ", substr(weather[['YR--MODAHRMN']], 9, 10), ":", 
										   substr(weather[['YR--MODAHRMN']], 11, 12), ":00", sep = "")]
  setkey(weather, date_captured)
  weather_by_date <- weather[, list(avg_temp = mean(TEMP),
                                    avg_humidity = mean(humidity)), by = date_captured]
}

ics_dpvs_vs_external_weather <- function()
{
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\DPV.csv"
  dpv_by_manuf_date <- fread(dpv_filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("Date", "numeric", "numeric", "Date", "numeric", "numeric", "numeric"), data.table = TRUE)
												 
  weather_by_date <- load_external_weather_data()
										   
  setkey(dpv_by_manuf_date, manuf_date)
  setkey(weather_by_date, date_captured)
  dpv_by_manuf_date <- dpv_by_manuf_date[weather_by_date, nomatch = 0]
  
  #Both total and primer DPV slightly decrease with increased temperature 
  #cor(dpv_by_manuf_date$avg_temp, dpv_by_manuf_date$total_DPV) -0.3868221

  #Both total and primer DPV slightly increase with increased humidity
  #cor(dpv_by_manuf_date$avg_humidity, dpv_by_manuf_date$total_DPV) 0.0902064
  
  #Do a linear interpolation plot between daily average temperature and total DPV
  #Linear model has intercept 12.06973 and slope -0.09833 (For each 11.11 degree F rise in temperature, Total DPV falls by 1)
  lm_total_temp <- lm(total_DPV ~ avg_temp, data = dpv_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\weather\\external\\loess_model_total_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_temp, total_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Total DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  #Do a linear interpolation plot between daily average humidity and total DPV
  #Linear model has intercept 7.17334 and slope 0.07396 (For each 13.5 degree F rise in humidity, Total DPV rises by 1)
  lm_total_humid <- lm(total_DPV ~ avg_humidity, data = dpv_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\weather\\external\\loess_model_total_DPV_humidity.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_humidity, total_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily humidity") + ylab("Total DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()

  dpv_by_manuf_date
}


paint_finish_dpvs_vs_external_weather <- function()
{
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\DPV.csv"
  dpv_by_manuf_date <- fread(dpv_filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("Date", "numeric", "numeric", "Date", "numeric", "numeric", "numeric"), data.table = TRUE)
												 
  weather_by_date <- load_external_weather_data()
										   
  setkey(dpv_by_manuf_date, manuf_date)
  setkey(weather_by_date, date_captured)
  dpv_by_manuf_date <- dpv_by_manuf_date[weather_by_date, nomatch = 0]
  
  #Both total and primer DPV slightly decrease with increased temperature 
  print(cor(dpv_by_manuf_date$avg_temp, dpv_by_manuf_date$pf_DPV)) #-0.3286385

  #Both total and primer DPV slightly increase with increased humidity
  print(cor(dpv_by_manuf_date$avg_humidity, dpv_by_manuf_date$pf_DPV)) #-0.0008161758
  
  #Do a linear interpolation plot between daily average temperature and PF DPV
  #Linear model has intercept 4.17621 and slope -0.03179 (For each 31.45 degree F rise in temperature, PF DPV falls by 1)
  lm_pf_temp <- lm(pf_DPV ~ avg_temp, data = dpv_by_manuf_date)
  print(lm_pf_temp)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\weather\\external\\loess_model_pf_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_temp, pf_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Total DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  #Do a linear interpolation plot between daily average humidity and PF DPV
  #Linear model has intercept 2.3493 and slope -0.0002547 (Negligible)
  lm_pf_humid <- lm(pf_DPV ~ avg_humidity, data = dpv_by_manuf_date)
  print(lm_pf_humid)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICS\\weather\\external\\loess_model_pf_DPV_humidity.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_humidity, pf_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily humidity") + ylab("Total DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()

  dpv_by_manuf_date
}

#The difference of paint finish defects between shifts A and B show sort of a quarterly seasonal (not cyclic) pattern.
time_series_analysis <- function(lexus_paint_defects)
{
  by_creation_time_wide <- analyze_paint_finish_by_creation_date_and_shift(lexus_paint_defects)
  shift_diff <- by_creation_time_wide$shift_diff
  shift_diff <- shift_diff[!is.na(shift_diff)]
  #Figure out the frequency here
  myts <- ts(x, start = c(2015,10,1), frequency = 365.25)
  fit <- stl(myts, s.window = "period")
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_all_paint_defect_analysis.R")
lexus_paint_defects <- load_ics_data() #215,821 rows
#galc <- load_galc_data()
#by_portion <- analyze_by_portion(lexus_paint_defects)
#by_item <- analyze_by_item(lexus_paint_defects)
#by_page <- analyze_by_page(lexus_paint_defects)
#by_terminal <- analyze_by_terminal(lexus_paint_defects)
#by_creation_time <- analyze_by_creation_date(lexus_paint_defects)
#by_discrepancy <- analyze_by_discrepancy(lexus_paint_defects)
#by_day_of_week <- analyze_by_day_of_week(lexus_paint_defects)
#by_creation_time <- analyze_by_creation_date_and_shift(lexus_paint_defects)
#defects_by_manuf_date <- dpv_from_ics_galc(lexus_paint_defects, galc, 40)
#by_creation_time_wide <- analyze_paint_finish_by_creation_date_and_shift(lexus_paint_defects)
pf_defects_by_manuf_date <- dpv_for_paint_finish(lexus_paint_defects, galc)
#dpv_by_manuf_date <- ics_dpvs_vs_external_weather()
#dpv_by_manuf_date <- paint_finish_dpvs_vs_external_weather()



