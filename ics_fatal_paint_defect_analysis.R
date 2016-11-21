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
}

load_ics_fatal_paint_finish_data <- function(fatal_defects)
{
  setkey(fatal_defects, SECTION_NUM)
  pf_defects <- fatal_defects[(SECTION_NUM == "PF"),]
}

analyze_by_zone <- function(pf_defects)
{
  library(stringr)
  #pf_defects[, zone_position_in_portion := as.numeric(str_locate_all(pattern = "ZONE", pf_defects$PORTION)[[1]][,"start"]) - 2]
  #pf_defects[, zone := substr(pf_defects$PORTION, zone_position_in_portion, zone_position_in_portion)]
  
  #pf_defects[, zone_position_in_portion := which(strsplit(pf_defects$PORTION, " ")[[1]] == "ZONE") - 1]
  #pf_defects[, zone := (strsplit(pf_defects$PORTION, " ")[[1]])[zone_position_in_portion]]
  
  system.time(pf_defects[, zone := apply(pf_defects, 1, function(row) get_zone_from_portion(as.character(row["PORTION"])))])
  #http://stackoverflow.com/questions/18154556/r-split-text-string-in-a-data-table-columns
  #dt[, c('PX','PY') := do.call(what = Map, args = c(f = c, strsplit(PREFIX, '-')))] #what is a function that needs to be called,
  #args is the list of arguments for it
  #pf_defects[, zone_position_in_portion := do.call(Map, args = c(f = c, which(strsplit(PORTION, " ")[[1]] == "ZONE") - 1))]
  pf_defects[, .SD, .SDcols = c("PORTION", "zone")]
}

get_zone_from_portion <- function(portion)
{
  if (!grepl("ZONE", portion))
  {
    return(NA)
  }
  #cat(paste("portion = ", portion, "\n", sep = ""))
  portion_split <- strsplit(portion, " ")[[1]]
  zone_position_in_portion <- which(grepl("ZONE", portion_split)) - 1
  raw_zone <- portion_split[zone_position_in_portion]
  parenth_pos <- which(strsplit(raw_zone, "")[[1]] == "(")
  zone <- substr(raw_zone, parenth_pos + 1, parenth_pos + 1)
  #cat(paste("zone = ", zone, "\n", sep = ""))
  zone
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


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_paint_defect_analysis.R")
fatal_defects <- load_ics_fatal_defects_data() #209,880 rows
pf_defects <- load_ics_fatal_paint_finish_data(fatal_defects) #72,629 rows
pf_defects <- analyze_by_zone(pf_defects)



