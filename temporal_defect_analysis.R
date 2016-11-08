library(data.table)
library(ggplot2)

load_dart_data <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DART\\DART-Source-TMMK-ALL(9-paint-defect-tables)\\DART\\tblBodyDefects.txt"
  tblBodyDefects <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric"), 
                    data.table = TRUE)
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DART\\DART-Source-TMMK-ALL(9-paint-defect-tables)\\DART\\tblBodyDefectParts.txt"
  tblBodyDefectParts <- fread(filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "numeric", "numeric", "numeric", "Date", "Date", "numeric", "character", "character"), 
                    data.table = TRUE)
			
  #ModelID 9 corresponds to Lexus			
  setkey(tblBodyDefectParts, ModelID)
  tblBodyDefectParts <- tblBodyDefectParts[(ModelID == 9),]
  
  #tblBodyDefectParts serves as a lookup table for tblBodyDefects, with lookup being done by BodyDefectPartID.
  #A value of PartIdentifier can be present multiple times in tblBodyDefectParts but a value of BodyDefectPartID is present only once.
  setkey(tblBodyDefects, BodyDefectPartID)
  setkey(tblBodyDefectParts, BodyDefectPartID)
  tblBodyDefects <- tblBodyDefects[tblBodyDefectParts, nomatch = 0]
  
  #Find the paint defects for Lexus body and bumper only
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DART\\DART-Source-TMMK-ALL(9-paint-defect-tables)\\DART\\tblPaintBooths.txt"
  tblPaintBooths <- fread(filename, header = TRUE, sep = "\t", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "numeric", "character"), data.table = TRUE)
  setkey(tblBodyDefects, PaintBoothID)
  setkey(tblPaintBooths, PaintBoothID)
  tblBodyDefects <- tblBodyDefects[tblPaintBooths, nomatch = 0]
  setkey(tblBodyDefects, PaintSystemID)
  tblBodyDefects <- tblBodyDefects[(PaintSystemID %in% 5:9),]
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
  galc
}

dpv_from_dart_galc <- function(tblBodyDefects, galc)
{
  #Generate a daily plot first with number of defects
  tblBodyDefects[, manuf_date := substr(tblBodyDefects$BoothTime, 1, 10)]
  setkey(tblBodyDefects, manuf_date)
  defects_by_manuf_date <- tblBodyDefects[, list(n_total_defects = length(BodyDefectID),
                                                 n_primer_defects = sum(as.numeric(PaintSystemID == 7)),
												 n_base_defects = sum(as.numeric(PaintSystemID == 8)),
												 n_other_defects = sum(as.numeric(!(PaintSystemID %in% c(7,8))))), 
												 by = manuf_date]  
  #On average, about 55% DPVs come from primer, 40% from base and 5% from other.
  #Get the number of Lexus vehicles manufactured each day to compute DPV  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]
  
  #Combine DART and GALC data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(defects_by_manuf_date, manuf_date)
  defects_by_manuf_date <- defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  defects_by_manuf_date[, total_DPV := n_total_defects/n_vehicles] 
  defects_by_manuf_date[, primer_DPV := n_primer_defects/n_vehicles] 
  defects_by_manuf_date[, base_DPV := n_base_defects/n_vehicles]
  defects_by_manuf_date[, other_DPV := n_other_defects/n_vehicles]
  defects_by_manuf_date[, c("n_total_defects", "n_primer_defects", "n_base_defects", "n_other_defects", "n_vehicles") := NULL]
  
  #fivenum(defects_by_manuf_date$DPV) 5.254902   54.954248   67.592593   77.905882 2157.000000
  #mean(defects_by_manuf_date$DPV) 85.10539
  #Leo found "average" as 79.42 (between our median and mean) using A0_CDATE in GALC and filtering DART for Lexus defects
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$primer_DPV) 0.9724779
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$base_DPV) 0.9686794
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$other_DPV) 0.9066242
  #cor(defects_by_manuf_date$primer_DPV, defects_by_manuf_date$base_DPV) 0.9454616
  
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DPV.csv"
  write.table(defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  defect_data_long <- melt(defects_by_manuf_date, id = "manuf_date")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\DPV.png"
  png(image_file, width = 1200, height = 400)
  defect_data_long[, manuf_date := as.Date(defect_data_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(defect_data_long, aes(x = manuf_date, y = value, colour=variable)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily DPVs by type of defect")
  print(p)
  aux <- dev.off()
  
  #Correlations among different types of DPVs are same even after removing high spikes
  truncated_defect_data <- defects_by_manuf_date[(total_DPV <= 500),]
  truncated_data_long <- melt(truncated_defect_data, id = "manuf_date")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\DPV_truncated.png"
  png(image_file, width = 1200, height = 400)
  truncated_data_long[, manuf_date := as.Date(truncated_data_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(truncated_data_long, aes(x = manuf_date, y = value, colour=variable)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily DPVs by type of defect")
  print(p)
  aux <- dev.off()
  
  truncated_defect_data
}

analyze_defects_by_time_of_day <- function(tblBodyDefects, shift_length = 8)
{
  #difftime expects date-time objects
  tblBodyDefects[, seconds_from_start_of_day := as.numeric(difftime(strptime(DefectTime, "%Y-%m-%d %H:%M:%S"), 
                                                                    strptime(paste(substr(DefectTime, 1, 10), "00:00:00"), "%Y-%m-%d %H:%M:%S"), 
																	units = "secs"))]
  tblBodyDefects[, shift_number := seconds_from_start_of_day%/%(shift_length*60*60) + 1]
  
  setkey(tblBodyDefects, shift_number)
  by_shift <- tblBodyDefects[, list(n_defects = length(BodyDefectID)), by = shift_number] 
  by_shift <- by_shift[order(-n_defects)]
  by_shift[, shift_start_time := (by_shift$shift_number - 1)*shift_length]
  by_shift[, shift_end_time := by_shift$shift_start_time + shift_length]
  by_shift[, shift_time := paste(by_shift$shift_start_time, ":00-", by_shift$shift_end_time, ":00", sep = "")]
  by_shift$shift_time <- factor(by_shift$shift_time, levels = by_shift$shift_time, ordered = TRUE)
  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\defects_by_time_of_day_", 
                      "bucket_size_", shift_length, "_hrs.png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  p <- ggplot(by_shift, aes(x = factor(shift_time), y = n_defects)) + geom_bar(stat = "identity") + xlab("Time of day") + 
       ylab("Number of defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_shift
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\temporal_defect_analysis.R")
tblBodyDefects <- load_dart_data() #2496743 defects, matches exactly with powerpoint based on DART. 1,377,300 defects for PaintSystemID == 7 (primer),
#whereas ppt says 1,288,568. 130,400 defects for PaintSystemID == 8 (Base), matches with ppt; 989,043 defects for PaintSystemID == 6 (Body Paint Lexus).
galc <- load_galc_data()
truncated_defect_data <- dpv_from_dart_galc(tblBodyDefects, galc) 
#by_shift <- analyze_defects_by_time_of_day(tblBodyDefects, 8)





