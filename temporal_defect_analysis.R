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
  #For Lexus, GALC data should be filtered on attribute KATASHIKI with value GSV60L-CETGKA. But currently all values are that only.
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
  #On average, about 55% DPVs come from primer, 5% from base and 40% from other.
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
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DPV.csv"
  write.table(defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  defects_by_manuf_date[, c("n_total_defects", "n_primer_defects", "n_base_defects", "n_other_defects", "n_vehicles") := NULL]
  
  #fivenum(defects_by_manuf_date$DPV) 5.254902   54.954248   67.592593   77.905882 2157.000000
  #mean(defects_by_manuf_date$DPV) 85.10539
  #Leo found "average" as 79.42 (between our median and mean) using A0_CDATE in GALC and filtering DART for Lexus defects
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$primer_DPV) 0.9724779
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$base_DPV) 0.9686794
  #cor(defects_by_manuf_date$total_DPV, defects_by_manuf_date$other_DPV) 0.9066242
  #cor(defects_by_manuf_date$primer_DPV, defects_by_manuf_date$base_DPV) 0.9454616
  
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
  #fivenum(truncated_defect_data$total_DPV) 5.254902  54.701657  67.242519  77.269841 216.833333
  #mean(truncated_defect_data$total_DPV) 69.7438
  #fraction <- truncated_defect_data$primer_DPV/truncated_defect_data$total_DPV; median(fraction) 0.549580
  #fraction <- truncated_defect_data$base_DPV/truncated_defect_data$total_DPV; median(fraction) 0.05524215
  #fraction <- truncated_defect_data$other_DPV/truncated_defect_data$total_DPV; median(fraction) 0.3938221
  
  truncated_defect_data
}

#Dig deeper into primer colors for primer-related defects. White primer DPVs have definitely increased with time.
analyze_primer_dpv_by_primer_colors <- function(tblBodyDefects, galc)
{
  #Take primer defects from the set of all Lexus defects
  setkey(tblBodyDefects, PaintSystemID)
  primer_defects <- tblBodyDefects[(PaintSystemID == 7),] 
  
  #Get the primer color names
  setkey(primer_defects, ColorID)
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DART\\DART-Source-TMMK-ALL(9-paint-defect-tables)\\DART\\tblColors.txt"
  tblColors <- fread(filename, header = TRUE, sep = "\t", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "character", "character", "character"), data.table = TRUE)
  setkey(primer_defects, ColorID)
  primer_defects <- primer_defects[tblColors, nomatch = 0]
  
  setkey(primer_defects, manuf_date)
  primer_defects_by_manuf_date <- primer_defects[, list(n_primer_defects = length(BodyDefectID),
                                                 n_dark_teal = sum(as.numeric(Description == "Dark Teal")),
												 n_light_gray_primer = sum(as.numeric(Description == "Light Gray Primer")),
												 n_dark_gray_primer = sum(as.numeric(Description == "Dark Gray Primer")),
                                                 n_white_primer = sum(as.numeric(Description == "White Primer"))),											 
												 by = manuf_date] 

  #Get the number of Lexus vehicles manufactured each day to compute DPV  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]		
  primer_defects_by_manuf_date <- primer_defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]  
  
  primer_defects_by_manuf_date[, primer_DPV := n_primer_defects/n_vehicles] 
  primer_defects_by_manuf_date[, dark_teal_DPV := n_dark_teal/n_vehicles]
  primer_defects_by_manuf_date[, light_gray_primer_DPV := n_light_gray_primer/n_vehicles]
  primer_defects_by_manuf_date[, dark_gray_primer_DPV := n_dark_gray_primer/n_vehicles]
  primer_defects_by_manuf_date[, white_primer_DPV := n_white_primer/n_vehicles]
  
  #Since we are doing this for all data (not for spikes only), the following line would be required for removing outliers
  primer_defects_by_manuf_date <- primer_defects_by_manuf_date[(primer_DPV <= 250),]
  
  primer_defects_by_manuf_date[, c("n_primer_defects", "n_dark_teal", "n_light_gray_primer", "n_dark_gray_primer", "n_white_primer", "n_vehicles") := NULL]
  
  primer_defects_by_manuf_date_long <- melt(primer_defects_by_manuf_date, id = "manuf_date")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\primer_colors.png"
  png(image_file, width = 1200, height = 400)
  primer_defects_by_manuf_date_long[, manuf_date := as.Date(primer_defects_by_manuf_date_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(primer_defects_by_manuf_date_long, aes(x = manuf_date, y = value, colour=variable)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily primer DPVs by primer color")
  print(p)
  aux <- dev.off()
  
  primer_defects_by_manuf_date
}


#Dig deeper into primer colors for primer-related defects to see what happened ONLY on the days with SPIKES 
analyze_spikes_by_primer_colors <- function(tblBodyDefects, galc, percentile = 0.9)
{
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DPV.csv"
  defects_by_manuf_date <- fread(dpv_filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", 
					               "numeric", "numeric", "numeric", "numeric", "numeric"
								   ), data.table = TRUE)
  truncated_defect_data <- defects_by_manuf_date[(total_DPV <= 500),]
  #Let us define the threshold on primer DPV only as we see it in this plot itself.
  threshold <- as.numeric(quantile(truncated_defect_data$primer_DPV, percentile))
  spikes <- truncated_defect_data[(primer_DPV >= threshold),]
  sentence <- paste("percentile = ", percentile, ", threshold = ", threshold, ", no. of spike days = ", nrow(spikes), sep = "")
  cat(paste("With ", sentence, "\n", sep = ""))
  
  #Take primer defects from the set of all Lexus defects
  setkey(tblBodyDefects, PaintSystemID)
  primer_defects <- tblBodyDefects[(PaintSystemID == 7),] 
  
  #Take the primer defects only for the dates when spikes happened
  setkey(primer_defects, manuf_date)
  primer_defects <- primer_defects[(manuf_date %in% spikes$manuf_date),] 
  
  #Get the primer color names
  setkey(primer_defects, ColorID)
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DART\\DART-Source-TMMK-ALL(9-paint-defect-tables)\\DART\\tblColors.txt"
  tblColors <- fread(filename, header = TRUE, sep = "\t", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("numeric", "character", "character", "character"), data.table = TRUE)
  setkey(primer_defects, ColorID)
  primer_defects <- primer_defects[tblColors, nomatch = 0]
  
  setkey(primer_defects, manuf_date)
  primer_defects_by_manuf_date <- primer_defects[, list(n_primer_defects = length(BodyDefectID),
                                                 n_dark_teal = sum(as.numeric(Description == "Dark Teal")),
												 n_light_gray_primer = sum(as.numeric(Description == "Light Gray Primer")),
												 n_dark_gray_primer = sum(as.numeric(Description == "Dark Gray Primer")),
                                                 n_white_primer = sum(as.numeric(Description == "White Primer"))),											 
												 by = manuf_date] 

  #Get the number of Lexus vehicles manufactured each day to compute DPV  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]		
  primer_defects_by_manuf_date <- primer_defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]  
  
  primer_defects_by_manuf_date[, primer_DPV := n_primer_defects/n_vehicles] 
  primer_defects_by_manuf_date[, dark_teal_DPV := n_dark_teal/n_vehicles]
  primer_defects_by_manuf_date[, light_gray_primer_DPV := n_light_gray_primer/n_vehicles]
  primer_defects_by_manuf_date[, dark_gray_primer_DPV := n_dark_gray_primer/n_vehicles]
  primer_defects_by_manuf_date[, white_primer_DPV := n_white_primer/n_vehicles]
  
  primer_defects_by_manuf_date[, c("n_primer_defects", "n_dark_teal", "n_light_gray_primer", "n_dark_gray_primer", "n_white_primer", "n_vehicles") := NULL]
  
  primer_defects_by_manuf_date_long <- melt(primer_defects_by_manuf_date, id = "manuf_date")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\primer_colors_on_spike_days.png"
  png(image_file, width = 1200, height = 400)
  primer_defects_by_manuf_date_long[, manuf_date := as.Date(primer_defects_by_manuf_date_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(primer_defects_by_manuf_date_long, aes(x = manuf_date, y = value, colour=variable)) + 
       geom_point() + geom_line() + ggtitle(sentence) + scale_x_date(date_labels = "%b-%Y") + 
	   xlab("Time") + ylab("Daily primer DPVs by primer color (on spike days)")
  print(p)
  aux <- dev.off()
  
  primer_defects_by_manuf_date
}

trend_in_white_primer <- function(tblBodyDefects, galc)
{
  primer_defects_by_manuf_date <- analyze_primer_dpv_by_primer_colors(tblBodyDefects, galc)
  
  #Do a time-series plot first
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\time_series_white_primer.png"
  png(image_file, width = 1200, height = 400)
  primer_defects_by_manuf_date[, manuf_date := as.Date(primer_defects_by_manuf_date$manuf_date, "%Y-%m-%d")]
  p <- ggplot(primer_defects_by_manuf_date, aes(x = manuf_date, y = white_primer_DPV)) + geom_line() + 
        scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily white primer DPVs")
  print(p)
  aux <- dev.off()
  
  #Do a linear interpolation plot
  primer_defects_by_manuf_date[, days_since_first_date := as.numeric(difftime(strptime(manuf_date, "%Y-%m-%d"), strptime("2015-11-11", "%Y-%m-%d"), units = "days"))]
  lm_white_primer <- lm(white_primer_DPV ~ days_since_first_date, data = primer_defects_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\linear_model_white_primer.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(primer_defects_by_manuf_date, aes(days_since_first_date, white_primer_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Days since first date") + ylab("White primer DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
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
#galc <- load_galc_data()
#truncated_defect_data <- dpv_from_dart_galc(tblBodyDefects, galc) 
#by_shift <- analyze_defects_by_time_of_day(tblBodyDefects, 8)
#primer_defects_by_manuf_date <- analyze_primer_dpv_by_primer_colors(tblBodyDefects, galc)
#primer_defects_by_manuf_date <- analyze_spikes_by_primer_colors(tblBodyDefects, galc, 0.7)
#trend_in_white_primer(tblBodyDefects, galc)
#defects_by_manuf_date <- daily_defects_from_dart(tblBodyDefects)
#primer_defects_by_manuf_date <- analyze_primer_defects_by_primer_colors(tblBodyDefects)
analyze_primer_defects_by_day_of_week(tblBodyDefects)







