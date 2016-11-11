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
  
  tblBodyDefects[, manuf_date := substr(tblBodyDefects$BoothTime, 1, 10)]
  
  #Drop the defect data corresponding to weekends as they are causing too many short-term spikes
  tblBodyDefects[, day_of_week := weekdays(as.Date(manuf_date, "%Y-%m-%d"))]
  setkey(tblBodyDefects, day_of_week)
  tblBodyDefects <- tblBodyDefects[(!(day_of_week %in% c("Saturday", "Sunday"))),]
  tblBodyDefects$day_of_week <- factor(tblBodyDefects$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"),
                                       ordered = TRUE)
									   
  tblBodyDefects
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

get_primer_dpvs <- function(tblBodyDefects, galc)
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
}

#Dig deeper into primer colors for primer-related defects. White primer DPVs have definitely increased with time.
analyze_primer_dpv_by_primer_colors <- function()
{
  primer_defects_by_manuf_date <- get_primer_dpvs(tblBodyDefects, galc)
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

load_weather_data <- function()
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
  weather[, transformed_humidity := (DEWP - TEMP)]
  weather[, date_captured := paste(substr(date_captured, 1, 4), "-", substr(date_captured, 5, 6), "-", 
                                           substr(date_captured, 7, 8), sep = "")]
  
  setkey(weather, date_captured)
  weather_by_date <- weather[, list(avg_temp = mean(TEMP),
                                    avg_transformed_humidity = mean(transformed_humidity)), by = date_captured]
									
  #Do a time-series plot of temperature and humidity									   
  weather_by_date_long <- melt(weather_by_date, id = "date_captured")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\weather_by_date.png"
  png(image_file, width = 1200, height = 400)
  weather_by_date_long[, date_captured := as.Date(weather_by_date_long$date_captured, "%Y-%m-%d")]
  p <- ggplot(weather_by_date_long, aes(x = date_captured, y = value, colour = variable)) + geom_line() + 
       scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily average temperature and (transformed) humidity")
  print(p)
  aux <- dev.off()
  
  weather_by_date
}

dart_dpvs_vs_weather <- function(tblBodyDefects)
{
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DPV.csv"
  dpv_by_manuf_date <- fread(dpv_filename, header = TRUE, sep = ",", stringsAsFactors = FALSE, showProgress = TRUE, 
                    colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", 
					               "numeric", "numeric", "numeric", "numeric", "numeric"
								   ), data.table = TRUE)
  dpv_by_manuf_date <- dpv_by_manuf_date[(total_DPV <= 500),]
												 
  weather_by_date <- load_weather_data()
										   
  setkey(dpv_by_manuf_date, manuf_date)
  setkey(weather_by_date, date_captured)
  dpv_by_manuf_date <- dpv_by_manuf_date[weather_by_date, nomatch = 0]
  
  temp_data_long <- melt(dpv_by_manuf_date[, .SD, .SDcols = c("avg_temp", "total_DPV", "primer_DPV", "base_DPV", "other_DPV")], 
                         id = "avg_temp")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\dpvs_vs_temp.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(temp_data_long, aes(x = avg_temp, y = value, colour = variable)) + geom_line() + 
       xlab("Temperature (in F)") + ylab("DPV for various paint types")
  print(p)
  aux <- dev.off()
  
  humidity_data_long <- melt(dpv_by_manuf_date[, .SD, .SDcols = c("avg_transformed_humidity", "total_DPV", "primer_DPV", "base_DPV", "other_DPV")], 
                         id = "avg_transformed_humidity")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\dpvs_vs_transformed_humidity.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(humidity_data_long, aes(x = avg_transformed_humidity, y = value, colour = variable)) + geom_line() + 
       xlab("Transformed humidity") + ylab("DPV for various paint types")
  print(p)
  aux <- dev.off()
  
  #Both total and primer DPV slightly decrease with increased temperature 
  #cor(dpv_by_manuf_date$avg_temp, dpv_by_manuf_date$total_DPV) -0.3850918
  #cor(dpv_by_manuf_date$avg_temp, dpv_by_manuf_date$primer_DPV) -0.4591692

  #Both total and primer DPV slightly increase with increased humidity
  #cor(dpv_by_manuf_date$avg_transformed_humidity, dpv_by_manuf_date$total_DPV) 0.08674465
  #cor(dpv_by_manuf_date$avg_transformed_humidity, dpv_by_manuf_date$primer_DPV) 0.05705113
  
  #Min temp = 8.933333, max temp = 83.5185, min total DPV = 5.254902, max total DPV = 125.5696, min primer DPV = 0, max primer DPV = 93.98
  #Do a linear interpolation plot between daily average temperature and total DPV
  #Linear model has intercept 88.8003 and slope -0.3687 (For each 2.7 degree F rise in temperature, Total DPV falls by 1)
  lm_total_temp <- lm(total_DPV ~ avg_temp, data = dpv_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\linear_model_total_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_temp, total_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Total DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  #Do a linear interpolation plot between daily average temperature and primer DPV
  #Linear model has intercept 58.1465 and slope -0.3564 (For each 2.8 degree F rise in temperature, Primer DPV falls by 1)
  lm_primer_temp <- lm(primer_DPV ~ avg_temp, data = dpv_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\linear_model_primer_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(dpv_by_manuf_date, aes(avg_temp, primer_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Primer DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()

  dpv_by_manuf_date
}

#How are the DPVs due to various primer colors varying with temperature?
primer_dpv_vs_weather <- function(tblBodyDefects, galc)
{
  primer_dpvs_by_manuf_date <- get_primer_dpvs(tblBodyDefects, galc)
  
  weather_by_date <- load_weather_data()
										   
  setkey(primer_dpvs_by_manuf_date, manuf_date)
  setkey(weather_by_date, date_captured)
  primer_dpvs_by_manuf_date <- primer_dpvs_by_manuf_date[weather_by_date, nomatch = 0]
  
  temp_data_long <- melt(primer_dpvs_by_manuf_date[, .SD, .SDcols = c("avg_temp", "primer_DPV", 
                                                                      "dark_gray_primer_DPV", "light_gray_primer_DPV",
																	  "white_primer_DPV", "dark_teal_DPV")], 
                         id = "avg_temp")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\primer_dpvs_vs_temp.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(temp_data_long, aes(x = avg_temp, y = value, colour = variable)) + geom_line() + 
       xlab("Temperature (in F)") + ylab("DPV for various paint types")
  print(p)
  aux <- dev.off()
  
  #Do a linear interpolation plot between daily average temperature and dark gray primer DPV
  #Linear model has intercept 37.9508 and slope -0.2699 (For each 3.7 degree F rise in temperature, dark gray DPV falls by 1)
  lm_primer_temp <- lm(dark_gray_primer_DPV ~ avg_temp, data = primer_dpvs_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\linear_model_dark_gray_primer_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(primer_dpvs_by_manuf_date, aes(avg_temp, dark_gray_primer_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Dark Gray Primer DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  #Do a linear interpolation plot between daily average temperature and light gray primer DPV
  #Linear model has intercept 21.371 and slope -0.163 (For each 6.13 degree F rise in temperature, light gray primer DPV falls by 1)
  lm_primer_temp <- lm(light_gray_primer_DPV ~ avg_temp, data = primer_dpvs_by_manuf_date)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\linear_model_light_gray_primer_DPV_temp.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(primer_dpvs_by_manuf_date, aes(avg_temp, light_gray_primer_DPV)) + geom_point() + geom_smooth(method = "lm") + 
       xlab("Average daily temperature") + ylab("Light Gray Primer DPVs") + 
       theme(axis.text = element_text(colour = 'blue', size = 20, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 20, face = 'bold'))
  print(p)
  dev.off()
  
  primer_dpvs_by_manuf_date
} 

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\temporal_defect_analysis.R")
tblBodyDefects <- load_dart_data() #2496743 defects, matches exactly with powerpoint based on DART. 1,377,300 defects for PaintSystemID == 7 (primer),
#whereas ppt says 1,288,568. 130,400 defects for PaintSystemID == 8 (Base), matches with ppt; 989,043 defects for PaintSystemID == 6 (Body Paint Lexus).
galc <- load_galc_data()
#truncated_defect_data <- dpv_from_dart_galc(tblBodyDefects, galc) 
#by_shift <- analyze_defects_by_time_of_day(tblBodyDefects, 8)
#primer_defects_by_manuf_date <- analyze_primer_dpv_by_primer_colors(tblBodyDefects, galc)
#primer_defects_by_manuf_date <- analyze_spikes_by_primer_colors(tblBodyDefects, galc, 0.7)
#trend_in_white_primer(tblBodyDefects, galc)
#defects_by_manuf_date <- daily_defects_from_dart(tblBodyDefects)
#primer_defects_by_manuf_date <- analyze_primer_defects_by_primer_colors(tblBodyDefects)
#analyze_primer_defects_by_day_of_week(tblBodyDefects)
#dpv_by_manuf_date <- dart_dpvs_vs_weather(tblBodyDefects)
primer_dpvs_by_manuf_date <- primer_dpv_vs_weather(tblBodyDefects, galc)







