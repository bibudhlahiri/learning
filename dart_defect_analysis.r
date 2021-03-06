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

daily_defects_from_dart <- function(tblBodyDefects)
{
  #Generate a daily plot first with number of defects
  setkey(tblBodyDefects, manuf_date)
  defects_by_manuf_date <- tblBodyDefects[, list(n_total_defects = length(BodyDefectID),
                                                 n_primer_defects = sum(as.numeric(PaintSystemID == 7)),
												 n_base_defects = sum(as.numeric(PaintSystemID == 8)),
												 n_other_defects = sum(as.numeric(!(PaintSystemID %in% c(7,8))))), 
												 by = manuf_date]  
  #On average, about 55% DPVs come from primer, 5% from base and 40% from other.
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\daily_defects.csv"
  write.table(defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  
  defect_data_long <- melt(defects_by_manuf_date, id = "manuf_date")
  print(defects_by_manuf_date)
  print(defect_data_long)
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\lexus_paint_defects.png"
  png(image_file, width = 1200, height = 400)
  defect_data_long[, manuf_date := as.Date(defect_data_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(defect_data_long, aes(x = manuf_date, y = value, colour=variable)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + 
       xlab("Time") + ylab("Daily Lexus paint defects by type of defect")
  print(p)
  aux <- dev.off()
  
  defects_by_manuf_date
}

#Dig deeper into primer colors for primer-related defects.
analyze_primer_defects_by_primer_colors <- function(tblBodyDefects)
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
  
  primer_defects_by_manuf_date_long <- melt(primer_defects_by_manuf_date, id = "manuf_date")
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\defects_by_primer_colors.png"
  png(image_file, width = 1200, height = 400)
  primer_defects_by_manuf_date_long[, manuf_date := as.Date(primer_defects_by_manuf_date_long$manuf_date, "%Y-%m-%d")]
  p <- ggplot(primer_defects_by_manuf_date_long, aes(x = manuf_date, y = value, colour=variable)) + geom_line() + 
       scale_x_date(date_labels = "%b-%Y") + xlab("Time") + ylab("Daily primer defects by primer color")
  print(p)
  aux <- dev.off()
  
  primer_defects_by_manuf_date
}

analyze_primer_defects_by_day_of_week <- function(tblBodyDefects)
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
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\primer_defects_by_day_of_week.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(primer_defects, aes(day_of_week)) + geom_bar() + facet_wrap(~Description) + 
       xlab("Day of Week") + ylab("Number of defects by primer color")
  print(p)
  aux <- dev.off()
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
  weather[, DefectTime := paste(date_captured, " ", substr(weather[['YR--MODAHRMN']], 9, 10), ":", 
										   substr(weather[['YR--MODAHRMN']], 11, 12), ":00", sep = "")]
  weather
}

plot_weather_by_date <- function()
{
  weather <- load_weather_data()
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


#input_data is a data.table with a column named DefectTime with date and time in the format 
#YYYY-MM-DD HH:MI:SS. It is returned with the shift values mapped as per rules of Toyota.
map_shifts <- function(input_data)
{
  #The plot should be based on a data.table with the following entries: (date, shift, avg_temp, num_defects).
  #First, get (date, shift, num_defects) from DART data, then, get (date, shift, avg_temp) from weather data.
  
  input_data[, defect_date := substr(input_data$DefectTime, 1, 10)]
  input_data[, defect_time := substr(input_data$DefectTime, 12, 19)]
  input_data[, defect_date_time := paste(input_data$defect_date, input_data$defect_time, sep = " ")]
  input_data[, DefectTime := NULL]
  input_data[, shift_A_start := paste(input_data$defect_date, "06:00:00", sep = " ")]
  input_data[, shift_A_end := paste(input_data$defect_date, "13:59:59", sep = " ")]
  input_data[, shift_B_start := paste(input_data$defect_date, "17:00:00", sep = " ")]
  #The shift B starting on a given day actually ends on the next day. The sequence of shifts in a given day would be
  #Overtime shift B, Shift A, Overtime Shift A and Shift B.
  input_data[, shift_B_end := paste(as.character(as.Date(input_data$defect_date) + 1), "00:59:59", sep = " ")]
  input_data[, prev_day_shift_B_start := paste(as.character(as.Date(input_data$defect_date) - 1), "17:00:00", sep = " ")]
  input_data[, prev_day_shift_B_end := paste(input_data$defect_date, "00:59:59", sep = " ")] #The end of the previous day's shift B falls today
  input_data[, overtime_A_start := paste(input_data$defect_date, "14:00:00", sep = " ")]
  input_data[, overtime_A_end := paste(input_data$defect_date, "16:59:59", sep = " ")]

  input_data[, shift := ifelse((input_data$defect_date_time >= input_data$shift_A_start & input_data$defect_date_time <= input_data$shift_A_end), "Shift_A",
                                 ifelse((input_data$defect_date_time >= input_data$shift_B_start & input_data$defect_date_time <= input_data$shift_B_end), "Shift_B", 
  							        ifelse((input_data$defect_date_time >= input_data$overtime_A_start & input_data$defect_date_time < input_data$overtime_A_end), "Overtime_A",
  										    ifelse((input_data$defect_date_time >= input_data$prev_day_shift_B_start & input_data$defect_date_time < input_data$prev_day_shift_B_end), "Shift_B", 
											"Overtime_B"))))]
											
  #Adding a shift_belongs_to_date as shift B (between 12:00 and 1 AM), can belong to a previous date 
  input_data[, shift_belongs_to_date := ifelse((input_data$defect_date_time >= input_data$prev_day_shift_B_start & input_data$defect_date_time < input_data$prev_day_shift_B_end), 
                                                as.character(as.Date(input_data$defect_date) - 1), input_data$defect_date)]
  input_data[ ,c("shift_A_start", "shift_A_end", "shift_B_start", "shift_B_end", 
                     "overtime_A_start", "overtime_A_end", "prev_day_shift_B_start", "prev_day_shift_B_end") := NULL]
  input_data
}

#Combine temperature and shift analysis. Take the average temperature for each shift each day,
#and get the number of defects in that shift, and plot three lines for shifts A, B and overtime, 
#with temperature on the X-axis and number of defects on the Y-axis. Shift A: 6 AM – 2 PM,
#Shift B: 5 PM – 1 AM, Overtime Shift A: 2 PM - 5 PM, Overtime shift B: 1 AM - 6 AM.
weather_and_shift_analysis <- function()
{
  weather <- load_weather_data()
  weather <- map_shifts(weather)
  #tblBodyDefects <- map_shifts(tblBodyDefects)
  
  #Get average temperature for each shift, each day. Use the shift_belongs_to_date.
  #In winter, the temperatures for 4 shifts are more overlapping, but in winter, the temperatures in 
  #shift B (5 PM – 1 AM) and overtime A (2 PM - 5 PM) are on average a few degrees higher than shift A (6 AM – 2 PM) 
  #and overtime B (1 AM - 6 AM), i.e., 2 PM - 1 AM (afternoon, evening and night) is warmer than 1 AM - 2 PM (late night through morning).
  #We have noticed that in summer, there is a significant dip in defects in Shift B but not so much in shift A, and 
  #also that once the temperature is past 65 F, the defects increase with increasing temperature. So the observations are 
  #not consistent at this point.
  setkey(weather, shift_belongs_to_date, shift)
  weather_by_shift <- weather[, list(avg_temp = mean(TEMP)), by = list(shift_belongs_to_date, shift)]
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\shift_vs_temperature.png"
  png(image_file, width = 1200, height = 400)
  weather_by_shift[, shift_belongs_to_date := as.Date(weather_by_shift$shift_belongs_to_date, "%Y-%m-%d")]
  p <- ggplot(weather_by_shift, aes(x = shift_belongs_to_date, y = avg_temp, colour = shift)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + 
       xlab("Time") + ylab("Daily temperature by shifts")
  print(p)
  aux <- dev.off()
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\dart_defect_analysis.R")
#tblBodyDefects <- load_dart_data()
#defects_by_manuf_date <- daily_defects_from_dart(tblBodyDefects)
#analyze_primer_defects_by_primer_colors(tblBodyDefects)
#analyze_primer_defects_by_day_of_week(tblBodyDefects)
#weather_by_date <- load_weather_data()
weather_and_shift_analysis()