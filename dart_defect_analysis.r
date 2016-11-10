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

daily_defects_from_dart <- function(tblBodyDefects)
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
  
  primer_defects[, manuf_date := substr(primer_defects$BoothTime, 1, 10)]
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
  
  primer_defects[, manuf_date := substr(primer_defects$BoothTime, 1, 10)]
  primer_defects[, day_of_week := weekdays(as.Date(primer_defects$manuf_date, "%Y-%m-%d"))]
  primer_defects$day_of_week <- factor(primer_defects$day_of_week, 
							           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                                       ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\DART\\primer_defects_by_day_of_week.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(primer_defects, aes(day_of_week)) + geom_bar() + facet_wrap(~Description) + 
       xlab("Day of Week") + ylab("Number of defects by primer color")
  print(p)
  aux <- dev.off()
}

tblBodyDefects <- load_dart_data()
analyze_primer_defects_by_day_of_week(tblBodyDefects)