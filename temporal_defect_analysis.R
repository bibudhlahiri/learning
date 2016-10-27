library(data.table)

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
  
  #Generate a daily plot first with number of defects
  tblBodyDefects[, manuf_date := substr(tblBodyDefects$BoothTime, 1, 10)]
  setkey(tblBodyDefects, manuf_date)
  defects_by_manuf_date <- tblBodyDefects[, list(n_defects = length(BodyDefectID)), by = manuf_date]
  
  #Get the number of Lexus vehicles manufactured each day to compute DPV
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\GALC\\GALC-DW-TMMK-Lexus-sinceNov2015\\Lexus_Data.csv"
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
								   ), data.table = TRUE)
  galc[, manuf_date := strftime(strptime(galc$LO_DATE, "%m-%d-%y"), "%Y-%m-%d")]
  
  setkey(galc, manuf_date)
  vehicles_by_manuf_date <- galc[, list(n_vehicles = length(VIN_NO)), by = manuf_date]
  
  #Combine DART and GALC data to compute DPV
  setkey(vehicles_by_manuf_date, manuf_date)
  setkey(defects_by_manuf_date, manuf_date)
  defects_by_manuf_date <- defects_by_manuf_date[vehicles_by_manuf_date, nomatch = 0]
  defects_by_manuf_date[, DPV := n_defects/n_vehicles]
  dpv_filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\DPV.csv"
  write.table(defects_by_manuf_date, dpv_filename, sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
  defects_by_manuf_date
}






