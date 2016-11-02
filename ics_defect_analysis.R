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
  setkey(ics_history, DISCREPANCY)
  primer_defects <- ics_history[(grepl("PRIMER", DISCREPANCY)),] #6,930 rows
}

analyze_by_portion <- function(primer_defects)
{
  setkey(primer_defects, PORTION)
  by_portion <- primer_defects[, list(n_defects = length(DEFECT_ID)), by = PORTION]
  setkey(by_portion, n_defects)
  by_portion <- by_portion[order(-n_defects)]
  total_defects <- nrow(primer_defects)
  by_portion[, percentage := 100*n_defects/total_defects]
  by_portion$PORTION <- factor(by_portion$PORTION, 
                              levels = by_portion$PORTION,
                              ordered = TRUE)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\primer_defects_by_portion.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(by_portion[1:10,], aes(x = factor(PORTION), y = percentage)) + geom_bar(stat = "identity") + xlab("Portion") + 
       ylab("Percentage of defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_portion
}

analyze_by_item <- function(primer_defects)
{
  setkey(primer_defects, ITEM)
  by_item <- primer_defects[, list(n_defects = length(DEFECT_ID)), by = ITEM]
  setkey(by_item, n_defects)
  by_item <- by_item[order(-n_defects)]
  total_defects <- nrow(primer_defects)
  by_item[, percentage := 100*n_defects/total_defects]
  by_item$ITEM <- factor(by_item$ITEM, 
                              levels = by_item$ITEM,
                              ordered = TRUE)
  print(by_item[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\primer_defects_by_item.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(by_item[1:10,], aes(x = factor(ITEM), y = percentage)) + geom_bar(stat = "identity") + xlab("Item") + 
       ylab("Percentage of defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_item
}

analyze_by_page <- function(primer_defects)
{
  setkey(primer_defects, PAGE)
  by_page <- primer_defects[, list(n_defects = length(DEFECT_ID)), by = PAGE]
  setkey(by_page, n_defects)
  by_page <- by_page[order(-n_defects)]
  total_defects <- nrow(primer_defects)
  by_page[, percentage := 100*n_defects/total_defects]
  by_page$PAGE <- factor(by_page$PAGE, 
                              levels = by_page$PAGE,
                              ordered = TRUE)
  print(by_page[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\primer_defects_by_page.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(by_page[1:10,], aes(x = factor(PAGE), y = percentage)) + geom_bar(stat = "identity") + xlab("Page") + 
       ylab("Percentage of defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_page
}

analyze_by_terminal <- function(primer_defects)
{
  setkey(primer_defects, TERMINAL)
  by_terminal <- primer_defects[, list(n_defects = length(DEFECT_ID)), by = TERMINAL]
  setkey(by_terminal, n_defects)
  by_terminal <- by_terminal[order(-n_defects)]
  total_defects <- nrow(primer_defects)
  by_terminal[, percentage := 100*n_defects/total_defects]
  by_terminal$TERMINAL <- factor(by_terminal$TERMINAL, 
                              levels = by_terminal$TERMINAL,
                              ordered = TRUE)
  print(by_terminal[1:10,])
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\primer_defects_by_terminal.png"
  png(image_file,  width = 1200, height = 960, units = "px")
  p <- ggplot(by_terminal[1:10,], aes(x = factor(TERMINAL), y = percentage)) + geom_bar(stat = "identity") + xlab("Terminal") + 
       ylab("Percentage of defects") + 
       theme(axis.text = element_text(colour = 'blue', size = 14, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 14, face = 'bold')) + 
         theme(axis.text.x = element_text(angle = 90))
  print(p)
  dev.off()
  
  by_terminal
}

analyze_by_creation_date <- function(primer_defects)
{
  setkey(primer_defects, CREATION_TIME)
  by_creation_time <- primer_defects[, list(n_defects = length(DEFECT_ID)), by = CREATION_TIME] #334 records in by_creation_time
  by_creation_time[, manuf_date := as.Date(by_creation_time$CREATION_TIME, "%d-%b-%y")]
  #fivenum(by_creation_time$n_defects) 1  12  20  26 202
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\defects_per_day.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(by_creation_time, aes(manuf_date, n_defects)) + geom_line() + scale_x_date(date_labels = "%b-%Y") + xlab("Date") + ylab("No. of primer defects")
  print(p)
  aux <- dev.off()
  
  #Sort by date before returning. Not necessary for the plot, though, as manuf_date is Date.
  by_creation_time <- by_creation_time[order(manuf_date)]
  #by_creation_time[(n_defects >= 75),] Spikes on 3 days: 202 on 22-SEP-15, 78 on 01-OCT-15, 89 on 02-OCT-15
  by_creation_time
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ics_defect_analysis.R")
primer_defects <- load_ics_data()
by_portion <- analyze_by_portion(primer_defects)
#by_item <- analyze_by_item(primer_defects)
#by_page <- analyze_by_page(primer_defects)
#by_terminal <- analyze_by_terminal(primer_defects)
#by_creation_time <- analyze_by_creation_date(primer_defects)






