library(dplyr)
library(tidyr)

process_heatmap_data_for_powerBI <- function()
{
  filename <- 
  "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\HeatMap_final.csv"
  heatmap_data <- read.csv(filename, header = T, stringsAsFactors = F)
  columns <- colnames(heatmap_data)
  columns_df <- data.frame(column_name = columns[columns != "Range_xcoor"])
  columns_df <- columns_df %>% separate(column_name, c("X", "lower_bound", "upper_bound", "end"), "\\.")
  library(stringr)
  columns_df$lower_bound <- apply(columns_df, 1, function(row) str_pad(as.character(row["lower_bound"]), 5, pad = "0"))
  columns_df$upper_bound <- apply(columns_df, 1, function(row) str_pad(as.character(row["upper_bound"]), 5, pad = "0"))
  columns_df$y_bucket <- apply(columns_df, 1, function(row) paste("(", as.character(row["lower_bound"]), ",", 
                                                                   as.character(row["upper_bound"]), "]", sep = ""))
  #columns_df <- columns_df[, c("y_bucket")]
  
  rows_df <- data.frame(row_name = heatmap_data$Range_xcoor)
  rows_df <- rows_df %>% separate(row_name, c("lower_bound", "upper_bound"), ",")
  rows_df$lower_bound <- apply(rows_df, 1, function(row) substr(as.character(row["lower_bound"]), 2, nchar(as.character(row["lower_bound"]))))
  rows_df$lower_bound <- apply(rows_df, 1, function(row) str_pad(as.character(row["lower_bound"]), 5, pad = "0"))
  rows_df$upper_bound <- apply(rows_df, 1, function(row) substr(as.character(row["upper_bound"]), 1, nchar(as.character(row["upper_bound"])) - 1))
  rows_df$upper_bound <- apply(rows_df, 1, function(row) str_pad(as.character(row["upper_bound"]), 5, pad = "0"))
  rows_df$x_bucket <- apply(rows_df, 1, function(row) paste("(", as.character(row["lower_bound"]), ",", 
                                                                   as.character(row["upper_bound"]), "]", sep = ""))
  
  heatmap_data$Range_xcoor <- rows_df$x_bucket
  colnames(heatmap_data) <- c("Range_xcoor", columns_df$y_bucket)
  write.csv(heatmap_data, "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\documents\\ICS\\HeatMap_final_padded.csv", row.names = FALSE)
  heatmap_data
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\process_heatmap_data_for_powerBI.r")
heatmap_data <- process_heatmap_data_for_powerBI()