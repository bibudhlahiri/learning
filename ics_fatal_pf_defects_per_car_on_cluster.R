#This analysis is the one that we did on primer after the entire defect data became available on the cluster.
library(plyr)
library(dplyr)
library(ggplot2)
library(rpart)
library(reshape2)
library(party)
library(gridExtra)

filepath_prefix <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\"
#filepath_prefix <- "/data03/users/hyadav/LatestPaint/"

load_raw_input <- function()
{
  filename <- paste(filepath_prefix, "primer_trackingtime_ics_defect_galc_activeplant_1_sample.csv", sep = "")
  from_ics <- read.csv(filename, header = T, stringsAsFactors = F)
  filename <- paste(filepath_prefix, "primer_trackingtime_ics_defect_galc_activeplant_2_sample.csv", sep = "")
  from_galc <- read.csv(filename, header = T, stringsAsFactors = F)
  vins <- rbind(from_ics, from_galc)
}

#Analyze the variance per variables per process per car and take means, SD and CV.
#Do not analyze by hour. 
primer_analysis_per_car_process_variable <- function()
{
  vins <- load_raw_input() 
  vins <- vins %>% filter(substr(a.variablename, nchar(a.variablename) - 4, nchar(a.variablename)) != "Color")
  variables_by_vehicles <- vins %>% select(vin, a.eventname, a.variablename, a.value) %>% group_by(vin, a.eventname, a.variablename) %>% summarise(average = mean(a.value), std_dev = sd(a.value))
  variables_by_vehicles$coeff_of_var <- ifelse((variables_by_vehicles$average == 0), 0, 
                                               variables_by_vehicles$std_dev/variables_by_vehicles$average) 
  filename <- paste(filepath_prefix, "variables_by_vehicles.csv", sep = "")
  write.table(variables_by_vehicles, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var <- variables_by_vehicles %>% ungroup() %>% select(a.eventname, a.variablename, coeff_of_var) %>% group_by(a.eventname, a.variablename) %>% summarise(median_CV = median(coeff_of_var))
  filename <- paste(filepath_prefix, "primer_median_CV_by_var.csv", sep = "")
  write.table(median_CV_by_var, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var
}

create_per_vehicle_averages <- function()
{
  vins <- load_raw_input()
  vins <- vins %>% filter(substr(a.variablename, nchar(a.variablename) - 4, nchar(a.variablename)) != "Color")
  vins$n_defects[(vins$n_defects == "NULL")] <- 0
  vins$n_defects <- as.numeric(vins$n_defects)
  
  #The number of defects are per vehicle and per discrepancy, not only per vehicle. Compute and save for joining later.
  n_defects_per_vehicle <- vins %>% select(vin, h.discrepancy, n_defects) %>% group_by(vin, h.discrepancy) %>% summarise(disc_wise_prim_defects = mean(n_defects)) %>% ungroup() %>% select(vin, disc_wise_prim_defects) %>% group_by(vin) %>% summarise(tot_prim_defects = sum(disc_wise_prim_defects))
  #Keep the manufacturing date for each vehicle, so that we can apply the tree model on a window of last one month's data and validate.   
  manuf_dates_of_vehicles <- vins %>% select(vin, recordtime) %>% mutate(recorddate = substr(recordtime, 1, 10)) %>% select(vin, recorddate) %>% group_by(vin) %>% summarise(manuf_date = max(recorddate))
  
  variables_by_vehicles <- vins %>% select(vin, a.eventname, a.variablename, a.value) %>% group_by(vin, a.eventname, a.variablename) %>% summarise(average = mean(a.value))
  variables_by_vehicles$short_a.eventname <- apply(variables_by_vehicles, 1, function(row)shorten_eventname(as.character(row["a.eventname"])))
  variables_by_vehicles$short_a.variablename <- apply(variables_by_vehicles, 1, function(row)shorten_variablename(as.character(row["a.variablename"])))
  variables_by_vehicles$complete_varname <- paste(variables_by_vehicles$short_a.eventname, variables_by_vehicles$short_a.variablename, sep = "_")
  drops <- c("a.eventname", "a.variablename", "short_a.eventname", "short_a.variablename")
  variables_by_vehicles <- variables_by_vehicles[,!(names(variables_by_vehicles) %in% drops)]
  primer_defects_per_car <- dcast(variables_by_vehicles, vin ~ complete_varname, value.var = "average")
  
  #Specific to Primer: Drop some Robot HV variables for Clear Processes 1 and 2 as their median CVs are all high (0.31-0.45).
  drops <- c("CP1_1_Robot_HV", "CP1_2_Robot_HV", "CP2_1_Robot_HV", "CP2_2_Robot_HV", "CP2_3_Robot_HV")
  primer_defects_per_car <- primer_defects_per_car[,!(names(primer_defects_per_car) %in% drops)]
  
  primer_defects_per_car <- merge(primer_defects_per_car, n_defects_per_vehicle)
  
  #Replace outlier a.values by medians of corresponding columns
  features <- colnames(primer_defects_per_car)
  features <- features[!(features %in% c("vin", "tot_prim_defects", "defect_report"))]
  print(features)
  primer_defects_per_car[features] <- lapply(primer_defects_per_car[features], outlier)
  
  #Add the manufacturing dates to the vehicles for validating the tree model
  primer_defects_per_car <- merge(primer_defects_per_car, manuf_dates_of_vehicles)
  
  filename <- paste(filepath_prefix, "primer_defects_per_car.csv", sep = "")
  write.table(primer_defects_per_car, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  print(table(primer_defects_per_car$tot_prim_defects))
  #Result on new whole data (total 18139 unique vehicles, starting from 15 Sep 2016, 18% have some primer defect):
  #   0     1     2     3     4     5     6     7     8     9    10    11
  #13077  3005  1301   477   161    63    31     8     7     5     3     1
  primer_defects_per_car
}

shorten_eventname <- function(eventname)
{
  eventnames <- c("Base Process 1", "Base Process 2", "C Booth Temp_Humidity", "Clear Process 1", "Clear Process 2")
  short_eventnames <- c("BP1", "BP2", "TH", "CP1", "CP2")
  short_eventnames[which(eventnames == eventname)]
}

shorten_variablename <- function(variablename)
{
  variablenames <- c("1 Robot Air Motor", "2 Robot Air Motor", "3 Robot Air Motor", "4 Robot Air Motor", 
                     "1 Robot HV", "2 Robot HV", "3 Robot HV", "4 Robot HV",
                     "Air House 1 Humidity", "Air House 2 Humidity", "Air House 3 Humidity", "Air House 4 Humidity", "Air House 5 Humidity", 
                     "Air House 1 Temp", "Air House 2 Temp", "Air House 3 Temp", "Air House 4 Temp", "Air House 5 Temp")
  short_variablenames <- c("1_Robot_AM", "2_Robot_AM", "3_Robot_AM", "4_Robot_AM",
                           "1_Robot_HV", "2_Robot_HV", "3_Robot_HV", "4_Robot_HV",
                           "AH_1_Hum", "AH_2_Hum", "AH_3_Hum", "AH_4_Hum", "AH_5_Hum",
                           "AH_1_Temp", "AH_2_Temp", "AH_3_Temp", "AH_4_Temp", "AH_5_Temp")
  short_variablenames[which(variablenames == variablename)]
}

el_yunque <- function(input_data, F = 5, T = 50)
{
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\primer\\Tree_output_", timestr, ".txt", sep = "")
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(input_data)
    features <- features[!(features %in% c("tot_prim_defects", "defect_report", "vin", "manuf_date"))]
    cat(paste("length(features) = ", length(features), "\n", sep = ""))
    sampled_features <- features[sample(length(features), F)]
    formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
    
    #input_data has 18% from Unacceptable class for threshold = 0
    #Creating decision stumps only as this will give insights easy to interpret
    dtree <- ctree(as.formula(formula_str), data = input_data, controls = ctree_control(maxdepth = 1, savesplitstats = TRUE))
    cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
    print(dtree)  
  }
  #sink()
  closeAllConnections()
  opfile
}

get_pdpc <- function(input_data, feature, cutpoint)
{
  input_data$tree_node <- ifelse((input_data[, feature] <= cutpoint), paste(" <= ", cutpoint, sep = ""),
                                 paste(" > ", cutpoint, sep = ""))
  pdpc_trans <- input_data %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = round(100*count/sum(count), 2)) %>% ungroup() %>% group_by(tree_node) %>% mutate(csum = cumsum(perc)) %>% mutate(pos = csum - 0.5*perc)
  pdpc_trans
}

bar_plots_for_AP_variables <- function(data_mode, input_data, feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5  
  pdpc_trans <- get_pdpc(input_data, feature, cutpoint)
  print(feature)
  print(pdpc_trans)
  image_file <- 
    paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\primer\\",
          feature, "_", cutpoint, "_", data_mode, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc, fill = defect_report)) +
    geom_bar(stat = "identity", width = 0.7) + 
	#In ggplot_2.2.0, fill order is based on the order of the factor levels. 
	#The default order will plot the first level at the top of the stack instead of the bottom. 
	#If you want the first level at the bottom of the stack, use position_stack(reverse = TRUE).
	geom_col(position = position_stack(reverse = TRUE)) + 
	geom_text(aes(label = perc, y = pos), size = 4, colour = "red", fontface = "bold") + scale_fill_manual(values = cbbPalette) + 
    labs(x = "tree_node", y = "percent", fill = "defect_report") + ggtitle(feature) + 
    theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
          plot.title = element_text(lineheight = .8, face = "bold"))
  print(p)
  dev.off()
  p
}

barplot_grid_view_from_tree_output <- function(data_mode, input_data, df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(data_mode, input_data, df_features_cutpoints[1, "feature"], df_features_cutpoints[1, "cutpoint"])))
  gp2 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(data_mode, input_data, df_features_cutpoints[2, "feature"], df_features_cutpoints[2, "cutpoint"])))
  gp3 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(data_mode, input_data, df_features_cutpoints[3, "feature"], df_features_cutpoints[3, "cutpoint"])))
  gp4 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(data_mode, input_data, df_features_cutpoints[4, "feature"], df_features_cutpoints[4, "cutpoint"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
  )
  grob <- grid.grab()
  
  image_file <- 
    paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\primer\\barplot_grid_view_",
          data_mode, ".png", sep = "")
  png(image_file, width = 700, height = 700)
  grid.newpage()
  grid.draw(grob)
  dev.off()
  frame_grob
}

#Convert the outliers of each column (outlier is any point that is less than the 25% quartile minus 1.5 times the IQR 
#OR more than the 75% quartile plus 1.5 times the IQR) into the median
outlier <- function(x) {
  x[((x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)) | (is.na(x)))] <- median(x, na.rm = TRUE)
  x
}

box_plots_for_AP_variables <- function(primer_defects_per_car, feature)
{  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\box_plots_ap_vars\\primer\\",
                      feature, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  
  #Add ranges to the title
  acceptable <- subset(primer_defects_per_car, (defect_report == "Acceptable"))
  unacceptable <- subset(primer_defects_per_car, (defect_report == "Unacceptable"))
  title_str <- paste(feature, ", Acc ", round(range(acceptable[, feature])[1],1), "-", round(range(acceptable[, feature])[2],1),
                     ", Unacc ", round(range(unacceptable[, feature])[1],1), "-", round(range(unacceptable[, feature])[2],1),
                     "\n", sep = "")
  
  p <- ggplot(primer_defects_per_car, aes(factor(defect_report), get(feature))) + geom_boxplot() + 
    labs(x = "Defect report", y = feature) + ggtitle(title_str) + 
    theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
          plot.title = element_text(lineheight = .8, face = "bold"))
  print(p)
  dev.off()
  p
}

boxplot_grid_view_from_tree_output <- function(primer_defects_per_car, df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(primer_defects_per_car, df_features_cutpoints[1, "feature"])))
  gp2 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(primer_defects_per_car, df_features_cutpoints[2, "feature"])))
  gp3 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(primer_defects_per_car, df_features_cutpoints[3, "feature"])))
  gp4 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(primer_defects_per_car, df_features_cutpoints[4, "feature"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
  )
  grob <- grid.grab()
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\box_plots_ap_vars\\primer\\boxplot_grid_view.png"
  png(image_file, width = 700, height = 700)
  grid.newpage()
  grid.draw(grob)
  dev.off()
  
  frame_grob
}

parse_tree_output <- function(model_file)
{
  lines <- readLines(model_file) 
  n_lines <- length(lines)
  df_features_cutpoints <- data.frame(feature = character(), cutpoint = numeric(), statistic = numeric(0), stringsAsFactors = FALSE)
  
  row_number <- 1
  for (i in 1:n_lines)
  {
    line <- lines[i]
    if ((substr(line, 1, 3) == "1) ") && (grepl("criterion", line)))
    {
      tokens <- unlist(strsplit(substr(line, 4, nchar(line)), ";"))
      feature_cutpoint <- unlist(strsplit(tokens[1], "<="))
      df_features_cutpoints[row_number, "feature"] <- substr(feature_cutpoint[1], 1, nchar(feature_cutpoint[1]) - 1)
      df_features_cutpoints[row_number, "cutpoint"] <- substr(feature_cutpoint[2], 2, nchar(feature_cutpoint[2]))
      crit_stat <- unlist(strsplit(tokens[2], ","))
      stat_val <- unlist(strsplit(crit_stat[2], "="))
      df_features_cutpoints[row_number, "statistic"] <- as.numeric(substr(stat_val[2], 2, nchar(stat_val[2])))
      row_number <- row_number + 1
    }
  }
  df_features_cutpoints <- unique(df_features_cutpoints)
  df_features_cutpoints <- df_features_cutpoints[order(-df_features_cutpoints$statistic),] 
  df_features_cutpoints
} 

generate_plots_from_dtree_output <- function(first_day_of_shutdown = "2016-12-23", last_day_of_shutdown = "2017-01-02", 
                                             history_win_in_weeks = 5, window_end = "2017-02-01", threshold = 0)
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- ifelse((primer_defects_per_car$tot_prim_defects <= threshold), "Acceptable", "Unacceptable")
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  primer_defects_per_car$manuf_date <- as.character(primer_defects_per_car$manuf_date)
  
  start_date <- as.character(seq(as.Date(window_end), length = 2, by = paste("-", (7*history_win_in_weeks-1), " days", sep = ""))[2])
  #If the start_date computed initially falls on or before the shutdown, check how many days we are losing because of shutdown, and 
  #adjust those many days from days before shutdown. Shutdown was for 11 days, including both ends.
  revised_start_date <- start_date
  if (start_date <= last_day_of_shutdown)
  {
    lost_days <- as.numeric(difftime(as.Date(last_day_of_shutdown, '%Y-%m-%d'), as.Date(start_date, '%Y-%m-%d'), units = c("days"))) + 1
    last_day_before_shutdown <- as.character(seq(as.Date(first_day_of_shutdown), length = 2, by = "-1 days")[2])
    revised_start_date <-  as.character(seq(as.Date(last_day_before_shutdown), length = 2, by = paste("-", (lost_days - 1), " days", sep = ""))[2])
  }
  cat(paste("start_date = ", start_date, ", revised_start_date = ", revised_start_date, "\n", sep = ""))  
  historical_data <- subset(primer_defects_per_car, ((manuf_date >= revised_start_date) & (manuf_date <= window_end))) 
  #Eliminate data corresponding to shutdown period (2016-12-23 to 2017-01-02)
  historical_data <- subset(historical_data, !((manuf_date >= first_day_of_shutdown) & (manuf_date <= last_day_of_shutdown)))
  
  model_file <- el_yunque(historical_data, F = 5, T = 50)
  df_features_cutpoints <- parse_tree_output(model_file)
  
  #Writing to file is taken care of within barplot_grid_view_from_tree_output()
  frame_grob <- barplot_grid_view_from_tree_output(data_mode = "historical", historical_data, df_features_cutpoints)
  
  #Validate the tree models on last one week's data using the bar plot for now
  recent_data <- subset(primer_defects_per_car, ((manuf_date >= "2017-02-02") & (manuf_date <= "2017-02-08")))  #Does not depend on window size
  frame_grob <- barplot_grid_view_from_tree_output(data_mode = "recent", recent_data, df_features_cutpoints)
}

get_pdpc_historical_or_recent <- function(input_data, data_mode, how_many_from_pdpc, df_features_cutpoints)
{
  pdpc <- data.frame(feature = character(), tree_node = character(), defect_report = character(), 
                     count = numeric(0), perc = numeric(0), stringsAsFactors = FALSE)
  for (i in 1:how_many_from_pdpc)
  {
    pdpc_this_row <- as.data.frame(get_pdpc(input_data, df_features_cutpoints[i, "feature"], df_features_cutpoints[i, "cutpoint"]))
    pdpc_this_row$feature <- df_features_cutpoints[i, "feature"]
    pdpc <- rbind(pdpc, pdpc_this_row)
  }
  pdpc$data_mode <- data_mode
  pdpc
}

#Generate a score for a given window size.
score_window_size <- function(historical_data, recent_data, df_features_cutpoints, how_many_from_pdpc = 4)
{
  #Apply df_features_cutpoints on historical data to get the percentage of acceptable and unacceptable cars 
  #in the children of root nodes of the trees (we call it pdpc). Get the pdpc for historical window first.   
  pdpc_historical <- get_pdpc_historical_or_recent(historical_data, "historical", how_many_from_pdpc, df_features_cutpoints)  
  #Now, get the pdpc for the recent window.
  pdpc_recent <- get_pdpc_historical_or_recent(recent_data, "recent", how_many_from_pdpc, df_features_cutpoints)
  
  pdpc_all <- merge(x = pdpc_historical, y = pdpc_recent, by = c("feature", "tree_node", "defect_report"), all = TRUE)
  pdpc_all$perc.y[is.na(pdpc_all$perc.y)] <- 0
  pdpc_all$perc.x[is.na(pdpc_all$perc.x)] <- 0
  print(pdpc_all)
  manhattan_distance <- sum(abs(pdpc_all$perc.x - pdpc_all$perc.y))
  drift <- mean(abs(pdpc_all$perc.x - pdpc_all$perc.y)/pdpc_all$perc.x)
  ret_val <- list("manhattan_distance" = manhattan_distance, "drift" = drift)
}

#Compute utility score for a model, by checking how much the fraction of unacceptable cars changes when the 
#attributes cross cut-points identified by the decision trees.
compute_utility_score <- function(input_data, data_mode, df_features_cutpoints, how_many_from_pdpc = 4)
{
  pdpc <- get_pdpc_historical_or_recent(input_data, data_mode, how_many_from_pdpc, df_features_cutpoints)  
  unacc <- subset(pdpc, (defect_report == "Unacceptable"))
  unacc$which_side <- ifelse((substr(unacc$tree_node, 1, 2) == ' >'), "greater_than", "less_than")
  unacc <- unacc[, c("feature", "which_side", "perc")]
  unacc_wide <- dcast(unacc, feature ~ which_side, value.var = "perc")
  unacc_wide$greater_than[is.na(unacc_wide$greater_than)] <- 0
  unacc_wide$less_than[is.na(unacc_wide$less_than)] <- 0
  #utility <- sum(abs(unacc_wide$greater_than - unacc_wide$less_than))
  utility <- mean(abs(unacc_wide$greater_than - unacc_wide$less_than)/unacc_wide$less_than)
}

plot_win_len_and_score <- function()
{
  filename <- paste(filepath_prefix, "win_len_and_score.csv", sep = "")
  win_len_and_score <- read.csv(filename, header = T, stringsAsFactors = F) 
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\primer\\win_len_and_score.png"
  png(image_file, width = 1200, height = 400)
  p <- ggplot(win_len_and_score, aes(x = win_len, y = score)) + geom_line() + geom_point() + 
    scale_x_continuous(breaks = win_len_and_score$win_len, labels = as.character(win_len_and_score$win_len)) + 
    theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
          axis.text.y = element_text(size = 12, color = 'black', face = 'bold')) + 
    xlab("Window length for historical training data (in months)") + ylab("Manhattan distance between historical and recent data")
  print(p)
  aux <- dev.off()
}

#Take last one week for validation, and n number of weeks before that for training. Make sure to drop the
#shutdown period from training data. Last date for training data is given by window_end, which is 2017-02-01.
find_optimal_window_for_training_model_by_week <- function(first_day_of_shutdown = "2016-12-23", last_day_of_shutdown = "2017-01-02", 
                                                           window_end = "2017-02-01", threshold = 0)
{
  filename <- paste(filepath_prefix, "primer_defects_per_car.csv", sep = "")
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- ifelse((primer_defects_per_car$tot_prim_defects <= threshold), "Acceptable", "Unacceptable")
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  primer_defects_per_car$manuf_date <- as.character(primer_defects_per_car$manuf_date)
  #There were no primer defects after shutdown!!
  recent_data <- subset(primer_defects_per_car, ((manuf_date >= "2017-02-02") & (manuf_date <= "2017-02-08")))  #Does not depend on window size
  win_len_and_score <- data.frame(win_len = 4:12, generalization_score = rep(0,9), utility_score_on_historical = rep(0,9), 
                                  utility_score_on_recent = rep(0,9), stringsAsFactors = FALSE)
  
  for (i in 1:9)
  {
    start_date <- as.character(seq(as.Date(window_end), length = 2, by = paste("-", (7*win_len_and_score[i, "win_len"]-1), " days", sep = ""))[2])
    #If the start_date computed initially falls on or before the shutdown, check how many days we are losing because of shutdown, and 
    #adjust those many days from days before shutdown. Shutdown was for 11 days, including both ends.
    revised_start_date <- start_date
    if (start_date <= last_day_of_shutdown)
    {
      lost_days <- as.numeric(difftime(as.Date(last_day_of_shutdown, '%Y-%m-%d'), as.Date(start_date, '%Y-%m-%d'), units = c("days"))) + 1
      last_day_before_shutdown <- as.character(seq(as.Date(first_day_of_shutdown), length = 2, by = "-1 days")[2])
      revised_start_date <-  as.character(seq(as.Date(last_day_before_shutdown), length = 2, by = paste("-", (lost_days - 1), " days", sep = ""))[2])
    }
    cat(paste("i = ", i, ", win_len_and_score[i, win_len] = ", win_len_and_score[i, "win_len"], 
              ", start_date = ", start_date, ", revised_start_date = ", revised_start_date, "\n", sep = ""))  
    historical_data <- subset(primer_defects_per_car, ((manuf_date >= revised_start_date) & (manuf_date <= window_end))) 
    #Eliminate data corresponding to shutdown period (2016-12-23 to 2017-01-02)
    historical_data <- subset(historical_data, !((manuf_date >= first_day_of_shutdown) & (manuf_date <= last_day_of_shutdown)))
        
    #Build tree model on historical data
    model_file <- el_yunque(historical_data)
    #Get the variables used for splitting the root nodes, their cut-points and statistic values
    df_features_cutpoints <- parse_tree_output(model_file)
    hmfp <- min(4, length(unique(df_features_cutpoints$feature))) #Not getting even 4 features when threshold = 2
    
	scores <- score_window_size(historical_data, recent_data, df_features_cutpoints, how_many_from_pdpc = hmfp) 
	#win_len_and_score[i, "generalization_score"] <- scores[["manhattan_distance"]]
	win_len_and_score[i, "drift"] <- scores[["drift"]]
    win_len_and_score[i, "utility_score_on_historical"] <- compute_utility_score(historical_data, "historical", df_features_cutpoints, 
                                                                                 how_many_from_pdpc = hmfp)
    win_len_and_score[i, "utility_score_on_recent"] <- compute_utility_score(recent_data, "recent", df_features_cutpoints, how_many_from_pdpc = hmfp)
  }
  win_len_and_score$final_score <- win_len_and_score$utility_score_on_recent/win_len_and_score$generalization_score
  print(win_len_and_score)
  filename <- paste(filepath_prefix, "win_len_and_score.csv", sep = "")
  write.table(win_len_and_score, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  win_len_and_score
}

#Update on 2017-02-24: Until the issue about post-shutdown data is resolved, we take the last one week before shutdown as 
#the recent window and the weeks before that as historical window. 
find_optimal_window_for_training_model_by_week_interim <- function(window_end = "2016-12-15", threshold = 0)
{
  filename <- paste(filepath_prefix, "primer_defects_per_car.csv", sep = "")
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- ifelse((primer_defects_per_car$tot_prim_defects <= threshold), "Acceptable", "Unacceptable")
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  primer_defects_per_car$manuf_date <- as.character(primer_defects_per_car$manuf_date)
  #There were no primer defects after shutdown!!
  recent_data <- subset(primer_defects_per_car, ((manuf_date >= "2016-12-16") & (manuf_date <= "2016-12-22")))  #Does not depend on window size
  win_len_and_score <- data.frame(win_len = 8:16, generalization_score = rep(0,9), utility_score_on_historical = rep(0,9), 
                                  utility_score_on_recent = rep(0,9), stringsAsFactors = FALSE)
  
  for (i in 1:9)
  {
    start_date <- as.character(seq(as.Date(window_end), length = 2, by = paste("-", (7*win_len_and_score[i, "win_len"]-1), " days", sep = ""))[2])
    cat(paste("i = ", i, ", win_len_and_score[i, win_len] = ", win_len_and_score[i, "win_len"], 
              ", start_date = ", start_date, "\n", sep = ""))  
    historical_data <- subset(primer_defects_per_car, ((manuf_date >= start_date) & (manuf_date <= window_end))) 
    
    #Build tree model on historical data
    model_file <- el_yunque(historical_data)
    #Get the variables used for splitting the root nodes, their cut-points and statistic values
    df_features_cutpoints <- parse_tree_output(model_file)
    hmfp <- min(4, length(unique(df_features_cutpoints$feature))) #Not getting even 4 features when threshold = 2
    
    win_len_and_score[i, "generalization_score"] <- score_window_size(historical_data, recent_data, df_features_cutpoints, how_many_from_pdpc = hmfp)
    win_len_and_score[i, "utility_score_on_historical"] <- compute_utility_score(historical_data, "historical", df_features_cutpoints, 
                                                                                 how_many_from_pdpc = hmfp)
    win_len_and_score[i, "utility_score_on_recent"] <- compute_utility_score(recent_data, "recent", df_features_cutpoints, how_many_from_pdpc = hmfp)
  }
  win_len_and_score$final_score <- win_len_and_score$utility_score_on_recent/win_len_and_score$generalization_score
  print(win_len_and_score)
  filename <- paste(filepath_prefix, "win_len_and_score.csv", sep = "")
  write.table(win_len_and_score, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  win_len_and_score
}


#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code_local\\ics_fatal_pf_defects_per_car_on_cluster.R")
#median_CV_by_var <- primer_analysis_per_car_process_variable()
#primer_defects_per_car <- create_per_vehicle_averages()
#dtree <- el_yunque(threshold = 0)
#Note the filename before running generate_plots_from_dtree_output()
frame_grob <- generate_plots_from_dtree_output()
#At threshold = 2, the decision tree algorithm does not generate any real tree at all since 
#only 4% of the vehicles have more than 2 defects
#win_len_and_score <- find_optimal_window_for_training_model_by_month(threshold = 2)
#plot_win_len_and_score()
#utility <- compute_utility_score(history_win_in_months = 12, threshold = 0)
#check_weekly_windows(threshold = 0)
#win_len_and_score <- find_optimal_window_for_training_model_by_week(threshold = 0)