#This analysis is done with ActivPlant data for fatal Top Coat defects only.
library(plyr)
library(dplyr)
library(ggplot2)
library(rpart)
library(reshape2)
library(party)
library(gridExtra)

filepath_prefix <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\"
#filepath_prefix <- "/home/impadmin/richa/"

#Analyze the variance per variables per process per car and take means, SD and CV.
#Do not analyze by hour. 
top_coat_analysis_per_car_process_variable <- function()
{
  filename <- paste(filepath_prefix, "NewTopCoatAP.csv", sep = "")
  vins <- read.csv(filename, header = T, stringsAsFactors = F) 
  vins <- vins %>% filter(substr(variablename, nchar(variablename) - 4, nchar(variablename)) != "Color")
  variables_by_vehicles <- vins %>% select(vin, eventname, variablename, value) %>% group_by(vin, eventname, variablename) %>% summarise(average = mean(value), std_dev = sd(value))
  #Almost all std_dev values are NA. Basically, all variables have one value each for each vehicle.
  variables_by_vehicles$coeff_of_var <- ifelse((variables_by_vehicles$average == 0), 0, 
                                               variables_by_vehicles$std_dev/variables_by_vehicles$average) 
  filename <- paste(filepath_prefix, "top_coat_variables_by_vehicles.csv", sep = "")
  write.table(variables_by_vehicles, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var <- variables_by_vehicles %>% ungroup() %>% select(eventname, variablename, coeff_of_var) %>% group_by(eventname, variablename) %>% summarise(median_CV = median(coeff_of_var))
  filename <- paste(filepath_prefix, "top_coat_median_CV_by_var.csv", sep = "")
  write.table(median_CV_by_var, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  #When run on whole data on cluster, Robot HV variables on Clear Processes 1 and 2 seem to have high variance
  median_CV_by_var
}

create_per_vehicle_averages <- function()
{
  #filename <- paste(filepath_prefix, "NewTopCoatAP.csv", sep = "")
  filename <- paste(filepath_prefix, "top_coat_sample.csv", sep = "")
  vins <- read.csv(filename, header = T, stringsAsFactors = F) 
  vins <- vins %>% filter(substr(variablename, nchar(variablename) - 4, nchar(variablename)) != "Color")
  vins$n_defects[(vins$n_defects == "NULL")] <- 0
  vins$n_defects <- as.numeric(vins$n_defects)
  
  #The number of defects are per vehicle and per discrepancy, not only per vehicle. Compute and save for joining later.
  n_defects_per_vehicle <- vins %>% select(vin, discrepancy, n_defects) %>% group_by(vin, discrepancy) %>% summarise(disc_wise_tc_defects = mean(n_defects)) %>% ungroup() %>% select(vin, disc_wise_tc_defects) %>% group_by(vin) %>% summarise(tot_tc_defects = sum(disc_wise_tc_defects))
  #Keep the manufacturing date for each vehicle, so that we can apply the tree model on a window of last one month's data and validate.   
  manuf_dates_of_vehicles <- vins %>% select(vin, recordtime) %>% mutate(recorddate = substr(recordtime, 1, 10)) %>% select(vin, recorddate) %>% group_by(vin) %>% summarise(manuf_date = max(recorddate))
  print(manuf_dates_of_vehicles)
  
  variables_by_vehicles <- vins %>% select(vin, eventname, variablename, value) %>% group_by(vin, eventname, variablename) %>% summarise(average = mean(value))
  variables_by_vehicles$short_eventname <- apply(variables_by_vehicles, 1, function(row)shorten_eventname(as.character(row["eventname"])))
  variables_by_vehicles$short_variablename <- apply(variables_by_vehicles, 1, function(row)shorten_variablename(as.character(row["variablename"])))
  variables_by_vehicles$complete_varname <- paste(variables_by_vehicles$short_eventname, variables_by_vehicles$short_variablename, sep = "_")  
  
  drops <- c("eventname", "variablename", "short_eventname", "short_variablename")
  variables_by_vehicles <- variables_by_vehicles[,!(names(variables_by_vehicles) %in% drops)]
  tc_defects_per_car <- dcast(variables_by_vehicles, vin ~ complete_varname, value.var = "average")
  
  #Specific to Top Coat: Drop all Robot HV variables for Clear Processes 1 and 2 as their median CVs are all high (0.31-0.45).
  drops <- c("CP1_1_Robot_HV", "CP1_2_Robot_HV", "CP1_3_Robot_HV", "CP1_4_Robot_HV",
			 "CP2_1_Robot_HV", "CP2_2_Robot_HV", "CP2_3_Robot_HV", "CP2_4_Robot_HV")
  tc_defects_per_car <- tc_defects_per_car[,!(names(tc_defects_per_car) %in% drops)]		 
  tc_defects_per_car <- merge(tc_defects_per_car, n_defects_per_vehicle)
  
  #Replace outlier values by medians of corresponding columns
  features <- colnames(tc_defects_per_car)
  features <- features[!(features %in% c("vin", "tot_tc_defects"))]
  tc_defects_per_car[features] <- lapply(tc_defects_per_car[features], outlier)
  
  #Add the manufacturing dates to the vehicles for validating the tree model
  tc_defects_per_car <- merge(tc_defects_per_car, manuf_dates_of_vehicles)
  
  filename <- paste(filepath_prefix, "tc_defects_per_car.csv", sep = "")
  write.table(tc_defects_per_car, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  #Result on new whole data (total 106557 unique vehicles, 15.24% have some top coat defect):
  #0        1     2     3     4     5     6     7     8     9    10    11     12  13    14    15    16    17    21
  #90314  7538  4508  2150  1014   439   226   130    90    51    35    26    16   8     5     1     3     2     1
  tc_defects_per_car
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

#TODO: Validate the performance of model built on historical data on recent data by classification error, precision and recall.
#Keep the trees which give best performance on recent data.
el_yunque <- function(F = 5, T = 1, threshold)
{
  library(DMwR)
  filename <- paste(filepath_prefix, "tc_defects_per_car.csv", sep = "")
  tc_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), "Acceptable", "Unacceptable")
  #tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), 0, 1)
  tc_defects_per_car$defect_report <- as.factor(tc_defects_per_car$defect_report)
  #For training, use data excluding last one month's 
  tc_defects_per_car$manuf_date <- as.character(tc_defects_per_car$manuf_date)
  historical_data <- subset(tc_defects_per_car, !((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))  
  recent_data <- subset(tc_defects_per_car, ((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\top_coat\\Tree_output_", timestr, ".txt", sep = "")
  
  #sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(historical_data)
	features <- features[!(features %in% c("tot_tc_defects", "defect_report", "vin", "manuf_date"))]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
	print(class(historical_data$defect_report))
	
	#historical_data has 18% from Unacceptable class for threshold = 0
    print(table(historical_data$defect_report))
    #Apply SMOTE on historical_data to oversample minority class and undersample majority class
    #historical_data <- SMOTE(as.formula(formula_str), historical_data, perc.over = 200, perc.under = 100)
    #print(table(historical_data$defect_report))
  
	#Creating decision stumps only as this will give insights easy to interpret
	#dtree <- ctree(as.formula(formula_str), data = historical_data, controls = ctree_control(maxdepth = 1))
	dtree <- SMOTE(as.formula(formula_str), data = historical_data[, c(sampled_features, "defect_report")], 
	                     perc.over = 200, perc.under = 100, learner = 'rpartXse', control = rpart.control(maxdepth = 2), se = 0.5)
	#preds <- predict(dtree, recent_data)
	#preds <- predict(dtree)
	#cont_tab <- table(recent_data$defect_report, preds)
	#cont_tab <- table(historical_data$defect_report, preds)
	
	#We treat Unacceptable as positive class since we want to detect defects
	#recall <- cont_tab[2,2]/sum(cont_tab[2,])
	#precision <- cont_tab[2,1]/sum(cont_tab[,2])	
	#cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	#print(cont_tab)
	#cat(paste("recall = ", recall, ", precision = ", precision, "\n", sep = ""))
	print(dtree)  
  }
  #sink()
  dtree
}

el_yunque_experimental <- function(F = 5, T = 1, threshold)
{
  library(DMwR)
  filename <- paste(filepath_prefix, "tc_defects_per_car.csv", sep = "")
  tc_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), "Acceptable", "Unacceptable")
  tc_defects_per_car$defect_report <- as.factor(tc_defects_per_car$defect_report)
  #For training, use data excluding last one month's 
  tc_defects_per_car$manuf_date <- as.character(tc_defects_per_car$manuf_date)
  historical_data <- subset(tc_defects_per_car, !((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))  
  recent_data <- subset(tc_defects_per_car, ((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\top_coat\\Tree_output_", timestr, ".txt", sep = "")
  
  #sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(historical_data)
	features <- features[!(features %in% c("tot_tc_defects", "defect_report", "vin", "manuf_date"))]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
	print(class(historical_data$defect_report))
	
	#historical_data has 18% from Unacceptable class for threshold = 0
    print(table(historical_data$defect_report))
    #Apply SMOTE on historical_data to oversample minority class and undersample majority class
	system.time(historical_data <- SMOTE(as.formula(formula_str), historical_data[, c(sampled_features, "defect_report")], 
	                                     perc.over = 200, perc.under = 100))
    print(table(historical_data$defect_report))
  
	#Creating decision stumps only as this will give insights easy to interpret
	dtree <- ctree(as.formula(formula_str), data = historical_data, controls = ctree_control(maxdepth = 1))
	preds <- predict(dtree, recent_data)
	#preds <- predict(dtree)
	cont_tab <- table(recent_data$defect_report, preds)
	#cont_tab <- table(historical_data$defect_report, preds)
	
	#We treat Unacceptable as positive class since we want to detect defects
	recall <- cont_tab[2,2]/sum(cont_tab[2,])
	precision <- cont_tab[2,2]/sum(cont_tab[,2])	
	cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	print(cont_tab)
	cat(paste("recall = ", recall, ", precision = ", precision, "\n", sep = ""))
	print(dtree)  
  }
  #sink()
  dtree
}

el_yunque_train_on_recent_data <- function(F = 5, T = 50, threshold)
{
  filename <- paste(filepath_prefix, "tc_defects_per_car.csv", sep = "")
  tc_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), "Acceptable", "Unacceptable")
  tc_defects_per_car$defect_report <- as.factor(tc_defects_per_car$defect_report)
  #For training, use data excluding last one month's 
  tc_defects_per_car$manuf_date <- as.character(tc_defects_per_car$manuf_date)
  recent_data <- subset(tc_defects_per_car, ((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\top_coat\\Tree_output_", timestr, ".txt", sep = "")
  
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(recent_data)
	features <- features[!(features %in% c("tot_tc_defects", "defect_report", "vin", "manuf_date"))]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
  
	#Creating decision stumps only as this will give insights easy to interpret
	dtree <- ctree(as.formula(formula_str), data = recent_data, controls = ctree_control(maxdepth = 1))
	cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	print(dtree)  
  }
  sink()
  dtree
}

el_yunque_original <- function(F = 5, T = 50, threshold)
{
  filename <- paste(filepath_prefix, "tc_defects_per_car.csv", sep = "")
  tc_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), "Acceptable", "Unacceptable")
  tc_defects_per_car$defect_report <- as.factor(tc_defects_per_car$defect_report)
  #For training, use data excluding last one month's 
  tc_defects_per_car$manuf_date <- as.character(tc_defects_per_car$manuf_date)
  historical_data <- subset(tc_defects_per_car, !((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))  
  recent_data <- subset(tc_defects_per_car, ((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\top_coat\\Tree_output_", timestr, ".txt", sep = "")
  
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(historical_data)
	features <- features[!(features %in% c("tot_tc_defects", "defect_report", "vin", "manuf_date"))]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
	
	#historical_data has 18% from Unacceptable class for threshold = 0
  
	#Creating decision stumps only as this will give insights easy to interpret
	dtree <- ctree(as.formula(formula_str), data = historical_data, controls = ctree_control(maxdepth = 1))
	cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	print(dtree)  
  }
  sink()
  dtree
}

bar_plots_for_AP_variables <- function(data_mode, input_data, feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5  
  input_data$tree_node <- ifelse((input_data[, feature] <= cutpoint), paste(" <= ", cutpoint, sep = ""),
                                             paste(" > ", cutpoint, sep = ""))
  pdpc_trans <- input_data %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = count/sum(count))
  print(feature)
  print(pdpc_trans)
  image_file <- 
    paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\top_coat\\",
          feature, "_", cutpoint, "_", data_mode, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc*100, fill = defect_report)) +
        geom_bar(stat = "identity", width = 0.7) + scale_fill_manual(values = cbbPalette) + 
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
  paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\top_coat\\barplot_grid_view_",
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

box_plots_for_AP_variables <- function(tc_defects_per_car, feature)
{  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\box_plots_ap_vars\\top_coat\\",
                      feature, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  
  #Add ranges to the title
  acceptable <- subset(tc_defects_per_car, (defect_report == "Acceptable"))
  unacceptable <- subset(tc_defects_per_car, (defect_report == "Unacceptable"))
  title_str <- paste(feature, ", Acc ", round(range(acceptable[, feature])[1],1), "-", round(range(acceptable[, feature])[2],1),
                              ", Unacc ", round(range(unacceptable[, feature])[1],1), "-", round(range(unacceptable[, feature])[2],1),
							  "\n", sep = "")
  
  p <- ggplot(tc_defects_per_car, aes(factor(defect_report), get(feature))) + geom_boxplot() + 
       labs(x = "Defect report", y = feature) + ggtitle(title_str) + 
	   theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
	         axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
			 plot.title = element_text(lineheight = .8, face = "bold"))
  print(p)
  dev.off()
  p
}

boxplot_grid_view_from_tree_output <- function(tc_defects_per_car, df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(tc_defects_per_car, df_features_cutpoints[1, "feature"])))
  gp2 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(tc_defects_per_car, df_features_cutpoints[2, "feature"])))
  gp3 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(tc_defects_per_car, df_features_cutpoints[3, "feature"])))
  gp4 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(tc_defects_per_car, df_features_cutpoints[4, "feature"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
                            )
  grob <- grid.grab()

  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\box_plots_ap_vars\\top_coat\\boxplot_grid_view.png"
  png(image_file, width = 700, height = 700)
  grid.newpage()
  grid.draw(grob)
  dev.off()
  
  frame_grob
}

generate_plots_from_dtree_output <- function(threshold = 0, filename)
{
  lines <- readLines(filename) 
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
  
  #apply(df_features_cutpoints, 1, function(row)bar_plots_for_AP_variables(as.character(row["feature"]), as.numeric(row["cutpoint"])))
  #apply(df_features_cutpoints, 1, function(row)box_plots_for_AP_variables(as.character(row["feature"])))
  
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\tc_defects_per_car.csv"
  tc_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  tc_defects_per_car$defect_report <- ifelse((tc_defects_per_car$tot_tc_defects <= threshold), "Acceptable", "Unacceptable")
  tc_defects_per_car$defect_report <- as.factor(tc_defects_per_car$defect_report)
  
  tc_defects_per_car$manuf_date <- as.character(tc_defects_per_car$manuf_date)
  #76% of the data is historical
  historical_data <- subset(tc_defects_per_car, !((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  frame_grob <- barplot_grid_view_from_tree_output(data_mode = "historical", historical_data, df_features_cutpoints)
  #frame_grob <- boxplot_grid_view_from_tree_output(tc_defects_per_car, df_features_cutpoints)
  
  #Validate the tree models on last one month's data using the bar plot for now
  recent_data <- subset(tc_defects_per_car, ((manuf_date >= "2017-01-03") & (manuf_date <= "2017-02-08")))
  frame_grob <- barplot_grid_view_from_tree_output(data_mode = "recent", recent_data, df_features_cutpoints)
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_top_coat_defects_per_car.R")
#median_CV_by_var <- top_coat_analysis_per_car_process_variable()
#tc_defects_per_car <- create_per_vehicle_averages()
#dtree <- el_yunque_original(threshold = 0)
#Note the filename before running generate_plots_from_dtree_output()
frame_grob <- generate_plots_from_dtree_output(filename = "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_outputs\\top_coat\\Tree_output_2017_02_15_18_56_34.txt")