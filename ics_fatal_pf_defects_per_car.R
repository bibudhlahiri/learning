#This analysis is the one that started after we got the feedback from Tara on 29/11/2016 (about REPAIR_TERMINAL , 2-level grid etc).
library(plyr)
library(dplyr)
library(ggplot2)
library(rpart)
library(reshape2)
library(party)

#Analyze the variance per variables per process per car and take means, SD and CV.
#Do not analyze by hour. 
primer_analysis_per_car_process_variable <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\LeftJ_PrimerGAP.csv"
  vins <- read.csv(filename, header = T, stringsAsFactors = F) #3,187,850 rows
  vins <- vins %>% filter(substr(a.variablename, nchar(a.variablename) - 4, nchar(a.variablename)) != "Color")
  variables_by_vehicles <- vins %>% select(vin, a.eventname, a.variablename, a.value) %>% group_by(vin, a.eventname, a.variablename) %>% summarise(average = mean(a.value), std_dev = sd(a.value))
  variables_by_vehicles$coeff_of_var <- ifelse((variables_by_vehicles$average == 0), 0, 
                                               variables_by_vehicles$std_dev/variables_by_vehicles$average) #200,544 rows
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\variables_by_vehicles.csv"
  write.table(variables_by_vehicles, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var <- variables_by_vehicles %>% ungroup() %>% select(a.eventname, a.variablename, coeff_of_var) %>% group_by(a.eventname, a.variablename) %>% summarise(median_CV = median(coeff_of_var))
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_median_CV_by_var.csv"
  write.table(median_CV_by_var, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
  median_CV_by_var
}

create_per_vehicle_averages <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\LeftJ_PrimerGAP.csv"
  vins <- read.csv(filename, header = T, stringsAsFactors = F) #3,187,850 rows
  vins <- vins %>% filter(substr(a.variablename, nchar(a.variablename) - 4, nchar(a.variablename)) != "Color")
  #Drop all Robot HV variables for Base Processes 1 and 2 as their median CVs are all 0.
  #vins <- vins %>% filter(!((substr(a.eventname, 1, nchar(a.eventname) - 2) == "Base Process")
  #                          && (substr(a.variablename, 3, nchar(a.variablename)) == "Robot HV")))
  
  #The number of defects are per vehicle and per discrepancy, not only per vehicle. Compute and save for joining later.
  n_defects_per_vehicle <- vins %>% select(vin, h.discrepancy, n_defects) %>% group_by(vin, h.discrepancy) %>% summarise(disc_wise_prim_defects = mean(n_defects)) %>% ungroup() %>% select(vin, disc_wise_prim_defects) %>% group_by(vin) %>% summarise(tot_prim_defects = sum(disc_wise_prim_defects))
    
  variables_by_vehicles <- vins %>% select(vin, a.eventname, a.variablename, a.value) %>% group_by(vin, a.eventname, a.variablename) %>% summarise(average = mean(a.value))
  variables_by_vehicles$short_eventname <- apply(variables_by_vehicles, 1, function(row)shorten_eventname(as.character(row["a.eventname"])))
  variables_by_vehicles$short_variablename <- apply(variables_by_vehicles, 1, function(row)shorten_variablename(as.character(row["a.variablename"])))
  variables_by_vehicles$complete_varname <- paste(variables_by_vehicles$short_eventname, variables_by_vehicles$short_variablename, sep = "_")
  drops <- c("a.eventname", "a.variablename", "short_eventname", "short_variablename")
  variables_by_vehicles <- variables_by_vehicles[,!(names(variables_by_vehicles) %in% drops)]
  primer_defects_per_car <- dcast(variables_by_vehicles, vin ~ complete_varname, value.var = "average")
  primer_defects_per_car <- merge(primer_defects_per_car, n_defects_per_vehicle)
  
  primer_defects_per_car$defect_report <- apply(primer_defects_per_car, 1, function(row)get_defect_report(as.numeric(row["tot_prim_defects"])))
  
  #Replace outlier values by medians of corresponding columns
  features <- colnames(primer_defects_per_car)
  features <- features[!(features %in% c("vin", "tot_prim_defects", "defect_report"))]
  primer_defects_per_car[features] <- lapply(primer_defects_per_car[features], outlier)
  
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  write.table(primer_defects_per_car, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
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

get_defect_report <- function(tot_prim_defects)
{
  if (tot_prim_defects <= 3)
  {
    return("Acceptable")
  }
  else return("Unacceptable")
}

el_yunque <- function(F = 5, T = 50)
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_output_", timestr, ".txt", sep = "")
  
  sink(file = opfile)
  for (i in 1:T)
  {
    features <- colnames(primer_defects_per_car)
	features <- features[!(features %in% c("tot_prim_defects", "defect_report", "vin"))]
	sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
    #dtree <- rpart(as.formula(formula_str), data = primer_defects_per_car)
	dtree <- ctree(as.formula(formula_str), data = primer_defects_per_car)
	#if (!is.null(dtree$splits))
	#{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	  #print_dtree(dtree)
	#}
  }
  sink()
  dtree
}

viz_from_tree_output <- function(feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)  
  primer_defects_per_car$tree_node <- ifelse((primer_defects_per_car[, feature] <= cutpoint), paste(feature, " <= ", cutpoint, sep = ""),
                                             paste(feature, " > ", cutpoint, sep = ""))
  pdpc_trans <- primer_defects_per_car %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = count/sum(count))
  print(pdpc_trans)
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\",
                      feature, "_", cutpoint, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc*100, fill = defect_report)) +
        geom_bar(stat = "identity", width = 0.7) + scale_fill_manual(values = cbbPalette) + 
        labs(x = "tree_node", y = "percent", fill = "defect_report") +
        theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
	          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'))
  print(p)
  dev.off()
}

#Convert the outliers of each column (outlier is any point that is less than the 25% quartile minus 1.5 times the IQR 
#OR more than the 75% quartile plus 1.5 times the IQR) into the median
outlier <- function(x) {
 x[((x < quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) | (x > quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE)) | (is.na(x)))] <- median(x, na.rm = TRUE)
 x
}

box_plots_for_AP_variables <- function(feature)
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\box_plots_ap_vars\\",
                      feature, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  p <- ggplot(primer_defects_per_car, aes(factor(defect_report), get(feature))) + geom_boxplot() + 
       labs(x = "Defect report", y = feature) + theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
	                                               axis.text.y = element_text(size = 12, color = 'black', face = 'bold'))
  print(p)
  dev.off()
}

generate_plots_from_dtree_output <- function(
    filename = "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\Tree_output_2017_02_09_11_54_24.txt")
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
  
  apply(df_features_cutpoints, 1, function(row)viz_from_tree_output(as.character(row["feature"]), as.numeric(row["cutpoint"])))
  apply(df_features_cutpoints, 1, function(row)box_plots_for_AP_variables(as.character(row["feature"])))
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_per_car.R")
#median_CV_by_var <- primer_analysis_per_car_process_variable()
#primer_defects_per_car <- create_per_vehicle_averages()
#dtree <- el_yunque()
#Based on Tree_output_2017_02_09_11_54_24.txt
#viz_from_tree_output("TH_AH_4_Temp", 73.5)
#viz_from_tree_output("TH_AH_2_Temp", 73.7)
#viz_from_tree_output("TH_AH_3_Temp", 72.1)
#viz_from_tree_output("TH_AH_2_Hum", 70.0729)
#viz_from_tree_output("TH_AH_5_Temp", 71.9)
#viz_from_tree_output("TH_AH_5_Hum", 67.8787)
#viz_from_tree_output("TH_AH_3_Hum", 69.1204)
#viz_from_tree_output("TH_AH_1_Hum", 69.8113)
#viz_from_tree_output("TH_AH_1_Temp", 72.4)

#box_plots_for_AP_variables("TH_AH_4_Temp")
#box_plots_for_AP_variables("TH_AH_2_Temp")
#box_plots_for_AP_variables("TH_AH_3_Temp")
#box_plots_for_AP_variables("TH_AH_2_Hum")
#box_plots_for_AP_variables("TH_AH_5_Temp")
#box_plots_for_AP_variables("TH_AH_5_Hum")
#box_plots_for_AP_variables("TH_AH_3_Hum")
#box_plots_for_AP_variables("TH_AH_1_Hum")
#box_plots_for_AP_variables("TH_AH_1_Temp")
generate_plots_from_dtree_output()