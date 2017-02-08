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
  
  variables_by_vehicles <- vins %>% select(vin, n_defects, a.eventname, a.variablename, a.value) %>% group_by(vin, n_defects, a.eventname, a.variablename) %>% summarise(average = mean(a.value))
  #dataf <- unique(vins[, c("a.eventname", "a.variablename")])
  variables_by_vehicles$short_eventname <- apply(variables_by_vehicles, 1, function(row)shorten_eventname(as.character(row["a.eventname"])))
  variables_by_vehicles$short_variablename <- apply(variables_by_vehicles, 1, function(row)shorten_variablename(as.character(row["a.variablename"])))
  variables_by_vehicles$complete_varname <- paste(variables_by_vehicles$short_eventname, variables_by_vehicles$short_variablename, sep = "_")
  drops <- c("a.eventname", "a.variablename", "short_eventname", "short_variablename")
  variables_by_vehicles <- variables_by_vehicles[,!(names(variables_by_vehicles) %in% drops)]
  data.wide <- dcast(variables_by_vehicles, vin + n_defects ~ complete_varname, value.var = "average")
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  write.table(data.wide, file = filename, sep = ",", row.names = FALSE, col.names = TRUE)
}

shorten_eventname <- function(eventname)
{
  if (eventname == "Base Process 1")
  {
    return("BP1")
  }
  if (eventname == "Base Process 2")
  {
    return("BP2")
  }
  if (eventname == "C Booth Temp_Humidity")
  {
    return("TH")
  }
  if (eventname == "Clear Process 1")
  {
    return("CP1")
  }
  if (eventname == "Clear Process 2")
  {
    return("CP2")
  }
}

shorten_variablename <- function(variablename)
{
  if (variablename == "1 Robot Air Motor")
  {
    return("1_Robot_AM")
  }
  if (variablename == "2 Robot Air Motor")
  {
    return("2_Robot_AM")
  }
  if (variablename == "3 Robot Air Motor")
  {
    return("3_Robot_AM")
  }
  if (variablename == "4 Robot Air Motor")
  {
    return("4_Robot_AM")
  }
  if (variablename == "1 Robot HV")
  {
    return("1_Robot_HV")
  }
  if (variablename == "2 Robot HV")
  {
    return("2_Robot_HV")
  }
  if (variablename == "3 Robot HV")
  {
    return("3_Robot_HV")
  }
  if (variablename == "4 Robot HV")
  {
    return("4_Robot_HV")
  }
  if (variablename == "Air House 1 Humidity")
  {
    return("AH_1_Hum")
  }
  if (variablename == "Air House 2 Humidity")
  {
    return("AH_2_Hum")
  }
  if (variablename == "Air House 3 Humidity")
  {
    return("AH_3_Hum")
  }
  if (variablename == "Air House 4 Humidity")
  {
    return("AH_4_Hum")
  }
  if (variablename == "Air House 5 Humidity")
  {
    return("AH_5_Hum")
  }
  if (variablename == "Air House 1 Temp")
  {
    return("AH_1_Temp")
  }
  if (variablename == "Air House 2 Temp")
  {
    return("AH_2_Temp")
  }
  if (variablename == "Air House 3 Temp")
  {
    return("AH_3_Temp")
  }
  if (variablename == "Air House 4 Temp")
  {
    return("AH_4_Temp")
  }
  if (variablename == "Air House 5 Temp")
  {
    return("AH_5_Temp")
  }
}

get_defect_report <- function(n_defects)
{
  if (n_defects <= 3)
  {
    return("Acceptable")
  }
  else return("Unacceptable")
}

el_yunque <- function(F = 5, T = 50)
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- apply(primer_defects_per_car, 1, function(row)get_defect_report(as.numeric(row["n_defects"])))
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
	features <- features[!(features %in% c("n_defects", "defect_report", "vin"))]
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
  primer_defects_per_car
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_per_car.R")
#median_CV_by_var <- primer_analysis_per_car_process_variable()
#variables_by_vehicles <- create_per_vehicle_averages()
primer_defects_per_car <- el_yunque()