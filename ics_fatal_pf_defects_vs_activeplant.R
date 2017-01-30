#This analysis is the one that started after we got the feedback from Tara on 29/11/2016 (about REPAIR_TERMINAL , 2-level grid etc).
library(plyr)
library(dplyr)
library(ggplot2)
library(rpart)

load_ics_activeplant_data <- function()
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\ICSDataReplacedNA.csv"
  ics_activeplant <- read.csv(filename, header = T, stringsAsFactors = F)
}


compute_dpvs <- function(ics_activeplant)
{
  ics_activeplant$record_hour <- substr(ics_activeplant$RecordTime, 1, 13)
  dpv_by_hour <- ics_activeplant %>% group_by(record_hour) %>% summarise(dpv = sum(tdefect)/length(RecordTime),
                                                                         mean_BP1_1_Robot_Air_Motor = mean(BP1_1_Robot_Air_Motor),
                                                                         mean_BP2_1_Robot_Air_Motor = mean(BP2_1_Robot_Air_Motor),
																		 mean_CP1_1_Robot_Air_Motor = mean(CP1_1_Robot_Air_Motor),
																		 mean_CP1_3_Robot_HV = mean(CP1_3_Robot_HV),
																		 mean_CP2_1_Robot_Air_Motor = mean(CP2_1_Robot_Air_Motor),
																		 mean_CBTH_Air_House_1_Humidity = mean(CBTH_Air_House_1_Humidity),
																		 mean_CBTH_Air_House_2_Humidity = mean(CBTH_Air_House_2_Humidity),
																		 mean_CBTH_Air_House_3_Humidity = mean(CBTH_Air_House_3_Humidity),
																		 mean_CBTH_Air_House_5_Humidity = mean(CBTH_Air_House_5_Humidity),
																		 mean_CBTH_Air_House_2_Temp = mean(CBTH_Air_House_2_Temp),
																		 mean_CBTH_Air_House_5_Temp = mean(CBTH_Air_House_5_Temp)
                                                                         )
}
  
build_dtree <- function(dpv_by_hour)
{
  features <- colnames(dpv_by_hour)
  features <- features[!(features %in% c("record_hour", "dpv"))]
  formula_str <- paste("dpv ~ ", paste(features, collapse = " + "), sep = "")
  dtree <- rpart(as.formula(formula_str), data = dpv_by_hour)
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code\\ics_fatal_pf_defects_vs_activeplant.R")
ics_activeplant <- load_ics_activeplant_data()
dpv_by_hour <- compute_dpvs(ics_activeplant)
dtree <- build_dtree(dpv_by_hour)


