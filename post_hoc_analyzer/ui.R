library(shiny)
filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\rfImputed.csv"
rfImputed <- read.csv(filename, header = T, stringsAsFactors = F)
rfImputed <- round(rfImputed, 3)
  
shinyUI(fluidPage(
  titlePanel("Toyota Fatal Paint Defect Optimizer"),  
      fluidRow(
	   column(2, h3("")),
       column(4, h3("Humidity")),
       column(2, h3("Temperature")),
	   column(3, h3("Air Motor Speed"))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 1")),
	   column(2, numericInput("AH1_min_hum", label = "Min Humidity", value = min(rfImputed$CBTH_Air_House_1_Humidity))),
	   column(2, numericInput("AH1_max_hum", label = "Max Humidity", value = max(rfImputed$CBTH_Air_House_1_Humidity))),
       column(2, helpText("")),
	   column(1, helpText("BP2 Robot 1")),
	   column(1, numericInput("BP2_1_Robot_AM_min", label = "Min Speed", value = min(rfImputed$BP2_1_Robot_Air_Motor))),
	   column(1, numericInput("BP2_1_Robot_AM_max", label = "Max Speed", value = max(rfImputed$BP2_1_Robot_Air_Motor)))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 2")),
	   column(2, numericInput("AH2_min_hum", label = "Min Humidity", value = min(rfImputed$CBTH_Air_House_2_Humidity))),
	   column(2, numericInput("AH2_max_hum", label = "Max Humidity", value = max(rfImputed$CBTH_Air_House_2_Humidity))),
       column(1, numericInput("AH2_min_temp", label = "Min Temp", value = min(rfImputed$CBTH_Air_House_2_Temp))),
	   column(1, numericInput("AH2_max_temp", label = "Max Temp", value = max(rfImputed$CBTH_Air_House_2_Temp))),
	   column(1, helpText("CP1 Robot 4")),
	   column(1, numericInput("CP1_4_Robot_AM_min", label = "Min Speed", value = min(rfImputed$CP1_4_Robot_Air_Motor))),
	   column(1, numericInput("CP1_4_Robot_AM_max", label = "Max Speed", value = max(rfImputed$CP1_4_Robot_Air_Motor)))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 3")),
	   column(2, numericInput("AH3_min_hum", label = "Min Humidity", value = min(rfImputed$CBTH_Air_House_3_Humidity))),
	   column(2, numericInput("AH3_max_hum", label = "Max Humidity", value = max(rfImputed$CBTH_Air_House_3_Humidity))),
       column(2, helpText("")),
	   column(1, helpText("CP2 Robot 4")),
	   column(1, numericInput("CP2_4_Robot_AM_min", label = "Min Speed", value = min(rfImputed$CP2_4_Robot_Air_Motor))),
	   column(1, numericInput("CP2_4_Robot_AM_max", label = "Max Speed", value = max(rfImputed$CP2_4_Robot_Air_Motor)))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 4")),
	   column(2, numericInput("AH4_min_hum", label = "Min Humidity", value = min(rfImputed$CBTH_Air_House_4_Humidity))),
	   column(2, numericInput("AH4_max_hum", label = "Max Humidity", value = max(rfImputed$CBTH_Air_House_4_Humidity))),
       column(5, helpText(""))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 5")),
	   column(2, numericInput("AH5_min_hum", label = "Min Humidity", value = min(rfImputed$CBTH_Air_House_5_Humidity))),
	   column(2, numericInput("AH5_max_hum", label = "Max Humidity", value = max(rfImputed$CBTH_Air_House_5_Humidity))),
       column(1, numericInput("AH5_min_temp", label = "Min Temp", value = min(rfImputed$CBTH_Air_House_5_Temp))),
	   column(1, numericInput("AH5_max_temp", label = "Max Temp", value = max(rfImputed$CBTH_Air_House_5_Temp))),
	   column(3, helpText(""))
       ),
	   fluidRow( 
         column(1, helpText("")),	   
         submitButton("What's optimal?")
       ),
	   fluidRow(column(2, h3(""))),
	   mainPanel(
         textOutput("optimal_soln")
    )    
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code")
#library(shiny)
#runApp("toyota_optimizer")