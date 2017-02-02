library(shiny)

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
	   column(2, numericInput("AH1_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH1_max_hum", label = "Max Humidity", value = 75)),
       column(2, helpText("")),
	   column(1, helpText("BP2 Robot 1")),
	   column(1, numericInput("BP2_1_Robot_AM_min", label = "Min Speed", value = 0)),
	   column(1, numericInput("BP2_1_Robot_AM_max", label = "Max Speed", value = 30000))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 2")),
	   column(2, numericInput("AH2_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH2_max_hum", label = "Max Humidity", value = 75)),
       column(1, numericInput("AH2_min_temp", label = "Min Temp", value = 55)),
	   column(1, numericInput("AH2_max_temp", label = "Max Temp", value = 75)),
	   column(1, helpText("CP1 Robot 4")),
	   column(1, numericInput("CP1_4_Robot_AM_min", label = "Min Speed", value = 0)),
	   column(1, numericInput("CP1_4_Robot_AM_max", label = "Max Speed", value = 30000))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 3")),
	   column(2, numericInput("AH3_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH3_max_hum", label = "Max Humidity", value = 75)),
       column(2, helpText("")),
	   column(1, helpText("CP2 Robot 4")),
	   column(1, numericInput("CP2_4_Robot_AM_min", label = "Min Speed", value = 0)),
	   column(1, numericInput("CP2_4_Robot_AM_max", label = "Max Speed", value = 30000))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 4")),
	   column(2, numericInput("AH4_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH4_max_hum", label = "Max Humidity", value = 75)),
       column(5, helpText(""))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 5")),
	   column(2, numericInput("AH5_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH5_max_hum", label = "Max Humidity", value = 75)),
       column(1, numericInput("AH5_min_temp", label = "Min Temp", value = 55)),
	   column(1, numericInput("AH5_max_temp", label = "Max Temp", value = 75)),
	   column(3, helpText(""))
       ),
	   fluidRow(  
         submitButton("What's optimal?")
       ),
	   mainPanel(
         textOutput("optimal_soln")
    )    
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code")
#library(shiny)
#runApp("toyota_optimizer")