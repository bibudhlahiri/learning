library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Toyota Fatal Paint Defect Optimizer"),

  # Sidebar with a slider input for the number of bins
  
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
       column(1, numericInput("AH1_min_temp", label = "Min Temp", value = 55)),
	   column(1, numericInput("AH1_max_temp", label = "Max Temp", value = 75)),
	   column(1, helpText("BP2 Robot 1")),
	   column(1, numericInput("BP2_1_Robot_AM_min", label = "Min Motor Speed", value = 0)),
	   column(1, numericInput("BP2_1_Robot_AM_max", label = "Max Motor Speed", value = 30000))
       ),
	  fluidRow(
	   column(2, helpText("CBTH Air House 2")),
	   column(2, numericInput("AH2_min_hum", label = "Min Humidity", value = 55)),
	   column(2, numericInput("AH2_max_hum", label = "Max Humidity", value = 75)),
       column(1, numericInput("AH2_min_temp", label = "Min Temp", value = 55)),
	   column(1, numericInput("AH2_max_temp", label = "Max Temp", value = 75)),
	   column(1, helpText("BP2 Robot 2")),
	   column(1, numericInput("BP2_2_Robot_AM_min", label = "Min Motor Speed", value = 0)),
	   column(1, numericInput("BP2_2_Robot_AM_max", label = "Max Motor Speed", value = 30000))
       )

    # Show a plot of the generated distribution
    
  )
)