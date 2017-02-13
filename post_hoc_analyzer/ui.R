library(shiny)
  
shinyUI(fluidPage(
  titlePanel("Toyota Fatal Paint Defect Analyzer"),  
      fluidRow(
        column(3, radioButtons("view", label = "View",
                  choices = list("Bar Plot" = 'bar_plot', "Box Plot" = 'box_plot'), selected = 'box_plot')),
	    column(3, numericInput("threshold", "Threshold on no. of defects (between 0 and 13):", 3, min = 0, max = 13))
	   ),
	   fluidRow(column(6, h3(""))),
	   fluidRow( 
         column(1, helpText("")),	   
         submitButton("Analyze with Plots")
       ),
	   mainPanel(
         plotOutput("plotFromTreeOutput")
    )
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code")
#library(shiny)
#runApp("post_hoc_analyzer")