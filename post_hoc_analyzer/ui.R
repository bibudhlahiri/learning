library(shiny)
  
shinyUI(fluidPage(
  titlePanel("Toyota Fatal Paint Defect Analyzer"),  
      fluidRow(
        column(2, radioButtons("view", label = "View",
                  choices = list("Bar Plot" = 'bar_plot', "Box Plot" = 'box_plot'), selected = 'bar_plot')),
		column(1, radioButtons("paint_type", label = "Paint Type",
                  choices = list("Primer" = 'primer', "Top Coat" = 'top_coat'), selected = 'primer')),
	    column(3, numericInput("threshold", "Threshold on no. of defects (between 0 and 13):", 0, min = 0, max = 13))
	   ),
	   fluidRow(column(6, h3(""))),
	   fluidRow( 
         column(1, helpText("")),	   
         submitButton("Analyze with Plots")
       ),
	   fluidRow(column(6, h3(""))),
	   mainPanel(
         plotOutput("plotFromTreeOutput", width = "70%")
    )
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code_local")
#library(shiny)
#runApp("post_hoc_analyzer")