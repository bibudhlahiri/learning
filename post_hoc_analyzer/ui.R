library(shiny)
  
shinyUI(fluidPage(
  titlePanel("Toyota Fatal Paint Defect Analyzer"),  
      fluidRow(
        column(4, radioButtons("view", label = "View",
                  choices = list("Bar Plot" = 'bar_plot', "Box Plot" = 'box_plot'), selected = 'bar_plot')),
		column(2, radioButtons("paint_type", label = "Paint Type",
                  choices = list("Primer" = 'primer', "Top Coat" = 'top_coat'), selected = 'primer')),
	    column(6, numericInput("threshold", "Threshold on no. of defects (between 0 and 13):", 0, min = 0, max = 13))
	   ),
	   fluidRow(column(12, h3(""))),
	   fluidRow( 
         column(2, helpText("")),	   
         submitButton("Analyze with Plots")
       ),
	   fluidRow(column(12, h3(""))),
	   mainPanel(
         tabsetPanel(
           tabPanel("Summary", dataTableOutput("dis")),
           tabPanel("Historical", plotOutput(outputId = "plotgraph1")),
           tabPanel("Recent", plotOutput(outputId = "plotgraph2"))
         )
       )
	   #mainPanel(
         #fluidRow(
         # column(6, plotOutput(outputId = "plotgraph1", width = "100%")),  
         # column(6, plotOutput(outputId = "plotgraph1", width = "100%"))
        #)
    #)
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code_local")
#library(shiny)
#runApp("post_hoc_analyzer")