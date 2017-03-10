library(shiny)
  
shinyUI(fluidPage(
  titlePanel("Toyota Fatal Paint Defect Analyzer"),  
      fluidRow(
        column(4, radioButtons("view", label = "View",
                  choices = list("Bar Plot" = 'bar_plot', "Box Plot" = 'box_plot'), selected = 'bar_plot')),
		column(2, radioButtons("paint_type", label = "Paint Type",
                  choices = list("Primer" = 'primer', "Top Coat" = 'top_coat'), selected = 'primer')),
	    column(6, numericInput("threshold", "Threshold on #defects:", 0, min = 0, max = 13))
		#column(3, checkboxInput("load_historical", "Load Historical?", value = F))
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
           tabPanel("Visualization", plotOutput(outputId = "plotgraph", width = "800px")) 
		   #The above statement generates HTML code like this: <div id="plotgraph" class="shiny-plot-output" style="width: 100% ; height: 400px"></div>
         )
       )
  )
)

#setwd("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code_local")
#library(shiny)
#runApp("post_hoc_analyzer")