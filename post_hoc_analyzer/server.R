library(shiny)

el_yunque <- function(primer_defects_per_car, F = 5, T = 50)
{  
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
  opfile
}

generate_plots_from_dtree_output <- function(filename)
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
  df_features_cutpoints <- df_features_cutpoints[1:6, ] #Keep top 6 on dashboard
  
  apply(df_features_cutpoints, 1, function(row)viz_from_tree_output(as.character(row["feature"]), as.numeric(row["cutpoint"])))
  apply(df_features_cutpoints, 1, function(row)box_plots_for_AP_variables(as.character(row["feature"])))
}

generate_dtree_and_plot <- function(user_inputs)
{
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  #primer_defects_per_car$defect_report <- apply(primer_defects_per_car, 1, function(row)get_defect_report(as.numeric(row["tot_prim_defects"],
  #                                                                                                        as.numeric(user_inputs[["threshold"]]))))
  primer_defects_per_car$defect_report <- ifelse((primer_defects_per_car$tot_prim_defects <= user_inputs[["threshold"]]), "Acceptable", "Unacceptable")
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)
  opfile <- el_yunque(primer_defects_per_car, F = 5, T = 50)
  generate_plots_from_dtree_output(opfile)
}

shinyServer(function(input, output) {
    userInput <- reactive({
    list("view" = input$view, "threshold" = input$threshold) 
  })
  
  output$plotFromTreeOutput <- renderPlot({ 
      user_inputs <- userInput()
      generate_dtree_and_plot(user_inputs)          
     })
 }
)