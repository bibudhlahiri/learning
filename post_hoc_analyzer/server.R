library(shiny)
library(party)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)

filepath_prefix <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\"

el_yunque <- function(defects_per_car, paint_type, F = 5, T = 50)
{  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  opfile <- paste(filepath_prefix, "Tree_outputs\\", paint_type, "\\Tree_output_", timestr, ".txt", sep = "")
  
  features <- colnames(defects_per_car)
  non_features <- c("defect_report", "vin", "manuf_date")
  column <- ifelse((paint_type == "primer"), "tot_prim_defects", "tot_tc_defects")
  non_features <- c(non_features, column)
  features <- features[!(features %in% non_features)]
  
  sink(file = opfile)
  for (i in 1:T)
  {
    sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
    #dtree <- rpart(as.formula(formula_str), data = defects_per_car)
	dtree <- ctree(as.formula(formula_str), data = defects_per_car, controls = ctree_control(maxdepth = 1))
	#if (!is.null(dtree$splits))
	#{
	  #No point in printing decision trees with root node only
	  cat(paste("\n\nformula_str = ", formula_str, "\n", sep = ""))
	  print(dtree)
	  #print_dtree(dtree)
	#}
  }
  #sink()
  closeAllConnections()
  opfile
}

bar_plots_for_AP_variables <- function(defects_per_car, feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5  
  defects_per_car$tree_node <- ifelse((defects_per_car[, feature] <= cutpoint), paste(" <= ", cutpoint, sep = ""),
                                             paste(" > ", cutpoint, sep = ""))
  pdpc_trans <- defects_per_car %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = round(100*count/sum(count), 2)) %>% ungroup() %>% group_by(tree_node) %>% mutate(csum = cumsum(perc)) %>% mutate(pos = csum - 0.5*perc)
  print(pdpc_trans)
  
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc, fill = defect_report)) +
    geom_bar(stat = "identity", width = 0.7) + 
	#In ggplot_2.2.0, fill order is based on the order of the factor levels. 
	#The default order will plot the first level at the top of the stack instead of the bottom. 
	#If you want the first level at the bottom of the stack, use position_stack(reverse = TRUE).
	geom_col(position = position_stack(reverse = TRUE)) + 
	geom_text(aes(label = perc, y = pos), size = 4, colour = "red", fontface = "bold") + scale_fill_manual(values = cbbPalette) + 
    labs(x = "tree_node", y = "percent", fill = "defect_report") + ggtitle(feature) + 
    theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
          plot.title = element_text(lineheight = .8, face = "bold"))
  p
}

barplot_grid_view_from_tree_output <- function(defects_per_car, df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(defects_per_car, df_features_cutpoints[1, "feature"], df_features_cutpoints[1, "cutpoint"])))
  gp2 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(defects_per_car, df_features_cutpoints[2, "feature"], df_features_cutpoints[2, "cutpoint"])))
  gp3 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(defects_per_car, df_features_cutpoints[3, "feature"], df_features_cutpoints[3, "cutpoint"])))
  gp4 <- ggplot_gtable(ggplot_build(bar_plots_for_AP_variables(defects_per_car, df_features_cutpoints[4, "feature"], df_features_cutpoints[4, "cutpoint"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
                            )
}

box_plots_for_AP_variables <- function(defects_per_car, feature)
{
  p <- ggplot(defects_per_car, aes(factor(defect_report), get(feature))) + geom_boxplot() + 
       labs(x = "Defect report", y = feature) + ggtitle(feature) + 
	   theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
	         axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
			 plot.title = element_text(lineheight = .8, face = "bold"))
  p
}

boxplot_grid_view_from_tree_output <- function(defects_per_car, df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(defects_per_car, df_features_cutpoints[1, "feature"])))
  gp2 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(defects_per_car, df_features_cutpoints[2, "feature"])))
  gp3 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(defects_per_car, df_features_cutpoints[3, "feature"])))
  gp4 <- ggplot_gtable(ggplot_build(box_plots_for_AP_variables(defects_per_car, df_features_cutpoints[4, "feature"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
                            )  
  frame_grob
}

generate_plots_from_dtree_output <- function(defects_per_car, filename, view)
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
  df_features_cutpoints <- df_features_cutpoints[1:4, ] #Keep top 4 on dashboard
  
  frame_grob <- ifelse((view == "bar_plot"), barplot_grid_view_from_tree_output(defects_per_car, df_features_cutpoints),
                                               boxplot_grid_view_from_tree_output(defects_per_car, df_features_cutpoints))
}

generate_dtree_and_plot <- function(user_inputs)
{
  filename <- paste(filepath_prefix, 
                    ifelse((user_inputs[["paint_type"]] == "primer"), "primer_defects_per_car.csv", "tc_defects_per_car.csv"),
					sep = "")
  defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  column <- ifelse((user_inputs[["paint_type"]] == "primer"), "tot_prim_defects", "tot_tc_defects")
  defects_per_car$defect_report <- ifelse((defects_per_car[, column] <= user_inputs[["threshold"]]), "Acceptable", "Unacceptable")
  defects_per_car$defect_report <- as.factor(defects_per_car$defect_report)
  
  recent_window_start <- "2017-02-02"
  recent_window_end <- "2017-02-08"
  recent_data <- subset(defects_per_car, ((manuf_date >= "2017-02-02") & (manuf_date <= "2017-02-08"))) #Last one week
  #For now, fix the length of the historical window at 9 weeks
  win_len <- 9
  window_end = "2017-02-01"
  start_date <- as.character(seq(as.Date(window_end), length = 2, by = paste("-", (7*win_len-1), " days", sep = ""))[2])
  #If the start_date computed initially falls on or before the shutdown, check how many days we are losing because of shutdown, and 
  #adjust those many days from days before shutdown. Shutdown was for 11 days, including both ends.
  revised_start_date <- start_date
  last_day_of_shutdown = "2017-01-02"
  first_day_of_shutdown = "2016-12-23"
  if (start_date <= last_day_of_shutdown)
  {
	lost_days <- as.numeric(difftime(as.Date(last_day_of_shutdown, '%Y-%m-%d'), as.Date(start_date, '%Y-%m-%d'), units = c("days"))) + 1
	last_day_before_shutdown <- as.character(seq(as.Date(first_day_of_shutdown), length = 2, by = "-1 days")[2])
    revised_start_date <-  as.character(seq(as.Date(last_day_before_shutdown), length = 2, by = paste("-", (lost_days - 1), " days", sep = ""))[2])
  }
  historical_data <- subset(defects_per_car, ((manuf_date >= revised_start_date) & (manuf_date <= window_end))) 
  #Eliminate data corresponding to shutdown period (2016-12-23 to 2017-01-02)
  historical_data <- subset(historical_data, !((manuf_date >= first_day_of_shutdown) & (manuf_date <= last_day_of_shutdown)))
  
  opfile <- el_yunque(historical_data, user_inputs[["paint_type"]], F = 5, T = 50)
  frame_grob <- generate_plots_from_dtree_output(defects_per_car, opfile, user_inputs[["view"]])
}

shinyServer(function(input, output) {
    userInput <- reactive({
    list("view" = input$view, "paint_type" = input$paint_type, "threshold" = input$threshold) 
  })
  
  output$plotFromTreeOutput <- renderPlot({ 
      user_inputs <- userInput()
      generate_dtree_and_plot(user_inputs)          
     })
 }
)