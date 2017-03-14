library(shiny)
library(party)
library(ggplot2)
library(gridExtra)
library(dplyr)

filepath_prefix <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\"
plot_historical <<- NA
plot_recent <<- NA

el_yunque <- function(defects_per_car, paint_type, F = 5, T = 50)
{  
  #Save the output trees in a file named by the timestamp at which it is generated, so that multiple runs do not overwrite results
  timestr <- as.character(Sys.time())
  timestr <- gsub("-", "_", timestr)
  timestr <- gsub(" ", "_", timestr)
  timestr <- gsub(":", "_", timestr)
  #opfile <- paste(filepath_prefix, "Tree_outputs\\", paint_type, "\\Tree_output_", timestr, ".txt", sep = "")
  opfile <- textConnection("model_file", open = "a")
    
  features <- colnames(defects_per_car)
  non_features <- c("defect_report", "vin", "manuf_date")
  column <- ifelse((paint_type == "primer"), "tot_prim_defects", "tot_tc_defects")
  non_features <- c(non_features, column)
  features <- features[!(features %in% non_features)]
  
  sink(file = opfile) #Needed even for in-memory textConnection
  for (i in 1:T)
  {
    sampled_features <- features[sample(length(features), F)]
	formula_str <- paste("defect_report ~ ", paste(sampled_features, collapse = " + "), sep = "")
    #dtree <- rpart(as.formula(formula_str), data = defects_per_car)
	dtree <- ctree(as.formula(formula_str), data = defects_per_car, controls = ctree_control(maxdepth = 1))
	print(dtree)
  }
  sink()
  #close(opfile)
  #closeAllConnections()
  #cat(model_file) 
  #class(model_file) = character, class(opfile) = textConnection
  #cat(paste("class(model_file) = ", class(model_file), ", class(opfile) = ", class(opfile), "\n", sep = ""))
  opfile
}

get_pdpc <- function(input_data, feature, cutpoint)
{
  input_data$tree_node <- ifelse((input_data[, feature] <= cutpoint), paste(" <= ", cutpoint, sep = ""),
                                 paste(" > ", cutpoint, sep = ""))
  pdpc_trans <- input_data %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = round(100*count/sum(count), 2)) %>%
                ungroup() %>% group_by(tree_node) %>% mutate(csum = cumsum(perc), range_sum = sum(count)) %>% mutate(pos = csum - 0.5*perc) %>%  
				mutate(caption = paste(perc, " (", count, "/", range_sum, ")", sep = ""))
  print(pdpc_trans)
  pdpc_trans
}

bar_plots_for_AP_variables <- function(input_data, feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5  
  pdpc_trans <- get_pdpc(input_data, feature, cutpoint)
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc, fill = defect_report)) +
    geom_bar(stat = "identity", width = 0.7) + coord_equal() +  
	#In ggplot_2.2.0, fill order is based on the order of the factor levels. 
	#The default order will plot the first level at the top of the stack instead of the bottom. 
	#If you want the first level at the bottom of the stack, use position_stack(reverse = TRUE).
	geom_col(position = position_stack(reverse = TRUE)) + 
	geom_text(aes(label = caption, y = pos), size = 4, colour = "red", fontface = "bold") + scale_fill_manual(values = cbbPalette) + 
    labs(x = paste("Range for ", feature, sep = ""), y = "percent") + ggtitle(feature) + 
    theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
          plot.title = element_text(lineheight = .8, face = "bold"),
		  aspect.ratio = 0.6)
  p
}

generate_summary_text <- function(input_data, df_features_cutpoints, how_many = 4)
{
  
} 

barplot_grid_view_from_tree_output <- function(input_data, df_features_cutpoints, how_many = 4)
{
  cat(paste("In barplot_grid_view_from_tree_output, how_many = ", how_many, "\n", sep = ""))
  if (how_many > 0)
  {
    cat("1\n")
    plots = lapply(1:how_many, 
          function(i) bar_plots_for_AP_variables(input_data, df_features_cutpoints[i, "feature"], df_features_cutpoints[i, "cutpoint"]))
    return(do.call(grid.arrange, plots))
  }
  else 
  {
    #TODO: Display of text when there is no viz to show is not working
    cat("2\n")
    text = "\nNo significant pattern found for the given input\n"
    return(ggplot() + annotate("text", x = 4, y = 25, size = 8, label = text) + theme_bw() + 
	       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
  }
}

box_plots_for_AP_variables <- function(defects_per_car, feature)
{
  acceptable <- subset(defects_per_car, (defect_report == "Acceptable"))
  unacceptable <- subset(defects_per_car, (defect_report == "Unacceptable"))
  title_str <- paste(feature, ", Acc median = ", round(median(acceptable[, feature]),1), 
                     ", Unacc median = ", round(median(unacceptable[, feature]),1), 
                     "\n", sep = "")
  p <- ggplot(defects_per_car, aes(factor(defect_report), get(feature))) + geom_boxplot() + 
       labs(x = "Defect report", y = feature) + ggtitle(title_str) + 
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

parse_tree_output <- function(model_file)
{
  #cat("Inside parse_tree_output\n")
  #cat(model_file, sep = "\n")
  #lines <- readLines(model_file) 
  lines <- model_file
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
  df_features_cutpoints
}

#We need to decide which of the features from df_features_cutpoints should be kept as top 4.
#Any range defined by a cutpoint on a feature, for which we are not getting any points in the
#recent dataset, should not be included. In such cases, we move on to the next feature.
check_df_features_cutpoints <- function(df_features_cutpoints, recent_data)
{
  new_df_features_cutpoints <- data.frame(feature = character(), cutpoint = numeric(), statistic = numeric(0), stringsAsFactors = FALSE)
  n_valid_features_obtained <- 0
  i <- 1
  while ((n_valid_features_obtained < 4) && (i <= nrow(df_features_cutpoints)))
  {
    pdpc_trans <- get_pdpc(recent_data, df_features_cutpoints[i, "feature"], df_features_cutpoints[i, "cutpoint"])
	if (nrow(pdpc_trans) == 4) #There should be exactly 4 rows in a pdpc_trans as there are four sections (2 yellow, 2 black) in a plot
	{
	  new_df_features_cutpoints <- rbind(new_df_features_cutpoints, df_features_cutpoints[i,])
	  n_valid_features_obtained <- n_valid_features_obtained + 1
	}
	i <- i + 1
  }
  new_df_features_cutpoints
}

generate_plots_from_dtree_output <- function(input_data, df_features_cutpoints, view)
{
  hmfp <- min(4, length(unique(df_features_cutpoints$feature))) #What if we do not get even 4 features?
  df_features_cutpoints <- df_features_cutpoints[1:hmfp, ] 
  frame_grob <- ifelse((view == "bar_plot"), barplot_grid_view_from_tree_output(input_data, df_features_cutpoints, hmfp),
                                               boxplot_grid_view_from_tree_output(input_data, df_features_cutpoints, hmfp))
}

#Reads input data, splits it into historical and recent, builds model on historical and applies it back on both historical and recent to 
#generate the plots. For now, fix the length of the historical window at 5 weeks for primer and 7 weeks for top coat.
generate_dtree_and_plot <- function(user_inputs, window_end = "2017-02-01", first_day_of_shutdown = "2016-12-23", last_day_of_shutdown = "2017-01-02")
{
  filename <- paste(filepath_prefix, 
                    ifelse((user_inputs[["paint_type"]] == "primer"), "primer_defects_per_car.csv", "tc_defects_per_car.csv"),
					sep = "")
  defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  column <- ifelse((user_inputs[["paint_type"]] == "primer"), "tot_prim_defects", "tot_tc_defects")
  win_len <- ifelse((user_inputs[["paint_type"]] == "primer"), 5, 7)
  defects_per_car$defect_report <- ifelse((defects_per_car[, column] <= user_inputs[["threshold"]]), "Acceptable", "Unacceptable")
  defects_per_car$defect_report <- as.factor(defects_per_car$defect_report)
  defects_per_car <- subset(defects_per_car, (manuf_date >= "2016-10-17")) #To keep primer data in synch with top coat
  
  recent_window_start <- "2017-02-02"
  recent_window_end <- "2017-02-08"
  recent_data <- subset(defects_per_car, ((manuf_date >= "2017-02-02") & (manuf_date <= "2017-02-08"))) #Last one week
  
  start_date <- as.character(seq(as.Date(window_end), length = 2, by = paste("-", (7*win_len-1), " days", sep = ""))[2])
  #If the start_date computed initially falls on or before the shutdown, check how many days we are losing because of shutdown, and 
  #adjust those many days from days before shutdown. Shutdown was for 11 days, including both ends.
  revised_start_date <- start_date
  
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
  cat(model_file, sep = "\n") #Works from here
  #Handling edge cases: what if we less than 4 significant variables? If there are some, but less than 4 (e.g., primer and threshold 3), 
  #we should adjust the plot dynamically. If there are none (e.g., primer and threshold 4), we should give a message only. 
  #df_features_cutpoints <- parse_tree_output(opfile)
  df_features_cutpoints <- parse_tree_output(model_file)
  df_features_cutpoints <- check_df_features_cutpoints(df_features_cutpoints, recent_data)
  print(df_features_cutpoints)
  #The visual will always load the one based on recent data, and will load the one on historical only if the user asks for it
  plot_historical <<- generate_plots_from_dtree_output(historical_data, df_features_cutpoints, user_inputs[["view"]]) 
  plot_recent <<- generate_plots_from_dtree_output(recent_data, df_features_cutpoints, user_inputs[["view"]])
  cat(paste("class(plot_recent) = ", class(plot_recent), "\n", sep = ""))
  plot_recent
}

shinyServer(function(input, output) {
    userInput <- reactive({
    list("view" = input$view, "paint_type" = input$paint_type, "threshold" = input$threshold) 
  })
  
  pt <- reactive({
    user_inputs <- userInput()
    generate_dtree_and_plot(user_inputs)  	
  })
    
  output$plotgraph <- renderPlot({pt()})
  output$dis <- renderDataTable({})
 }
)