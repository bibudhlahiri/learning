library(shiny)
library(party)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)

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

viz_from_tree_output <- function(feature, cutpoint)
{
  #tree_node defines which group, based on a cutpoint on a feature, a data point belongs to, e.g., TH_AH_4_Temp <= 73.5 or TH_AH_4_Temp > 73.5
  filename <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\primer_defects_per_car.csv"
  primer_defects_per_car <- read.csv(filename, header = T, stringsAsFactors = F) 
  primer_defects_per_car$defect_report <- as.factor(primer_defects_per_car$defect_report)  
  primer_defects_per_car$tree_node <- ifelse((primer_defects_per_car[, feature] <= cutpoint), paste(" <= ", cutpoint, sep = ""),
                                             paste(" > ", cutpoint, sep = ""))
  #primer_defects_per_car$tree_node <- gsub("TH_AH_", "AH", primer_defects_per_car$tree_node)
  pdpc_trans <- primer_defects_per_car %>% group_by(tree_node, defect_report) %>% summarise(count = n()) %>% mutate(perc = count/sum(count))
  print(pdpc_trans)
  image_file <- paste("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\output_from_decision_tree\\",
                      feature, "_", cutpoint, ".png", sep = "")
  png(image_file, width = 600, height = 480, units = "px")
  cbbPalette <- c("#E69F00", "#000000", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  p <- ggplot(pdpc_trans, aes(x = factor(tree_node), y = perc*100, fill = defect_report)) +
        geom_bar(stat = "identity", width = 0.7) + scale_fill_manual(values = cbbPalette) + 
        labs(x = "tree_node", y = "percent", fill = "defect_report") + ggtitle(feature) + 
        theme(axis.text.x = element_text(size = 12, color = 'black', face = 'bold'),
	          axis.text.y = element_text(size = 12, color = 'black', face = 'bold'),
			  plot.title = element_text(lineheight = .8, face = "bold"))
  print(p)
  dev.off()
  p
}

barplot_grid_view_from_tree_output <- function(df_features_cutpoints, how_many = 4)
{
  gp1 <- ggplot_gtable(ggplot_build(viz_from_tree_output(df_features_cutpoints[1, "feature"], df_features_cutpoints[1, "cutpoint"])))
  gp2 <- ggplot_gtable(ggplot_build(viz_from_tree_output(df_features_cutpoints[2, "feature"], df_features_cutpoints[2, "cutpoint"])))
  gp3 <- ggplot_gtable(ggplot_build(viz_from_tree_output(df_features_cutpoints[3, "feature"], df_features_cutpoints[3, "cutpoint"])))
  gp4 <- ggplot_gtable(ggplot_build(viz_from_tree_output(df_features_cutpoints[4, "feature"], df_features_cutpoints[4, "cutpoint"])))
  
  frame_grob <- grid.arrange(gp1, gp2, gp3, gp4, ncol = 2
                             #, heights = rep(3, 3), widths = rep(10,3), padding = unit(5.0, "line")
                            )
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
  df_features_cutpoints <- df_features_cutpoints[1:4, ] #Keep top 4 on dashboard
  
  frame_grob <- barplot_grid_view_from_tree_output(df_features_cutpoints)
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
  frame_grob <- generate_plots_from_dtree_output(opfile)
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