library(ggplot2)

filepath_prefix <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\ICSDefectDataC\\"

create_differences <- function()
{
  filename <- paste(filepath_prefix, "PrimerTestVin.csv", sep = "")
  vins <- read.csv(filename, header = T, stringsAsFactors = F) 
  #attach(vins)
  #For cars with defect, ICS tracking time accurately captures when the primer was done. But this is not available for cars without defect. 
  #So we are seeing if P8 (time when primer inspection is done) can be used as a proxy for that. 
  vins$icstrt_minus_p8 <- as.numeric(difftime(strptime(vins$galc_paint_defect_raw.p8_cdate, '%d-%m-%Y %H:%M'), strptime(vins$ICS.Trackingtime, '%d-%m-%Y %H:%M'),  
                                              units = c("mins")))
  #detach(vins)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\icstrt_minus_p8.png"
  png(image_file, width = 600, height = 480, units = "px")
  p <- ggplot(vins, aes(x = icstrt_minus_p8)) + geom_histogram(bins = 10) + xlab("(ICS Tracking Point - P8)") + 
       ylab("Frequency") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold'))
  print(p)
  dev.off()

  #P2 is the time the vehicle starts approaching the paint booth, and P4 is the time it enters the booth. Check its distribution as a sanity check.
  vins$p4_minus_p2 <- as.numeric(difftime(strptime(vins$galc_paint_defect_raw.p4_cdate, '%d-%m-%Y %H:%M'), 
                                          strptime(vins$galc_paint_defect_raw.p2_cdate, '%d-%m-%Y %H:%M'),  
                                          units = c("mins")))
  #detach(vins)
  
  image_file <- "C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\data\\Phase2\\figures\\ICSDefectDataC\\p4_minus_p2.png"
  png(image_file, width = 600, height = 480, units = "px")
  p <- ggplot(vins, aes(x = p4_minus_p2)) + geom_histogram(bins = 10) + xlab("Time to approach booth") + 
       ylab("Frequency") + 
       theme(axis.text = element_text(colour = 'blue', size = 16, face = 'bold')) +
         theme(axis.title = element_text(colour = 'red', size = 16, face = 'bold'))
  print(p)
  dev.off()
  vins
}

#source("C:\\Users\\blahiri\\Toyota\\Paint_Shop_Optimization\\code_local\\primer_defects_tracking_point_differences.R")
vins <- create_differences()