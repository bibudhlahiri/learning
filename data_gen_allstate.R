library(data.table)

gen_driving_data <- function()
{
  N <- 100000
  g_force <- rnorm(N, 0.08, 0.12)
  driving_data <- data.table(g_force = g_force)
  setkey(driving_data, g_force)
  
  risky <- driving_data[abs(g_force) >= 0.2]
  non_risky <- driving_data[abs(g_force) < 0.2]
  cat(paste("nrow(risky) = ", nrow(risky), ", nrow(non_risky) = ", nrow(non_risky),
            ", total = ", sum(nrow(risky) + nrow(non_risky)), sep = ""))
  
  risky[, time_of_day := sample(c("night", "day"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, raining := sample(c("true", "false"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, accident_prone_area := sample(c("true", "false"), nrow(risky), replace = TRUE, prob = c(0.95, 0.05))]
  risky[, risky := "true"]
  
  non_risky[, time_of_day := sample(c("night", "day"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, raining := sample(c("true", "false"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, accident_prone_area := sample(c("true", "false"), nrow(non_risky), replace = TRUE, prob = c(0.5, 0.5))]
  non_risky[, risky := "false"]
  
  driving_data <- rbindlist(list(risky, non_risky))
}


driving_data <- gen_driving_data()
setkey(driving_data, label, accident_prone_area, raining, time_of_day)
stats <- driving_data[, list(n_elems = length(g_force), median_g_force = median(g_force)), 
                      by = list(label, accident_prone_area, raining, time_of_day)] 
print(stats)
