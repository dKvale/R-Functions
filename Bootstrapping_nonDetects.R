## Bootstrapping the LCL and UCL for each group in a data frame with Non-detects or censored data ##

# bootStrap() function returns a summary table of bootstrap statistics for each group
# percentile is used to find the % Upper Confidence Limit, 
# 100-percentile is used for the % Lower Confidence Limit  
bootStrap <- function(   data = dataTable, results = "result", censored = "censored",
                       groups = "groupID", repeats = 2000, percentile = 95){
  library(NADA)
  library(dplyr)
# Remove unnecessary columns to speed things up
  data <- data[,c(results, groups, censored)]
  data$groupID <- as.character(data$groupID)

# Rename columns
  names(data) <- c("result", "groupID", "censored")
  
# Get alpha in decimal form, default = 95% = 0.95
  alpha <- (100-percentile)/100
  
# start stopwatch
  start   <- proc.time()
  count   <- 1
  nGroups <- length(unique(data$groupID))
  
# Suppress repeated warnings
  options(warn = -1)
  
# Create empty tables to store results
  bootedMean        <- data.frame()
  bootstrap_Summary <- data.frame()
  
# Start bootstrapping each group one at a time to limit table size
  for (group in unique(data$groupID)){
    subGroup_table    <- filter(data, groupID == group)
                 n    <- nrow(subGroup_table)
# Print % done to screen
    cat(round(count/nGroups*100),"%", sep="")
    
# Break into two groups to inrease speed
    repeats1 <- repeats/2
    repeats2 <- repeats - repeats1
    bootstrap_Results <- data.frame()
    bootstrap_Results2 <- data.frame()
    
    for (i in 1:repeats1){
             bootedMean  <- data.frame(groupID=group, bootMeans=censNada(subGroup_table$result, subGroup_table$censored, sample(1:n, replace=T)))
      bootstrap_Results  <- rbind(bootstrap_Results, bootedMean)
} 
    for (i in 1:repeats2){
             bootedMean   <- data.frame(groupID=group, bootMeans=censNada(subGroup_table$result, subGroup_table$censored, sample(1:n, replace=T)))
      bootstrap_Results2  <- rbind(bootstrap_Results2, bootedMean)
}
    bootstrap_Results <- rbind(bootstrap_Results,bootstrap_Results2)
    bootstrap_Results <- summarize(bootstrap_Results, groupID = groupID[1], boot_LCL = quantile(bootMeans, probs= alpha, na.rm=T), boot_Mean= mean(bootMeans) , boot_UCL= quantile(bootMeans, probs=percentile, na.rm=T), b_StDev= sd(bootMeans))
    bootstrap_Summary <- rbind(bootstrap_Summary, bootstrap_Results)
    count <- count + 1
    cat("\b\b\b\b")

}
  # Bring back original groups column name
  names(bootstrap_Summary)[1] <- c(groups)
  row.names(bootstrap_Summary) <- NULL 
  
  # Turn warnings back on
  options(warn=1)
  
  # Show time elapsed
  proc.time() - start
  head(bootstrap_Summary)
     
}
