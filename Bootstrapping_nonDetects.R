###   Bootstrapping the LCL and UCL for each group in a data frame  ### 
###   with Non-detects or censored data                             ###

# The bootStrap_NADA() function takes a censored column containing 0's for detects and 1's for non-detects,
# or alternatively, TRUE for non-detected and FALSE for detected 
bootStrap_NADA <- function(   data = dataTable, results = "result", censored = "censored",
                       groups = "groupID", repeats = 2000, percentile = 0.95){
  library(NADA)
  library(dplyr)
    
# start stopwatch
  start   <- proc.time()
  count   <- 0
  
# Remove unnecessary columns to speed up repetitions
  data <- data[,c(results, groups, censored)]

# Set censored column as logical (True/False)
  data$censored<- as.logical(data$censored)

# Rename columns
  names(data) <- c("result", "groupID", "censored")
  
# Drop groups with less than 10% detects
  drop_Cutoff <- 0.10
  detect_data <- group_by(data, groupID) %.% summarize(detect_Fraction = length(subset(censored, censored==F))/length(censored) )
  detect_data <- filter(detect_data, detect_Fraction >= drop_Cutoff)
  data <- filter(data, groupID %in% detect_data$groupID)
  
# Remove factor from groupID
  data$groupID <- as.character(data$groupID)

# Get alpha in decimal form, default = 95% = 0.95
  alpha <- (1-percentile)

# Suppress repeated warnings
  options(warn = -1)
  
# Create empty tables to store results
  bootedMean        <- data.frame()
  bootstrap_Summary <- data.frame()
   
# Break into two groups to inrease speed    
    repeats1 <- round(repeats/2)
    repeats2 <- repeats - repeats1
    bootstrap_Results <- data.frame()
    bootstrap_Results2 <- data.frame()

cat("Repetition: ")

  for (i in 1:repeats1){
    bootedMean <- group_by(data, groupID) %.% summarize(bootMeans = as.numeric(mean(cenfit(result[sample(1:length(result), replace=T)], censored))[1]))
    bootstrap_Results  <- rbind_list(bootstrap_Results, bootedMean)
    count <- count + 1
    cat(count, ",", sep="")
} 

  for (i in 1:repeats2){
    bootedMean <- group_by(data, groupID) %.% summarize(bootMeans = as.numeric(mean(cenfit(result[sample(1:length(result), replace=T)], censored))[1]))
    bootstrap_Results2  <- rbind_list(bootstrap_Results2, bootedMean)
    count <- count + 1
    cat(count, ",", sep="")
}

  bootstrap_Results <- rbind_list(bootstrap_Results,bootstrap_Results2)
  bootstrap_Summary<- group_by(bootstrap_Results, groupID) %.% summarize(boot_LCL = quantile(bootMeans, probs= alpha, na.rm=T), boot_Mean= mean(bootMeans) , boot_UCL= quantile(bootMeans, probs=percentile, na.rm=T), b_StDev= sd(bootMeans))

  # Bring back original groups column name
  names(bootstrap_Summary)[1]  <- c(groups)
  row.names(bootstrap_Summary) <- NULL 
  
  # Turn warnings back on
  options(warn=1)
  
  # Show time elapsed
  print(proc.time() - start)
  print(head(bootstrap_Summary))
  return(bootstrap_Summary)   
}
