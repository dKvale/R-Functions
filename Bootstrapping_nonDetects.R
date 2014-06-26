###   Bootstrapping the LCL and UCL for each group in a data frame  ### 
###   with Non-detects or censored data                             ###

# Example: 

# The bootStrap_NADA() function takes a censored column containing 0's for detects and 1's for non-detects,
# or alternatively, TRUE for non-detected and FALSE for detected 
boot_NADA <- function(data = dataTable, results = "result", censored = "censored",
                           groups = "groupID", repeats = 2000, percentile = 0.95){
  library(NADA)
  library(dplyr)
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
  
# start stopwatch
  start   <- proc.time()
  count   <- 0
  
# Suppress repeated warnings
  options(warn = -1)

# Initiate final summary table
  bootstrap_Summary <- data.frame()

# Start bootstrapping one group at a time to limit table size
for (group in unique(data$groupID)){
  subGroup_table <- filter(data, groupID == group)
  n <- nrow(subGroup_table)
  
  # Print % done to screen
  nGroups <- length(unique(data$groupID))
  cat(round(count/nGroups*100),"%", sep="")
  
  # Break into two groups to inrease speed    
  repeats1          <- repeats/2
  repeats2          <- repeats - repeats1
  bootedMeans       <- c()
  bootedMeans2      <- c()
  bootstrap_Results <- data.frame()
  
  for (i in 1:repeats1){
    bootedMeans <- c(bootedMeans, as.numeric(mean(cenfit(subGroup_table$result[sample(1:n, replace=T)], subGroup_table$censored))[1]))
  } 
  for (i in 1:repeats2){
    bootedMeans2 <- c(bootedMeans2, as.numeric(mean(cenfit(subGroup_table$result[sample(1:n, replace=T)], subGroup_table$censored))[1]))
  }
  bootstrap_Results <- data.frame(groupID = group, bootMeans=unlist(c(bootedMeans,bootedMeans2)))
  bootstrap_Results <- summarize(bootstrap_Results, groupID = groupID[1], boot_LCL = quantile(bootMeans, probs= alpha, na.rm=T), boot_Mean= mean(bootMeans) , boot_UCL= quantile(bootMeans, probs=percentile, na.rm=T), boot_StDev= sd(bootMeans))
  bootstrap_Summary <- rbind_list(bootstrap_Summary, bootstrap_Results)
  
  count <- count + 1
  cat("\b\b\b\b")
}
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
