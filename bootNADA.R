###-----------------------------------------------------------------------------------###
#
# File: bootNADA.R
#
# Purpose: Using the NADA and dplyr package, the function below Bootstraps
# the 5% LCL and 95% UCL of the Kaplan-Meier mean for each group or
# pollutant in a data frame that contains non-detected or censored data
#
###-----------------------------------------------------------------------------------###

library(NADA)
library(dplyr)

#************ Example ************#

# Bootstrap the 95% UCL and 5% LCL of ozone measurements for each month in "airquality" dataset
ozone <- airquality
ozone$Non_detect <- is.na(ozone$Ozone)
ozone[is.na(ozone$Ozone),"Ozone"]<-0

##  Ozone Solar.R Wind Temp Month Day Non_detect
#1    41     190  7.4   67     5   1      FALSE
#2    36     118  8.0   72     5   2      FALSE
#3    12     149 12.6   74     5   3      FALSE
#4    18     313 11.5   62     5   4      FALSE
#5     0      NA 14.3   56     5   5       TRUE
#6    28      NA 14.9   66     5   6      FALSE

group_by(ozone, Month) %>% summarize(Ozone_KapM_mean = mean(cenfit(Ozone, Non_detect))[1])
##  Month Ozone_KapM_mean
#1    5      20.0
#2    6      17.2
#3    7      50.7
#4    8      51.7
#5    9      30.6

ozone.Booted <- bootNADA(data=ozone, results="Ozone", censored="Non_detect", groups = "Month", repeats=100)
head(ozone.Booted)
##   Month boot_LCL boot_Mean boot_UCL boot_StDev
#1     5       14        20       29        4.3
#2     6       14        18       23        2.8
#3     7       41        50       60        5.8
#4     8       39        52       67        8.6
#5     9       24        31       38        4.3

#*********** End Example *****************#

# The boot_NADA() function takes a censored column containing 0's for detects and 1's for
# non-detects, or alternatively FALSE for detects and TRUE for non-detects
bootNADA <- function(data = dataTable, results = "result", censored = "censored", groups = "groupID", repeats = 2000, percentile = 0.95){
    
library(NADA)
library(dplyr)
    
data <- data[,c("Ozone", "Month", "Non_detect")]

# Rename columns
names(data) <- c("result", "groupID", "censored")

# Set censored column as logical (True/False)
data$censored<- as.logical(data$censored)

# Drop groups with less than 10% detects
drop_Cutoff <- 0.10
detect_data <- group_by(data, groupID) %>% summarize(detect_Fraction = length(subset(censored, censored==F))/length(censored) )
detect_data <- filter(detect_data, detect_Fraction >= drop_Cutoff)
data <- filter(data, groupID %in% detect_data$groupID)

# Set NA's and negative result values at 0.0
data[is.na(data$result), "result"] <- 0
data[data$result < 0, "result"] <- 0

# Remove factor from groupID
data$groupID <- as.character(data$groupID)

# Get alpha from percentile, default is 1 - .95 = 0.05
alpha <- (1-percentile)

# Start stopwatch
start <- proc.time()
count <- 0

#Count groups for progress counting
nGroups <- length(unique(data$groupID))

# Suppress repeated warnings
options(warn = -1)

# Initiate final summary table
bootstrap_Summary <- data.frame()

# Define function to record cenfit mean of re-sampled table
getCenMean <- function(n=n){
    random.rows <- sample(1:n, replace=T)
    mean(cenfit(subGroup_table$result[random.rows], subGroup_table$censored[random.rows]))[[1]]
}

# Start bootstrapping one group at a time to limit table size
for (group in unique(data$groupID)){
    
    subGroup_table <- filter(data, groupID == group)
    subGroup_table$groupID <- as.character(subGroup_table$groupID)
    n <- nrow(subGroup_table)
    
    # Print % done to screen
    cat(ceiling(count/nGroups*100),"% ", sep="")
    
    # Initiate final summary table
    bootstrap_Results <- data.frame()
    
    # Use Lapply to repeat Cenfit from NADA package and bootstrap the mean of the censured data
    bootedMeans <- lapply(1:repeats, FUN= function(x) getCenMean(n=n))

    # Summarize the booted Cenfit means: LCL, UCL, Mean, and Std. Dev
    bootstrap_Results <- data.frame(groupID = group, bootMeans=unlist(bootedMeans, use.names=F))
    bootstrap_Results <- summarize(bootstrap_Results, groupID = groupID[1], 
                                   boot_LCL   = quantile(bootMeans, probs= alpha, na.rm=T),
                                   boot_Mean  = mean(bootMeans), 
                                   boot_UCL   = quantile(bootMeans, probs=percentile, na.rm=T),
                                   boot_StDev = sd(bootMeans))
    bootstrap_Summary <- rbind(bootstrap_Summary, bootstrap_Results)
    
    count <- count + 1
    cat("\b\b\b\b\b")
}

# Bring back the original group name
names(bootstrap_Summary)[1] <- groups
row.names(bootstrap_Summary) <- NULL

# Turn warnings back on
options(warn=1)

# Show time elapsed
print(proc.time() - start)
cat("\n")

print(head(bootstrap_Summary))
return(bootstrap_Summary)
}
