# get_n_max() function finds the nth Max value for each group in a data frame
# and adds the results to the table as a new column named "high_n" 

# nth Max function, where n is the nth max or nth high value
get_n_max <- function(data = dataTable, results = "result", groups = "groupid", n = 2)
{
  library(dplyr)
  
  names(data)[c(grep(results,names(data)), grep(groups,names(data)))] <- c("result", "groupid")
  data <- group_by(data, groupid) %.% mutate(high_n = sort(result, decreasing=T)[n])
  names(data)[c(grep("result",names(data)), grep("groupid",names(data)), grep("high_n",names(data)))] <- 
       c(results, groups, paste("high_", n, sep=""))
  
  return(data)
}

# Example, find 2nd Max mpg value for all "cyl" groups in mtcars data
data <- mtcars
data <- get_n_max(data, results = "mpg", groups = "cyl", 2)
head(data)
#   mpg cyl disp  hp drat    wt  qsec vs am gear carb high_2
#1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4   21.0
#2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4   21.0
#3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1   32.4
#4 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1   21.0
#5 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2   18.7


## Create summary table of results -------------------------------------

# summary_n_max() function finds the nth Max value for each group in a data frame
# and produces a summary table

summary_n_max <- function(data = dataTable, results = "result", groups = "groupid", n = 2)
{
  library(dplyr)
  
  names(data)[c(grep(results,names(data)), grep(groups,names(data)))] <- c("result", "groupid")
  data <- group_by(data, groupid) %.% summarize(high_n = sort(result, decreasing=T)[n])
  names(data)[2] <- paste("high_", n, sep="")
 
  return(data)
}

# Summary of 2nd Max mpg value for all "cyl" groups in mtcars data
data <- mtcars
data_2nd_max <- summary_n_max(data, "mpg", "cyl", 2)
head(data_2nd_max)
#     groupid    high_2
# 1         4      32.4
# 2         6      21.0
# 3         8      18.7 
