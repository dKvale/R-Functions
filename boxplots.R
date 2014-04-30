#Define outliers for boxplots using ggplot
require("reshape2")
require(“ggplot2”)

setwd("M:/MNRiskS 2008 Results/ID top pollutants")
topcopc = read.csv("Inhalation risks by pollutant highest risk pollutants all receptors.csv", header=T, stringsAsFactors=F)
top = topcopc

#Eliminate zeros
topcopc[ topcopc == 0 ] = NA
topcopc = na.omit(topcopc)

#Switch to long format
topcopc = melt(topcopc[,c(6:35)])
topcopc$variable = as.character(topcopc$variable)

#Separate Cancer and Non-cancer
Canc_list=names(top[,c(7,23,35,9,13,15,33,31,27,25,11,19,21,29,17)])
HI_list=names(top[,-c(1:5,7,23,35,9,13,15,33,31,27,25,11,19,21,29,17)])
Canc_copc = subset(topcopc, topcopc$variable %in% Canc_list)
HI_copc = subset(topcopc, topcopc$variable %in% HI_list)

#Function to find outliers below 1% and above 99%
Outliers.fun <- function(x) {  subset(x, x < quantile(x,.01) | x > quantile(x,.99)) }

#Function to find quantiles for boxplot:  [ 1%,  25%, 50%, 75%, 99% ]
Quants.fun  <- function(x)
   {
    quants  <- quantile(x,  probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    quants
  }

#Make boxplot with ggplot2, order pollutants by median, set vertical x-axis, set log scale
ggplot(topcopc3, aes(x = reorder(variable, -value, median), y =value, fill=variable)) + 
    stat_summary(fun.data=f, geom="boxplot") + theme_bw() + theme(axis.text.x = element_text(size =13, lineheight=1, angle = 45))+
    stat_summary(fun.y = o, geom="point") + scale_y_log10()
