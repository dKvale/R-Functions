#Define outliers for boxplots using ggplot
require("reshape2")
require("ggplot2")
require("scales")

setwd("M:/MNRiskS 2008 Results/ID top pollutants")
topcopc = read.csv("Inhalation risks by pollutant highest risk pollutants all receptors.csv", header=T, stringsAsFactors=F)
top = topcopc

#Eliminate zeros
topcopc[ topcopc == 0 ] = NA
topcopc = na.omit(topcopc)

#Switch to long format
topcopc = melt(topcopc[,c(6:35)])
names(topcopc)[1] = "Copc"
topcopc$Copc = as.character(topcopc$Copc)

#Separate Cancer and Non-cancer
Canc_list=names(top[,c(7,23,35,9,13,15,33,31,27,25,11,19,21,29,17)])
HI_list=names(top[,-c(1:5,7,23,35,9,13,15,33,31,27,25,11,19,21,29,17)])
Canc_copc = subset(topcopc, topcopc$Copc %in% Canc_list)
HI_copc = subset(topcopc, topcopc$Copc %in% HI_list)

#Function to find outliers below 1% and above 99%
Outliers.fun <- function(x) {  subset(x, x < quantile(x,.01) | x > quantile(x,.99)) }

#Function to find quantiles for boxplot, 1%,  25%, 50%, 75%, 99% 
Quants.fun  <- function(x)
{ quants  <- quantile(x,  probs = c(0.01, 0.25, 0.5, 0.75, 0.99))
  names(quants) <- c("ymin", "lower", "middle", "upper", "ymax")
  quants}

#Make boxplots using default
##Hazard
boxes <- lapply(levels(factor(reorder(HI_copc$Copc, -HI_copc$value, median))), function(x) Quants.fun(subset(HI_copc, Copc==x)$value) )
outs  <- lapply(levels(factor(reorder(HI_copc$Copc, -HI_copc$value, median))), function(x) Outliers.fun(subset(HI_copc, Copc==x)$value) )
par(mar=c(8,4,2,2))
boxplot(boxes, log='y',col = cm.colors(15), las=2, range=0, ylim=c(1e-10,9), names = levels(factor(reorder(HI_copc$Copc, -HI_copc$value, median))))
for(x in 1:length(outs)) points(rep(x,length(outs[[x]])), cex=.9, as.numeric(outs[[x]]), col = alpha("black",.25))

##Cancer
boxes <- lapply(levels(factor(reorder(Canc_copc$Copc, -Canc_copc$value, median))), function(x) Quants.fun(subset(Canc_copc, Copc==x)$value) )
outs  <- lapply(levels(factor(reorder(Canc_copc$Copc, -Canc_copc$value, median))), function(x) Outliers.fun(subset(Canc_copc, Copc==x)$value) )
boxplot(boxes, log='y',col = rainbow(15), las=2, range=0, ylim=c(8e-12,.008), names = levels(factor(reorder(Canc_copc$Copc, -Canc_copc$value, median))))
for(x in 1:length(outs)) points(rep(x,length(outs[[x]])), cex=.9, as.numeric(outs[[x]]), col = alpha("black",.25))


#Make boxplots with ggplot2, order pollutants by median, set vertical x-axis, set to log scale
risk_pal = cm.colors(15)
ggplot(HI_copc, aes(x = reorder(Copc, -value, median), y= value, fill=Copc)) +
    stat_summary(fun.data=Quants.fun, geom="boxplot") + 
    stat_summary(fun.y = Outliers.fun, geom="point") + 
    scale_y_log10() + 
    scale_fill_manual(values=risk_pal, limits=levels(factor(reorder(HI_copc$Copc, -HI_copc$value, median))) ) + 
    labs(x="COPC", y="Hazard index across all receptors", title = "Hazard Index Distributions in MnRisk") +
    theme_bw() + theme(axis.text.x = element_text(size =13, angle = 45, hjust=1, vjust=1))

ggplot(Canc_copc, aes(x = reorder(Copc, -value, median), y= value, fill= Copc)) +
  stat_summary(fun.data=Quants.fun, geom="boxplot") + 
  stat_summary(fun.y = Outliers.fun, geom="point") + 
  scale_y_log10() + 
  labs(x="COPC", y="Cancer risk across all receptors", title = "Cancer Risk Distributions in MnRisk") +
  theme_bw() + theme(axis.text.x = element_text(size =13, angle = 45, hjust=1, vjust=1))
