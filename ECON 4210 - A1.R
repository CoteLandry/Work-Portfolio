#########################################################
#  Economic forecasting and analysis
#  Megan Cote (214224398)
#  Assignment 1
#  September 24, 2018
##########################################################

library(fpp2)

# Question 1
# import Walmart data
Walmart_Sales = read.csv("F:/Uni - Final Year/ECON 4210/A1/Walmart_Sales.csv")
WalmartSalesData = ts(data=Walmart_Sales,start=c(2003,3),end=c(2018,2),frequency=4)

# plot of Walmart data
autoplot(WalmartSalesData[,"Revenue"]) +
  ggtitle("Walmart Sales Data") +
  xlab("Quarters") +
  ylab("Walmart Revenue in Billions")

# seasonal plot of Walmart data
ggseasonplot(WalmartSalesData[,"Revenue"]) +
  ylab("Walmart Revenue in Billions") +
  ggtitle("Seasonal plot: Walmart quarterly sales")

# ACF plot of Walmart data
Acf(WalmartSalesData[,"Revenue"], main = "Walmart Sales", lwd=3)
ggAcf(WalmartSalesData[,"Revenue"])

# Question 2
# import GAP data. set the training and test data sets
A1Data = read.csv(file="C:/Users/Meg/Documents/as1_data..csv")
GAPData = ts(data = A1Data,start=c(2000,1),end=c(2018,2),frequency=4)
GAPTrainingData = window(GAPData,start=c(2000,1),end=c(2013,4),frequency=4)
GAPTestData = window(GAPData,start=c(2014,1),end=c(2018,2),frequency=4)

yGAP = GAPTrainingData[,"sales"]

A1Avg = meanf(yGAP,h=20)
A1Naive = naive(yGAP,h=20)
A1SNaive = snaive(yGAP,h=20)

autoplot(A1Avg)
autoplot(A1Naive)
autoplot(A1SNaive)

autoplot(yGAP) +  
  autolayer(A1Avg, series="Mean", PI=FALSE) + 
  autolayer(A1Naive, series="Naïve", PI=FALSE) +
  autolayer(A1SNaive, series="Seasonal naïve", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast")) +
  ggtitle("GAP Quarterly Sales") +
  xlab("Quarters") + ylab("Sales")

accuracy(A1Avg,GAPTestData[,"sales"])
accuracy(A1Naive,GAPTestData[,"sales"])
accuracy(A1SNaive,GAPTestData[,"sales"])  