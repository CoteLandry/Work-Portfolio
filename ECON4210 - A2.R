# Assignment 2

library(fpp2)

PAYNSAOriginalData = read.csv("F:/Uni - Final Year/ECON 4210/A2/PAYNSA.csv")
PAYNSAData = ts(data=PAYNSAOriginalData,start=c(2009,1),end=c(2016,12),frequency=12)
PAYNSAData2017 = ts(data=PAYNSAOriginalData,start=c(2009,1),end=c(2018,8),frequency=12)
test = ts(data=PAYNSAData,start=c(2017,1),end=c(2018,8),frequency=12) 
"A) TSLM"

PAYNSA = PAYNSAData[,c("PAYNSA")]
PAYNSAActual = PAYNSAData2017[,c("PAYNSA")]
PAYNSASeasonal = tslm(PAYNSA ~ trend + season,data=PAYNSAData)

summary(PAYNSASeasonal)
# From first glance the model seems to fit decently well.
# Multiple R-Squared is 0.9325 & Adjusted R Squared is 0.9228, 
# which is close to 1 & signals the forecasts are closely matching actual
# which is  the sign of a well fitted model.

mean(PAYNSA)

# Residual standard error is 1412 on 83 degrees of freedom.
# In comparison to the mean for PAYSNA or y (136164.5),
# this is significantly low, which indicates this is a well fitting model

# F stat is 95.57, which is much larger than 1 & indicates
# that there is a relationship between these trend/seasonal components
# and PAYNSA

checkresiduals(PAYNSASeasonal)
# ACF is decreasing linearly, which signals an aspect is missing
# The residuals histogram is skewed to the left with a spike between
# -2000 and 0 that might affect the accuracy of prediction intervals
# High amount of residuals

# Fourier
PAYNSAFourier6 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=6),data=PAYNSAData)
PAYNSAFourier5 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=5),data=PAYNSAData)
PAYNSAFourier4 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=4),data=PAYNSAData)
PAYNSAFourier3 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=3),data=PAYNSAData)
PAYNSAFourier2 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=2),data=PAYNSAData)
PAYNSAFourier1 = tslm(PAYNSA ~ trend + fourier(PAYNSA,K=1),data=PAYNSAData)


CV(PAYNSAFourier6)
CV(PAYNSAFourier5)
CV(PAYNSAFourier4)
CV(PAYNSAFourier3)
CV(PAYNSAFourier2)
CV(PAYNSAFourier1)

# K value of 2 produces the lowest CV,AIC, AICc, and BIC values
# K value of 2 also produces the highest adjusted R squared

summary(PAYNSAFourier2)
# R squared & Adjusted R squared are both close to 1, signifying a wellfitting model
# Residual standard error is 1392 on 90 degrees of freedom.
# In comparison to the mean for PAYSNA or y (136164.5),
# this is significantly low, which indicates this is a well fitting model
# F stat is higher than 1, which also signifies there is a 
# relationship between the fourier and trend components and PAYNSA or y

checkresiduals(PAYNSAFourier2)
# Still surprised.
# ACF is decreasing both linearly indicating a trend, but also
# on every 6th lag, there is an irregular spike indicating a seasonal
# component may be midding

# the residual histogram still remains skewed to the left with a spike
# between -2000 and 0

# the residual time plot has seasonal spikes
# before 2010, the data has a sharp decrease
# then it follows a slight upward trend

# F)
autoplot(PAYNSA) + 
  autolayer(forecast(PAYNSASeasonal,h=20)) +
  ylab("US Total Non-farm payroll (in Thousands of People") + 
  xlab("Time")

autoplot(PAYNSA) + 
  autolayer(forecast(PAYNSAFourier2, newdata=data.frame(fourier(PAYNSA,2,20))),"Number 2") +
  ylab("US Total Non-farm payroll (in Thousands of People") + 
  xlab("Time")

#G) 
CV(PAYNSASeasonal)
CV(PAYNSAFourier2)
# The harmonic regression minimizes CV AICc, AIC, and BIC and has the 
# highest adjusted R squared, so the forecast accuracy is much better.

#H) 
autoplot(PAYNSAActual) +
  autolayer(forecast(PAYNSAFourier2, newdata=data.frame(fourier(PAYNSA,2,20))),"Fourier Forecast",PI=FALSE) +
  autolayer(forecast(PAYNSASeasonal,h=20),series="Trend+Seasonal Forecast",PI=FALSE) +
  guides(colour=guide_legend(title="Forecast")) +
  ylab("US Total Non-farm payroll (in Thousands of People") + 
  xlab("Time")
# The forecasts do not do well in comparison to the outliers
# because there is a very sharp drop that occurs in this year

regressionForecast = forecast(PAYNSASeasonal,h=20)
accuracy(regressionForecast,test[,c("PAYNSA")])

         