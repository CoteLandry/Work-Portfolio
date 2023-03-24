#########################################################
#  Economic forecasting and analysis
#  Megan Cote (214224398)
#  Assignment 3
#  Nov 9, 2018
##########################################################

# import library and PAYNSA data. Define training and test data
library(fpp2)
library(knitr)
library(gridExtra)
library(grid)

PAYNSAOriginalData = read.csv("F:/Uni - Final Year/ECON 4210/A3/PAYNSA.csv")
y = PAYNSAOriginalData[,"PAYNSA"]
both = ts(data=y,start=c(2009,1),end=c(2018,8),frequency=12)
train = ts(data=y,start=c(2009,1),end=c(2016,12),frequency=12)
test = window(both,start=c(2017,1),end=c(2018,8))
h=length(test)

# TSLM with season and trend
y.TSLM = tslm(train ~ trend + season)
PAYNSATSLM = forecast(y.TSLM,h=h)
p1 = autoplot (PAYNSATSLM)

# ETS
y.ETS = ets(train, model="ZZZ") 
summary(y.ETS)
PAYNSAETS = forecast(y.ETS,h=h)
summary(PAYNSAETS)
p2 = autoplot(PAYNSAETS)

# ARIMA
# After using the auto.arima function, R has chosen the ARIMA model (0,2,1)(0,1,1)[12]

y.ARIMA = auto.arima(train,seasonal=TRUE)
PAYNSAArima = forecast(y.ARIMA,h=h)
p3 = autoplot(PAYNSAArima)

#Random Walk with Drift
PAYNSARW = rwf(train,h=h,drift=TRUE)
p4 = autoplot(PAYNSARW)

# Accuracy Measures
a1 = accuracy(PAYNSATSLM,test)
a2 = accuracy(PAYNSAETS,test)
a3 = accuracy(PAYNSAArima,test)
a4 = accuracy(PAYNSARW,test)

a.table=rbind(a1, a2, a3, a4)
row.names(a.table)<-c('TSLM training','TSLM test', 'ETS training', 'ETS test', 'Arima training', 'Arima test' ,
                      'Random walk with drift test', 'Random walk with drift test')

# order the table according to MASE
a.table<-as.data.frame(a.table)
a.table<-a.table[order(a.table$MASE),]
a.table


# Display actual data in comparison to 4 forecasting methods
autoplot(both) +  
  autolayer(PAYNSATSLM, series="TSLM", PI=FALSE) +
  autolayer(PAYNSAETS, series="ETS", PI=FALSE) +
  autolayer(PAYNSAArima, series="ARIMA", PI=FALSE) + 
  autolayer(PAYNSARW, series="RW", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast")) +
  ggtitle("PAYNSA Forecasts") +
  xlab("Time") + ylab("US Total Non-farm payroll (in Thousands of People)")

# Question 2

# TSLM - Forecast Horizon of 6 Periods
TSLMCV = function(x, h) {
  forecast(tslm(x ~ trend + season, data=x), h=h)
}

# ETS - Forecast Horizon of 6 Periods
ETSCV <- function(x, h) {
  forecast(ets(x,model="ZZZ"), h = h)
}


#ARIMA
ARIMACV <- function(x, h) {
  forecast(auto.arima(x), h=h)
}
#RW with Drift
RWCV <- function(x, h) {
  rwf(x, h=h, drift=TRUE)
}

# set up tsCV for 
PAYNSACV1 = tsCV(both, TSLMCV,h = 6, window = 60)
PAYNSACV2 = tsCV(both, ETSCV,h = 6, window = 60)
PAYNSACV3 = tsCV(both, ARIMACV,h = 6, window = 60)
PAYNSACV4 = tsCV(both, RWCV,h = 6, window = 60)

# calculating MSE
mse1 = colMeans(PAYNSACV1^2, na.rm = TRUE)
mse2 = colMeans(PAYNSACV2^2, na.rm = TRUE)
mse3 = colMeans(PAYNSACV3^2, na.rm = TRUE)
mse4 = colMeans(PAYNSACV4^2, na.rm = TRUE)

mse.table = rbind(mse1,mse2,mse3,mse4)
mse.table

# plot values

g1 = ggplot(data.frame(h = 1:6, MSE = mse1), aes_string(x = "h", y = mse1))+ geom_point()+
  xlab("Forecast Period (H)" )+ ylab("MSE")+ ggtitle("TSLM - MSE")
g2 = ggplot(data.frame(h = 1:6, MSE = mse2), aes_string(x = "h", y = mse2))+ geom_point()+
  xlab("Forecast Period (H)" )+ ylab("MSE")+ ggtitle("ETS - MSE")
g3 = ggplot(data.frame(h = 1:6, MSE = mse3), aes_string(x = "h", y = mse3))+ geom_point()+
  xlab("Forecast Period (H)" )+ ylab("MSE")+ ggtitle("Arima - MSE")
g4 = ggplot(data.frame(h = 1:6, MSE = mse4), aes_string(x = "h", y = mse4))+ geom_point()+
  xlab("Forecast Period (H)" )+ ylab("MSE")+ ggtitle("Random Walk with Drift - MSE")

gridExtra::grid.arrange(g1,g2,g3,g4)