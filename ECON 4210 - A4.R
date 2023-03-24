# Econ 4210 A

# import library
library(fpp2)
library(vars)
library(knitr)
library(gridExtra)

# import data
AS4 = read.csv("C:/Users/Meg/Documents/ECON 4210/as4_data.csv")
df = ts(data=AS4, start=c(2000,1), frequency=4)
y = df[,"LOW"]

##########################################################
# VAR

# basic VAR
vardata = log(df[,c(4,5,3)])
plot(vardata, main = "VAR data for Lows Sales", xlab = "Year")

# Check for best Var model & select
VARselect(vardata, lag.max = 9, type = "both", season=4)
VARselect(vardata, lag.max = 9, type = "both", season=4)[[ "selection"]]

VARselect(vardata, lag.max = 9, type = "const", season=4)
VARselect(vardata, lag.max = 9, type = "const", season=4)[[ "selection"]]

# Plot VAR Model & check roots
var.1 = VAR(vardata, p=2, type = "both", season =4)
roots(var.1)
# all roots are below 1, which is good for stability

# Plot all Variables
plot(var.1, names = "PERMIT")
plot(var.1, names =  "CS" )
plot(var.1, names = "LOW")
gridExtra::grid.arrange(p1,p2,p3)

## closer look at residuals
acf(residuals(var.1), type="partial", lag.max=10)

#summary(var.1)
#plot(var.1)

causality(var.1, cause= c("PERMIT", "CS" ))

# impulse responses
var1a.irf <- irf(var.1,  n.ahead = 16, boot = TRUE,  runs=500, seed=99, cumulative=FALSE)

par(mfrow=c(3,3))
plot(var1a.irf, plot.type = "single")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
plot( irf(var.1, response = "MGA", n.ahead = 24, boot = TRUE,  runs=500) , plot.type = "single")
par(mfrow=c(1,1))

# fevd
fevd(var.1, n.ahead = 16)


# forecast
var.fc = predict(var.1, n.ahead= 4)
plot(var.fc)

var.fc$fcst$MGA
exp (var.fc$fcst$MGA[,1])

##  Compute forecasts for 2017:3 to 2018:2
var.fc = predict(var.1, n.ahead= 4)
plot(var.fc)

var.fc$fcst$LOW
convert <-exp (var.fc$fcst$LOW[,1])
fcast <- as.data.frame(convert)
colnames(fcast) <-"Sales in Millions of Dollars"
rownames(fcast) <- c("2017:3","2017:4","2018:1","2018:2")

print(fcast)


