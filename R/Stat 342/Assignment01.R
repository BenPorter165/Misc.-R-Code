
library(RTseries)
BostonRobberies <- read.csv("BostonRobberies.csv")
CO2_MLO_MonthlyData <- scan("CO2_MLO_MonthlyData.txt")
ozone <- read.csv("ozone.csv")
Grocery <- scan("Grocery.txt")

BostonRobberies.ts <- ts(BostonRobberies, frequency=12, start=1966)
CO2_MLO_MonthlyData.ts <- ts(CO2_MLO_MonthlyData, frequency=12, start=1975)
ozone.ts <- ts(ozone, frequency=12, start=1955)
Grocery.ts <- ts(Grocery, frequency=4, start=1992)

BostonRobberies.tsd <- tsd(BostonRobberies.ts, 
                           data.title="Number of Robberies in Boston in each Month from January 1966 to October 1975", 
                           time.units="Year", 
                           response.units="Robberies")
CO2_MLO_MonthlyData.tsd <- tsd(CO2_MLO_MonthlyData.ts, 
                           data.title="Monthly Mean Carbon Dioxide Measured at Mauna Loa Observatory, Hawaii from January 1975 to December 2019", 
                           time.units="Year", 
                           response.units="Mean CO2")
ozone.tsd <- tsd(ozone.ts, 
                           data.title="Measured Amount of Ozone in the Air in Downtown Los Angeles from January 1955 to December 1972", 
                           time.units="Year", 
                           response.units="parts per hundred million")
Grocery.tsd <- tsd(Grocery.ts, 
                           data.title="Quarterly Data on Grocery Sales in the US from 1992-2018", 
                           time.units="Year", 
                           response.units="Billions of dollars")

plot(BostonRobberies.tsd)
plot(CO2_MLO_MonthlyData.tsd)
plot(ozone.tsd)
plot(Grocery.tsd)





