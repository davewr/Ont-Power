#total <- merge(hoep,hwf, by=c("Date","Hour"), stringsAsFactors = FALSE) 
#head(total)

# Merge other files here...

# datafile1 <- read.csv("c:/datafile1.csv", header=T, sep=",")
# datafile2 <- read.csv("c:/datafile2.csv", header=T, sep=",")
# datafile <- rbind(datafile1,datafile2)

# HOEP
hoep <- read.csv("J:/environment/Imports/HOEP/HOEP-20141231A.csv" , header=T, sep=",", stringsAsFactors = FALSE)

class(hoep$Date)=="Date"
hoep$Date <- as.Date(hoep$Date, format ="%m/%d/%Y")
str(total)

# DEMAND

hrdmdmaster <- read.csv("J:/environment/Imports/HourlyDemand/HourlyDemands_2002-2013AAA.csv" , header=T, sep=",", stringsAsFactors = FALSE)
hrdmdsupp <- read.csv("J:/environment/Imports/HourlyDemand/HourlyDemands-20141231A.csv" , header=T, sep=",", stringsAsFactors = FALSE)
hrdemand <- rbind( hrdmdmaster, hrdmdsupp)
class(hrdemand$Date)=="Date"
str(hrdemand)
hrdemand$Date <- as.Date(hrdemand$Date, format ="%m/%d/%Y")
# Switch colums from integer to numeric (it works because detects integer as numeric)
i <- sapply(hrdemand, is.numeric)
hrdemand[i] <- lapply(hrdemand[i], as.numeric)
str(hrdemand)
good <- complete.cases(hrdemand)
sum(good) # is sum same as number of rows in hwf??


# WIND TURBINES

hwf <- read.csv("J:/environment/Imports/HourlyWindGen/HourlyWindFarmGen_20141226A.csv", header=T, sep=",", , stringsAsFactors = FALSE)
hwf$Date <- as.Date(hwf$Date, format="%m/%d/%Y")
str(hwf)

# Switch colums from integer to numeric (it works because detects integer as numeric)
i <- sapply(hwf, is.numeric)
hwf[i] <- lapply(hwf[i], as.numeric)
str(hwf)

good <- complete.cases(hwf)
sum(good) # is sum same as number of rows in hwf??


# IMPORT EXPORT
impexpmaster <- read.csv("J:/environment/Imports/HourlyImpExp/HourlyImportExportSchedules_2002-2013.csv", header=T, sep=",", stringsAsFactors = FALSE)
# Date -- #b is short form text month
impexpmaster$Date <- as.Date(impexpmaster$Date, format ="%d-%b-%y")

impexpsupp <- read.csv("J:/environment/Imports/HourlyImpExp/HourlyImportExportSchedules-20141231A.csv", header=T, sep=",", stringsAsFactors = FALSE)
impexpsupp$Date <- as.Date(impexpsupp$Date, format ="%m/%d/%Y")

impexp <- rbind(impexpmaster, impexpsupp)
str(impexp)

# Find character columns and set to numeric
# This is raw text so original import is charcters instead of numbers
i <- sapply(impexp, is.character)
i
impexp[i] <- lapply(impexp[i], as.numeric)
i <- sapply(impexp, is.numeric)
i
impexp[i] <- lapply(impexp[i], as.numeric)

str(impexp)

# Set NA vlaues to Zero
impexp[is.na(impexp)] <- 0


# This is a check....
test <- impexp
head(test)
good <- complete.cases(test)
test2 <- test[good,]
sum(good)

# end check

str(impexp)
head(impexp)
tail(impexp)

# START TOTAL FILES
# _________________

total <- merge(hoep,hrdemand, by=c("Date","Hour"), stringsAsFactors = FALSE) 
totalA <- merge(total,impexp, by=c("Date","Hour"), stringsAsFactors = FALSE) 
totalwind <- merge(hoep,hwf, by=c("Date","Hour"), stringsAsFactors = FALSE) 
totalwindA <- merge(totalwind,hrdemand, by=c("Date","Hour"), stringsAsFactors = FALSE) 
totalwindB <- merge(totalwind,impexp, by=c("Date","Hour"), stringsAsFactors = FALSE) 



totalA_S <- totalA[ with(totalA, order(Date, Hour)),]
head(totalA_S, 20)
tail(totalA_S,20)
str(totalA_S)

totalwindB_S <- totalwindB[ with(totalwindB, order(Date, Hour)),]
head(totalwindB_S, 20)
tail(totalwindB_S,20)
str(totalwindB_S)

write.table(totalA, file = "totalA.csv", sep = ",", col.names=T, row.names=F)
write.table(totalwindB, file = "totalwindB.csv", sep = ",", col.names=T, row.names=F)

getwd()



#dd[with(dd, order(-z, b)), ]
#dd[ order(-dd[,4], dd[,1]), ]

tws <- totalwindB_S

# merge tws with demad...
totalW_All <- merge(tws,hrdemand, by=c("Date","Hour"), stringsAsFactors = FALSE) 
str(totalW_All)
twa <- totalW_All

plot(twa$HOEP ~twa$Total.Wind..MWh )
plot(twa$HOEP ~twa$Hour )


hist(tws$HOEP, breaks = 20)
fivenum(tws$HOEP)
summary(tws$HOEP)

plot(tws$Date+tws$Hour, tws$Total.Wind..MWh., col="blue" )

str(tws)
wbd <- aggregate(cbind(Total.Wind..MWh.,Exports) ~ Date, data = twa, sum)
str(wbd)
plot(wbd$Date, wbd$Total.Wind..MWh. )

#wbd2 <- aggregate(cbind(Total.Wind..MWh.,Exports) ~ Date, data = tws, sum)
#str(wbd)

library(lubridate)
#aggregate(z, day, mean)
tws-m <- aggregate(twa, Date$month, sum)
#aggregate(z, year, mean)
head(tws)

# Should be able to do this for yearweek etc...
twsX <- cbind(twa, ym = (year(tws$Date) + month(twa$Date)/100 ),yw = (year(tws$Date) + week(twa$Date)/100 ) , year = year(tws$Date), month = format(month(twa$Date),digits=2),  day = day(twa$Date), week = week(twa$Date))
twsXX <- cbind(twsX, yearmon = paste(twsX$year, twsX$month, sep=""))
head(twsXX)
tail(twsXX)
# --------------------------------------


# Now some test aggregates...
# Exports vs wind
twsX2 <- aggregate(cbind(Total.Wind..MWh., Exports, Ontario.Demand) ~ yearmon, data = twsXX, sum)
head(twsX2)
tail(twsX2)


plot(twsX2$yearmon,twsX2$Exports, col="green", main = "Exports by Year and Month", xlab ="year-mon", ylab = "MWh")
lines(twsX2$yearmon,twsX2$Total.Wind..MWh., col="red")

plot( twsX2$Total.Wind..MWh., twsX2$Exports)

#abline(lm(y ~ x, data=test))

abline(lm(Exports ~ Total.Wind..MWh. , data = twsX2))

twsX2


# Now try HOEP vs wind... aggregated monthly
twsXHOEP <- aggregate(cbind(Total.Wind..MWh., HOEP, Ontario.Demand) ~ yearmon, data = twsXX, mean)
head(twsXHOEP)
tail(twsXHOEP)
str(twsXHOEP)

plot( twsXHOEP$Total.Wind..MWh., twsXHOEP$HOEP, main="Mean HOEP vs Mean WInd Production", xlab = "Mean Wind Production",ylab= "Mean HOEP", col="blue")
abline(lm(HOEP ~ Total.Wind..MWh. , data = twsXHOEP), col="red")

plot(twsXHOEP$Total.Wind..MWh.,twsXHOEP$Ontario.Demand, col="red", main="Wind Production vs Ontario Demand", xlab="Total Wind Production MWh", ylab = "Ontario Demand MWh")
abline(lm(Ontario.Demand ~ Total.Wind..MWh. , data = twsXHOEP), col="blue")

# -----------------------------------------------------------------

twsx11 <- twsXX[twsXX$Date >= "2011-01-01" ,]
str(twsx11)
twsXHOEP11 <- aggregate(cbind(Total.Wind..MWh., HOEP, Ontario.Demand) ~ yearmon, data = twsx11, mean)

plot( twsXHOEP11$Total.Wind..MWh., twsXHOEP11$HOEP, main="2011 - 2014 Mean HOEP vs Mean WInd Production", xlab = "Mean Wind Production",ylab= "Mean HOEP", col="blue")
abline(lm(HOEP ~ Total.Wind..MWh. , data = twsXHOEP11), col="red")

head(twsXHOEP11)

# Wind and Ontario Demand
plot( twsXHOEP11$Total.Wind..MWh., twsXHOEP11$Ontario.Demand, main="2011 - 2014 Mean Ontario Demand  vs Mean Wind Production", xlab = "Mean Wind Production",ylab= "Mean HOEP", col="blue")
abline(lm(Ontario.Demand ~ Total.Wind..MWh. , data = twsXHOEP11), col="red")

str(twsXHOEP11)
head(twsXHOEP11)
tail(twsXHOEP11)
str(twsXHOEP11)
twsXHOEP11
head(twsXX)
str(twsXX)

twsxyz <- cbind(twsXX)
str(twsxyz)

twsZZ <- aggregate(cbind(Total.Wind..MWh.,  Ontario.Demand ) ~ yearmon, data = twsXX, sum)
str(twsZZ)
head(twsZZ)
plot (twsZZ$yearmon, twsZZ$Ontario.Demand  ,  col="green",  type="l", main="Ontario Demand by year and month", xlab = "Year-Month",ylab= "Mean MWh")

plot (twsZZ$yearmon, twsZZ$Total.Wind..MWh. , col="red", type ="l")

