GetPower <- function() {
  #  Download FIles
  #download.file(url, destfile, method, quiet = FALSE, mode = "w",
  #              cacheOK = TRUE,
  #              extra = getOption("download.file.extra"))
  
  require("RCurl")
  require(plyr)
  require(lubridate)
  #read.table(textConnection(getURL("http://.../data.csv",
  #                                 userpwd = "user:pass")),
  #sep=",", header=TRUE)
  
  # hoep <- read.csv("J:/environment/Imports/HOEP/HOEP-20141231A.csv" , header=T, sep=",", stringsAsFactors = FALSE
  
  # Starting Page
  #http://www.ieso.ca/Pages/Power-Data/default.aspx#supply
  
  # Demand # Master + add-on
  # http://www.ieso.ca/Pages/Power-Data/Demand.aspx
  # http://ieso.ca/imoweb/pubs/marketReports/download/HourlyDemands_2002-2013.csv
  # Demand all
  print("Demand Files")
  DEMAND_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyDemands_2002-2013.csv"
  DEMAND_Add <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyDemands-20150102.csv"
  DM <- read.table(textConnection(getURL(DEMAND_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  DA <- read.table(textConnection(getURL(DEMAND_Add)) , header=T, sep=",", stringsAsFactors = FALSE)
  
  #str(DM) # Date = chr rest is int
  #str(DA) # as above
  
  hrd <- rbind( DM, DA)
  #class(hrdemand$Date)=="Date"
  #str(hrd)
  hrd$Date <- as.Date(hrd$Date, format ="%d-%b-%y")
  str(hrd)
  # Switch colums from integer to numeric (it works because detects integer as numeric)
  i <- sapply(hrd, is.numeric)
  hrd[i] <- lapply(hrd[i], as.numeric)
  #good <- complete.cases(hrdemand)
  #sum(good) # is sum same as number of rows in hwf??
  str(hrd)
  
  # Demand working 1/3/2015 11:12AM
  rm(DM,DA) # Get rid of uneeded tables
  rm(DEMAND_Master, DEMAND_Add)
  
  
  print("HOEP FIles")
  #HOEP
  # http://www.ieso.ca/Pages/Power-Data/price.aspx
  HOEP_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HOEP-20150102.csv"
  hoep <- read.table(textConnection(getURL(HOEP_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  str(hoep)# Date = chr hour = int, hoep=num
  hoep$Date <- as.Date(hoep$Date, format ="%d-%b-%y")#"%m/%d/%Y")
  i <- sapply(hoep, is.numeric)
  hoep[i] <- lapply(hoep[i], as.numeric)
  # str(hoep)
  
  # http://www.ieso.ca/Pages/Power-Data/price.aspx
  # Imp/Exp base + addon
  
  print("Import Export Files")
  # http://www.ieso.ca/Pages/Power-Data/Supply.aspx
  IMPEXP_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyImportExportSchedules_2002-2013.csv"   
  IMPEXP_Add <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyImportExportSchedules-20150102.csv"
  IM <- read.table(textConnection(getURL(IMPEXP_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  IA <- read.table(textConnection(getURL(IMPEXP_Add)) , header=T, sep=",", stringsAsFactors = FALSE)

  # COmbine tables and format Date correctly
  iex <- rbind( IM, IA)
  iex$Date <- as.Date(iex$Date, format ="%d-%b-%y")
  str(iex)

    #Create vecttors of Import and Export -- must clean commas 
  x1 <- iex[,3] # Imports
  x2 <- iex[,4] # Exports
  
  # Clue on removing commas came from here:
  # https://stat.ethz.ch/pipermail/r-help/2008-January/152350.html
  
  # Create new vectors with cleaned data
  z1 <- unname(sapply(sapply(x1, gsub, patt=",", replace=""), as.numeric))
  z2  <- unname(sapply(sapply(x2, gsub, patt=",", replace=""), as.numeric))
  # Bind Vectors to a dat frame with old conntents as well
  yy <- cbind(iex,Imports1=z1, Exports1=z2)
  # Keep only the "good" columns
  myCols <- c("Date","Hour","Imports1","Exports1")
  myCols
  iex <- yy[myCols]
  # Rename colums using PLYR -- see require(plyr) above
  iex <- rename(iex, c("Imports1"="Imports", "Exports1"="Exports"))
  
  #rm(IMPEXP_Master, IMPEXP_Add)
  rm(x1,x2,z1,z2)
  rm(IM,IA,yy)
  
  print("Wind File")
  # Wind Gen -- all
  # http://www.ieso.ca/Pages/Power-Data/Supply.aspx
  WIND_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyWindFarmGen_20150102.csv"
  wind <- read.table(textConnection(getURL(WIND_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  str(wind) # date is chr -- rest is int. Must set NA's to Zero
  wind$Date <- as.Date(wind$Date, format ="%d-%b-%y")
  str(wind)
  
  # Set NA vlaues to Zero
  wind[is.na(wind)] <- 0
  
  i <- sapply(wind, is.numeric)
  wind[i] <- lapply(wind[i], as.numeric)
  wind <- rename(wind, c("Total.Wind..MWh."="TotWindMWh"))
  
  print ("Done Getting files...")
  
  print("Combining Files")
  
  # Mergefiles 
  total1 <- merge(hrd,hoep, by=c("Date","Hour"), stringsAsFactors = FALSE) 
  total <- merge(total1,iex, by=c("Date","Hour"), stringsAsFactors = FALSE) 
  #names(total)<-c("TotWindMWh")
  
  
  totalf <- total[total$Date < "2006-03-01",]
  totalf$TotWindMWh   <- 0
  str(totalf)
  
  www <- wind[1:3]
  final1 <- merge(total,www, by=c("Date","Hour"), stringsAsFactors = FALSE) 
  
  final <- rbind(totalf,final1)
  
  finala <- cbind(final, ym = (year(final$Date) + (month(final$Date)-1 )/ 12 ), yw = (year(final$Date) + (week(final$Date)-1)/52 ) , year = year(final$Date), month = format(month(final$Date),digits=2),  day = day(final$Date), week = week(final$Date))
  final <- finala
  
  write.table(final, file = "Data/final.csv", sep = ",", col.names=T, row.names=F)
  write.table(wind, file = "Data/wind.csv", sep = ",", col.names=T, row.names=F)
  
  return (list("final"=final, "wind"=wind))
  
}
