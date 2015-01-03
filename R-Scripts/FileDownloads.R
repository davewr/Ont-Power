GetPower <- function() {
  #  Download FIles
  #download.file(url, destfile, method, quiet = FALSE, mode = "w",
  #              cacheOK = TRUE,
  #              extra = getOption("download.file.extra"))
  
  require("RCurl")
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
  
  str(DM) # Date = chr rest is int
  str(DA) # as above
  
  print("HOEP FIles")
  #HOEP
  # http://www.ieso.ca/Pages/Power-Data/price.aspx
  HOEP_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HOEP-20150102.csv"
  HM <- read.table(textConnection(getURL(HOEP_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  str(HM)# Date = chr hour = int, hoep=num
  
  # http://www.ieso.ca/Pages/Power-Data/price.aspx
  # Imp/Exp base + addon
  
  print("Import Export Files")
  # http://www.ieso.ca/Pages/Power-Data/Supply.aspx
  IMPEXP_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyImportExportSchedules_2002-2013.csv"   
  IMPEXP_Add <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyImportExportSchedules-20150102.csv"
  IM <- read.table(textConnection(getURL(IMPEXP_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  IA <- read.table(textConnection(getURL(IMPEXP_Add)) , header=T, sep=",", stringsAsFactors = FALSE)
  str(IM) # Screwed up date = chr, hour = int Im/Exp = chr (quoted)
  str(IA) # as above
  
  print("Wind File")
  # Wind Gen -- all
  # http://www.ieso.ca/Pages/Power-Data/Supply.aspx
  WIND_Master <- "http://ieso.ca/imoweb/pubs/marketReports/download/HourlyWindFarmGen_20150102.csv"
  WM <- read.table(textConnection(getURL(WIND_Master)) , header=T, sep=",", stringsAsFactors = FALSE)
  str(WM) # date is chr -- rest is int. Must set NA's to Zero
  
  print ("Done Getting files")
  
  return (list("DM"=DM,"DA"=DA, "HM"=HM, "IM"=IM, "IA"=IA,"WM"=WM))
  
}
