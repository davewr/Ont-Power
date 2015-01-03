# Main

source("R-Scripts/FileDownloads.R")
myFrameList <- GetPower()

# Consolidate files and do fixups in download routine...
# Then cut down data list returned...

#return (list("hrd"=hrd, "hoep"=hoep, "iex"=iex, "wind"=WM))

final <- myFrameList$final
wind <- myFrameList$wind

# Get rid of frame
rm(myFrameList)
 
  