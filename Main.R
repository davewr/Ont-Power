# Main

source("R-Scripts/FileDownloads.R")
myFrameList <- GetPower()

# Consolidate files and do fixups in download routine...
# Then cut down data list returned...

dm <- myFrameList$DM
da <- myFrameList$DA
hm <- myFrameList$HM
im <- myFrameList$IM
ia <- myFrameList$IA
wm <- myFrameList$WM

# Get rid of frame
rm(myFrameList)
 
  