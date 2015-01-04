# Plot WInd from 2011 vs Ont Demad
WindVSODmd <- function(wod) {
  # wod <- final
  twsx11 <- wod[wod$Date >= "2003-01-01" & wod$Date < "2015-01-01" ,]
  twsxH11 <- aggregate(cbind(TotWindMWh, Ontario.Demand, Exports, Imports) ~ ym, data = twsx11, sum)
  twsH <-  aggregate(cbind(HOEP) ~ ym, data = twsx11, mean)
  twsH11 <- cbind(twsxH11,HOEP=twsH$HOEP)
  
  plot( twsxH11$TotWindMWh, twsxH11$HOEP, main="2011 - 2014 Mean HOEP vs Mean WInd Production", xlab = "Mean Wind Production",ylab= "Mean HOEP", col="blue")
  abline(lm(HOEP ~ TotWindMWh , data = twsxH11), col="red")
  
  #par(las=2) # make label text perpendicular to axis
  #par(mar=c(5,8,4,2)) # increase y-axis margin.
  
  plot( twsxH11$Ontario.Demand, twsxH11$HOEP, main="2011 - 2014 HOEP vs Ontario Demand", xlab = "Energy Demand",ylab= "Mean HOEP", col="blue")
  abline(lm(HOEP ~ Ontario.Demand , data = twsxH11), col="red")
  
  #Ontario Demand
  plot(twsxH11$ym, twsxH11$Ontario.Demand, main="Energy Consumption",xlab="Year+Month", ylab="MWh Consumed","s")
  
  # ----------------------------------------------
  #HOEP and Total demand -- uses twsH11
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  plot(twsH11$ym, twsH11$Ontario.Demand, col="blue", axes=T, main="HOEP and Ontario Energy Consumption",xlab="Year+Month",ylab="","s") #, ylim=c(0,15000000))
#  axis(2,  col="blue",col.axis="blue",las=2)
  
  par(new = TRUE)
  plot(twsH11$ym, twsH11$HOEP, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n")#, ylim=c(0,50000))
  axis(4,  col="red",col.axis="red",las=3, ylim=c(0,50))
  
  #Right Margin Text
  mtext("HOEP $/MWh Monthly Average",col="red", side=4,line=2.5)  
  # Left Margin Text
  mtext("MWh Consumed",side=2,line=2, col="blue")  
  #box()
  
  #axis(side=2, ylim=c(0,140000))
  #  mtext("z", side=4, line=3)
  #  lines(twsxH11$ym, twsxH11$Exports type ="l")
  
  # ----------------------------------------------------
  
  # ****************************************************
  #HOEP and Wind Production -- uses twsH11
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  plot(twsH11$ym, twsH11$TotWindMWh, col="blue", axes=T, main="HOEP and Wind Energy Production",xlab="Year+Month",ylab="","s")
  #axis(2,  col="blue",col.axis="blue",las=2)
  
  par(new = TRUE)
  plot(twsH11$ym, twsH11$HOEP, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n")#, ylim=c(0,50000))
  axis(4,  col="red",col.axis="red",las=2, ylim=c(0,50))
  
  #Right Margin Text
  mtext("HOEP $/MWh Monthly Average",col="red", side=4,line=2.5)  
  # Left Margin Text
  mtext("MWh Produced/month",side=2,line=2, col="blue")  
  
  # *****************************************************
  
  
  # ========================================================
  # HOEP and Imports -- uses twsH11
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  plot(twsH11$ym, twsH11$Imports, col="blue", axes=T, main="HOEP and Ontario Energy Imports",xlab="Year+Month",ylab="","s")
  #axis(2,  col="blue",col.axis="blue",las=2)
  
  par(new = TRUE)
  plot(twsH11$ym, twsH11$HOEP, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n")#, ylim=c(0,50000))
  axis(4,  col="red",col.axis="red",las=2, ylim=c(0,50))
  
  #Right Margin Text
  mtext("HOEP $/MWh Monthly Average",col="red", side=4,line=2.5)  
  # Left Margin Text
  mtext("MWh Imported",side=2,line=2, col="blue")  
  
  # ========================================================

  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # HOEP and Export -- uses twsH11
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  plot(twsH11$ym, twsH11$Exports, col="blue", axes=T, main="HOEP and Ontario Energy Exports",xlab="Year+Month",ylab="","s")
  #axis(2,  col="blue",col.axis="blue",las=2)
  
  par(new = TRUE)
  plot(twsH11$ym, twsH11$HOEP, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n")#, ylim=c(0,50000))
  axis(4,  col="red",col.axis="red",las=3, ylim=c(0,50))
  
  #Right Margin Text
  mtext("HOEP $/MWh Monthly Average",col="red", side=4,line=2.5)  
  # Left Margin Text
  mtext("MWh Exported",side=2,line=2, col="blue")  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Wind and Export -- uses twsH11
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  plot(twsH11$ym, twsH11$Exports, col="blue", axes=T, main="Exports and Wind Energy",xlab="Year+Month",ylab="","s", ylim=c(0,3000000))
  #axis(2,  col="blue",col.axis="blue",las=2)
  
  par(new = TRUE)
  plot(twsH11$ym, twsH11$TotWindMWh, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n", ylim=c(0,3000000))
  axis(4,  col="red",col.axis="red",las=3)
  
  #Right Margin Text
  mtext("Wind Energy MWh",col="red", side=4,line=2)  
  # Left Margin Text
  mtext("MWh Exported",side=2,line=2, col="blue")  
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  plot(twsxH11$ym, twsxH11$Exports main="Energy Consumption",xlab="Year+Month", ylab="MWh Consumed","s")
  
  #Export
  plot(twsxH11$ym, twsxH11$Exports, main="Exports",xlab="Year+Month", ylab="MWh Exported","s")
  
  str(twsxH11)
  
  warning()
  #rm wod
  
  
  
  
}

