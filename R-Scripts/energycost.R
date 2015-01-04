EnergyCost <- function(ec) {
  # ec <- final
  ecx11 <- ec[ec$Date >= "2003-01-01" & ec$Date < "2015-01-01" ,]
  ecxH11 <- aggregate(cbind(TotWindMWh, Ontario.Demand, Exports, Imports) ~ ym, data = ecx11, sum)
  ecH <-  aggregate(cbind(HOEP) ~ ym, data = ecx11, mean)
  ecH11 <- cbind(ecxH11,HOEP=ecH$HOEP)
  ecH11A <- cbind(ecH11, netship=(ecH11$Exports-ecH11$Imports), windcost=((ecH11$TotWindMWh *125.0)/1000000) ,netval = (((ecH11$Exports-ecH11$Imports)* ecH11$HOEP))/1000000)

  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # HOEP and Export -- uses ech11A
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  #axis(2,  col="blue",col.axis="blue",las=2)
  plot(ecH11A$ym, ecH11A$netval -ecH11A$windcost, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n", ylim=c(min(ecH11A$netval),max(ecH11A$netval)))
  axis(4,  col="red",col.axis="red",las=2, ylim=c(0,50))
  abline(a=0,b=0, col="red")
  par(new = TRUE)
  plot(ecH11A$ym, ecH11A$Exports, col="blue", axes=T, main="HOEP and Ontario Energy Exports- Wind Cost",xlab="Year+Month",ylab="","s")
  
  #Right Margin Text
  mtext("Value in $Millions",col="red", side=4,line=2.5, las=3)  
  # Left Margin Text
  mtext("MWh Exported",side=2,line=2, col="blue")  
  #box()
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  # Wind vs NetSHip -- uses ecH11A
  # Bottom, left, top, right
  par(mar=c(4, 4, 2, 4 ) + 0.1)
  #axis(2,  col="blue",col.axis="blue",las=2)
  plot(ecH11A$ym, ecH11A$netship, col="red",type = "s", axes = F,  xlab = "", ylab = "", bty = "n")#, ylim=c(min(ecH11A$netval),max(ecH11A$netval)))
  axis(4,  col="red",col.axis="red",las=3)#, ylim=c(0,50))
  abline(a=0,b=0, col="red")
  par(new = TRUE)
  plot(ecH11A$ym, ecH11A$TotWindMWh/1000, col="blue", axes=T, main="HOEP and Ontario Energy Exports",xlab="Year+Month",ylab="","s")
  
  #Right Margin Text
  mtext("MWh Net",col="red", side=4,line=2.5, las=3)  
  # Left Margin Text
  mtext("MWh WInd Generation (1000's)",side=2,line=2, col="blue")  
  #box()
  # ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  
  
}