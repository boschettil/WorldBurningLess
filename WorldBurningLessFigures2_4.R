WorldBurningLessFigures2_4.R <- function(zone = 1, crop = 1, fig = 2, start.month = 3, col = "red")
{

  library(rlang)
  #
  # Code to generated Figures 2 and Figure 4 
  #
  #to plot figure 2
  # WorldBurningLessFigures.R(zone = 15, fig = 2)
  #
  # to plot figure 4
  # sort zones from largest to smallest average annual area burned
  # > zones <- c(15, 12, 2, 6, 3, 4, 9, 14, 13, 11, 8, 10, 5, 7, 1)
  # par(mfrow=c(5,3))
  # for (zone in 2:15) WorldBurningLessFigures.R(zones[zone], fig=2)
  #
  
  #datapath = "./Data"
  if((zone < 1) || (zone > 15)) print(
    "Geographic Zone in range 1-15 ")
  #
  #
  #
  #
  par(mar = c(2,4,4,2))
  
  if (crop == 1) {
    z <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.yearly.txt",sep=""),
                    header = T)
    colnames <- names(z)
    #
  }
  
  if (crop == 0) {
    z <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.nocrop.yearly.txt",sep=""),
                    header = T)
    colnames <- names(z)
    #
  }
  
  y <- z[[zone + 2]]
  n.years <- length(y) %/% 12
  years = (unique(z$YEAR))[1:n.years+1]

  
  if(fig == 2) {
    #
    z.all <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.yearly.txt",sep=""),
                        header = T)
    z.nocrop <- read.table(file = paste(datapath,"/Summary.Zone.AgMask.totals.nocrop.yearly.txt",sep=""),
                           header = T)
    colnames <- names(z.all)

    
    y.all <- z.all[[zone + 2]]/1000000
    y.nocrop <- z.nocrop[[zone + 2]]/1000000
    
    yearlytotal.fy.all <- rep(0.0,n.years)
    yearlytotal.fy.nocrop <- rep(0.0,n.years)
    
    startmonth.series = z.all$MONTH[1]
    startmonth.fy = start.month+13-startmonth.series
    
    for(i in 1:(n.years)){
      yearlytotal.fy.all[i]=sum(y.all[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
      yearlytotal.fy.nocrop[i]=sum(y.nocrop[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
    }
    
    yearlytotal.fy.crop=yearlytotal.fy.all-yearlytotal.fy.nocrop 
    
    ymin = 0
    ymax = max(c(yearlytotal.fy.all,yearlytotal.fy.nocrop,yearlytotal.fy.crop,5))
    plot(years, yearlytotal.fy.all, ylim=c(ymin,ymax), ylab="", xlab = "year")
    title(ylab = expression(paste("area burned [10"^"6"," km"^"2","]", sep="")),line=2, cex.lab=1 )
    
    lines(years, yearlytotal.fy.all, type = "b", pch = 19)
    
    lines(years, yearlytotal.fy.nocrop, type = "b", col="red", pch = 19)
    
    lines(years, yearlytotal.fy.crop, type = "b", col ="#336600", pch = 19)
    
    linearMod.fy.all <- lm(yearlytotal.fy.all ~ years)
    linearMod.fy.nocrop <- lm(yearlytotal.fy.nocrop ~ years)
    linearMod.fy.crop <- lm(yearlytotal.fy.crop ~ years)
    
    modelSummary.fy.all <- summary(linearMod.fy.all)
    modelCoeffs.fy.all  <- modelSummary.fy.all$coefficients
    offset.fy.all <- modelCoeffs.fy.all[1,1]
    gain.fy.all   <- modelCoeffs.fy.all[2,1]
    p.fy.all <- modelCoeffs.fy.all[2,4]
    
    if (p.fy.all < 0.1) linetype = "solid" else linetype = "dotted"
    #tit2 = paste (round(gain.fy.all), "km2/yr (all lands)  ")
    
    
    lines(c(min(years),max(years)),c(offset.fy.all+min(years)*gain.fy.all,offset.fy.all+max(years)*gain.fy.all), type="l", lty=linetype)
    
    modelSummary.fy.nocrop <- summary(linearMod.fy.nocrop)
    modelCoeffs.fy.nocrop <- modelSummary.fy.nocrop$coefficients
    offset.fy.nocrop <- modelCoeffs.fy.nocrop[1,1]
    gain.fy.nocrop <- modelCoeffs.fy.nocrop[2,1]
    p.fy.nocrop <- modelCoeffs.fy.nocrop[2,4]
    
    if (p.fy.nocrop < 0.1) linetype = "solid" else linetype = "dotted"
    
    lines(c(min(years),max(years)),c(offset.fy.nocrop+min(years)*gain.fy.nocrop,offset.fy.nocrop+max(years)*gain.fy.nocrop), type="l", lty = linetype, col = "red")
    
    modelSummary.fy.crop <- summary(linearMod.fy.crop)
    modelCoeffs.fy.crop <- modelSummary.fy.crop$coefficients
    offset.fy.crop <- modelCoeffs.fy.crop[1,1]
    gain.fy.crop <- modelCoeffs.fy.crop[2,1]
    p.fy.crop <- modelCoeffs.fy.crop[2,4]
    
    if (p.fy.crop < 0.1) linetype = "solid" else linetype = "dotted"
    
    lines(c(min(years),max(years)),c(offset.fy.crop+min(years)*gain.fy.crop,offset.fy.crop+max(years)*gain.fy.crop), type="l", lty = linetype, col = "#336600")
    
    
    gain.fy.all.relative.percent = gain.fy.all/mean(yearlytotal.fy.all)*100
    gain.fy.nocrop.relative.percent = gain.fy.nocrop/mean(yearlytotal.fy.nocrop)*100
    gain.fy.crop.relative.percent = gain.fy.crop/mean(yearlytotal.fy.crop)*100
    
    gain2 = sprintf("%3.1f",gain.fy.all*1000)
    gain2rel = sprintf("%3.2f",gain.fy.all.relative.percent)
    if (p.fy.all < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.all),")",sep="") 
    tit2 =expr(paste("all land:                      ",!!gain2," 10"^"3", " km"^"2","yr"^"-1"," ",!!p.txt))
    mtext (tit2, cex=0.9, side = 3, line = -1.5, adj = 0,  at = 2012.5)
    
    
    gain3 = round(gain.fy.nocrop*1000, digits = 1)
    gain3rel = sprintf("%3.2f",gain.fy.nocrop.relative.percent)
    if (p.fy.nocrop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.nocrop),")",sep="") 
    tit3 =expr(paste("non-agricultural land: ",!!gain3," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    mtext (tit3, cex=0.9, side = 3, line = -2.5, adj = 0, at = 2012.5, col = 2)
    
    gain4 = round(gain.fy.crop*1000, digits = 1)
    gain4rel = sprintf("%3.2f",gain.fy.crop.relative.percent)
    if (p.fy.crop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.crop),")",sep="") 
    tit4 =expr(paste("agricultural land:          ",!!gain4," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    mtext (tit4, cex=0.9, side = 3, line = -3.5, adj = 0, at = 2012.5, col = "#336600")
    
    return(c(p.fy.all, p.fy.nocrop, p.fy.crop))
  }
  
  if(fig == 4) {
    #
    z.all <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.yearly.txt",sep=""),
                        header = T)
    z.nocrop <- read.table(file = paste(datapath,"/Summary.Zone.AgMask.totals.nocrop.yearly.txt",sep=""),
                           header = T)
    colnames <- names(z.all)
    #
    
    
    y.all <- z.all[[zone + 2]]
    y.all.global <- z.all[[17]]
    y.nocrop <- z.nocrop[[zone + 2]]
    
    yearlytotal.fy.all <- rep(0.0,n.years)
    yearlytotal.fy.nocrop <- rep(0.0,n.years)
    
    yearlytotal.fy.all.global <- rep(0.0,n.years)
    
    
    startmonth.series = z.all$MONTH[1]
    startmonth.fy = start.month+13-startmonth.series
    
    for(i in 1:(n.years)){
      yearlytotal.fy.all[i]=sum(y.all[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
      yearlytotal.fy.nocrop[i]=sum(y.nocrop[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
      yearlytotal.fy.all.global[i]=sum(y.all.global[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
    }
    
    
    
    
    yearlytotal.fy.all.percent=(yearlytotal.fy.all/yearlytotal.fy.all.global)*100
    yearlytotal.fy.all.avgpercent = mean(yearlytotal.fy.all.percent)
    
    title <- paste(colnames[zone + 2], " ")
    ymin = min(c(yearlytotal.fy.all,yearlytotal.fy.nocrop))
    ymax = max(c(yearlytotal.fy.all,yearlytotal.fy.nocrop))
    ymax = ymax+0.1*(ymax-ymin)
    plot(years, yearlytotal.fy.all, ylim=c(ymin,ymax), ylab="", xlab = "year")
    title(ylab = expression(paste("area burned [km"^"2","]", sep="")),line=2.2, cex.lab=1.1 )
    
    lines(years, yearlytotal.fy.all, type = "b", pch=19)
    
    lines(years, yearlytotal.fy.nocrop, type = "b", col="red", pch=19)
    
    linearMod.fy.all <- lm(yearlytotal.fy.all ~ years)
    linearMod.fy.nocrop <- lm(yearlytotal.fy.nocrop ~ years)
    
    modelSummary.fy.all <- summary(linearMod.fy.all)
    modelCoeffs.fy.all  <- modelSummary.fy.all$coefficients
    offset.fy.all <- modelCoeffs.fy.all[1,1]
    gain.fy.all   <- modelCoeffs.fy.all[2,1]
    p.fy.all <- modelCoeffs.fy.all[2,4]
    
    
    
    if (p.fy.all < 0.1) linetype = "solid" else linetype = "dotted"
    #tit2 = paste (round(gain.fy.all), "km2/yr")
    gain.txt = sprintf("%3.2f",(gain.fy.all/1000))
    if (p.fy.all < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.all),")",sep="") 
    
    #if (p.fy.all < 0.01) tit2  = expr(paste(!!gain.txt," 10"^"3"," km"^"2","yr"^"-1",""^"(***)"))
    tit2  = expr(paste(!!gain.txt," 10"^"3"," km"^"2","yr"^"-1",!!p.txt))
    
    
    
    
    
    lines(c(min(years),max(years)),c(offset.fy.all+min(years)*gain.fy.all,offset.fy.all+max(years)*gain.fy.all), type="l", lty=linetype)
    
    modelSummary.fy.nocrop <- summary(linearMod.fy.nocrop)
    modelCoeffs.fy.nocrop <- modelSummary.fy.nocrop$coefficients
    offset.fy.nocrop <- modelCoeffs.fy.nocrop[1,1]
    gain.fy.nocrop <- modelCoeffs.fy.nocrop[2,1]
    p.fy.nocrop <- modelCoeffs.fy.nocrop[2,4]
    
    if (p.fy.nocrop < 0.1) linetype = "solid" else linetype = "dotted"
    
    gain.txt = sprintf("%3.2f",(gain.fy.nocrop/1000))
    if (p.fy.nocrop < 0.01)      sig.symb = "(***)"
    else if (p.fy.nocrop < 0.05) sig.symb = "(**)"
    else if (p.fy.nocrop < 0.1)  sig.symb = "(*)"
    else sig.symb = ""
    
    if (p.fy.nocrop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.nocrop),")",sep="") 
    
    
    tit3  = expr(paste(!!gain.txt," 10"^"3"," km"^"2","yr"^"-1",!!p.txt))
    
    lines(c(min(years),max(years)),c(offset.fy.nocrop+min(years)*gain.fy.nocrop,offset.fy.nocrop+max(years)*gain.fy.nocrop), type="l", lty = linetype, col = "red")
    
    
    mtext(title, font = 2, line = 1.2 , adj = 0, cex = 0.8,  at  = 2000.5)
    mtext(tit2, line = 1,  adj = 0, cex = 0.8,  at  = 2003.5)
    mtext(tit3, line = 1,  adj = 0, cex = 0.8,  at  = 2013.5, col = 2)
    
    mtext (paste(round(yearlytotal.fy.all.avgpercent, digits =2),"%",sep=""), cex=0.9, side = 3, line = -1.5, at = 2019.5)
    return(c(max(yearlytotal.fy.all), mean(yearlytotal.fy.all))) #
    #
  }
  if(fig == 3) {
    #
    z.all <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.yearly.txt",sep=""),
                        header = T)
    z.nocrop <- read.table(file = paste(datapath,"/Summary.Zone.AgMask.totals.nocrop.yearly.txt",sep=""),
                           header = T)
    colnames <- names(z.all)
    
    
    y.all <- z.all[[zone + 2]]/1000000
    y.nocrop <- z.nocrop[[zone + 2]]/1000000
    
    yearlytotal.fy.all <- rep(0.0,n.years)
    yearlytotal.fy.nocrop <- rep(0.0,n.years)
    
    startmonth.series = z.all$MONTH[1]
    startmonth.fy = start.month+13-startmonth.series
    
    for(i in 1:(n.years)){
      yearlytotal.fy.all[i]=sum(y.all[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
      yearlytotal.fy.nocrop[i]=sum(y.nocrop[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
    }
    
    yearlytotal.fy.crop=yearlytotal.fy.all-yearlytotal.fy.nocrop 
    
    ymin = min(c(yearlytotal.fy.all,0.5))
    ymax = max(c(yearlytotal.fy.all,yearlytotal.fy.nocrop,yearlytotal.fy.crop))
    plot(years, yearlytotal.fy.all, ylim=c(ymin,ymax), ylab="", xlab = "year")
    title(ylab = expression(paste("area burned [10"^"6"," km"^"2","]", sep="")),line=2, cex.lab=1 )
    
    lines(years, yearlytotal.fy.all, type = "b", pch = 19)
    
    #lines(years, yearlytotal.fy.nocrop, type = "b", col="red", pch = 19)
    
    #lines(years, yearlytotal.fy.crop, type = "b", col ="#336600", pch = 19)
    
    linearMod.fy.all <- lm(yearlytotal.fy.all ~ years)
    linearMod.fy.nocrop <- lm(yearlytotal.fy.nocrop ~ years)
    linearMod.fy.crop <- lm(yearlytotal.fy.crop ~ years)
    
    modelSummary.fy.all <- summary(linearMod.fy.all)
    modelCoeffs.fy.all  <- modelSummary.fy.all$coefficients
    offset.fy.all <- modelCoeffs.fy.all[1,1]
    gain.fy.all   <- modelCoeffs.fy.all[2,1]
    p.fy.all <- modelCoeffs.fy.all[2,4]
    
    if (p.fy.all < 0.1) linetype = "solid" else linetype = "dotted"
    #tit2 = paste (round(gain.fy.all), "km2/yr (all lands)  ")
    
    
    lines(c(min(years),max(years)),c(offset.fy.all+min(years)*gain.fy.all,offset.fy.all+max(years)*gain.fy.all), type="l", lty=linetype)
    
    modelSummary.fy.nocrop <- summary(linearMod.fy.nocrop)
    modelCoeffs.fy.nocrop <- modelSummary.fy.nocrop$coefficients
    offset.fy.nocrop <- modelCoeffs.fy.nocrop[1,1]
    gain.fy.nocrop <- modelCoeffs.fy.nocrop[2,1]
    p.fy.nocrop <- modelCoeffs.fy.nocrop[2,4]
    
    if (p.fy.nocrop < 0.1) linetype = "solid" else linetype = "dotted"
    
    #lines(c(min(years),max(years)),c(offset.fy.nocrop+min(years)*gain.fy.nocrop,offset.fy.nocrop+max(years)*gain.fy.nocrop), type="l", lty = linetype, col = "red")
    
    modelSummary.fy.crop <- summary(linearMod.fy.crop)
    modelCoeffs.fy.crop <- modelSummary.fy.crop$coefficients
    offset.fy.crop <- modelCoeffs.fy.crop[1,1]
    gain.fy.crop <- modelCoeffs.fy.crop[2,1]
    p.fy.crop <- modelCoeffs.fy.crop[2,4]
    
    if (p.fy.crop < 0.1) linetype = "solid" else linetype = "dotted"
    
    #lines(c(min(years),max(years)),c(offset.fy.crop+min(years)*gain.fy.crop,offset.fy.crop+max(years)*gain.fy.crop), type="l", lty = linetype, col = "#336600")
    
    
    gain.fy.all.relative.percent = gain.fy.all/mean(yearlytotal.fy.all)*100
    gain.fy.nocrop.relative.percent = gain.fy.nocrop/mean(yearlytotal.fy.nocrop)*100
    gain.fy.crop.relative.percent = gain.fy.crop/mean(yearlytotal.fy.crop)*100
    
    gain2 = sprintf("%3.1f",gain.fy.all*1000)
    gain2rel = sprintf("%3.2f",gain.fy.all.relative.percent)
    if (p.fy.all < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.all),")",sep="") 
    title <- paste(colnames[zone + 2], " ")
    tit2 =expr(paste(!!colnames[zone + 2], " trend ",!!gain2," 10"^"3", " km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (title, cex=0.9, side = 3, line = -1.5, adj = 0,  at = 2011.5)
    mtext (tit2, cex=0.9, side = 3, line = -1.5, adj = 0,  at = 2012.5)
    
    
    gain3 = round(gain.fy.nocrop*1000, digits = 1)
    gain3rel = sprintf("%3.2f",gain.fy.nocrop.relative.percent)
    if (p.fy.nocrop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.nocrop),")",sep="") 
    tit3 =expr(paste("uncultivated land: ",!!gain3," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (tit3, cex=0.9, side = 3, line = -2.5, adj = 0, at = 2012.5, col = 2)
    
    gain4 = round(gain.fy.crop*1000, digits = 1)
    gain4rel = sprintf("%3.2f",gain.fy.crop.relative.percent)
    if (p.fy.crop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.crop),")",sep="") 
    tit4 =expr(paste("cultivated land:       ",!!gain4," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (tit4, cex=0.9, side = 3, line = -3.5, adj = 0, at = 2012.5, col = "#336600")
    
    return(c(p.fy.all, p.fy.nocrop, p.fy.crop))
  }
}
