WorldBurningLessTable.R <- function(start.month = 3)
{
  
  library(rlang)
  #
  # Code to generate Table 1 
  #
  #
  #
  #
  #
  # sort zones from largest to smallest average annual area burned
  # zones <- c(12, 2, 6, 3, 4, 9, 14, 13, 11, 8, 10, 5, 7, 1, 15)
  # 
  # 
  #

  
  datapath = "./Data"
  file_table_out = paste(datapath,"/Table1GFEDzones.csv", sep = "")
  
  z.all <- read.table(file = 	paste(datapath,"/Summary.Zone.AgMask.totals.yearly.txt",sep=""),
                    header = T)
  
  z.nocrop <- read.table(file = paste(datapath,"/Summary.Zone.AgMask.totals.nocrop.yearly.txt",sep=""),
                         header = T)
  
  colnames <- names(z.all)
  n.years <- nrow(z.all) %/% 12
  years = (unique(z.all$YEAR))[1:n.years+1]
  
  # sort zones from largest to smallest average annual area burned
  zones <- c(12, 2, 6, 3, 4, 9, 14, 13, 11, 8, 10, 5, 7, 1, 15)
  
  table_out = matrix(0, ncol=15,nrow=15)
  colnames(table_out) = c("AVG.BA.ALL","AVG.BA.NC","AVG.BA.C","GAIN.KMQ.ALL","GAIN.KMQ.NC","GAIN.KMQ.C","GAIN.PC.ALL","GAIN.PC.NC","GAIN.PC.C","P.ALL","P.NC","P.C","COR.ALL.NC","COR.ALL.C","ZONE")
  
  rownames(table_out) = colnames[zones+2]
                        
  for (i_zone in 1:15)  {
    
  zone = zones[i_zone]
  
  y.all <- z.all[[zone + 2]]
  y.nocrop <- z.nocrop[[zone + 2]]
    
  yearlytotal.fy.all <- rep(0.0,n.years)
  yearlytotal.fy.nocrop <- rep(0.0,n.years)
    
  startmonth.series = z.all$MONTH[1]
  startmonth.fy = start.month+13-startmonth.series
    
  for(i in 1:(n.years)){
      yearlytotal.fy.all[i]=sum(y.all[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
      yearlytotal.fy.nocrop[i]=sum(y.nocrop[(startmonth.fy+12*(i-1)):(startmonth.fy-1+12*i)])
  }
    
  yearlytotal.fy.crop=yearlytotal.fy.all-yearlytotal.fy.nocrop 
 
  
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
    
    
  #lines(c(min(years),max(years)),c(offset.fy.all+min(years)*gain.fy.all,offset.fy.all+max(years)*gain.fy.all), type="l", lty=linetype)
    
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
    
    #if (p.fy.crop < 0.1) linetype = "solid" else linetype = "dotted"
    
    #lines(c(min(years),max(years)),c(offset.fy.crop+min(years)*gain.fy.crop,offset.fy.crop+max(years)*gain.fy.crop), type="l", lty = linetype, col = "#336600")
    
    
    gain.fy.all.relative.percent = gain.fy.all/mean(yearlytotal.fy.all)*100
    gain.fy.nocrop.relative.percent = gain.fy.nocrop/mean(yearlytotal.fy.nocrop)*100
    gain.fy.crop.relative.percent = gain.fy.crop/mean(yearlytotal.fy.crop)*100
    
    gain2 = sprintf("%3.1f",gain.fy.all)
    gain2rel = sprintf("%3.2f",gain.fy.all.relative.percent)
    if (p.fy.all < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.all),")",sep="") 
    #tit2 =expr(paste("all land:                 ",!!gain2," 10"^"3", " km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (tit2, cex=0.9, side = 3, line = -1.5, adj = 0,  at = 2012.5)
    
    
    gain3 = round(gain.fy.nocrop, digits = 1)
    gain3rel = sprintf("%3.2f",gain.fy.nocrop.relative.percent)
    if (p.fy.nocrop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.nocrop),")",sep="") 
    #tit3 =expr(paste("uncultivated land: ",!!gain3," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (tit3, cex=0.9, side = 3, line = -2.5, adj = 0, at = 2012.5, col = 2)
    
    gain4 = round(gain.fy.crop, digits = 1)
    gain4rel = sprintf("%3.2f",gain.fy.crop.relative.percent)
    if (p.fy.crop < 0.01)      p.txt = "(p<0.01)"
    else p.txt = paste("(p=",sprintf("%3.2f",p.fy.crop),")",sep="") 
    #tit4 =expr(paste("cultivated land:       ",!!gain4," 10"^"3"," km"^"2","yr"^"-1"," ",!!p.txt))
    #mtext (tit4, cex=0.9, side = 3, line = -3.5, adj = 0, at = 2012.5, col = "#336600")
    
    
    
    table_out[i_zone,1]  <- mean(yearlytotal.fy.all)/1000
    table_out[i_zone,2]  <- mean(yearlytotal.fy.nocrop)/1000
    table_out[i_zone,3]  <- mean(yearlytotal.fy.crop)/1000
    table_out[i_zone,4]  <- gain.fy.all/1000
    table_out[i_zone,5]  <- gain.fy.nocrop/1000
    table_out[i_zone,6]  <- gain.fy.crop/1000
    table_out[i_zone,7]  <- gain.fy.all.relative.percent
    table_out[i_zone,8]  <- gain.fy.nocrop.relative.percent
    table_out[i_zone,9]  <- gain.fy.crop.relative.percent
    table_out[i_zone,10] <- p.fy.all
    table_out[i_zone,11] <- p.fy.nocrop
    table_out[i_zone,12] <- p.fy.crop
    table_out[i_zone,13] <- cor(yearlytotal.fy.nocrop,yearlytotal.fy.all)
    table_out[i_zone,14] <- cor(yearlytotal.fy.crop,yearlytotal.fy.all)
    
    table_out[i_zone,15] <- zone
    
  }
    write.csv(table_out, file = file_table_out)
  
  
  
}


