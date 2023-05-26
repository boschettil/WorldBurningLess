WorldBurningLessTrendMap.R <- function(nocrop = 0)
{
  library(rlang)
  library(ijtiff)
  library(raster)
  
  yearstart  = 2002
  yearend    = 2021
  datapath = "./Data"
  if (nocrop == 1) {
    basefile = "/noAg/MCD64A1_NoAg_50km.A"
    outfile_slope=paste(datapath,'/MCD64A1_noAg_50km_trend_2002-2021.txt',sep="")
    outfile_p=paste(datapath,'/MCD64A1_noAg_50km_p_2002-2021.txt',sep="")
    }

  if (nocrop == 2) {
    basefile = "/Ag/MCD64A1_Ag_50km.A"
    outfile_slope=paste(datapath,'/MCD64A1_Ag_50km_trend_2002-2021.txt',sep="")
    outfile_p=paste(datapath,'/MCD64A1_Ag_50km_p_2002-2021.txt',sep="")
  }
  
    
  if (nocrop == 0) {
    basefile = "/AllLand/MCD64A1_50km.A"
    outfile_slope=paste(datapath,'/MCD64A1_AllLand_50km_trend_2002-2021.txt',sep="")
    outfile_p=paste(datapath,'/MCD64A1_AllLand_50km_p_2002-2021.txt',sep="")
  }
  
  ncols = 864
  nrows = 432
  nyears = yearend-yearstart+1
  raster_cube <- array(NA, c(ncols,nrows,nyears)) 
  slope_rast = array(-99,c(ncols,nrows))
  p_rast = array(-99,c(ncols,nrows))
  years = c(yearstart:yearend)
  
  for (i_year in yearstart:yearend) {
  basefile_month_id ="060yearly.tif"
   if ((i_year == 2004) || (i_year == 2008) ||  (i_year == 2012) || (i_year == 2016) || (i_year == 2020)) basefile_month_id ="061yearly.tif"
   
  raster_in_file = paste(datapath,basefile,i_year,basefile_month_id, sep = "")
  raster_in = read_tif(raster_in_file)
  raster_cube[ , , (i_year-yearstart+1)]<-t(raster_in[,,1,1])*100./(50.*50.)   
  }
  for (i_col in 1:ncols) {
    for (i_row in 1:nrows){
      BA.fy = raster_cube[i_col,i_row,]
      if (max(BA.fy) > 0) {
      linearMod.fy <- lm(BA.fy ~ years)
      modelSummary.fy <- summary(linearMod.fy)
      modelCoeffs.fy  <- modelSummary.fy$coefficients
      offset.fy <- modelCoeffs.fy[1,1]
      gain.fy   <- modelCoeffs.fy[2,1]
      p.fy <- modelCoeffs.fy[2,4]
      
      slope_rast[i_col,i_row]=gain.fy
      p_rast[i_col,i_row]=p.fy
      }
      if (max(BA.fy) == 0) {
        slope_rast[i_col,i_row]=-99
        p_rast[i_col,i_row]=-99
      }
    }
  }
  write.table(t(slope_rast), file = outfile_slope, row.names = F, col.names = F)
  write.table(t(p_rast), file = outfile_p, row.names = F, col.names = F)
  
}
  