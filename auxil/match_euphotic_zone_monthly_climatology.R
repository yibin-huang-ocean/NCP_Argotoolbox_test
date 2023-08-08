

match_euphotic_zone_monthly_climatology <- function (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                                   "Ancillary data and toolbox/Ancilary data/Euphotic zone_climatology", sep="") ,
                                                     longitude=ancilary_data_per_cycle$longitude,
                                                     latitude=ancilary_data_per_cycle$latitude,
                                                     date=ancilary_data_per_cycle$date){
  grid=1
  # Import the ancillary data 
  setwd(path)
  
  euphotic_zone_monthly_climatology <- read.table("euphotic zone climatolgy.txt",
                                            header=T)
  
  euphotic_zone_monthly_climatology=arrange(euphotic_zone_monthly_climatology,
                                            euphotic_zone_monthly_climatology$lon,
                                            euphotic_zone_monthly_climatology$lat)
  # Create a vector to store the results
  match_up <- rep(0,length(date))
  month_label<- month(Convert_matlabtime(date) )
  i=1
  for (i in 1:length(date)){
    

    
  euphotic_zone <- subset(  euphotic_zone_monthly_climatology,   
                                 
                            euphotic_zone_monthly_climatology$lon> longitude[i]  &
                              euphotic_zone_monthly_climatology$lon<=  longitude[i]+ grid &
                                    
                              euphotic_zone_monthly_climatology$lat> latitude[i] &
                              euphotic_zone_monthly_climatology$lat<=  latitude[i]+ grid )
    
    if ( length(  euphotic_zone$lon)>0){
      match_up[i]=  euphotic_zone[  1, month_label[i]+2]
      
      if (is.na(    match_up[i])){
        # use the annual mean if the monthly climatology is still available 
        match_up[i]=  euphotic_zone[  1, 15]
      }
    } # Bracket for  " if ( length(  euphotic_zone$lon)>0){"
  } # bracket for "for (i in 1:length(date))"
  
  return(    match_up) # m
  
}