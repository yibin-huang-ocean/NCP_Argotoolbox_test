
compute_TA_Canyon_B= function(date=float_profile_data$date,
                              latitude=float_profile_data$latitude, 
                              longitude=float_profile_data$longitude, 
                              pressure=float_profile_data$pressure, 
                              temperature=float_profile_data$temperature, 
                              salinity=float_profile_data$salinity,
                              oxygen=float_profile_data$oxygen ){
  
  
  # compute the TA
  TA<- CANYONB(date=  Convert_matlabtime(  date),  
               lat=latitude, 
               lon= longitude, 
               pres= pressure, 
               temp=  temperature, 
               psal= salinity,
               doxy= oxygen,
               param=c('AT'))$AT
  
  return(  TA)
}

