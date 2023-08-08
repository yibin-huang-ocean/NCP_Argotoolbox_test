

# This function is to adjust the year label according to the hemispheres;
# In the Southern hemisphere, the data collected from July to the Jun of following year are
# set to the same calender year
year_North_South <- function ( latitiude,
                               date) {
  new_year=seq(1,length(date),1)
 
  time <- Convert_matlabtime (date) # convert into the as.Date format
  

  month <- month(time) # extract month
  year <- year(time)  # exctact the yeaer
  

  new_year[which(latitiude>= 0)]=  year[which(latitiude>= 0)]
  
  
  new_year[which(month<7 & latitiude<0    )] <- year[which(month<7& latitiude<0    )]-1
  
  new_year[which(month>6& latitiude<0    )] <- year[which(month>6& latitiude<0    )]
  
  
  return ( new_year )
  
  
  
}
