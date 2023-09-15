

match_euphotic_zone_climatolgy <- function (path_NCP_toolbox= Model_setting_list$path_NCP_toolbox,
                                          longitude=ancilary_data_per_cycle$longitude,
                                          latitude=ancilary_data_per_cycle$latitude,
                                          date= ancilary_data_per_cycle$date,
                                          time_window="monthly"){
  # Import the ancillary data 
  setwd(paste(path_NCP_toolbox,"Ancillary data and toolbox/Ancilary data/Euphotic zone",sep=""))
  
  ekman_v_monthly_climatology <- read.table("euphotic zone climatolgy.txt",
                        header=T)
  
  
  # Create a vector to store the results
  match_up <- rep(NaN,length(date))
  i=1
  for (i in 1:length(date)){
   
    month_label<- month(Convert_matlabtime(date) )
   
    ekman_v_one_month <- subset(  ekman_v_monthly_climatology,   
                                    ekman_v_monthly_climatology$lon> longitude[i]  &
                                    ekman_v_monthly_climatology$lon<=  longitude[i]+ grid &
                                    
                                    ekman_v_monthly_climatology$lat> latitude[i] &
                                    ekman_v_monthly_climatology$lat<=  latitude[i]+ grid )
    
    if ( length(ekman_v_one_month$lon)>0){
      if (      time_window=="monthly"){
        match_up[i]=ekman_v_one_month[1,     month_label+2  ]
      } 
      
      if (      time_window=="annual"){
        match_up[i]=ekman_v_one_month[1,     15  ]
      } 
      
      
    } # Bracket for  "if ( length(ekman_v_one_month$lon)>0)"
  } # bracket for "for (i in 1:length(date))"
    
  return(    match_up) # 
  
}