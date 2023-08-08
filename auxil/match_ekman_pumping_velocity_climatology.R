

match_ekman_pumping_velocity_climatology <- function (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                       "Ancillary data and toolbox/Ancilary data/Ekman pumping velocity", sep="") ,
                                          longitude=ancilary_data_per_cycle$longitude,
                                          latitude=ancilary_data_per_cycle$latitude,
                                          date= ancilary_data_per_cycle$date){
  grid=5
  # Import the ancillary data 
  setwd(path)
  
  ekman_v_monthly_climatology <- read.table("ekman_pumping_velocity_month_climatology.txt",
                        header=T)
  
  
  # Create a vector to store the results
  match_up <- rep(0,length(date))
  i=1
  for (i in 1:length(date)){
   
    month_label<- month(Convert_matlabtime(date) )
   
    ekman_v_one_month <- subset(  ekman_v_monthly_climatology,   
                                  ekman_v_monthly_climatology$month==month_label[i] &
                                    ekman_v_monthly_climatology$lon> longitude[i]  &
                                    ekman_v_monthly_climatology$lon<=  longitude[i]+ grid &
                                    
                                    ekman_v_monthly_climatology$lat> latitude[i] &
                                    ekman_v_monthly_climatology$lat<=  latitude[i]+ grid )
    
    if ( length(ekman_v_one_month$lon)>0){
      match_up[i]=ekman_v_one_month$vertical_advection_v[1]
    } # Bracket for  "if ( length(ekman_v_one_month$lon)>0)"
  } # bracket for "for (i in 1:length(date))"
    
  return(    match_up) # m d-1
  
}