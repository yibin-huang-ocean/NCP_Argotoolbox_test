

match_ekman_pumping_velocity_climatology <- function (path= paste( Model_setting_list$path_NCP_toolbox,  
                                                       "Ancillary data and toolbox/Ancilary data/Ekman pumping velocity", sep="") ,
                                          longitude=ancilary_data_per_cycle$longitude,
                                          latitude=ancilary_data_per_cycle$latitude,
                                          date= ancilary_data_per_cycle$date){

  # Import the ancillary data 
  setwd(path)
  
  ekman_v_monthly_climatology <- read.table("ekman_pumping_velocity_month_climatology.txt",
                        header=T)
  
  lon_ekman_v_monthly_climatology<- unique(  ekman_v_monthly_climatology$lon)
  lat_ekman_v_monthly_climatology<- unique(  ekman_v_monthly_climatology$lat)
  
  # Create a vector to store the results
  match_up <- rep(0,length(date))
  i=1
  for (i in 1:length(date)){
   
    month_label<- month(Convert_matlabtime(date) )
   
    lon_closet_line = which.min(abs(  lon_ekman_v_monthly_climatology - longitude[i]))
    lat_closet_line= which.min(abs(  lat_ekman_v_monthly_climatology - latitude[i]));
    
    
    ekman_v_one_month <- subset(  ekman_v_monthly_climatology,   
                                  ekman_v_monthly_climatology$month==month_label[i] &
                                    ekman_v_monthly_climatology$lon==  lon_ekman_v_monthly_climatology[    lon_closet_line ]  &
                                    ekman_v_monthly_climatology$lat==  lat_ekman_v_monthly_climatology[    lat_closet_line ]  
                                   )
    
    if ( length(ekman_v_one_month$lon)>0){
      match_up[i]=ekman_v_one_month$vertical_advection_v[1]
    } # Bracket for  "if ( length(ekman_v_one_month$lon)>0)"
  } # bracket for "for (i in 1:length(date))"
    
  return(    match_up) # m d-1
  
}