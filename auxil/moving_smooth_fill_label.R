
# This function is designed to moving-smooth the float data 
# over time;
# The moving-smooth is performed for each subset of data that shares the 
# same "label";
# Note: this function will help keep the original data, which are automatically eliminated when applying 
# moving_smooth function.

moving_smooth_fill_label=function (data=cycle_data$tracer_background_mean_concentration,  
                                   
                                   label= year_North_South( cycle_data$latitude,  cycle_data$date), 
                                   smooth_number=5 ) {
  
  new_data=data.frame(data=data,  
                      
                      label=label)
  library(zoo)
  median=ddply(  new_data,.(label),rollmean,k=5,fill = NA)
  
  median$data[which(is.na(median$data))]=data[which(is.na(median$data))]
  return  (median)
  
}



moving_smooth_label=function (data=cycle_data$tracer_background_mean_concentration,  
                                   
                                   label= year_North_South( cycle_data$latitude,  cycle_data$date), 
                                   smooth_number=5 ) {
  
  new_data=data.frame(data=data,  
                      
                      label=label)
  library(zoo)
  median=ddply(  new_data,.(label),rollmean,k=5,fill = NA)
  
 # median$data[which(is.na(median$data))]=data[which(is.na(median$data))]
  return  (median)
  
}

