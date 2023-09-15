

create_cycle_data_frame= function(
    WMOID=float_profile_data$WMOID,
    cycle=float_profile_data$cycle,
    date=float_profile_data$date,
    pressure=float_profile_data$pressure){
  
  float_data= data.frame(  WMOID=float_profile_data$WMOID,
                               pressure=float_profile_data$pressure,
                               cycle=float_profile_data$cycle,
                               date=float_profile_data$date)
  
  new_data_frame= data.frame(cycle= unique(  float_data$cycle))
  
  i=1
  for (i in 1:length(    new_data_frame$cycle)){
    d=subset(  float_data,  float_data$cycle== new_data_frame$cycle[i])
    
    new_data_frame$date[i]=d$date[1]
    new_data_frame$WMOID[i]=d$WMOID[1]
  }
  
  return(new_data_frame)
  
}








