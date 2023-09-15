# compute the middle float location during 
# each growing season 
compute_median_location_productive_season=function ( cycle=float_profile_data$cycle,
                                                   latitiude =  float_profile_data$latitude_N,
                                           longitude =  float_profile_data$longitude_E,
                                           date=float_profile_data$date,
                                           pressure=float_profile_data$pressure_m){
  
  
  
  new_data_set = data.frame(cycle=cycle,
       latitiude =       latitiude,
                            longitude = longitude,
                            date=date,
       pressure=pressure)
  
  new_data_set=subset(  new_data_set,  new_data_set$pressure==1)
  new_data_set$new_year_PS = year_North_South( latitiude =    new_data_set$latitiude,
                                               new_data_set$date)
  
  
  new_data_set$month =month(Convert_matlabtime(   new_data_set$date))
  new_data_set$season_label= "UP"
  new_data_set$season_label[  new_data_set$latitiude>=0 &
                                new_data_set$month >3 &    new_data_set$month<11    ]= "P"
  
  new_data_set$season_label[  new_data_set$latitiude<0 &
                                new_data_set$month <4    ]= "P"
  
  new_data_set$season_label[  new_data_set$latitiude<0 &
                                new_data_set$month >9   ]= "P"
  
  
  new_year_PS_label= unique(   new_data_set$new_year_PS)
  
  productive_season_location= data.frame(new_year_PS=   new_year_PS_label  )
  productive_season_location$longitude_median_PS_E=   NaN
  productive_season_location$latitude_median_PS_N= NaN
  
  i=1
  for (i in 1:length(    productive_season_location$new_year_PS)){
    
    productive_period<- subset(  new_data_set,  
                                 new_data_set$new_year_PS== productive_season_location$new_year_PS[i]  &
                                   new_data_set$season_label=="P")
    middle_index <- round(length(    productive_period$cycle)/2)
    if(    middle_index>0){
      productive_season_location$longitude_median_PS_E[i]=    productive_period$longitude[  middle_index]
      productive_season_location$latitude_median_PS_N[i]= productive_period$latitiude[  middle_index]
    }
   
    
  }
  
  new_data_set=left_join(  new_data_set,    productive_season_location,"new_year_PS")
  
  new_data_set=data.frame(cycle=new_data_set$cycle,
                          longitude_median_PS_E=new_data_set$longitude_median_PS_E,
                          
                          latitude_median_PS_N=new_data_set$latitude_median_PS_N,
                          season_label=new_data_set$season_label)
  
  return(   new_data_set)

}


