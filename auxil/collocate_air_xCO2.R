

collocate_air_xCO2=function(
    path_code=    path_code,
    float_date= ancilary_data_per_cycle$date,
    float_latitude=     ancilary_data_per_cycle$latitude,
    float_longitude=     ancilary_data_per_cycle$longitude){
  
  
  
  # create the vector to deposiste the matchup 
  xCO2_air= rep(NaN, length(    float_date))
  
  # import the xCO2 dataset
  setwd(    path_code)
  pco2_air_MPL=read.table("xco2_air_MPL.txt",header=T)
  pco2_air_MPL$year=  year( Convert_matlabtime( pco2_air_MPL$date))
  pco2_air_MPL$month=month( Convert_matlabtime( pco2_air_MPL$date))
  
 
 # Extract the float sampling date 
  float_time=Convert_matlabtime(float_date)
  float_year=year(    float_time)
  float_month=month(     float_time)
  
  i=1
  for (i in seq(length(  float_time))){
    
    if (  float_year[i]<= 2020 | float_year[i]<=2005){
      
      xCO2_air_latitude_band=filter(pco2_air_MPL,
                                    pco2_air_MPL$year==float_year[i]  &
                                      pco2_air_MPL$month==float_month[i]  
      )
      
    } else{ # since the xCO2 data is only available before 2021,  
      # we gap-fill the XCO2 in other calendar years by assuming the annual change in air XCO2 approximating 2.2 uatm
      xCO2_air_latitude_band=filter(pco2_air_MPL,
                                    pco2_air_MPL$year==2020  &
                                      pco2_air_MPL$month==float_month[i]  
      )
      
      xCO2_air_latitude_band$xco2_air=    xCO2_air_latitude_band$xco2_air + 2.2 *(float_year[i]-2020)
    } # loop for "else{ "
   
    
   xCO2_air[i]=approx  ( xCO2_air_latitude_band$lat,
                         xCO2_air_latitude_band$xco2_air, 
                                   float_latitude[i] ,rule=2 )$y
    
  } # Bracket for "for (i in seq(lengt"
  

  return( xCO2_air)
  
}


