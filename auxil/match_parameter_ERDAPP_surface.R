# This function is to match the ancillary data 
# alongside the float trajectory archived from ERDDAP

match_parameter_ERDAPP_surface=function(
    date=     ancilary_data_per_cycle$date ,
    longitude=  ancilary_data_per_cycle$longitude,
    latitude=  ancilary_data_per_cycle$latitude,
    dataset="erdQMstress1day",
    url="https://coastwatch.pfeg.noaa.gov/erddap/",
    parameter_name="upwelling",
    #  pressure= float_profile_data$pressure_m,
    zcoord_included="yes",
    zcoord_level=0,
    zcoord_name="altitude"){
  parameter_match= rep(NaN,length(date))
  dataInfo <- rerddap::info(dataset, url)
  dataInfo
  # Extract the variable information 
  zcoord_level <- matrix(zcoord_level, nrow = 1, ncol = length(longitude), byrow = TRUE)
  parameter_match <- rxtracto(dataInfo, 
                              parameter=parameter_name, 
                              
                              xcoord=longitude, 
                              xName="longitude",
                              
                              ycoord=latitude, 
                              yName="latitude",
                              
                              tcoord=Convert_matlabtime( date),
                              tName="time",
                              
                              zcoord=zcoord_level,
                              zName= zcoord_name)
  
  return(      parameter_match)
}





