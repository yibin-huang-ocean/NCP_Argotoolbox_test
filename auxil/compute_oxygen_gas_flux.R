# compute the air-sea oxygen flux
compute_oxygen_gas_flux <- function ( cycle=float_profile_data$cycle,
                                   temperature=float_profile_data$temperature,
                                   salinity=float_profile_data$salinity,
                                   pressure=float_profile_data$pressure,
                                   oxygen = float_profile_data$tracer, 
                                   sea_level_pressure = float_profile_data$sea_level_pressure,
                                   wind_speed=float_profile_data$wind_speed,
                                   ice_coverage= float_profile_data$ice_coverage,
                                   ks_oxygen_error=       O2_gas_diffsuion_error, 
                                   kb_oxygen_error=      O2_gas_bubble_error, 
                                   kc_oxygen_error=      O2_gas_bubble_error ,
                                   gas_model=Model_setting_list$gas_model){
  
  
  # Merge the input data 
  float_profile_merge <- data.frame(cycle=cycle,
                                    temperature=temperature,
                                    salinity=salinity,
                                    pressure=pressure,
                                    tracer =       oxygen, 
                                    sea_level_pressure = sea_level_pressure,
                                    wind_speed= wind_speed,
                                    ice_coverage=ice_coverage)
  
  float_profile_merge =arrange(  float_profile_merge ,  
                                 float_profile_merge$cycle,  
                                 float_profile_merge$pressure)
  
  # Extract the surface profile
  float_profile_merge= subset(  float_profile_merge, 
                                float_profile_merge$pressure==1)
  
  if (gas_model==1){ # Emerson et al., 2019
    
    oxygen_gas_flux  <-  O2_flux_Liang2013_Emerson_air_presssure_correction (windspeed =float_profile_merge$wind_speed, 
                                                                                      PT= float_profile_merge$temperature, 
                                                                                      S=float_profile_merge$salinity, 
                                                                                      O2=float_profile_merge$tracer, #input the O2 in the unit of umol kg-1
                                                                                      SLP=float_profile_merge$sea_level_pressure,
                                                                                      ks_oxygen_error=             ks_oxygen_error, 
                                                                                      kb_oxygen_error=       kb_oxygen_error, 
                                                                                      kc_oxygen_error=         kb_oxygen_error,
                                                                             ice_coverage=         float_profile_merge$ice_coverage)
    
  }
  
  
  if (gas_model==2){ # Emerson et al., 2019
    
    oxygen_gas_flux  <-  O2_flux_Liang2013_air_presssure_correction (windspeed =float_profile_merge$wind_speed, 
                                                                             PT= float_profile_merge$temperature, 
                                                                             S=float_profile_merge$salinity, 
                                                                             O2=float_profile_merge$tracer, #input the O2 in the unit of umol kg-1
                                                                             SLP=float_profile_merge$sea_level_pressure,
                                                                             ks_oxygen_error=             ks_oxygen_error, 
                                                                             kb_oxygen_error=       kb_oxygen_error, 
                                                                             kc_oxygen_error=         kb_oxygen_error,
                                                                     ice_coverage=         float_profile_merge$ice_coverage)
    
  }
  
  if (gas_model==3){ # Emerson et al., 2019
    
    oxygen_gas_flux  <- O2_flux_Wanni_2014 (wind_speed =float_profile_merge$wind_speed, 
                                                                     PT= float_profile_merge$temperature, 
                                                                     S=float_profile_merge$salinity, 
                                                                     O2=float_profile_merge$tracer, #input the O2 in the unit of umol kg-1
                                                                     SLP=float_profile_merge$sea_level_pressure,
                                                                     ks_oxygen_error=             ks_oxygen_error,
                                            ice_coverage=           float_profile_merge$ice_coverage)
    
  }
  
  
  
  return(  oxygen_gas_flux)
}








