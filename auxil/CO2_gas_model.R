
CO2_gas_model =function( wind_speed=float_profile_merge$wind_speed,
                               temperature=float_profile_merge$temperature,
                               salinity=float_profile_merge$salinity,
                               pCO2_air=float_profile_merge$pCO2_air,
                               pCO2_water=float_profile_merge$pCO2_water,
                               ice_coverage= float_profile_merge$ice_coverage,
                         gas_model=gas_model){
  
  
  if (gas_model=='W14'){
    A=0.251
  }
  
  if (gas_model=='W92'){
    A=0.39
  }
  
  if (gas_model=='HO06'){
    A=0.254
  }
  
  if (gas_model=='SW07'){
    A=0.27
  }
  k <- A *  wind_speed^2 * ( Schmidt(      temperature,    salinity)/600) ^ (-0.5)   ###unit: cm hr-1
  k <- k / 100 /3600 ### cm hr-1 to m s-1
  koo= Ko (salinity,     temperature)
  CO2_flux= 3600*24* k * (   pCO2_air-   pCO2_water) * koo* (1-   ice_coverage/100)  # mmol C m-2 d-1
  
  
  
  return (  CO2_flux)
  
  
}
