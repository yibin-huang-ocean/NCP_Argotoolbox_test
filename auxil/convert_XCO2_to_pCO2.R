# calculate the pco2_in_situ from xco2
convert_XCO2_to_pCO2= function(temperature=ancilary_data_per_cycle$temperature, 
                               salinity=ancilary_data_per_cycle$salinity, 
                               xCO2_air=ancilary_data_per_cycle$xCO2_air,
                               sea_level_pressure=ancilary_data_per_cycle$sea_level_pressure,
                               relative_humidity=ancilary_data_per_cycle$relative_humidity
){
  sea_level_pressure <- sea_level_pressure*10^-5*10^3 # convert to millibar
  
  # calculate the pco2_in_situ from xco2
  D0=24.4543
  D1=-67.4509
  D2=-4.8489
  D3=-5.44*10^-4
  Tabs=temperature +273.15
  PH2O_millibar=1013.25*exp(D0+D1*(100/Tabs)+D2*log(Tabs/100)+D3*    salinity) # millibar 
  
  pCO2_air=   xCO2_air/1000000 *(      sea_level_pressure-     relative_humidity/100* PH2O_millibar)*1000
  return(     pCO2_air)
}
