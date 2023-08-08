


compute_DIC_gas_flux <- function ( cycle=cycle,
                                   temperature=temperature,
                                   salinity=salinity,
                                   pressure=pressure,
                                   pCO2_air=pCO2_air,
                                   pCO2_water=    pCO2_water,
                                   wind_speed=wind_speed,
                                   ks_error=    CO2_gas_diffsuion_error,
                                   ice_coverage= ice_coverage,
                                   gas_model=gas_model){
  
  
  # Merge the input data 
  float_profile_merge <- data.frame(  cycle=cycle,
                                      temperature=temperature,
                                      salinity=salinity,
                                      pressure=pressure,
                                      pCO2_air=pCO2_air,
                                      pCO2_water=    pCO2_water,
                                      wind_speed=wind_speed,
                                      ice_coverage= ice_coverage,
                                      gas_model=gas_model)
  
  float_profile_merge =arrange(  float_profile_merge ,  
                                 float_profile_merge$cycle,  
                                 float_profile_merge$pressure)
  
  # Extract the surface profile
  float_profile_merge= subset(  float_profile_merge, 
                                float_profile_merge$pressure==1)
  
  
   if (Model_setting_list$gas_model==1){
     gas_model='W14'
   }
  
  if (Model_setting_list$gas_model==2){
    gas_model='W92'
  }
  
  if (Model_setting_list$gas_model==3){
    gas_model='HO06'
  }
  
  if (Model_setting_list$gas_model==4){
    gas_model='SW07'
  }

    DIC_air_sea  <-  CO2_gas_model(wind_speed=float_profile_merge$wind_speed,
                                          temperature=float_profile_merge$temperature,
                                          salinity=float_profile_merge$salinity,
                                          pCO2_air=float_profile_merge$pCO2_air,
                                          pCO2_water=float_profile_merge$pCO2_water,
                                          ice_coverage= float_profile_merge$ice_coverage,
                                   gas_model=gas_model)    # mmol C m-2 d-1
  
  # Add the uncertainty 
  DIC_air_sea=  DIC_air_sea* (1+ ks_error) # mmol C m-2 d-1
  return(  DIC_air_sea)
}






###  Schmidt Number, For water of temperature range 0-30°C
Schmidt=function(T,S ){
  A = 2073.1;
  B = 125.62;
  C = 3.6276;
  D = 0.043219;
  Schmidt= A - (B*T)+(C*T^2)-(D*T^3);
  Schmidt=  Schmidt/(S*3.14*10^(-3)+1)
  return(  Schmidt)
}







point <- function (x,y,z){
  k=data.frame(x=x,y=y)
  fig= ggplot(k,aes(x,y))+geom_point(size=z)
  #  fig= G_axix_size(fig,
  #                  y_title_size=20,
  #                 x_title_size=20,
  #                text.y_size=20,
  #               text.x_size=20,
  #              x_name=expression (bold()),
  #             y_name=expression ()
  # )
  print(fig)
}


