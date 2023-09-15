
convert_umol_kg_to_mmol_m3= function(temperature=float_profile_merge$temperature, 
                                     salinity=float_profile_merge$salinity,
                                     tracer=float_profile_data$tracer){
  # compute the density
  density <- swSigmaTheta (temperature,temperature,
                           10)+1000 ## 
  
  tracer=tracer*density/1000
  
  return(  tracer)
  
}