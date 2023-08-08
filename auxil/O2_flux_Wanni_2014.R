O2_flux_Wanni_2014 =function( wind_speed=float_profile_merge$wind_speed,
                              PT=float_profile_merge$temperature,
                              S=float_profile_merge$salinity,
                               O2=float_profile_merge$tracer, #input the O2 in the unit of umol kg-1
                               SLP=float_profile_merge$sea_level_pressure,
                               ks_oxygen_error=       O2_gas_diffsuion_error, 
                               ice_coverage= float_profile_merge$ice_coverage){
  
  
  m2cm = 100;#% cm in a meter
  h2s = 3600
  
  m2cm = 100;# % cm in a meter
  h2s = 3600; #% sec in hour
  atm2Pa = 1.01325e5; #% Pascals per atm
  
  
  pslpc= SLP/1013.25/100
  
  # Calculate potential density at surface
  SA = S *35.16504/35; # absolute salinity 
  
  CT=gsw_CT_from_t(SA, PT,1)# Conservative Temperature from potential temperature
  
  rhow = gsw_sigma0(SA,CT)+1000;
  rhoa = 1.225;
  
  O2=O2*  (rhow /1000) # convert to umol L-1
  
  lam = 13.3;
  A = 1.3;
  phi = 1;
  tkt = 0.01;
  hw=lam/A/phi;
  ha=lam; 
  
  # air-side schmidt number
  ScA = 0.9;
  
  R = 8.314;  # units: m3 Pa K-1 mol-1
  
  # Calculate gas physical properties
  
  xG = 0.2094
  
  
  Geq=o2sat(S,PT)* (rhow /1000)*  pslpc # mmol O2 m-3 d-1 with SLP correction
  alc = (Geq/atm2Pa)*R*(PT+273.15);
  
  Gsat = O2/Geq;
  ScW= marelac::gas_schmidt(t = PT, species = c('O2'))
  

  # diffusive gas transfer coefficient (L13 eqn 9)
  Ks <- 0.251 *  wind_speed^2 * ( Schmidt(        PT,          S)/600) ^ (-0.5)   ###unit: cm hr-1
  Ks <-   Ks / 100 /3600 ### cm hr-1 to m s-1
 
  # -------------------------------------------------------------------------
  # Calculate air-sea fluxes
  # -------------------------------------------------------------------------
  
  Fs = Ks*Geq*(1-Gsat)*3600*24*  (1-   ice_coverage/100); #  mmol O2 m-2 d-1

  F_total <-  Fs*(1+  ks_oxygen_error) 
  return(F_total) # mmol O2 m-2 d-1
  # return ( c(F_total,Fs,Fp,Fc,Ks*3600*24) )

  return (   F_total)
  
  
}
