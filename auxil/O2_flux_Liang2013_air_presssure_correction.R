
O2_flux_Liang2013_air_presssure_correction=function (windspeed =float_profile_merge$wind_speed, 
                                                             PT= float_profile_merge$temperature, 
                                                             S=float_profile_merge$salinity, 
                                                             O2=float_profile_merge$tracer, #input the O2 in the unit of umol kg-1
                                                             SLP=float_profile_merge$sea_level_pressure,
                                                             ks_oxygen_error=       O2_gas_diffsuion_error, 
                                                             kb_oxygen_error=      O2_gas_bubble_error, 
                                                             kc_oxygen_error=      O2_gas_bubble_error,
                                                             ice_coverage=   ice_coverage){  
  
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
  
  #% Calculate COARE 3.0 and gas transfer velocities
  cd10=c(1:length(windspeed))
  cd10[which(windspeed<11)]=(0.0012)
  cd10[which(windspeed>20)]=(0.0018)
  cd10[which(windspeed>=11 & windspeed<=20  )]=(0.49+0.065*windspeed[which(windspeed>=11 & windspeed<=20  )])*10^(-3);
  
  ustar = windspeed*sqrt(cd10);
  
  #% water-side ustar
  ustarw = ustar/sqrt(rhow/rhoa);
  
  # water-side resistance to transfer
  rwt = sqrt(rhow/rhoa)*(hw*sqrt(ScW)+(log(0.5/tkt)/0.4));
  
  # air-side resistance to transfer
  rat = ha*sqrt(ScA)+1/sqrt(cd10)-5+0.5*log(ScA)/0.4;
  
  
  
  # diffusive gas transfer coefficient (L13 eqn 9)
  Ks = ustar/(rwt+rat*alc) * (1+  ks_oxygen_error[1])
  
  # bubble transfer velocity (L13 eqn 14)
  Kb = 1.98*10^6*ustarw^2.76*(ScW/660)^(-2/3)/(m2cm*h2s)* (1+  kb_oxygen_error[1])
  
  # overpressure dependence on wind speed (L13 eqn 16)
  dP = 1.5244*ustarw^1.06;
  
  # -------------------------------------------------------------------------
  # Calculate air-sea fluxes
  # -------------------------------------------------------------------------
  
  Fs = (1-   ice_coverage/100)*Ks*Geq*(1-Gsat)*3600*24; # Fs in L13 eqn 3
  Fp = (1-   ice_coverage/100)*Kb*Geq*((1+dP)*1-Gsat)*3600*24 # Fp in L13 eqn 3
  Fc = (1-   ice_coverage/100)*xG*5.56*ustarw^3.86*3600*24*1000 * (1+kc_oxygen_error[1]); # L13 eqn 15
  
  
  F_total <-  Fs*(1+  ks_oxygen_error) + (Fp + Fc) *1 *(1+  kb_oxygen_error)
  return(F_total) # mmol O2 m-2 d-1
  # return ( c(F_total,Fs,Fp,Fc,Ks*3600*24) )
  
}


