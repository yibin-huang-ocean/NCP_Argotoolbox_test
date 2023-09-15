# CO2 Solubility constant (Weiss, 1974)
Ko=function (S,T){  # input : T(°C) and salinity
  T=T+273.15;  # Conversion from Celsius degrees to Kelvins
  
  A= c(-60.2409, 93.4517, 23.3585)   #  mol/Kg.atm
  B=c(0.023517,-0.023656, 0.0047036);  # mol/Kg.atm
  LnKo=A[1]+A[2]*(100/T)+(A[3]*log(T/100))+S*( B[1]+B[2]*T/100 )+B[3]* ((T/100)^2)
  Ko=exp(LnKo)
  return(Ko)   ### unit: mmol m^-3 uatm^-1
}
