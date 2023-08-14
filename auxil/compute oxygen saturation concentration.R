
###Compute the oxygen saturation concentration （umol kg-1)
# Reference: Herncin E. Garcfa 1992  
o2sat=function(S,T){
  TS = log((298.15- T) / (273.15 + T));
  A0 = 5.80818;
  A1 = 3.20684;
  A2 = 4.11890;
  A3 = 4.93845
  A4 = 1.01567
  A5 = 1.41575
  B0 =-7.01211*10^-3
  B1 =-7.25985*10^-3
  B2 = -7.93334*10^-3
  B3 = -5.54491*10^-3
  C0 = -1.32412*10^-7
  
  lnC= A0 + A1*TS + A2*(TS^ 2) + A3 * (TS ^ 3) + A4* (TS ^ 4) +A5 * (TS ^ 5) + S * (B0 + B1 * TS + B2 * (TS ^ 2) + B3 * (TS ^ 3)) +C0 * (S ^ 2);
  return(exp(lnC))#
}
