
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
