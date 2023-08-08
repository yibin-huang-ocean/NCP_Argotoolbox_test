
compute_DIC_pCO2_Canyon_B= function(date=float_profile_data$date,
                                    latitude=float_profile_data$latitude, 
                                    longitude=float_profile_data$longitude, 
                                    pressure=float_profile_data$pressure, 
                                    temperature=float_profile_data$temperature, 
                                    salinity=float_profile_data$salinity,
                                    oxygen=float_profile_data$oxygen ,
                                    pH=float_profile_data$pH,
                                    Car_Equ_Cons_K1_K2 = Model_setting_list$Car_Equ_Cons_K1_K2,
                                    Car_Equ_Cons_Ks= Model_setting_list$Car_Equ_Cons_Ks,
                                    Total_boron_concentration= Model_setting_list$Total_boron_concentration){
  
  
  # compute the TA
  TA<- CANYONB(date=  Convert_matlabtime(  date),  
               lat=latitude, 
               lon= longitude, 
               pres= pressure, 
               temp=  temperature, 
               psal= salinity,
               doxy= oxygen,
               param=c('AT'))$AT
  # calculate the DIC 
  
  # Assign the carbonate constant based on the user-defined options
  
  
  if (        Car_Equ_Cons_K1_K2==1){
    
    k1k2="l" # Dickson (1990)
  }
  
  if (        Car_Equ_Cons_K1_K2==2){
    
    k1k2="m02" # Millero et al. (2002)
  }
  
  if (        Car_Equ_Cons_K1_K2==3){
    
    k1k2="w14"# Waters et al. (2014)
  }
  
  if (        Car_Equ_Cons_K1_K2==4){
    
    k1k2="m10" # Millero (2010)
  }
  
  
  
  # Ks
  if (Car_Equ_Cons_Ks==1){
    
    ks="d"  # Dickson (1990)
  }
  
  if (Car_Equ_Cons_Ks==2){
    
    ks="k"  # Khoo et al. (1977)
  }
  
  # Total_boron_concentration
  if (Total_boron_concentration==1){
    
    b="l10" # Lee et al. (2010)
  }
  
  if (Total_boron_concentration==2){
    
    b="u74" # Uppstrom (1974) 
  }
  

  Carbonate_parameters=carb ( flag=8, 
                              pH, 
                              TA/1000000, 
                              S= salinity, 
                              T= temperature, 
                              Patm=1, 
                              k1k2=   k1k2,     
                              ks= ks,         
                              b= b,      
                              pHscale="T",       
                              P=pressure/10,
                              warn="n" )
  return( Carbonate_parameters)
  
}
