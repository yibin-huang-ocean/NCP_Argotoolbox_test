
# Output: a data frame containing the cycle number (first column)
# and mixed layer depth (second column)

compute_MLD <- function ( cycle=float_profile_data$cycle,
                                            #    date=float_profile_data$date,
                                                pressure=float_profile_data$pressure,
                                                temperature= float_profile_data$temperature,
                          salinity=float_profile_data$salinity,
                          method= 2 # 1, Temperature threshold; 2. density threshold
){
  
  # Merge the input data 
  float_profile_merge <- data.frame(cycle=cycle,
                                    pressure=pressure,
                                  #  date=date,
                                    temperature= temperature,
                                    salinity=salinity)
  
  float_profile_merge =arrange(  float_profile_merge ,  float_profile_merge$cycle,  float_profile_merge$pressure)
  
  float_profile_merge$density <- swSigmaTheta ( salinity= float_profile_merge$salinity,
                                                 
                                                temperature=    float_profile_merge$temperature, 
                                          10)+1000 ###计算密度
  
  # Create a data frame to store the result
  cycle_MLD= data.frame(cycle=unique(  float_profile_merge$cycle),
                        MLD_m=10)
  
  # Loop for determining the mixed layer depth based on the temperature difference 
  i=1
  for (i in 1:length(  cycle_MLD$cycle)){
    
    cycle_data= subset(  float_profile_merge,
                         float_profile_merge$cycle==cycle_MLD$cycle[i])
    
    cycle_data=arrange(    cycle_data,    cycle_data$pressure)
    
    # Step 2.1 compute the mixed layer depth
    if (method ==1){
      MLD <-  which(cycle_data$temperature<  cycle_data$temperature[10]-0.2 &
                      cycle_data$pressure>10       )[1]
    }
    
    if (method==2){
      MLD <-  which(cycle_data$density>  cycle_data$density[10]+0.03 &
                      cycle_data$pressure>10       )[1]
    }
   
  
     cycle_MLD$MLD_m[i]=    MLD
    
     
   # cycle_MLD$date[i] <- cycle_data$date[1]
    
  }                      
  
  # Only apply the gap-filling if there are more than 2 available data
  if ( Count_No_NA(cycle_MLD$MLD_m)>2  ){
    # Gap-filling the MLD 
    cycle_MLD$MLD_m=      approx(      cycle_MLD$cycle,
                                       cycle_MLD$MLD_m,
                                       cycle_MLD$cycle,rule=2)$y
  }

  
  
  cycle_MLD$MLD_m= floor(cycle_MLD$MLD_m)
  return(  cycle_MLD)
}

